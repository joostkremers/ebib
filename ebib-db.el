;;; ebib-db.el --- Part of Ebib, a BibTeX database manager

;; Copyright (c) 2003-2014 Joost Kremers
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 2003
;; Version: 1.15
;; Keywords: text bibtex

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ; LOSS OF USE,
;; DATA, OR PROFITS ; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; This file is part of Ebib, a BibTeX database manager for Emacs. It
;; contains the database code.

;;; Code:

(eval-when-compile
  (if (string< (format "%d.%d" emacs-major-version emacs-minor-version) "24.3")
      (progn
        (require 'cl)
        (defalias 'cl-defstruct 'defstruct))
    (require 'cl-lib)))
(require 'ebib-utils)

(defcustom ebib-biblatex-inheritance nil
  "Inheritance scheme for cross-referencing.
Inheritances are specified per entry type. The source is the
field name in the cross-referencing entry, the target the field
in the cross-referenced entry.

To define inheritances for all entry types, specify `all' as the
entry type. If you combine inheritances for `all' with
entry-specific inheritances, the latter override the former."
  :group 'ebib
  :type '(repeat (group (string :tag "Entry type")
                        (repeat :tag "Inherited fields"
                                (group (string :tag "Source")
                                       (string :tag "Target"))))))

(defun ebib-remove-from-string (string remove)
  "Remove all the occurrences of REMOVE from STRING.
Return value is the modified string. STRING itself is not
changed. REMOVE can be a regex."
  (apply 'concat (split-string string remove)))

;; each database is represented by a struct
(cl-defstruct ebib-dbstruct
  (database (make-hash-table :test 'equal)) ; hashtable containing the database itself
  (strings)                                 ; alist with the @STRING definitions
  (preamble)                                ; string with the @PREAMBLE definition
  (cur-entry)                               ; the current entry
  (marked-entries)                          ; list of marked entries
  (filter)                                  ; the active filter
  (filename)                                ; name of the BibTeX file that holds this database
  (modified)                                ; flag indicating whether this database has been modified
  (backup))                                 ; flag indicating whether we need to make a backup of the .bib file

(defun ebib-db-new-database ()
  "Creates a new database instance and returns it."
  (make-ebib-dbstruct))

(defun ebib-db-clear-database (db)
  "Remove all information in DB.
The database object itself is retained, only the information in
it is deleted."
  (clrhash (ebib-dbstruct-database db))
  (setf (ebib-dbstruct-strings db) nil)
  (setf (ebib-dbstruct-preamble db) nil)
  (setf (ebib-dbstruct-cur-entry db) nil)
  (setf (ebib-dbstruct-marked-entries db) nil)
  (setf (ebib-dbstruct-filter db) nil)
  (setf (ebib-dbstruct-filename db) nil)
  (setf (ebib-dbstruct-modified db) nil)
  (setf (ebib-dbstruct-backup db) nil))

(defun ebib-db-get-current-entry-key (db)
  "Return the key of the current entry in DB."
  (ebib-dbstruct-cur-entry db))

(defun ebib-db-set-current-entry-key (entry db &optional noerror)
  "Set the current entry in DB to ENTRY.
ENTRY is a key in DB. If ENTRY is not in DB, an error is raised
unless NOERROR is non-NIL. In this case, if NOERROR is 'first,
the current entry key is set to the alphabetically first key in
DB. Any other non-NIL value means do not change the current entry
if ENTRY is not in DB.

ENTRY may also be T, in which case the current entry is
unconditionally set to the alphabetically first entry in DB.

Return the new entry key if successful, NIL otherwise."
   (cond
    ((stringp entry)
     (if (ebib-db-get-entry entry db 'noerror)
         (setf (ebib-dbstruct-cur-entry db) entry)
       (unless noerror
         (error "No entry key `%s' in the current database" entry))
       (if (eq noerror 'first)
           (setf (ebib-dbstruct-cur-entry db) (car (ebib-db-list-keys db))))))
   ((eq entry t)
    (setf (ebib-dbstruct-cur-entry db) (car (ebib-db-list-keys db))))))

(defun ebib-db-set-entry (key data db &optional if-exists)
  "Add or modify entry KEY in database DB.
DATA is an alist of (FIELD . VALUE) pairs.

IF-EXISTS defines what to do when the key already exists in DB.
If it is 'overwrite, replace the existing entry. If it is 'uniquify,
generate a unique key by appending a letter `b', `c', etc. to it.
If it is 'noerror, a duplicate key is not stored and the function
returns NIL. If it is NIL (or any other value), a duplicate key
triggers an error.

In order to delete an entry, DATA must be NIL and IF-EXISTS must be
'overwrite.

If storing/updating/deleting the entry is successful, return its key."
  (let ((exists (gethash key (ebib-dbstruct-database db))))
    (when exists
      (cond
       ;;  if so required, make the entry unique:
       ((eq if-exists 'uniquify)
	(setq key (ebib-db-uniquify-key key db))
	(setq exists nil))
       ;; if the entry is an update, we simply pretend the key does not exist:
       ((eq if-exists 'overwrite)
	(setq exists nil))
       ;; otherwise signal an error, if so requested:
       ((not (eq if-exists 'noerror))
	(error "Ebib: key `%s' exists in database; cannot overwrite" key))))
    (unless exists
      (if data
	  (puthash key data (ebib-dbstruct-database db))
	(remhash key (ebib-dbstruct-database db)))
      key)))

(defun ebib-db-remove-entry (key db)
  "Remove entry KEY from DB."
  (ebib-db-set-entry key nil db 'overwrite))

(defun ebib-db-get-entry (key db &optional noerror)
  "Return entry KEY in database DB as an alist.
The entry is returned as an alist of (FIELD . VALUE) pairs.
Trigger an error if KEY does not exist, unless NOERROR is T."
  (let ((entry (gethash key (ebib-dbstruct-database db))))
    (unless (or entry noerror)
      (error "Ebib: entry `%s' does not exist" key))
    entry))

(defun ebib-db-uniquify-key (key db)
  "Create a unique key in DB from KEY.
The key is made unique by suffixing `b' to it. If that does not
yield a unique key, `c' is suffixed instead, etc. until a unique
key is found. If suffixing `z' does not yield a unique key, `aa'
is suffixed, then `ab' etc."
  (let* ((suffix ?b)
	 (unique-key (concat key (list suffix))))
    (while (gethash unique-key (ebib-dbstruct-database db))
      (setq suffix (1+ suffix))
      (setq unique-key (concat key (list suffix)))
      (when (eq suffix ?z)
	(setq key (concat key "a"))
	(setq suffix ?a)))
    unique-key))

(defun ebib-db-list-keys (db &optional nosort)
  "Return a list of keys in DB.
The list is sorted, unless NOSORT is non-nil."
  (let (keys-list)
    (maphash #'(lambda (key value)
		 (push key keys-list))
	     (ebib-dbstruct-database db))
    (if nosort
        keys-list
      (sort keys-list 'string<))))

(defun ebib-db-change-key (key new-key db &optional if-exists)
  "Change entry key KEY to NEW-KEY in DB.
ENTRY must be a key itself. IF-EXISTS determines what to do when
NEW-KEY already exists. If it is NIL, an error is triggered. If
it is 'noerror, no error is triggered and nothing is updated. If
it is 'overwrite, the existing entry under NEW-KEY is overwritten.
If it is 'uniquify, a unique key is created.

If there is no entry with KEY in DB, an error is triggered.

Returns the new key upon succes, or NIL if nothing was updated."
  (let* ((data (ebib-db-get-entry key db))
	 (actual-new-key (ebib-db-set-entry new-key data db if-exists)))
    (when actual-new-key
      (ebib-db-remove-entry key db)
      actual-new-key)))

(defun ebib-db-set-field-value (field value key db &optional if-exists nobrace)
  "Set FIELD of entry KEY in database DB to VALUE.

IF-EXISTS determines what to do if the field already exists. If
it is 'overwrite, the existing value is overwritten. If it is
'noerror, the value is not stored and the function returns NIL.
If it is NIL (or any other value), an error is raised.

IF-EXISTS can also be the symbol 'append or a string. In this
case, the new value is appended to the old value, separated by a
space or by the string. Before appending, braces/double quotes
are removed from both values.

If NOBRACE is t, the value is stored without braces. If it is
nil, braces are added if not already present. NOBRACE may also be
the symbol `as-is', in which case the value is stored as is.

A field can be removed from the entry by passing NIL as VALUE and
setting IF-EXISTS to 'overwrite.

Return non-NIL upon success, or NIL if the value could not be stored."
  (when (eq if-exists 'append)
    (setq if-exists " "))
  ;; we first retrieve the old value and the alist of all fields with the
  ;; relevant field removed
  (let* ((old-value (ebib-db-get-field-value field key db 'noerror))
	 (fields-list (delete (cons field old-value) (ebib-db-get-entry key db))))
    (when old-value
      (cond
       ((eq if-exists 'overwrite)
	(setq old-value nil))
       ((stringp if-exists)
	(setq value (concat (ebib-db-unbrace old-value) if-exists (ebib-db-unbrace value)))
	(setq old-value nil))
       ((not (eq if-exists 'noerror))
	(error "Ebib: field `%s' exists in entry `%s'; cannot overwrite" field key))))
    ;; if there is an old value, we do nothing
    (unless old-value
      ;; if there is a new value, it's added to FIELDS-LIST; if there isn't, we
      ;; don't need to do anything, because we've already deleted the existing
      ;; field value when we retrieved the entry above.
      (when value
        (if nobrace
            (unless (eq nobrace 'as-is)
              (setq value (ebib-db-unbrace value)))
          (setq value (ebib-db-brace value)))
	(push (cons field value) fields-list))
      (ebib-db-set-entry key fields-list db 'overwrite))))

(defun ebib-db-remove-field-value (field key db)
  "Remove FIELD from entry KEY in DB."
  (ebib-db-set-field-value field nil key db 'overwrite))

(defun ebib-db-get-field-value (field key db &optional noerror unbraced xref)
  "Return the value of FIELD in entry KEY in database DB.
If FIELD or KEY does not exist, trigger an error, unless NOERROR
is non-NIL, in which case return NIL. If UNBRACED is non-NIL,
return the value without braces.

If XREF is non-NIL, the field value may be retrieved from a
cross-referenced entry. The return value is then a list of two
elements. The first is the field value (or NIL if the field has
no value), the second element indicates whether the value was
retrieved from a cross-referenced entry. If so, it is the key of
that entry, if not, the second value is NIL."
  (let ((value (cdr (assoc field (ebib-db-get-entry key db noerror))))
	(xref-key))
    (when (and (not value) xref)
      (setq xref-key (ebib-db-get-field-value 'crossref key db 'noerror 'unbraced))
      (when xref-key
        (let* ((type (ebib-db-get-field-value '=type= xref-key db 'noerror))
               (xref-field (ebib-db-get-xref-field field type)))
          (setq value (ebib-db-get-field-value xref-field xref-key db 'noerror)))))
    (unless (or value noerror)
      (error "Ebib: field `%s' does not exist in entry `%s'" field key))
    (when unbraced
	(setq value (ebib-db-unbrace value)))
    (if xref
	(list value xref-key)
      value)))

(defun ebib-db-get-xref-field (field entry-type)
  "Return the field from which FIELD inherits in ENTRY-TYPE.
For BibTeX, a field simply inherits from the same field in the
cross-referenced entry. BibLaTeX, however, allows complex
inheritance relations, so that e.g., the field `booktitle' may
inherit from the field `title' in the cross-referenced entry.

The inheritance scheme is stored in `ebib-biblatex-inheritance'."
  (let ((inheritances (or (cadr (assoc entry-type ebib-biblatex-inheritance))
                          (cadr (assoc 'all ebib-biblatex-inheritance)))))
    (or (cadr (assoc field inheritances))
        field)))

(defun ebib-db-set-string (abbr value db &optional if-exists)
  "Set the @string definition ABBR in database DB to VALUE.
If ABBR does not exist, create it. VALUE is enclosed in braces if
it isn't already.

IF-EXISTS determines what to do when ABBR already exists. If it
is 'overwrite, the new string replaces the existing one. If it is
'noerror, the string is not stored and the function returns NIL.
If it is NIL (or any other value), an error is raised.

In order to remove a @STRING definition, pass NIL as VALUE and
set IF-EXISTS to 'overwrite."
  (let* ((old-string (ebib-db-get-string abbr db 'noerror))
	 (strings-list (delete (cons abbr old-string) (ebib-dbstruct-strings db))))
    (when old-string
      (cond
       ((eq if-exists 'overwrite)
	(setq old-string nil))
       ((not (eq if-exists 'noerror))
	(error "Ebib: @STRING abbreviation `%s' exists in database %s" abbr (ebib-db-get-filename db 'short)))))
    (unless old-string
      (setf (ebib-dbstruct-strings db)
	    (if (null value)
		strings-list
	      (push (cons abbr (ebib-db-brace value)) strings-list))))))

(defun ebib-db-remove-string (abbr db)
  "Remove @STRING definition from DB."
  (ebib-db-set-string abbr nil db 'overwrite))

(defun ebib-db-get-string (abbr db &optional noerror unbraced)
  "Return the value of @STRING definition ABBR in database DB.
If ABBR does not exist, trigger an error, unless NOERROR is
non-NIL, in which case return NIL. If UNBRACED is non-NIL, return
the value without braces."
  (let ((value (cdr (assoc abbr (ebib-dbstruct-strings db)))))
    (unless (or value noerror)
      (error "Ebib: @STRING abbreviation `%s' does not exist" abbr))
    (if unbraced
        (ebib-db-unbrace value)
      value)))

(defun ebib-db-get-all-strings (db)
  "Return the alist containing all @STRING definitions."
  (ebib-dbstruct-strings db))

(defun ebib-db-list-strings (db &optional nosort)
  "Return a list of @STRING abbreviations (without expansions).
The list is sorted unless NOSORT is non-nil."
  (let ((list (mapcar #'car (ebib-dbstruct-strings db))))
    (if nosort
        list
      (sort list 'string<))))

(defun ebib-db-set-preamble (preamble db &optional if-exists)
  "Set the preamble of DB to PREAMBLE.

IF-EXISTS determines what to do if there already is a preamble:
if its value is 'append, PREAMBLE is appended to the existing
text (with a newline and hash in between); if it is 'overwrite,
PREAMBLE replaces the existing text. If it is 'noerror, PREAMBLE
is not stored and the function returns NIL. If it is NIL (or any
other value), an error is raised.

In order to delete the preamble, PREAMBLE should be NIL and
IF-EXISTS should be 'overwrite.

Return non-NIL on success or NIL if PREAMBLE could not be stored."
  (let ((existing-preamble (ebib-db-get-preamble db)))
    (when existing-preamble
      (cond
       ((eq if-exists 'append)
	(setq preamble (concat existing-preamble "\n# " preamble))
	(setq existing-preamble nil))
       ((eq if-exists 'overwrite)
	(setq existing-preamble nil))))
    (if (not existing-preamble)
	(setf (ebib-dbstruct-preamble db) preamble)
      (unless (eq if-exists 'noerror)
	(error "Ebib: Preamble is not empty; cannot overwrite")))))

(defun ebib-db-remove-preamble (db)
  "Remove the @PREAMBLE definition from DB."
  (ebib-db-set-preamble nil db 'overwrite))

(defun ebib-db-get-preamble (db)
  "Return the preamble of DB.
If DB has no preamble, return NIL."
  (ebib-dbstruct-preamble db))

(defun ebib-db-set-modified (mod db)
  "Set the modification flag of DB to MOD."
  (setf (ebib-dbstruct-modified db) mod))

(defun ebib-db-modified-p (db)
  "Return T if DB has been modified, NIL otherwise."
  (ebib-dbstruct-modified db))

(defun ebib-db-set-filename (filename db &optional if-exists)
  "Set filename of DB to FILENAME.
IF-EXISTS determines what to do when the database already has a
filename. If it is 'overwrite, the filename is changed. If 'noerror,
the filename is not changed an NIL is returned. If IF-EXISTS is
NIL, an existing filename triggers an error."
  (let ((exists (ebib-dbstruct-filename db)))
    (when exists
      (cond
       ((eq if-exists 'overwrite)
	(setq exists nil))
       ((not (eq if-exists 'noerror))
	(error "Ebib: database has a filename; cannot overwrite"))))
    (unless exists
      (setf (ebib-dbstruct-filename db) filename))))

(defun ebib-db-get-filename (db &optional shortened)
  "Return the filename of DB.
If SHORTED is non-NIL, return only the filename part, otherwise
return the full path."
  (if shortened
      (file-name-nondirectory (ebib-dbstruct-filename db))
    (ebib-dbstruct-filename db)))

(defun ebib-db-marked-entries-p (db)
  "Return T if there are marked enries in DB."
  (ebib-dbstruct-marked-entries db))

(defun ebib-db-marked-p (entry db)
  "Return T if ENTRY is marked in DB.
ENTRY is an entry key."
  (member entry (ebib-dbstruct-marked-entries db)))

(defun ebib-db-mark-entry (entry db)
  "Add ENTRY to the list of marked entries in DB.
ENTRY is an entry key. ENTRY is added unconditionally, no check
is performed to see if it is already on the list.

ENTRY can also be 'all, in which case all entries are marked."
  (cond
   ((stringp entry)
    (setf (ebib-dbstruct-marked-entries db) (cons entry (ebib-dbstruct-marked-entries db))))
   ('all
    (setf (ebib-dbstruct-marked-entries db) (ebib-db-list-keys db 'nosort)))))

(defun ebib-db-unmark-entry (entry db)
  "Remove ENTRY from the list of marked entries in DB.
ENTRY is an entry key. If ENTRY is 'all, all entries are
unmarked."
  (cond
   ((stringp entry)
    (setf (ebib-dbstruct-marked-entries db) (remove entry (ebib-dbstruct-marked-entries db))))
   ('all
    (setf (ebib-dbstruct-marked-entries db) nil))))

(defun ebib-db-toggle-mark (entry db)
  "Toggle the mark on ENTRY in DB."
  (if (ebib-db-marked-p entry db)
      (ebib-db-unmark-entry entry db)
    (ebib-db-mark-entry entry db)))

(defun ebib-db-list-marked-entries (db &optional nosort)
  "Return a list of entry keys of all marked entries in DB.
The list is sorted, unless NOSORT is non-nil."
  (if nosort
      (sort (ebib-dbstruct-marked-entries db) #'string<)
    (ebib-dbstruct-marked-entries db)))

(defun ebib-db-filtered-p (db)
  "Return T if a filter exists for DB."
  (ebib-dbstruct-filter db))

(defun ebib-db-set-filter (filter db)
  "Set the filter of DB to FILTER.
The filter is set unconditionally, overwriting any existing filter."
  (setf (ebib-dbstruct-filter db) filter))

(defun ebib-db-get-filter (db)
  "Return the filter of DB."
  (ebib-dbstruct-filter db))

(defun ebib-db-set-backup (backup db)
  "Set the backup flag of DB to BACKUP.
BACKUP must be either T (make backup at next save) or NIL (do not
make backup at next save)."
  (setf (ebib-dbstruct-backup db) backup))

(defun ebib-db-backup-p (db)
  "Return backup flag of DB."
  (ebib-dbstruct-backup db))

;; EBIB-DB-UNBRACED-P determines if STRING is enclosed in braces. Note that
;; we cannot do this by simply checking whether STRING begins with { and
;; ends with } (or begins and ends with "), because something like "{abc} #
;; D # {efg}" would then be incorrectly recognised as braced. So we need to
;; do the following: take out everything that is between braces or quotes,
;; and see if anything is left. If there is, the original string was
;; braced, otherwise it was not.

;; So we first check whether the string begins with { or ". if not, we
;; certainly have an unbraced string. (EBIB-DB-UNBRACED-P recognises this
;; through the default clause of the COND.) If the first character is { or
;; ", we first take out every occurrence of backslash-escaped { and } or ",
;; so that the rest of the function does not get confused over them.

;; Then, if the first character is {, EBIB-REMOVE-FROM-STRING takes out
;; every occurrence of the regex "{[^{]*?}", which translates to "the
;; smallest string that starts with { and ends with }, and does not contain
;; another {. IOW, it takes out the innermost braces and their contents.
;; Because braces may be embedded, we have to repeat this step until no
;; more balanced braces are found in the string. (Note that it would be
;; unwise to check for just the occurrence of { or }, because that would
;; throw EBIB-DB-UNBRACED-P in an infinite loop if a string contains an
;; unbalanced brace.)

;; For strings beginning with " we do the same, except that it is not
;; necessary to repeat this in a WHILE loop, for the simple reason that
;; strings surrounded with double quotes cannot be embedded; i.e.,
;; "ab"cd"ef" is not a valid (BibTeX) string, while {ab{cd}ef} is.

;; Note: because these strings are to be fed to BibTeX and ultimately
;; (La)TeX, it might seem that we don't need to worry about strings
;; containing unbalanced braces, because (La)TeX would choke on them. But
;; the user may inadvertently enter such a string, and we therefore need to
;; be able to handle it. (Alternatively, we could perform a check on
;; strings and warn the user.)

(defun ebib-db-unbraced-p (string)
  "Non-NIL if STRING is not enclosed in braces or quotes."
  (when (stringp string)
    (cond
     ((eq (string-to-char string) ?\{)
      ;; first, remove all escaped { and } from the string:
      (setq string (ebib-remove-from-string (ebib-remove-from-string string "[\\][{]")
				       "[\\][}]"))
      ;; then remove the innermost braces with their contents and continue until
      ;; no more braces are left.
      (while (and (member ?\{ (string-to-list string)) (member ?\} (string-to-list string)))
	(setq string (ebib-remove-from-string string "{[^{]*?}")))
      ;; if STRING is not empty, the original string contains material not in braces
      (> (length string) 0))
     ((eq (string-to-char string) ?\")
      ;; remove escaped ", then remove any occurrences of balanced quotes with
      ;; their contents and check for the length of the remaining string.
      (> (length (ebib-remove-from-string (ebib-remove-from-string string "[\\][\"]")
				     "\"[^\"]*?\""))
	 0))
     (t t))))

(defun ebib-db-unbrace (string)
  "Convert STRING to its unbraced counterpart.
If STRING is already unbraced, do nothing."
  (if (and (stringp string)
           (not (ebib-db-unbraced-p string)))
      (substring string 1 -1)
    string))

(defun ebib-db-brace (string)
  "Put braces around STRING.
If STRING is already braced, do nothing."
  (if (ebib-db-unbraced-p string)
      (concat "{" string "}")
    string))

(provide 'ebib-db)

;;; ebib-db ends here
