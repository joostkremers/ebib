;;; ebib-db.el --- Part of Ebib, a BibTeX database manager  -*- lexical-binding: t -*-

;; Copyright (c) 2003-2018 Joost Kremers
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>

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

;; This file is part of Ebib, a BibTeX database manager for Emacs.  It contains
;; the database code.

;;; Code:

(require 'cl-lib)
(require 'bibtex)

;; Each database is represented by a struct.
(cl-defstruct ebib--db-struct
  (database (make-hash-table :test 'equal)) ; Hashtable containing the database itself.
  (strings)                                 ; Alist with the @STRING definitions.
  (preamble)                                ; String with the @PREAMBLE definition.
  (comments)                                ; List of @COMMENTS.
  (local-vars)                              ; The file's local variable block.
  (dialect)                                 ; The dialect of this database.
  (cur-entry)                               ; The current entry.
  (marked-entries)                          ; List of marked entries.
  (filter)                                  ; The active filter.
  (sortinfo)                                ; Custom sorting.
  (filename)                                ; Name of the BibTeX file that holds this database.
  (modtime)                                 ; Modification time of the .bib file.
  (modified)                                ; Flag indicating whether this database has been modified.
  (backup))                                 ; Flag indicating whether we need to make a backup of the .bib file.

(defun ebib-db-new-database ()
  "Create a new database instance and return it."
  (make-ebib--db-struct))

(defun ebib-db-clear-database (db)
  "Remove all information in DB.
The database object itself is retained, only the information in
it is deleted."
  (clrhash (ebib--db-struct-database db))
  (setf (ebib--db-struct-strings db) nil)
  (setf (ebib--db-struct-preamble db) nil)
  (setf (ebib--db-struct-comments db) nil)
  (setf (ebib--db-struct-local-vars db) nil)
  (setf (ebib--db-struct-dialect db) nil)
  (setf (ebib--db-struct-cur-entry db) nil)
  (setf (ebib--db-struct-marked-entries db) nil)
  (setf (ebib--db-struct-filter db) nil)
  (setf (ebib--db-struct-sortinfo db) nil)
  (setf (ebib--db-struct-filename db) nil)
  (setf (ebib--db-struct-modified db) nil)
  (setf (ebib--db-struct-backup db) nil))

(defun ebib-db-count-entries (db)
  "Return the number of entries in DB."
  (hash-table-count (ebib--db-struct-database db)))

(defun ebib-db-get-dialect (db)
  "Return the BibTeX dialect of DB."
  (ebib--db-struct-dialect db))

(defun ebib-db-set-dialect (dialect db)
  "Set DIALECT as the BibTeX dialect of DB."
  (setf (ebib--db-struct-dialect db) dialect))

(defun ebib-db-get-comments (db)
  "Return a list of @COMMENTS for DB."
  (ebib--db-struct-comments db))

(defun ebib-db-set-comment (comment db)
  "Add COMMENT to the list of comments in DB."
  (setf (ebib--db-struct-comments db) (append (ebib--db-struct-comments db) (list comment))))

(defun ebib-db-set-local-vars (vars db)
  "Store VARS as the local variable block of DB.
No check is performed to see if VARS is really a local variable block."
  (setf (ebib--db-struct-local-vars db) vars))

(defun ebib-db-get-local-vars (db)
  "Return the local variable block of DB."
  (ebib--db-struct-local-vars db))

(defun ebib--db-get-current-entry-key (db)
  "Return the key of the current entry in DB."
  (ebib--db-struct-cur-entry db))

(defun ebib-db-set-current-entry-key (key db)
  "Set KEY as the current entry of DB.
KEY is a key in DB.  If KEY is not in DB, the current entry is
set to nil, which signifies that a new current entry should be
set.  It is allowed to pass a value of nil for KEY, to unset the
current entry unconditionally.

Return the new entry key if it could be made the new entry key,
nil otherwise."
  (if (and (stringp key)
           (ebib-db-get-entry key db 'noerror))
      (setf (ebib--db-struct-cur-entry db) key)
    (setf (ebib--db-struct-cur-entry db) nil)))

(defun ebib-db-set-entry (key data db &optional if-exists)
  "Set KEY to DATA in database DB.
DATA is an alist of (FIELD . VALUE) pairs.

IF-EXISTS defines what to do when the key already exists in DB.
If it is `overwrite', replace the existing entry.  If it is `uniquify',
generate a unique key by appending a letter `b', `c', etc. to it.
If it is `noerror', a duplicate key is not stored and the function
returns nil.  If it is nil (or any other value), a duplicate key
triggers an error.

In order to delete an entry, DATA must be nil and IF-EXISTS must be
`overwrite'.

If storing/updating/deleting the entry is successful, return its key."
  (let ((exists (gethash key (ebib--db-struct-database db))))
    (when exists
      (cond
       ;;  If so required, make the entry unique:
       ((eq if-exists 'uniquify)
	(setq key (ebib-db-uniquify-key key db))
	(setq exists nil))
       ;; If the entry is an update, we simply pretend the key does not exist:
       ((eq if-exists 'overwrite)
	(setq exists nil))
       ;; Otherwise signal an error, if so requested:
       ((not (eq if-exists 'noerror))
	(error "[Ebib] Key `%s' exists in database; cannot overwrite" key))))
    (unless exists
      (if data
	  (puthash key data (ebib--db-struct-database db))
	(remhash key (ebib--db-struct-database db)))
      key)))

(defun ebib-db-remove-entry (key db)
  "Remove entry KEY from DB."
  (ebib-db-set-entry key nil db 'overwrite))

(defun ebib-db-get-entry (key db &optional noerror)
  "Return entry KEY in database DB as an alist.
The entry is returned as an alist of (FIELD . VALUE) pairs.
Trigger an error if KEY does not exist, unless NOERROR is T."
  (let ((entry (gethash key (ebib--db-struct-database db))))
    (unless (or entry noerror)
      (error "[Ebib] Entry `%s' does not exist" key))
    entry))

(defun ebib-db-uniquify-key (key db)
  "Return a key based on KEY that is unique in DB.
The key is made unique by suffixing `b' to it.  If that does not
yield a unique key, `c' is suffixed instead, etc., until a unique
key is found.  If suffixing `z' does not yield a unique key, `aa'
is suffixed, then `ab' etc."
  (let* ((suffix ?b)
	 (unique-key (concat key (list suffix))))
    (while (gethash unique-key (ebib--db-struct-database db))
      (setq suffix (1+ suffix))
      (setq unique-key (concat key (list suffix)))
      (when (eq suffix ?z)
	(setq key (concat key "a"))
	(setq suffix ?a)))
    unique-key))

(defun ebib-db-list-keys (db)
  "Return a list of keys in DB."
  (hash-table-keys (ebib--db-struct-database db)))

(defun ebib-db-change-key (key new-key db &optional if-exists)
  "Change entry key KEY to NEW-KEY in DB.
ENTRY must be a key itself.  IF-EXISTS determines what to do when
NEW-KEY already exists.  If it is nil, an error is triggered.  If
it is `noerror', no error is triggered and nothing is updated.  If
it is `overwrite', the existing entry under NEW-KEY is
overwritten.  If it is `uniquify', a unique key is created.

If there is no entry with KEY in DB, an error is triggered.

Return the new key upon succes, or nil if nothing was updated."
  (let* ((data (ebib-db-get-entry key db))
	 (actual-new-key (ebib-db-set-entry new-key data db if-exists)))
    (when actual-new-key
      (ebib-db-remove-entry key db)
      actual-new-key)))

(defun ebib-db-set-field-value (field value key db &optional if-exists nobrace)
  "Set FIELD to VALUE in entry KEY in database DB.

IF-EXISTS determines what to do if the field already exists.  If
it is `overwrite', the existing value is overwritten.  If it is
`noerror', the value is not stored and the function returns nil.
If it is nil (or any other value), an error is raised.

IF-EXISTS can also be the symbol `append' or a string.  In this
case, the new value is appended to the old value, separated by a
space or by the string.  Before appending, braces/double quotes
are removed from both values.

If NOBRACE is t, the value is stored without braces.  If it is
nil, braces are added if not already present.  NOBRACE may also be
the symbol `as-is', in which case the value is stored as is.

A field can be removed from the entry by passing nil as VALUE and
setting IF-EXISTS to 'overwrite.

Return non-nil upon success, or nil if the value could not be stored."
  (if (eq if-exists 'append)
      (setq if-exists " "))
  (let* ((entry (ebib-db-get-entry key db))
	 (elem (assoc-string field entry 'case-fold))
         (old-value (cdr elem)))
    ;; If the field has a value, decide what to do:
    (if old-value
        (cond
         ((eq if-exists 'overwrite)
          (setq old-value nil))
         ((stringp if-exists)
          (setq value (concat (ebib-db-unbrace old-value) if-exists (ebib-db-unbrace value)))
          (setq old-value nil))
         ((not (eq if-exists 'noerror))
          (error "[Ebib] Field `%s' exists in entry `%s'; cannot overwrite" field key)))
      ;; Otherwise add the new field.  We just add the field here, the value is
      ;; added later, so that we can put braces around it if needed.  This also
      ;; makes it easier to return nil when storing/changing the field value
      ;; wasn't successful.  Note that if `elem' is non-nil, we mustn't add the
      ;; field again.  Note also: we use `setcdr' to modify the entry in place.
      (unless elem
        (setq elem (car (setcdr (last entry) (list (cons field nil))))))) ; Make sure `elem' points to the newly added field.
    ;; If there is (still) an old value, do nothing.
    (unless old-value
      ;; Otherwise overwrite the existing entry.  Note that to delete a field,
      ;; we set its value to nil, rather than removing it altogether from the
      ;; database.  In `ebib--display-fields', such fields are ignored, so they're
      ;; not saved.
      (if (and value nobrace)
          (unless (eq nobrace 'as-is)
            (setq value (ebib-db-unbrace value)))
        (setq value (ebib-db-brace value)))
      (setcdr elem value)
      t))) ; Make sure we return non-nil.

(defun ebib-db-remove-field-value (field key db)
  "Remove FIELD from entry KEY in DB."
  (ebib-db-set-field-value field nil key db 'overwrite))

(defun ebib-db-get-field-value (field key db &optional noerror)
  "Return the value of FIELD in entry KEY in database DB.
If FIELD or KEY does not exist, trigger an error, unless NOERROR
is non-nil.  In this case, if NOERROR is a string, return NOERROR,
otherwise return nil."
  (let ((value (cdr (assoc-string field (ebib-db-get-entry key db noerror) 'case-fold))))
    (unless (or value noerror)
      (error "[Ebib] Field `%s' does not exist in entry `%s'" field key))
    (when (and (not value)
               (stringp noerror))
      (setq value noerror))
    value))

(defun ebib-db-set-string (abbr value db &optional if-exists)
  "Set the @string definition ABBR to VALUE in database DB.
If ABBR does not exist, create it.  VALUE is enclosed in braces if
it isn't already.

IF-EXISTS determines what to do when ABBR already exists.  If it
is `overwrite', the new string replaces the existing one.  If it is
`noerror', the string is not stored and the function returns nil.
If it is nil (or any other value), an error is raised.

In order to remove a @STRING definition, pass nil as VALUE and
set IF-EXISTS to `overwrite'."
  (let* ((old-string (ebib-db-get-string abbr db 'noerror))
	 (strings-list (delete (cons abbr old-string) (ebib--db-struct-strings db))))
    (when old-string
      (cond
       ((eq if-exists 'overwrite)
	(setq old-string nil))
       ((not (eq if-exists 'noerror))
	(error "[Ebib] @STRING abbreviation `%s' exists in database %s" abbr (ebib-db-get-filename db 'short)))))
    (unless old-string
      (setf (ebib--db-struct-strings db)
	    (if (null value)
                strings-list
              ;; Put the new string at the end of the list, to keep them in the
              ;; order in which they appear in the .bib file.  This is
              ;; preferable for version control.
              (append strings-list (list (cons abbr (ebib-db-brace value)))))))))

(defun ebib-db-remove-string (abbr db)
  "Remove @STRING definition ABBR ttfrom DB."
  (ebib-db-set-string abbr nil db 'overwrite))

(defun ebib-db-get-string (abbr db &optional noerror unbraced)
  "Return the value of @STRING definition ABBR in database DB.
If ABBR does not exist, trigger an error, unless NOERROR is
non-nil, in which case return nil.  If UNBRACED is non-nil, return
the value without braces."
  ;; I assume abbreviations should be case-sensitive, so I use assoc
  ;; instead of assoc-string here.
  (let ((value (cdr (assoc abbr (ebib--db-struct-strings db)))))
    (unless (or value noerror)
      (error "[Ebib] @STRING abbreviation `%s' does not exist" abbr))
    (if unbraced
        (ebib-db-unbrace value)
      value)))

(defun ebib-db-get-all-strings (db)
  "Return the alist containing all @STRING definitions in DB."
  (ebib--db-struct-strings db))

(defsubst ebib-db-list-strings (db)
  "Return a list of @STRING abbreviations in DB without expansions."
  (mapcar #'car (ebib--db-struct-strings db)))

(defun ebib-db-set-preamble (preamble db &optional if-exists)
  "Set PREAMBLE as the preamble of DB.

IF-EXISTS determines what to do if there already is a preamble:
if its value is `append', PREAMBLE is appended to the existing
text (with a newline and hash in between); if it is `overwrite',
PREAMBLE replaces the existing text.  If it is `noerror', PREAMBLE
is not stored and the function returns nil.  If it is nil (or any
other value), an error is raised.

In order to delete the preamble, PREAMBLE should be nil and
IF-EXISTS should be 'overwrite.

Return non-nil on success or nil if PREAMBLE could not be stored."
  (let ((existing-preamble (ebib-db-get-preamble db)))
    (when existing-preamble
      (cond
       ((eq if-exists 'append)
	(setq preamble (concat existing-preamble "\n# " preamble))
	(setq existing-preamble nil))
       ((eq if-exists 'overwrite)
	(setq existing-preamble nil))))
    (if (not existing-preamble)
	(setf (ebib--db-struct-preamble db) preamble)
      (unless (eq if-exists 'noerror)
	(error "[Ebib] Preamble is not empty; cannot overwrite")))))

(defun ebib-db-remove-preamble (db)
  "Remove the @PREAMBLE definition from DB."
  (ebib-db-set-preamble nil db 'overwrite))

(defun ebib-db-get-preamble (db)
  "Return the preamble of DB.
If DB has no preamble, return nil."
  (ebib--db-struct-preamble db))

(defun ebib-db-set-modified (mod db)
  "Set the modification flag of DB to MOD."
  (setf (ebib--db-struct-modified db) mod))

(defun ebib-db-modified-p (db)
  "Return t if DB has been modified, nil otherwise."
  (ebib--db-struct-modified db))

(defun ebib-db-set-filename (filename db &optional if-exists)
  "Set FILENAME as the filename of DB.
IF-EXISTS determines what to do when the database already has a
filename.  If it is `overwrite', the filename is changed.  If
`noerror', the filename is not changed an nil is returned.  If
IF-EXISTS is nil, an existing filename triggers an error."
  (let ((exists (ebib--db-struct-filename db)))
    (when exists
      (cond
       ((eq if-exists 'overwrite)
	(setq exists nil))
       ((not (eq if-exists 'noerror))
	(error "[Ebib] Database has a filename; cannot overwrite"))))
    (unless exists
      (setf (ebib--db-struct-filename db) filename))))

(defun ebib-db-get-filename (db &optional shortened)
  "Return the filename of DB.
If SHORTENED is non-nil, return only the filename part, otherwise
return the full path."
  (if shortened
      (file-name-nondirectory (ebib--db-struct-filename db))
    (ebib--db-struct-filename db)))

(defun ebib-db-get-modtime (db)
  "Return the mod time stored for DB."
  (ebib--db-struct-modtime db))

(defun ebib-db-set-modtime (modtime db)
  "Set MODTIME of DB."
  (setf (ebib--db-struct-modtime db) modtime))

(defun ebib-db-marked-entries-p (db)
  "Return t if there are marked enries in DB."
  (ebib--db-struct-marked-entries db))

(defun ebib-db-marked-p (entry db)
  "Return t if ENTRY is marked in DB.
ENTRY is an entry key."
  (member entry (ebib--db-struct-marked-entries db)))

(defun ebib-db-mark-entry (entry db)
  "Add ENTRY to the list of marked entries in DB.
ENTRY is an entry key.  ENTRY is added unconditionally, no check
is performed to see if it is already on the list.

ENTRY can also be 'all, in which case all entries are marked."
  (cond
   ((stringp entry)
    (setf (ebib--db-struct-marked-entries db) (cons entry (ebib--db-struct-marked-entries db))))
   ('all
    (setf (ebib--db-struct-marked-entries db) (ebib-db-list-keys db)))))

(defun ebib-db-unmark-entry (entry db)
  "Remove ENTRY from the list of marked entries in DB.
ENTRY is an entry key.  If ENTRY is 'all, all entries are
unmarked."
  (cond
   ((stringp entry)
    (setf (ebib--db-struct-marked-entries db) (remove entry (ebib--db-struct-marked-entries db))))
   ('all
    (setf (ebib--db-struct-marked-entries db) nil))))

(defun ebib-db-toggle-mark (entry db)
  "Toggle the mark on ENTRY in DB."
  (if (ebib-db-marked-p entry db)
      (ebib-db-unmark-entry entry db)
    (ebib-db-mark-entry entry db)))

(defun ebib-db-list-marked-entries (db)
  "Return a list of entry keys of all marked entries in DB."
  (copy-sequence (ebib--db-struct-marked-entries db)))

(defun ebib-db-filtered-p (db)
  "Return t if a filter exists for DB."
  (ebib--db-struct-filter db))

(defun ebib-db-set-filter (filter db)
  "Set FILTER as the filter of DB.
The filter is set unconditionally, overwriting any existing filter."
  (setf (ebib--db-struct-filter db) filter))

(defun ebib-db-get-filter (db)
  "Return the filter of DB."
  (ebib--db-struct-filter db))

(defun ebib-db-set-sortinfo (sortinfo db)
  "Set the SORTINFO of DB.
The sortinfo is set unconditionally, overwriting any existing
sortinfo."
  (setf (ebib--db-struct-sortinfo db) sortinfo))

(defun ebib-db-custom-sorted-p (db)
  "Return t if DB has a custom sort."
  (ebib--db-struct-sortinfo db))

(defun ebib-db-get-sort-field (db)
  "Return the sort field of DB, or nil if there is none."
  (car (ebib--db-struct-sortinfo db)))

(defun ebib-db-get-sort-order (db)
  "Return the sort order of DB, or nil if there is none."
  (cdr (ebib--db-struct-sortinfo db)))

(defun ebib-db-set-backup (backup db)
  "Set BACKUP as the backup flag of DB.
BACKUP must be either t (make backup at next save) or nil (do not
make backup at next save)."
  (setf (ebib--db-struct-backup db) backup))

(defun ebib-db-backup-p (db)
  "Return backup flag of DB."
  (ebib--db-struct-backup db))

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

;; Then, if the first character is {, REMOVE-FROM-STRING takes out every
;; occurrence of the regex "{[^{]*?}", which translates to "the smallest string
;; that starts with { and ends with }, and does not contain another {. IOW, it
;; takes out the innermost braces and their contents.  Because braces may be
;; embedded, we have to repeat this step until no more balanced braces are found
;; in the string. (Note that it would be unwise to check for just the occurrence
;; of { or }, because that would throw EBIB-DB-UNBRACED-P in an infinite loop if
;; a string contains an unbalanced brace.)

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
  "Non-nil if STRING is not enclosed in braces or quotes."
  (cl-flet ((remove-from-string (string remove)
                                (apply #'concat (split-string string remove))))
    (when (stringp string)
      (cond
       ((eq (string-to-char string) ?\{)
        ;; First, remove all escaped { and } from the string:
        (setq string (remove-from-string (remove-from-string string "[\\][{]")
                                         "[\\][}]"))
        ;; Then remove the innermost braces with their contents and continue until
        ;; no more braces are left.
        (while (and (member ?\{ (string-to-list string)) (member ?\} (string-to-list string)))
          (setq string (remove-from-string string "{[^{]*?}")))
        ;; If STRING is not empty, the original string contains material not in braces
        (> (length string) 0))
       ((eq (string-to-char string) ?\")
        ;; Remove escaped ", then remove any occurrences of balanced quotes with
        ;; their contents and check for the length of the remaining string.
        (> (length (remove-from-string (remove-from-string string "[\\][\"]")
                                       "\"[^\"]*?\""))
           0))
       (t t)))))

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

;;; ebib-db.el ends here
