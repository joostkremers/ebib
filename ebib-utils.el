;;; ebib-macs.el --- Part of Ebib, a BibTeX database manager

;; Copyright (c) 2003-2014 Joost Kremers
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 2014
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
;; contains general macros.

;;; Code:

;; We sometimes (often, in fact ;-) need to do something with a string, but
;; take special action (or do nothing) if that string is empty.
;; `ebib-ifstring' makes that easier:

(defmacro ebib-ifstring (bindvar then &rest else)
  "Execute THEN only if STRING is nonempty.
Format: (ebib-ifstring (var value) then-form [else-forms]) VAR is bound
to VALUE, which is evaluated. If VAR is a nonempty string,
THEN-FORM is executed. If VAR is either \"\" or nil, ELSE-FORM is
executed. Returns the value of THEN or of ELSE."
  (declare (indent 2))
  `(let ,(list bindvar)
     (if (not (or (null ,(car bindvar))
                  (equal ,(car bindvar) "")))
         ,then
       ,@else)))

(defmacro ebib-last1 (lst &optional n)
  "Returns the last (or Nth last) element of LST."
  `(car (last ,lst ,n)))

;; we sometimes need to walk through lists.  these functions yield the
;; element directly preceding or following ELEM in LIST. in order to work
;; properly, ELEM must be unique in LIST, obviously. if ELEM is the
;; first/last element of LIST, or if it is not contained in LIST at all,
;; the result is nil.
(defun ebib-next-elem (elem list)
  (cadr (member elem list)))

(defun ebib-prev-elem (elem list)
  (if (or (equal elem (car list))
          (not (member elem list)))
      nil
    (ebib-last1 list (1+ (length (member elem list))))))

(defun ebib-locate-bibfile (file &optional dirs)
  "Locate and/or expand FILE to an absolute filename.
First try to locate BibTeX file FILE with `locate-file' and with
`ebib-bibtex-extensions' as possible suffixes. If this does not
yield a result, expand FILE with `expand-file-name', adding the
first extension in `ebib-bibtex-extensions' if FILE has no
filename suffix."
  (or (locate-file file (or dirs "/") (append '("") ebib-bibtex-extensions))
      (expand-file-name (if (file-name-extension file)
                            file
                          (concat file (car ebib-bibtex-extensions))))))

(defun ebib-ensure-extension (filename ext)
  "Make sure FILENAME has an extension.
Return FILENAME if it alread has an extension, otherwise return
FILENAME appended with EXT. Note that EXT should start with a
dot."
  (if (file-name-extension filename)
      filename
    (concat filename ext)))

(defmacro with-ebib-buffer-writable (&rest body)
  "Makes the current buffer writable and executes the commands in BODY.
After BODY is executed, the buffer modified flag is unset."
  (declare (indent defun))
  `(let ((modified (buffer-modified-p)))
     (unwind-protect
         (let ((buffer-read-only nil))
           ,@body)
       (set-buffer-modified-p modified))))

(defmacro with-ebib-window-nondedicated (&rest body)
  "Execute BODY with the current window non-dedicated.
Restore the dedicated status after executing BODY."
  (declare (indent defun))
  `(let ((dedicated (window-dedicated-p)))
     (unwind-protect
         (progn
           (set-window-dedicated-p (selected-window) nil)
           ,@body)
       (if dedicated
           (set-window-dedicated-p (selected-window) t)))))

;; XEmacs doesn't know about propertize...
(if (not (fboundp 'propertize))
    (defun propertize (string &rest properties)
      "Return a copy of STRING with text properties added.
First argument is the string to copy.  Remaining arguments form a
sequence of PROPERTY VALUE pairs for text properties to add to
the result."
      (let ((new-string (copy-sequence string)))
        (add-text-properties 0 (length new-string) properties new-string)
        new-string)))

;; TODO decide what to do with this.
;; (defun region-active ()
;;   (if (featurep 'xemacs)
;;       (region-active-p)
;;     mark-active))

(defun ebib-remove-from-string (string remove)
  "Returns a copy of STRING with all the occurrences of REMOVE taken out.
REMOVE can be a regex."
  (apply 'concat (split-string string remove)))

(defun ebib-multiline-p (string)
  "True if STRING is multiline."
  (if (stringp string)
      (string-match "\n" string)))

(defun ebib-first-line (string)
  "Returns the first line of a multi-line string."
  (string-match "\n" string)
  (substring string 0 (match-beginning 0)))

(defun ebib-sort-in-buffer (limit str)
  "Moves POINT to the right position to insert STR in a buffer with lines sorted A-Z."
  (let ((upper limit)
        middle)
    (when (> limit 0)
      (let ((lower 0))
        (goto-char (point-min))
        (while (progn
                 (setq middle (/ (+ lower upper 1) 2))
                 (goto-char (point-min))
                 (forward-line (1- middle)) ; if this turns out to be where we need to be,
                 (beginning-of-line)        ; this puts POINT at the right spot.
                 ;; if upper and lower differ by only 1, we have found the
                 ;; position to insert the entry in.
                 (> (- upper lower) 1))
          (save-excursion
            (let ((beg (point)))
              (end-of-line)
              (if (string< (buffer-substring-no-properties beg (point)) str)
                  (setq lower middle)
                (setq upper middle)))))))))

(defun ebib-match-all-in-string (match-str string)
  "Highlights all the matches of MATCH-STR in STRING.
The return value is a list of two elements: the first is the
modified string, the second either t or nil, indicating whether a
match was found at all."
  (cl-do ((counter 0 (match-end 0)))
      ((not (string-match match-str string counter)) (cl-values string (not (= counter 0))))
    (add-text-properties (match-beginning 0) (match-end 0) '(face highlight) string)))

(defun ebib-looking-at-goto-end (str &optional match)
  "Like `looking-at' but moves point to the end of the matching string.
MATCH acts just like the argument to MATCH-END, and defaults to
0."
  (or match (setq match 0))
  (let ((case-fold-search t))
    (if (looking-at str)
        (goto-char (match-end match)))))

;; this needs to be wrapped in an eval-and-compile, to keep Emacs from
;; complaining that ebib-execute-helper isn't defined when it compiles
;; ebib-execute-when.
(eval-and-compile
  (defun ebib-execute-helper (env)
    "Helper function for `ebib-execute-when'."
    (cond
     ((eq env 'entries)
      'ebib-cur-keys-list)
     ((eq env 'marked-entries)
      '(and ebib-cur-db
            (ebib-db-marked-entries-p ebib-cur-db)))
     ((eq env 'database)
      'ebib-cur-db)
     ((eq env 'real-db)
      '(and ebib-cur-db
            (not (ebib-db-get-filter ebib-cur-db))))
     ((eq env 'filtered-db)
      '(and ebib-cur-db
            (ebib-db-get-filter ebib-cur-db)))
     ((eq env 'no-database)
      '(not ebib-cur-db))
     (t t))))

(defmacro ebib-execute-when (&rest forms)
  "Macro to facilitate writing Ebib functions.
This functions essentially like a `cond' clause: the basic format
is (ebib-execute-when FORMS ...), where each FORM is built up
as (ENVIRONMENTS BODY). ENVIRONMENTS is a list of symbols (not
quoted) that specify under which conditions BODY is to be
executed. Valid symbols are:

entries: execute when there are entries in the database,
marked-entries: execute when there are marked entries in the database,
database: execute if there is a database,
no-database: execute if there is no database,
real-db: execute when there is a database and it is not filtered,
filtered-db: execute when there is a database and it is filtered,
default: execute if all else fails.

Just like with `cond', only one form is actually executed, the
first one that matches. If ENVIRONMENT contains more than one
condition, BODY is executed if they all match (i.e., the
conditions are AND'ed.)"
  (declare (indent defun))
  `(cond
    ,@(mapcar #'(lambda (form)
                  (cons (if (= 1 (length (car form)))
                            (ebib-execute-helper (caar form))
                          `(and ,@(mapcar #'(lambda (env)
                                              (ebib-execute-helper env))
                                          (car form))))
                        (cdr form)))
              forms)))

;; the numeric prefix argument is 1 if the user gave no prefix argument at
;; all. the raw prefix argument is not always a number. so we need to do
;; our own conversion.
(defun ebib-prefix (num)
  (when (numberp num)
    num))

(defun ebib-called-with-prefix ()
  "Returns T if the command was called with a prefix key."
  (if (featurep 'xemacs)
      (member (character-to-event ebib-prefix-key) (append (this-command-keys) nil))
    (member (event-convert-list (list ebib-prefix-key))
            (append (this-command-keys-vector) nil))))

(defmacro ebib-called-interactively-p ()
  "Returns T if the command was called interactively.
This is a compatibility macro for Emacs 23, in which
called-interactively-p doesn't take an argument, while in Emacs
24, it takes one obligatory argument."
  (if (< emacs-major-version 24)
      '(interactive-p)
    '(called-interactively-p 'any)))

(defmacro ebib-export-to-db (num message copy-fn)
  "Exports data to another database.
NUM is the number of the database to which the data is to be copied.

MESSAGE is a string displayed in the echo area if the export was
succesful. It must contain a %d directive, which is used to
display the database number to which the entry was exported.

COPY-FN is the function that actually copies the relevant
data. It must take as argument the database to which the data is
to be copied. COPY-FN must return T if the copying was
successful, and NIL otherwise."
  (let ((goal-db (make-symbol "goal-db")))
    `(let ((,goal-db (nth (1- ,num) ebib-databases)))
       (if (not ,goal-db)
           (error "Database %d does not exist" ,num)
         (when (funcall ,copy-fn ,goal-db)
           (ebib-set-modified t ,goal-db)
           (message ,message ,num))))))

(defmacro ebib-export-to-file (prompt-string message insert-fn)
  "Exports data to a file.
PROMPT-STRING is the string that is used to ask for the filename
to export to. INSERT-FN must insert the data to be exported into
the current buffer: it is called within a `with-temp-buffer',
whose contents is appended to the file the user enters.

MESSAGE is shown in the echo area when the export was
successful. It must contain a %s directive, which is used to
display the actual filename."
  (let ((filename (make-symbol "filename")))
    `(let ((insert-default-directory (not ebib-export-filename)))
       (ebib-ifstring (,filename (read-file-name
                           ,prompt-string "~/" nil nil ebib-export-filename))
           (with-temp-buffer
             (funcall ,insert-fn)
             (append-to-file (point-min) (point-max) ,filename)
             (setq ebib-export-filename ,filename))))))

(defun ebib-get-obl-fields (entry-type)
  "Returns the obligatory fields of ENTRY-TYPE."
  (nth 1 (assoc entry-type ebib-entry-types)))

(defun ebib-get-opt-fields (entry-type)
  "Returns the optional fields of ENTRY-TYPE."
  (nth 2 (assoc entry-type ebib-entry-types)))

(defun ebib-get-all-fields (entry-type)
  "Returns all the fields of ENTRY-TYPE as a list.
The first element in the list is the symbol `=type='."
  (cons '=type= (append (ebib-get-obl-fields entry-type)
                       (ebib-get-opt-fields entry-type)
                       ebib-additional-fields)))

(defun ebib-get-extra-fields (entry)
  "Return an alist of extra fields and values of ENTRY.
Extra fields are those fields that are not part of the definition
of the entry type of ENTRY and are also not defined as additional
fields. ENTRY is an alist representing a BibTeX entry."
  (let ((fields (ebib-get-all-fields (cdr (assq '=type= entry)))))
    (cl-remove-if #'(lambda (elt)
                      (memq (car elt) fields))
                  entry)))

;; This is simply to save some typing.
(defun ebib-cur-entry-key ()
  "Get the key of the current entry."
  (ebib-db-get-current-entry-key ebib-cur-db))

(defun ebib-erase-buffer (buffer)
  (with-current-buffer buffer
    (with-ebib-buffer-writable
      (erase-buffer))))

(provide 'ebib-macs)

;;; ebib-macs ends here
