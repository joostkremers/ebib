;;; ebib-utils.el --- Part of Ebib, a BibTeX database manager  -*- lexical-binding: t -*-

;; Copyright (c) 2003-2016 Joost Kremers
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 2016
;; Version: 2.6
;; Keywords: text bibtex
;; Package-Requires: ((dash "2.5.0") (emacs "24.3"))

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
;; the code for managing notes files.

;;; Code:

(require 'ebib-utils)
(require 'ebib-db)

(defgroup ebib-notes nil "Settings for notes files." :group 'ebib)

(defcustom ebib-notes-use-single-file nil
  "Name of the notes file.
To use a single file for all notes, set this variable to the full
path of the notes file.  If this variable is nil, Ebib creates
one file per note, using the options `ebib-notes-directory',
`ebib-notes-file-extension' and
`ebib-notes-name-transform-function'."
  :group 'ebib-notes
  :type '(choice (const :tag "Use multiple notes files" nil)
                 (file :tag "Notes File")))

(defcustom ebib-notes-symbol "[N]"
  "Symbol used to indicate the presence of a note for the current entry.
If there is a note for the current entry, this symbol is
displayed in the mode line of the entry buffer after the entry
key."
  :group 'ebib-notes
  :type '(string :tag "Note file symbol"))

(define-obsolete-variable-alias 'ebib-notes-file-symbol 'ebib-notes-symbol)

(defcustom ebib-notes-directory nil
  "Directory to save notes files to.
If this is nil, the first directory in `ebib-file-search-dirs' is
used.  This option is ignored if `ebib-notes-use-single-file' is set."
  :group 'ebib-notes
  :type '(choice (const :tag "Use first of `ebib-file-search-dirs'")
                 (directory :tag "Specify directory")))

(defcustom ebib-notes-file-extension "org"
  "Extension used for notes files.
The extension should be specified without a dot.  Note that this
option is only used for multiple notes files, i.e., when
`ebib-notes-use-single-file' is unset."
  :group 'ebib-notes
  :type '(string :tag "Extension"))

(defcustom ebib-notes-name-transform-function nil
  "Function for transforming keys into notes file names.
This only takes effect files when multiple notes files are used.
If this is nil, the function `ebib-name-transform-function' is
used instead."
  :group 'ebib-notes
  :type '(choice (const :tag "Use `ebib-name-transform-function'" nil)
                 (function :tag "Apply function")))

(defcustom ebib-notes-template "* %T\n:PROPERTIES:\n%K\n:END:\n>|<\n"
  "Template for a note entry in the notes file.
New notes are created on the basis of this template, which can
contain the following directives:

%T  : the title of the entry
%K  : the unique identifier of the note
>|< : the position of the cursor

If all notes are stored in a single notes file, the %K directive
must be present in the template, since Ebib uses it to identify
the note.  Without it, Ebib cannot determine whether an entry
already has a note or not.  The identifier is created on the
basis of the entry key using the function in the option
`ebib-notes-identifier-function'.  The %T directive is replaced
with the title of the note, which is created using the function
in `ebib-notes-title-function'.  The sequence '>|<' is removed
from the template and the cursor will be positioned in its
place."
  :group 'ebib-notes
  :type '(string :tag "Note template"))

(defcustom ebib-notes-title-function 'ebib-notes-create-org-title
  "Function to create the title for a notes entry.
This function is used to fill the %T directive in
`ebib-notes-template'.  It should take one argument, the key of
the entry for which a title is to be created."
  :group 'ebib-notes
  :type 'function)

(defun ebib-notes-create-org-title (key)
  "Return a title for an orgmode note for KEY.
The title is formed from the author(s) or editor(s) of the entry,
its year and its title.  Newlines are removed from the resulting
string."
  (let ((author (or (ebib-db-get-field-value "author" key ebib--cur-db 'noerror 'unbraced 'xref)
                    (ebib-db-get-field-value "editor" key ebib--cur-db 'noerror 'unbraced 'xref)
                    "(No Author)"))
        (year (or (ebib-db-get-field-value "year" key ebib--cur-db 'noerror 'unbraced 'xref)
                  "????"))
        (title (or (ebib-db-get-field-value "title" key ebib--cur-db 'noerror 'unbraced 'xref)
                   "(No Title)")))
    (replace-regexp-in-string "\n" "" (format "%s (%s): %s" author year title))))

(defcustom ebib-notes-identifier-function 'ebib-notes-create-org-identifier
  "Function to create the identifier of a note.
This function should take the key of the entry as argument and
should return as string that uniquely identifies the entry in the
notes file."
  :group 'ebib-notes
  :type 'function)

(defun ebib-notes-create-org-identifier (key)
  "Create a unique identifier for KEY for use in an org file.
The key is prepended with the string \"Custom_id:\", so that it
can be used in a :PROPERTIES: block."
  (format ":Custom_id: %s" key))

(defcustom ebib-notes-search-note-before-hook '(widen)
  "Hook run before searching for a note.
This hook is only used when notes are stored in a common notes
file.  It can be used to prepare the buffer for searching the
note.

This hook is also run when a new note is being created."
  :group 'ebib
  :type 'hook)

(defcustom ebib-notes-open-note-after-hook '(org-back-to-heading org-narrow-to-subtree)
  "Hook run after a note is found.
This hook is only used when notes are stored in a common notes
file.  It can be used to position the cursor after the note has
been found.

This hook is not run when a new note is created, see
`ebib-notes-new-note-hook'."
  :group 'ebib-notes
  :type 'hook)

(defcustom ebib-notes-new-note-hook '(org-narrow-to-subtree)
  "Hook run when a new note is created."
  :group 'ebib-notes
  :type 'hook)

(defun ebib--notes-create-new-note-from-template (key)
  "Create a new note for KEY.
Return a cons of the new note as a string and a position in this
string where point should be located."
  (let* ((note (format-spec ebib-notes-template
                            `((?K . ,(funcall ebib-notes-identifier-function key))
                              (?T . ,(funcall ebib-notes-title-function key)))))
         (point (string-match-p ">|<" note)))
    (if point
        (setq note (replace-regexp-in-string ">|<" "" note))
      (setq point 0))
    (cons note point)))

(defun ebib--notes-exists-note (key)
  "Return t if a note exists for KEY."
  (if ebib-notes-use-single-file
      (with-current-buffer (ebib--notes-buffer)
        (ebib--notes-locate-note key))
    (file-readable-p (ebib--create-notes-file-name key))))

;;; one file per note

(defun ebib--create-notes-file-name (key)
  "Create a notes filename for KEY.
First, `ebib-notes-name-transform-function' is applied to KEY,
and `ebib-notes-file-extension' is added to it.  Then, the file
name is fully qualified by prepending the directory in
`ebib-notes-directory'."
  (format "%s/%s.%s"
          (or ebib-notes-directory (car ebib-file-search-dirs))
          (funcall (or ebib-notes-name-transform-function
                       ebib-name-transform-function)
                   key)
          ebib-notes-file-extension))

(defun ebib--notes-open-notes-file-for-entry (key)
  "Open or create a notes file for KEY."
  (let* ((filename (expand-file-name (ebib--create-notes-file-name (ebib--cur-entry-key))))
         (new (not (file-exists-p filename))))
    (if (not (file-writable-p filename))
        (error "[Ebib] Could not create file `%s' " filename))
    (ebib-lower)
    (find-file filename)
    (if new
        (let ((initial-contents (ebib--notes-create-new-note-from-template key)))
          (insert (car initial-contents))
          (forward-char (cdr initial-contents))))))

;;; common notes file

(defun ebib--notes-buffer ()
  "Return the buffer containing the notes file.
If the file has not been opened yet, open it, creating it if
necessary.  Note that this function assumes that the
`ebib-notes-use-single-file' is set and will raise an error if not.  An
error is also raised if the location for the notes file is not
accessible to the user."
  (unless ebib-notes-use-single-file
    (error "[Ebib] No notes file defined"))
  (if (not (file-writable-p ebib-notes-use-single-file))
      (error "[Ebib] Cannot read or create notes file"))
  (find-file-noselect ebib-notes-use-single-file))

(defun ebib--notes-locate-note (key)
  "Locate the note identified by KEY.
Convert KEY into an identifier using the function in
`ebib-notes-identifier-function' and search this identifier.  If
found, return its location as a buffer position, otherwise return
nil.  The search is performed in the current buffer, so the notes
buffer must be made active before calling this function.

This function also runs `ebib-notes-search-note-before-hook'."
  (run-hooks 'ebib-notes-search-note-before-hook)
  (save-excursion
    (goto-char (point-min))
    (search-forward (funcall ebib-notes-identifier-function key) nil t)))

(defun ebib--notes-open-common-notes-file (key)
  "Open the notes file to entry KEY or create a new note."
  (let ((buf (ebib--notes-buffer)))
    (with-current-buffer buf
      (let ((location (ebib--notes-locate-note key)))
        (if location
            (progn
              (goto-char location)
              (run-hooks 'ebib-notes-open-note-after-hook))
          (goto-char (point-max))
          (let ((new-note (ebib--notes-create-new-note-from-template key))
                (beg (point)))
            (insert (car new-note))
            (goto-char (+ beg (cdr new-note)))
            (run-hooks 'ebib-notes-new-note-hook)))))
    (ebib-lower)
    (switch-to-buffer buf)))

(provide 'ebib-notes)

;;; ebib-notes.el ends here
