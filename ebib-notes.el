;;; ebib-notes.el --- Part of Ebib, a BibTeX database manager  -*- lexical-binding: t -*-

;; Copyright (c) 2003-2016 Joost Kremers
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

(defcustom ebib-notes-symbol "N"
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
New notes are created on the basis of this template.  The
template can contain format specifiers consisting of a percent
sign and a character.  These specifiers are defined by
`ebib-notes-template-specifiers'.  Note that the `%K' specifier
must be present in the template and should be replaced by an
identifier that is unique for the entry.  This identifier is used
to retrieve the note.  Without it, Ebib is not able to determine
whether an entry has a note or not.

The template can also contain the string \">|<\" to indicate the
position where the cursor is to be placed when creating a new
note."
  :group 'ebib-notes
  :type '(string :tag "Note template"))

(defcustom ebib-notes-template-specifiers '((?K . ebib-create-org-identifier)
                                        (?T . ebib-create-org-title)
                                        (?L . ebib-create-org-link)
                                        (?F . ebib-create-org-file-link)
                                        (?D . ebib-create-org-doi-link)
                                        (?U . ebib-create-org-url-link))
  "Specifiers used in `ebib-notes-template'.
Each specifier consists of a character (which is preceded by a
percent sign in `ebib-notes-template') and a symbol, which
either names a function to be executed or a variable, which
should hold a string.  If a function, it should take two
arguments, the entry key and the database, and should return a
string that is substituted for the specifier in the template.

Note that the `K' specifier should not be removed, since it is
used to create an identifier for the note."
  :group 'ebib-notes
  :type '(repeat (cons :tag "Specifier"
                       (character :tag "Character")
                       (symbol :tag "Function or variable"))))

(defcustom ebib-notes-search-note-before-hook '(widen)
  "Hook run before searching for a note.
This hook is only used when notes are stored in a common notes
file.  It can be used to prepare the buffer for searching the
note.

This hook is also run when a new note is being created."
  :group 'ebib
  :type 'hook)

(defcustom ebib-notes-open-note-after-hook '(org-back-to-heading org-narrow-to-subtree org-show-subtree)
  "Hook run after a note is found.
This hook is only used when notes are stored in a common notes
file.  It can be used to prepare the note for display, position
the cursor, etc.

This hook is not run when a new note is created, see
`ebib-notes-new-note-hook'."
  :group 'ebib-notes
  :type 'hook)

(defcustom ebib-notes-new-note-hook '(org-narrow-to-subtree)
  "Hook run when a new note is created."
  :group 'ebib-notes
  :type 'hook)

(defun ebib--notes-fill-template (key db)
  "Create a new note for KEY in DB.
Return a cons of the new note as a string and a position in this
string where point should be located."
  (let* ((note (ebib-format-template ebib-notes-template ebib-notes-template-specifiers key db))
         (point (string-match-p ">|<" note)))
    (if point
        (setq note (replace-regexp-in-string ">|<" "" note))
      (setq point 0))
    (cons note point)))

(defun ebib--notes-exists-note (key)
  "Return t if a note exists for KEY."
  (if ebib-notes-use-single-file
      (if (file-writable-p ebib-notes-use-single-file)
          (with-current-buffer (ebib--notes-buffer)
            (ebib--notes-locate-note key)))
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

(defun ebib--notes-open-notes-file-for-entry (key db)
  "Open or create a notes file for KEY in DB."
  (let* ((filename (expand-file-name (ebib--create-notes-file-name key)))
         (new (not (file-exists-p filename))))
    (if (not (file-writable-p filename))
        (error "[Ebib] Could not create file `%s' " filename))
    (ebib-lower)
    (find-file filename)
    (if new
        (let ((note (ebib--notes-fill-template key db)))
          (insert (car note))
          (forward-char (cdr note))))))

;;; common notes file

(defun ebib--notes-buffer ()
  "Return the buffer containing the notes file.
If the file has not been opened yet, open it, creating it if
necessary.  Note that this function assumes that
`ebib-notes-use-single-file' is set.  An error is raised if it is
not.  An error is also raised if the location for the notes file
is not accessible to the user."
  (unless ebib-notes-use-single-file
    (error "[Ebib] No notes file defined"))
  (unless (file-writable-p ebib-notes-use-single-file)
    (error "[Ebib] Cannot read or create notes file"))
  (find-file-noselect ebib-notes-use-single-file))

(defun ebib--notes-locate-note (key)
  "Locate the note identified by KEY.
Convert KEY into an identifier using the function associated with
`%K' in `ebib-notes-template-specifiers' and search this
identifier.  If found, return its location as a buffer position,
otherwise return nil.  The search is performed in the current
buffer, so the notes buffer must be made active before calling
this function.

This function also runs `ebib-notes-search-note-before-hook'."
  (run-hooks 'ebib-notes-search-note-before-hook)
  (save-excursion
    (goto-char (point-min))
    (search-forward (funcall (cdr (assoc ?K ebib-notes-template-specifiers)) key nil) nil t)))

(defun ebib--notes-open-common-notes-file (key db)
  "Open the notes file for entry KEY in DB or create a new note."
  (let ((buf (ebib--notes-buffer)))
    (with-current-buffer buf
      (let ((location (ebib--notes-locate-note key)))
        (if location
            (progn
              (goto-char location)
              (run-hooks 'ebib-notes-open-note-after-hook))
          (goto-char (point-max))
          (let ((new-note (ebib--notes-fill-template key db))
                (beg (point)))
            (insert (car new-note))
            (goto-char (+ beg (cdr new-note)))
            (run-hooks 'ebib-notes-new-note-hook)))))
    (ebib-lower)
    (switch-to-buffer buf)))

(provide 'ebib-notes)

;;; ebib-notes.el ends here
