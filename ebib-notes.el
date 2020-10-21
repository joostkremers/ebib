;;; ebib-notes.el --- Part of Ebib, a BibTeX database manager  -*- lexical-binding: t -*-

;; Copyright (c) 2003-2020 Joost Kremers
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

(require 'org-element nil t) ; Load org-element.el if available.

(defgroup ebib-notes nil "Settings for notes files." :group 'ebib)

(define-obsolete-variable-alias 'ebib-notes-use-single-file 'ebib-notes-file)

(defcustom ebib-notes-file nil
  "Name of the notes file.
To use a single file for all notes, set this variable to the full
path of the notes file.  If this variable is nil, Ebib creates
one file per note, provided `ebib-notes-directory' is set."
  :group 'ebib-notes
  :type '(choice (const :tag "Use multiple notes files" nil)
                 (file :tag "Notes File")))

(define-obsolete-variable-alias 'ebib-notes-file-symbol 'ebib-notes-symbol)

(defcustom ebib-notes-symbol "N"
  "Symbol used to indicate the presence of a note for the current entry.
If there is a note for the current entry, this symbol is
displayed in the mode line of the entry buffer after the entry
key."
  :group 'ebib-notes
  :type '(string :tag "Note file symbol"))

(defcustom ebib-notes-directory nil
  "Directory to save notes files to.
Ebib creates notes files based on the entry key using the options
`ebib-notes-directory', `ebib-notes-file-extension' and
`ebib-notes-name-transform-function'.

If this option is nil, the first directory in `ebib-file-search-dirs' is
used.

Note that this option is ignored if `ebib-notes-file' is set."
  :group 'ebib-notes
  :type '(choice (const :tag "Use first of `ebib-file-search-dirs'")
                 (directory :tag "Specify directory")))

(defcustom ebib-notes-file-extension "org"
  "Extension used for notes files.
The extension should be specified without a dot.  Note that this
option is only used for multiple notes files, i.e., when
`ebib-notes-file' is unset."
  :group 'ebib-notes
  :type '(string :tag "Extension"))

(defcustom ebib-notes-name-transform-function nil
  "Function for transforming keys into notes file names.
This only takes effect when multiple notes files are used.  If
this is nil, the function `ebib-name-transform-function' is used
instead."
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
					    (?T . ebib-create-org-description)
                                            (?X . ebib-create-org-title)
					    (?C . ebib-create-org-cite)
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
  "Hook run when a new note is created.
This hook is only used when notes are stored in a common notes
file.  It can be used to prepare the note for display, position
the cursor, etc."
  :group 'ebib-notes
  :type 'hook)

(defcustom ebib-notes-get-ids-function #'ebib-notes-extract-org-ids
  "Function to extract all entry keys for which a note exists.
This function is run once on the common notes file (see
`ebib-notes-file' to extract all the keys of the entries for
which a note exists in the file."
  :group 'ebib-notes
  :type 'function)

(defun ebib-notes-extract-org-ids ()
  "Return a list of all Org CUSTOM_IDs in the current buffer."
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (headline)
      (org-element-property :CUSTOM_ID headline))))

(defcustom ebib-notes-show-note-method 'top-lines
  "Method for showing the note of an entry.
This option controls how the contents of an external note is
shown in the entry buffer.  Possible values are `all' (default),
which displays the entire note in a separate window;`top-lines',
which shows only the first `ebib-notes-display-max-lines' lines
of the note; or nil, which does not show the note at all.  Note
that the value `all' can only be used when `ebib-layout' is set
to `full', whereas the value `top-lines' requires the note file
to be in Org format."
  :group 'ebib-notes
  :type '(choice (const :tag "Show first few lines" top-lines)
                 (const :tag "Show entire note" all)
                 (const :tag "Do not show note" nil)))

(defcustom ebib-notes-display-max-lines 10
  "The number of lines to show of a note in the entry buffer."
  :group 'ebib-notes
  :type 'integer)

(defun ebib--notes-fill-template (key db)
  "Fill out `ebib-notes-template' for KEY in DB.
Return a cons of the new note as a string and a position in this
string where point should be located."
  (let* ((note (ebib-format-template ebib-notes-template ebib-notes-template-specifiers key db))
         (pos (string-match-p ">|<" note)))
    (if pos
        (setq note (replace-regexp-in-string ">|<" "" note))
      (setq pos 0))
    (cons note pos)))

(defun ebib--notes-locate-note (key)
  "Locate the note identified by KEY in the current buffer.
Convert KEY into an identifier using the function associated with
`%K' in `ebib-notes-template-specifiers' and search this
identifier.  If found, return its location as a buffer position,
otherwise return nil.  The search is performed in the current
buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat (regexp-quote (funcall (cdr (assoc ?K ebib-notes-template-specifiers)) key nil)) "$") nil t)))

(defvar ebib--notes-list nil "List of entry keys for which a note exists.")

(defun ebib--notes-has-note (key)
  "Return non-nil if entry KEY has an associated note."
  (or (member key ebib--notes-list)
      (cond
       (ebib-notes-file
        (unless ebib--notes-list
          ;; We need to initialize `ebib--notes-list'.
          (if (not (file-writable-p ebib-notes-file))
              (ebib--log 'error "Could not open the notes file `%s'" ebib-notes-file)
            (with-current-buffer (ebib--notes-buffer)
              (setq ebib--notes-list (funcall ebib-notes-get-ids-function)))))
        (member key ebib--notes-list))
       ((not ebib-notes-file)
        (if (file-readable-p (ebib--create-notes-file-name key))
            (cl-pushnew key ebib--notes-list))))))

(defun ebib--notes-goto-note (key)
  "Find or create a buffer containing the note for KEY.
If `ebib-notes-file' is set, run
`ebib-notes-search-note-before-hook' before locating the note.
Otherwise just open the note file for KEY.

Return a cons of the buffer and the position of the note in the
buffer: in `ebib-notes-file', this is the position of the
Custom_ID of the note; if each note has its own file, the
position is simply 1.

If KEY has no note, return nil."
  (cond
   (ebib-notes-file
    (with-current-buffer (ebib--notes-buffer)
      (run-hooks 'ebib-notes-search-note-before-hook)
      (let ((location (ebib--notes-locate-note key)))
        (when location
          (cons (current-buffer) location)))))
   ((not ebib-notes-file)
    (let* ((filename (expand-file-name (ebib--create-notes-file-name key)))
           (buf (or
                 ;; In case the user created the note already but didn't save
                 ;; the file yet, check if there's a buffer visiting the note
                 ;; file.
                 (get-file-buffer filename)
                 ;; Otherwise try and open the file.
                 (and (file-readable-p filename)
                      (ebib--notes-open-single-note-file filename)))))
      (when buf (cons buf 1))))))

(defun ebib--notes-create-new-note (key db)
  "Create a note for KEY in DB and .
If `ebib-notes-file' is set, add an entry to the bottom of this
file and leave point there.  Otherwise, create a new note file in
`ebib-notes-directory'.

Return a cons of the buffer in which the new note is created and
the position where point should be placed."
  (let (buf pos)
    (cond
     (ebib-notes-file
      (setq buf (ebib--notes-buffer)
            pos (1+ (buffer-size buf))))
     ((not ebib-notes-file)
      (let ((filename (expand-file-name (ebib--create-notes-file-name key))))
        (if (file-writable-p filename)
            (setq buf (ebib--notes-open-single-note-file filename)
                  pos 1)
          (error "[Ebib] Could not create note file `%s' " filename)))))
    (let ((note (ebib--notes-fill-template key db)))
      (with-current-buffer buf
        (goto-char pos)
        (insert (car note))
        (setq pos (+ pos (cdr note)))
        (push key ebib--notes-list)))
    (cons buf pos)))

(defun ebib-notes-display-note-symbol (_field key _db)
  "Return the note symbol for displaying if a note exists for KEY."
  (if (ebib--notes-has-note key)
      (propertize ebib-notes-symbol
                  'face '(:height 0.8 :inherit ebib-link-face)
                  'mouse-face 'highlight)
    (propertize (make-string (string-width ebib-notes-symbol) ?\s)
                'face '(:height 0.8))))

(defun ebib--notes-maybe-update-entry-buffer ()
  "Update the entry buffer if it exists."
  (if (ebib--buffer 'entry)
      (ebib--update-entry-buffer)))

;;; One file per note.

(defun ebib--create-notes-file-name (key)
  "Create a notes filename for KEY.
First, `ebib-notes-name-transform-function' is applied to KEY,
and `ebib-notes-file-extension' is added to it.  Then, the file
name is fully qualified by prepending the directory in
`ebib-notes-directory'."
  (format "%s/%s.%s"
          (or ebib-notes-directory (car ebib-file-search-dirs))
          (funcall (or ebib-notes-name-transform-function
                       ebib-name-transform-function
                       #'identity)
                   key)
          ebib-notes-file-extension))

(defun ebib--notes-open-single-note-file (file)
  "Open the note file for FILE.
Return the buffer but do not select it."
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (add-hook 'after-save-hook 'ebib--notes-maybe-update-entry-buffer nil t))
    buf))

;;; Common notes file.

(let (notes-buffer)
  (defun ebib--notes-buffer ()
    "Return the buffer containing the notes file.
If the file has not been opened yet, open it, creating it if
necessary.  Note that this function assumes that
`ebib-notes-file' is set.  An error is raised if it is
not.  An error is also raised if the location for the notes file
is not accessible to the user."
    (if (buffer-live-p notes-buffer)
        notes-buffer
      (unless ebib-notes-file
        (error "[Ebib] No notes file defined"))
      (unless (file-writable-p ebib-notes-file)
        (error "[Ebib] Cannot read or create notes file"))
      (setq notes-buffer (find-file-noselect ebib-notes-file))
      (with-current-buffer notes-buffer
        (add-hook 'after-save-hook 'ebib--notes-maybe-update-entry-buffer nil t))
      notes-buffer)))

(provide 'ebib-notes)

;;; ebib-notes.el ends here
