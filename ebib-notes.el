;;; ebib-notes.el --- Part of Ebib, a BibTeX database manager  -*- lexical-binding: t -*-

;; Copyright (c) 2003-2024 Joost Kremers
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

(require 'org-element nil t) ; Load org-element.el if available.

(declare-function org-capture-get "org-capture" (prop &optional local))
(declare-function ebib-extract-note-text-default "ext:ebib" (key truncate))

(defgroup ebib-notes nil "Settings for notes files." :group 'ebib)

(defcustom ebib-notes-symbol "N"
  "Symbol used to indicate the presence of a note for the current entry.
If there is a note for the current entry, this symbol is
displayed in the mode line of the entry buffer after the entry
key."
  :group 'ebib-notes
  :type '(string :tag "Note file symbol"))

(defcustom ebib-notes-backend #'ebib-notes-singleton-backend
  "Notes back-end to use.
The value of this option is a function that implements a notes
backend.  The function's signature must be of the following
form: (ACTION &rest PARAMETERS).  ACTION is a keyword indicating
what action to perform, and PARAMETERS is a list of parameters,
which vary based on ACTION.  The following ACTION keywords must
be supported:

 - `:has-note' takes a KEY, and returns non-nil if a note exists
   for the entry.  This function should never raise an error.
   Instead, if the notes system has not been configured, it
   should simply return nil.

 - `:create-note' takes a KEY and a DATABASE and returns a list
   of (BUFFER POINT HOOKS), POINT being the position where the
   note begins and HOOKS a list of hook functions (which can be
   nil).  Ebib displays the buffer, selects it, positions point
   at POINT and runs the functions in HOOKS.  The notes back-end
   can also take care of displaying and selecting the buffer
   itself, in which case this function should return nil.  This
   function may also raise a `user-error' if the notes system has
   not been configured.

 - `:open-note' takes a KEY and opens its note in a buffer,
   returning a list of (BUFFER POINT HOOKS), POINT being some
   position inside the note (not necessarily the start of the
   note), HOOKS a list of hook functions (which can be nil).
   Ebib displays the buffer, positions point at POINT and runs
   the functions in HOOKS.  If KEY does not have a note, or if
   the notes system has not been configured, this function should
   return nil.  It should never raise an error.

 - `:extract-text' takes a KEY and an optional boolean TRUNCATE
   argument and returns the text of the note as a list of lines.
   If TRUNCATE is non-nil, truncate the text at
   `ebib-notes-display-max-lines' of text.  If no note exists for
   KEY, if the notes system has not been configured or if the
   note text cannot be extracted, this function should return
   nil.  It should never raise an error.

 - `:delete-note' takes a key and an optional database, tries to
   delete the note and returns non-nil if the deletion succeeded,
   or nil if it failed.  This function may also raise a
   `user-error' if deletion is not possible or not desired."
  :group 'ebib-notes
  :type '(choice (function-item :tag "Use one file per note" ebib-notes-singleton-backend)
                 (function-item :tag "Use multiple notes per file" ebib-notes-multiple-backend)
                 (function-item :tag "Use Org capture" ebib-notes-org-capture-backend)
                 (function-item :tag "Use Citar" ebib-citar-backend)
                 (function :tag "Use custom back-end")))

(make-obsolete-variable 'ebib-notes-storage "The variable `ebib-notes-storage' is no longer used. See the manual for details." "Ebib 2.50")

(make-obsolete-variable 'ebib-notes-directory "The variable `ebib-notes-directory' is no longer used. See the manual for details." "Ebib 2.50")

(defcustom ebib-notes-locations nil
  "Locations for notes files.
Entries can be files or directories.  Entries should be specified
with their full paths and files should have
`ebib-notes-file-extension' as their extension.  For directories,
all files with `ebib-notes-file-extension' are considered files
that can contain notes.

This option is used for all three built-in notes back-ends.  For
`ebib-notes-singleton-backend', this option should list a single
directory, which is the directory to which all notes are saved.
For the back-ends `ebib-notes-multiple-backend' and
`ebib-notes-org-capture-backend', this option can remain nil if
`ebib-notes-multiple-default-file' is set."
  :group 'ebib-notes
  :type '(repeat (file :tag "Notes location (file or directory)")))

(defcustom ebib-notes-multiple-default-file nil
  "Path to the default notes file.
If `ebib-notes-backend' is set to `ebib-notes-multiple-backend',
set this option to define the file to which notes should be
stored.  If you leave this option unset, you are prompted for the
file to store a new note to.

Note that this file does not need to be listed in
`ebib-notes-locations'."
  :group 'ebib-notes
  :type '(file :tag "Default notes file"))

(make-obsolete-variable 'ebib-notes-default-file 'ebib-notes-multiple-default-file "Ebib 2.50")

(defcustom ebib-notes-file-extension "org"
  "Extension used for notes files.
The extension should be specified without a dot.  Note that this
option is only used if `ebib-notes-storage' is set to
`one-file-per-note'."
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

(defcustom ebib-notes-template "* %T\n:PROPERTIES:\n%K\n:END:\n%%?\n"
  "Template for a note entry in the notes file.
New notes are created on the basis of this template.  The
template can contain format specifiers consisting of a percent
sign and a character.  These specifiers are defined by
`ebib-notes-template-specifiers'.

Note that the `%K' specifier must be present in the template and
should be replaced by an identifier that is unique for the entry.
This identifier is used to retrieve the note.  Without it, Ebib
is not able to determine whether an entry has a note or not.
Note also that `%K` must occur on a line of its own, i.e., must
be surrounded by \\n characters in the template.''

The template can also contain the string \"%%?\" to indicate the
position where the cursor is to be placed when creating a new
note.

If `org-capture' is used to create notes, the template can also
contain format specifiers for `org-capture'; these need to be
preceded by an extra `%', which is stripped before the template
is passed to the `org-capture' mechanism.  In this case, this
option can also be set to a list of templates, each identified by
a key corresponding to a template in `org-capture-templates'."
  :group 'ebib-notes
  :type '(choice (string :tag "Note template")
                 (repeat :tag "List of templates"
                         (cons (string :tag "Key")
                               (string :tag "Note template")))))

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

(defvar ebib--org-current-key nil
  "Key of the current entry when `org-capture' is called from Ebib.")

(defun ebib-notes-create-org-template ()
  "Create an `org-capture' template for a note.
This function should be used in `org-capture-templates' as the
`template' element.  It takes `ebib-notes-template' and converts
it into a suitable template for `org-capture' to use."
  (let* ((key (org-capture-get :key))
         (template (cond
                    ((stringp ebib-notes-template)
                     ebib-notes-template)
                    ((listp ebib-notes-template)
                     (cdr (assoc-string key ebib-notes-template)))
                    (t "* %T\n:PROPERTIES:\n%K\n:END:\n%%?\n"))))
    (ebib-format-template template ebib-notes-template-specifiers ebib--org-current-key ebib--cur-db)))

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
         (pos (string-match-p "\\(>|<\\|%\\?\\)" note)))
    (if pos
        (setq note (replace-regexp-in-string "\\(>|<\\|%\\?\\)" "" note))
      (setq pos 0))
    (cons note pos)))

(defvar ebib--notes-list nil "List of entry keys for which a note exists.")

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

(defun ebib--notes-goto-note (key backend)
  "Find or create a buffer containing the note for KEY.
BACKEND is the notes back-end being used.  It should be one of
the symbols `singleton' or `multiple'.

Return a cons of the buffer and the position of the note in the
buffer: if BACKEND is `mulitple,', this is the position of the
Custom_ID of the note; if BACKEND is `singleton', the
position is set to point.

If KEY has no note, return nil."
  (cond
   ((eq backend 'singleton)
    (let* ((filename (expand-file-name (ebib--notes-singleton-create-file-name key)))
           (buf (or
                 ;; In case the user created the note already but didn't save
                 ;; the file yet, check if there's a buffer visiting the note
                 ;; file.
                 (get-file-buffer filename)
                 ;; Otherwise try and open the file.
                 (and (file-readable-p filename)
                      (ebib--notes-singleton-open-note-file filename)))))
      (when buf (list buf (with-current-buffer buf (point))))))
   ((eq backend 'multiple)
    (catch 'found
      (dolist (file (ebib--notes-multiple-list-files))
        (with-current-buffer (ebib--notes-multiple-get-buffer file)
          (run-hooks 'ebib-notes-search-note-before-hook)
          (let ((location (ebib--notes-locate-note key)))
            (when location
              (throw 'found (list (current-buffer) location ebib-notes-open-note-after-hook))))))))))

(defun ebib--notes-create-new-org-note (key db backend)
  "Create a note for KEY in DB.
BACKEND is the notes back-end being used.  It should be one of
the symbols `singleton' or `multiple'.

Return a cons of the buffer in which the new note is created and
the position where point should be placed."
  (let (buf pos hooks)
    (cond
     ((eq backend 'multiple)
      (setq buf (ebib--notes-multiple-get-buffer (or ebib-notes-multiple-default-file
                                                     (completing-read "Save note to file: " (ebib--notes-multiple-list-files) nil t)))
            pos (1+ (buffer-size buf))
            hooks ebib-notes-new-note-hook))
     ((eq backend 'singleton)
      (let ((filename (expand-file-name (ebib--notes-singleton-create-file-name key))))
        (if (file-writable-p filename)
            (setq buf (ebib--notes-singleton-open-note-file filename)
                  pos 1)
          (error "[Ebib] Could not create note file `%s' " filename)))))
    (let ((note (ebib--notes-fill-template key db)))
      (with-current-buffer buf
        (goto-char pos)
        (insert (car note))
        (setq pos (+ pos (cdr note)))
        (push key ebib--notes-list)))
    (list buf pos hooks)))

(defun ebib-notes-display-note-symbol (_field key _db)
  "Return a string to indicate if a note exists for KEY.
If the entry KEY has an external note, return `ebib-notes-symbol'
propertized with `ebib-link-face'.  Otherwise, return an empty
string of the same width as `ebib-notes-symbol'."
  (if (funcall ebib-notes-backend :has-note key)
      (propertize ebib-notes-symbol
                  'face '(:height 0.8 :inherit ebib-link-face)
		  'font-lock-face '(:height 0.8 :inherit ebib-link-face)
                  'mouse-face 'highlight
		  'help-echo "mouse-1: popup note"
		  'button t
		  'follow-link t
		  'category t
		  'button-data key
		  'keymap button-map
		  'action 'ebib-popup-note)
    (propertize (make-string (string-width ebib-notes-symbol) ?\s)
                'face '(:height 0.8))))

(defun ebib--notes-extract-org-text (key truncate backend)
  "Extract the text of the note for KEY.
The note must be an Org entry under its own headline.

If TRUNCATE is non-nil, the note is truncated at
`ebib-notes-display-max-lines' lines.  If the original text is
longer than that, an ellipsis marker \"[...]\" is added.

BACKEND is the notes back-end being used.  It should be one of
the symbols `singleton' or `multiple'.

The return value is a list of strings, each a separate line,
which can be passed to `ebib--display-multiline-field'."
  (with-temp-buffer
    (cond
     ((eq backend 'singleton)
      (let ((filename (expand-file-name (ebib--notes-singleton-create-file-name key))))
        (when (file-readable-p filename)
          (insert-file-contents filename))))
     ((eq backend 'multiple)
      (let* ((location (ebib--notes-goto-note key backend))
             (buffer (car location))
             (position (cadr location))
             beg end)
        (when location
          (with-current-buffer buffer
            (save-mark-and-excursion
              (goto-char position)
              (org-mark-subtree)
              (setq beg (region-beginning)
                    end (region-end))))
          (insert-buffer-substring buffer beg end)))))
    (let ((truncated nil)
	  string)
      ;; If appropriate, first reduce the size of the text we need to
      ;; pass to `org-element-parse-buffer', since this function can
      ;; be slow if the note is long.
      (when truncate
	(let ((max (progn
                     (goto-char (point-min))
                     (pos-bol (* 2 ebib-notes-display-max-lines)))))
	  (when (< max (point-max))
            (setq truncated t)
            (delete-region max (point-max)))))

      ;; Extract any property drawers.
      (let ((contents (org-element-parse-buffer)))
	(org-element-map contents 'property-drawer
          (lambda (drawer)
            (org-element-extract-element drawer)))
	(erase-buffer)
	(insert (org-element-interpret-data contents)))
      ;; Extract relevant lines
      (let* ((beg (progn
                    (goto-char (point-min))
                    (forward-line 1)	; Skip the headline.
                    (point)))
	     ;; If `truncate', then take the first
	     ;; `ebib-notes-display-max-lines' lines.
             (end (if truncate
		      (progn
			(goto-char (point-min))
			(forward-line (1+ ebib-notes-display-max-lines))
			(point))
		    ;; Otherwise take all lines
		    (point-max))))
        (setq string (buffer-substring-no-properties beg end))
	(if (or truncated
		(< end (point-max)))
            (setq string (concat string "[...]\n"))))
      (split-string string "\n"))))

;;; `ebib-notes-singleton-backend'

(defun ebib--notes-singleton-create-file-name (key)
  "Create a notes filename for KEY.
First, `ebib-notes-name-transform-function' is applied to KEY,
and `ebib-notes-file-extension' is added to it.  Then, the file
name is fully qualified by prepending the directory in
`ebib-notes-locations'."
  (format "%s/%s.%s"
          (or (car ebib-notes-locations) (car ebib-file-search-dirs))
          (funcall (or ebib-notes-name-transform-function
                       ebib-name-transform-function
                       #'identity)
                   key)
          ebib-notes-file-extension))

(defun ebib--notes-singleton-open-note-file (file)
  "Open the note FILE.
Return the buffer but do not select it."
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (add-hook 'after-save-hook 'ebib--update-entry-buffer-keep-note))
    buf))

(defun ebib-notes-singleton-backend (action &rest args)
  "Notes back-end using one file per note.
Execute ACTION given ARGS per `ebib-notes-backend'.  See its doc
string for details."
  (pcase action
    (:has-note (or (member (car args) ebib--notes-list)
                   (if (file-readable-p (ebib--notes-singleton-create-file-name (car args)))
                       (cl-pushnew (car args) ebib--notes-list))))
    (:create-note (ebib--notes-create-new-org-note (car args) (cadr args) 'singleton))
    (:open-note (ebib--notes-goto-note (car args) 'singleton))
    (:extract-text (ebib--notes-extract-org-text (car args) (cadr args) 'singleton))
    (:delete-note (delete-file (expand-file-name (ebib--notes-singleton-create-file-name (car args))))
		  (delete (car args) ebib--notes-list))))

;;; `ebib-notes-multiple-backend'

(defun ebib--notes-multiple-get-buffer (file)
  "Return the buffer containing the notes file FILE.
If the file has not been opened yet, open it, creating it if
necessary.  If FILE cannot be opened, an error is raised."
  (let ((buf (find-buffer-visiting file)))
    (unless buf
      (when (not (file-writable-p file))
        (error "[Ebib] Cannot read or create notes file"))
      (setq buf (find-file-noselect file))
      (with-current-buffer buf
        (add-hook 'after-save-hook 'ebib--update-entry-buffer-keep-note nil t)))
    buf))

(defun ebib--notes-multiple-has-note (key)
  "Return non-nil if entry KEY has an associated note.
This function is only useful if multiple notes are stored in a
file, i.e., with the back-ends `ebib-notes-multiple-backend' and
`ebib-notes-org-capture-backend'."
  (unless ebib--notes-list
    ;; We need to initialize `ebib--notes-list'.
    (setq ebib--notes-list
          (seq-reduce (lambda (lst file)
                        (if (not (file-writable-p file))
                            (ebib--log 'error "Could not open notes file `%s'" file)
                          (with-current-buffer (ebib--notes-multiple-get-buffer file)
                            (append (funcall ebib-notes-get-ids-function) lst))))
                      (ebib--notes-multiple-list-files) '())))
  (member key ebib--notes-list))

(defun ebib--notes-multiple-list-files ()
  "Return a list of notes files.
List all the files in `ebib-notes-locations' and all files in the
directories in `ebib-notes-locations' that have the extension in
`ebib-notes-file-extension'."
  (cond
   ;; If `ebib-notes-locations' is nil, we don't need to do all this, but we
   ;; still need to check `ebib-notes-multiple-default-file' (see below).
   (ebib-notes-locations
    (cl-flet ((list-files (loc)
                (cond
                 ((file-directory-p loc)
                  (directory-files loc 'full (concat (regexp-quote ebib-notes-file-extension) "\\'") 'nosort))
                 ((string= (downcase (file-name-extension loc)) "org")
                  (list loc)))))
      (seq-reduce (lambda (lst loc)
                    (append (list-files loc) lst))
                  ebib-notes-locations (if ebib-notes-multiple-default-file
                                           (list ebib-notes-multiple-default-file)
                                         '()))))
   (ebib-notes-multiple-default-file
    (list ebib-notes-multiple-default-file))))

(defun ebib-notes-multiple-backend (action &rest args)
  "Notes back-end using multiple notes per notes file.
Execute ACTION given ARGS per `ebib-notes-backend'.  See its doc
string for details."
  (pcase action
    (:has-note (ebib--notes-multiple-has-note (car args)))
    (:create-note (ebib--notes-create-new-org-note (car args) (cadr args) 'multiple))
    (:open-note (ebib--notes-goto-note (car args) 'multiple))
    (:extract-text (ebib--notes-extract-org-text (car args) (cadr args) 'multiple))
    (:delete-note (user-error "[Ebib] Deleting a note is not supported with the current notes back-end"))))

;;; `ebib-notes-org-capture-backend'

(defcustom ebib-notes-org-capture-default-template nil
  "Default `org-capture' template to use.
This should be the key of the template as defined in
`org-capture-templates'.  If this option is nil, the user is
presented with the normal Org Capture menu."
  :group 'ebib-notes
  :type '(choice (const :tag "Show org Capture menu" nil)
                 (string :tag "Org Capture template key")))

(make-obsolete-variable 'ebib-notes-use-org-capture 'ebib-notes-org-capture-default-template "Ebib 2.5")

(defun ebib-notes-org-capture-backend (action &rest args)
  "Notes back-end using `org-capture'.
Execute ACTION given ARGS per `ebib-notes-backend'.  See its doc
string for details."
  (pcase action
    (:has-note (ebib--notes-multiple-has-note (car args)))
    (:create-note (let ((ebib--org-current-key (car args)))
                    (org-capture nil ebib-notes-org-capture-default-template)
                    (push (car args) ebib--notes-list)
                    nil))  ; We must return nil, cf. comment in `ebib-popup-note'.
    (:open-note (ebib--notes-goto-note (car args) 'multiple))
    (:extract-text (ebib--notes-extract-org-text (car args) (cadr args) 'multiple))
    (:delete-note (user-error "[Ebib] Deleting a note is not supported with the current notes back-end"))))

(provide 'ebib-notes)

;;; ebib-notes.el ends here
