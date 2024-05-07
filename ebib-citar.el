;;; ebib-citar.el --- Ebib Notes from Citar          -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Samuel W. Flint
;; All rights reserved.

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; Maintainer: Samuel W. Flint <me@samuelwflint.com>

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

;; This file is part of Ebib, a BibTeX database manager for Emacs.  It
;; contains functions that integrate [citar] with Ebib.
;;
;; [citar]: https://github.com/emacs-citar/citar
;;
;; To use this code, `require' it in your init file, and enable
;; `ebib-citar-mode'.  This mode will use `citar' to manage notes, and
;; set `citar-open-entry-function' to open entries in Ebib.

;;; Code:

(require 'ebib)
(require 'citar)

(defun ebib-citar-backend (operation &rest args)
  "Citar backend for ebib notes.
Execute OPERATION given ARGS per `ebib-notes-storage', which see."
  (pcase operation
    (`:has-note
     (let ((has-notes-p (citar-has-notes)))
       (funcall has-notes-p (car args))))
    (`:create-note
     (citar-create-note (car args))
     (ebib-citar-backend :open-note (list (car args))))
    (`:open-note
     (when-let* ((notes-file (car (last (citar-ebib-backend :has-note args))))
                 (buffer (find-file-noselect notes-file)))
       buffer))))

(defun ebib-citar-entry-function (citekey)
  "Open CITEKEY using `ebib'."
  (ebib nil citekey))

(defvar ebib--citar-previous-values nil
  "Ebib-Citar integration variable backup.
When `ebib-citar-mode' is enabled, this variable is used to
backup the values of the following variables:
 - `ebib-notes-storage'
 - `citar-open-entry-function'")

(define-minor-mode ebib-citar-mode
  "Integrate Ebib and Citar.
When enabled, use Citar to manage notes and Ebib to open
bibliographic entries."
  :global t
  :group 'ebib-notes
  (if ebib-citar-mode
      (setf ebib--citar-previous-values (list ebib-notes-storage citar-open-entry-function)
            ebib-notes-storage #'ebib-citar-backend
            citar-open-entry-function #'ebib-citar-entry-function)
    (setf ebib-notes-storage (nth 0 ebib--citar-previous-values)
          citar-open-entry-function (nth 1 ebib--citar-previous-values)
          ebib--citar-previous-values nil)))

(provide 'ebib-citar)

;;; ebib-citar.el ends here
