;;; ebib-ido.el --- Part of Ebib, a BibTeX database manager  -*- lexical-binding: t -*-

;; Copyright (c) 2021 Joost Kremers
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
;; the code to integrate helm's features more tightly into the interface.  To
;; use this integration, add (require 'ebib-helm) to your init.el file.

;;; Code:

(require 'ebib)

(defun ebib-read-entry-ido (prompt databases _)
  "Read an entry from the user using ido.
Offer the entries in DATABASES (a list of database structs) for
completion.  PROMPT is the prompt to be displayed.

For compatibility with the other `ebib-read-entry-*' functions,
the return value is a list with a single cons cell of the key and
the database containing the selected entry."
  (let ((collection (ebib--create-completion-collection databases)))
    (if collection
        (let* ((candidates (mapcar #'car collection))
               (entry (ido-completing-read prompt candidates nil t nil 'ebib--key-history))
               (key (cadr (assoc-string entry collection)))
               (db (caddr (assoc-string entry collection))))
          (list (cons key db)))
      (error "[Ebib] No BibTeX entries found"))))

(defvar ebib-read-entry-function)

(setq ebib-read-entry-function #'ebib-read-entry-ido)

(provide 'ebib-ido)

;;; ebib-ido.el ends here
