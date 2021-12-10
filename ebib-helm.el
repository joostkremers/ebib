;;; ebib-helm.el --- Part of Ebib, a BibTeX database manager  -*- lexical-binding: t -*-

;; Copyright (c) 2021 Hugo Heagren
;; All rights reserved.

;; Author: Hugo Heagren <hugo@heagren.com>
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

(require 'helm)
(require 'ebib)

(defvar ebib-completion-finish-key)

(setq ebib-completion-finish-key `((completing-read . ,(key-description (where-is-internal 'helm-cr-empty-string (list helm-comp-read-map) 'non-ascii)))
                                   (read-file-name . ,(key-description (where-is-internal 'helm-cr-empty-string (list helm-read-file-map) 'non-ascii)))))

(defun ebib-helm-action-function (_)
  "Return a list of cons cells of the selected candidates and the databases that contain them."
  (mapcar (lambda (item)
            (cons (nth 0 item) (nth 1 item)))
          (helm-marked-candidates)))

(defun ebib-read-entry-helm (prompt databases _)
  "Read an entry from the user using helm.
Offer the entries in DATABASES (a list of database structs) for
completion.  PROMPT is the prompt to be displayed.

It is possible to select multiple entries in the helm buffer.

Return value is a list of cons cells of the selected keys and the
databases containing them."
  (let ((sources (helm-build-sync-source prompt
                                         :candidates (ebib--create-completion-collection databases t)
                                         :action '(("Select entry" . ebib-helm-action-function)))))
    (helm :sources sources
          :sort t
          :buffer "*helm ebib*"
          :prompt "Select entry: ")))

(defvar ebib-read-entry-function)

(setq ebib-read-entry-function #'ebib-read-entry-helm)

(provide 'ebib-helm)

;;; ebib-helm.el ends here
