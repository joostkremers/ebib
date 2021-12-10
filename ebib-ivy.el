;;; ebib-ivy.el --- Part of Ebib, a BibTeX database manager  -*- lexical-binding: t -*-

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
;; the code to integrate ivy's features more tightly into the interface.
;; To use this integration, add (require 'ebib-ivy) to init.el file.

;;; Code:

(require 'ivy)
(require 'ebib)

(defvar ebib-completion-finish-key)

(setq ebib-completion-finish-key (let ((key (key-description (where-is-internal 'ivy-immediate-done (list ivy-minibuffer-map) 'non-ascii))))
                                   (list `(completing-read . ,key)
                                         `(read-file-name . ,key))))


(defun ebib-read-entry-ivy (prompt databases _)
  "Read an entry from the user using ivy.
Offer the entries in DATABASES (a list of database structs) for
completion.  PROMPT is the prompt to be displayed.

It is possible to select multiple entries either by using
`ivy-call' or by marking them with `ivy-mark'.

Return value is a list of cons cells of the selected keys and the
databases containing them."
  (let ((minibuffer-allow-text-properties t)
        (ivy-sort-max-size (expt 256 6))
        entries)
    (let ((collection (ebib--create-completion-collection databases t)))
      (if (not collection)
          (error "[Ebib] No entries found in database(s)")
        (ivy-read prompt collection
                  :action (lambda (item)
                            (let ((key (cadr item))
                                  (db (caddr item)))
                              (unless (cl-find key entries :key #'car)
                                (push (cons key db) entries))))
                  :history 'ebib--citation-history
                  :sort t)
        (nreverse entries)))))

(defvar ebib-read-entry-function)

(setq ebib-read-entry-function #'ebib-read-entry-ivy)

(ivy-add-actions
 'ebib-jump-to-field
 '(("c" ebib-copy-field-contents "copy field contents")
   ("k" ebib-kill-field-contents "kill field contents")
   ("d" ebib-delete-field-contents "delete field contents")
   ("y" ebib-yank-field-contents "yank field contents")
   ("e" ebib-edit-field "edit field")
   ("m" ebib--edit-multiline-field "edit multiline field")
   ("v" ebib-view-field-as-help "view field as help")
   ("s" ebib-insert-abbreviation "insert abbreviation")
   ("r" ebib-toggle-raw "toggle raw")
   ("a" ebib-add-field "add field")
   ("u" (lambda (_) (ebib-browse-url)) "browse url")
   ("I" (lambda (_) (ebib-browse-doi)) "browse doi")))

(provide 'ebib-ivy)

;;; ebib-ivy.el ends here
