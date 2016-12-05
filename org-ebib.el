;;; org-ebib.el --- Support for links to Ebib entries in Org  -*- lexical-binding: t -*-

;; Copyright (c) 2014-2016 Grégoire Jadi, Joost Kremers
;; Al rights reserved

;; Author: Grégoire Jadi <daimrod@gmail.com>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This file provides two functions to integrate Ebib with org-mode.
;; `org-ebib-open' can be called with a BibTeX key as argument.  It opens Ebib
;; and shows the entry corresponding to the key.  `org-ebib-store-link' creates
;; a link that when followed opens Ebib.

;;; Code:

(require 'org)

;; This is to silence the byte-compiler and flycheck.
(defvar ebib--cur-db)
(defvar ebib-citation-description-function)
(declare-function ebib "ebib" (&optional file key))
(declare-function ebib--get-key-at-point "ebib" ())
(declare-function org-link-set-parameters "org" (type &rest parameters))

(org-link-set-parameters "ebib" :follow #'org-ebib-open :store #'org-ebib-store-link)

;;;###autoload
(defun org-ebib-open (key)
  "Open Ebib and jump to KEY."
  (ebib nil key))

(defun org-ebib-store-link ()
  "Store a link to an Ebib entry."
  (when (memq major-mode '(ebib-index-mode ebib-entry-mode))
    ;; This is an Ebib entry
    (let* ((key (ebib--get-key-at-point))
           (link (concat "ebib:" key))
           (description (ignore-errors (funcall ebib-citation-description-function key ebib--cur-db))))
      (org-store-link-props :type "ebib"
                            :link link
                            :description description))))

(provide 'org-ebib)

;;; org-ebib.el ends here
