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
;; contains general macros, utilities and variables.

;;; Code:

;; make sure we can call bibtex-generate-autokey
(declare-function bibtex-generate-autokey "bibtex" nil)
(if (fboundp 'bibtex-set-dialect)
    (bibtex-set-dialect)) ; this initializes some stuff that is needed for bibtex-generate-autokey

;;;;;;;;;;;;;;;;;;;;;;
;; global variables ;;
;;;;;;;;;;;;;;;;;;;;;;

;; user customisation

(defgroup ebib nil "Ebib: a BibTeX database manager" :group 'tex)

(defgroup ebib-windows nil "Ebib window management" :group 'ebib)

(defcustom ebib-default-type 'article
  "The default type for a newly created BibTeX entry."
  :group 'ebib
  :type 'symbol)

(defcustom ebib-preload-bib-files nil
  "List of BibTeX files to load automatically when Ebib starts."
  :group 'ebib
  :type '(repeat (file :must-match t)))

(defcustom ebib-bib-search-dirs '("~")
  "List of directories to search for BibTeX files.
A file passed to the function `ebib' is searched in these
directories if it is not in the current directory. similarly, the
files in `ebib-preload-bib-files' are searched in these
directories."
  :group 'ebib
  :type '(repeat :tag "Search directories for BibTeX files" (string :tag "Directory")))

(defcustom ebib-create-backups t
  "If set, create a backup file of a BibTeX file when it is first saved."
  :group 'ebib
   :type '(choice (const :tag "Create backups" t)
                 (const :tag "Do not create backups" nil)))

(defcustom ebib-additional-fields '(crossref
                                    url
                                    annote
                                    abstract
                                    keywords
                                    file
                                    timestamp
                                    doi)
  "List of the additional fields."
  :group 'ebib
  :type '(repeat (symbol :tag "Field")))

(defcustom ebib-layout 'full
  "Ebib window layout.
This option defines how Ebib displays the buffers its uses. By
default, Ebib takes over the entire frame and creates two windows
to display the index and the entry buffer. Alternatively, Ebib
can display only the index buffer in a window of the size
`ebib-index-window-size', displaying the entry buffer only when
it is needed (see `ebib-popup-entry-window' as well). A third
option uses the right part of the frame to the size of
`ebib-width' to display both the index and the entry buffer."
  :group 'ebib-windows
  :type '(choice (const :tag "Use full frame" full)
                 (const :tag "Display only index window" index-only)
                 (const :tag "Custom width" custom)))

(defcustom ebib-popup-entry-window nil
  "If non-NIL, create a popup window to display the entry window.
By default, if `ebib-layout' is set to `index-only', Ebib will
use an existing window to display the entry buffer when needed.
If this option is set, however, Ebib will use the function
`display-buffer-pop-up-window' to show the entry buffer,
which (usually) means that a new window will be created.

Note that setting this option has no effect unless `ebib-layout'
is set to `index-only'."
  :group 'ebib-windows
  :type 'boolean)

(defcustom ebib-window-vertical-split nil
  "If non-NIL, display the index buffer at the left of the frame.
If you set this option, you should probably set
`ebib-index-window-size' to a larger value."
  :group 'ebib-windows
  :type 'boolean)

(defcustom ebib-width 80
  "Width of the Ebib windows.
Only takes effect if `ebib-layout' is set to CUSTOM."
  :group 'ebib-windows
  :type 'integer)

(defcustom ebib-index-window-size 10
  "The size of the index buffer window.
This is either the height of the window, or, if
`ebib-window-vertical-split' is set, the width of the window."
  :group 'ebib-windows
  :type 'integer)

(defcustom ebib-index-mode-line '("%e"
                                  mode-line-front-space
                                  mode-line-modified
                                  mode-line-buffer-identification
                                  (:eval (if (ebib-cur-entry-key) "     Entry %l" "     No Entries"))
                                  (:eval (if (ebib-db-get-filter ebib-cur-db) " (Filtered)" "")))
  "The mode line for the index window."
  :group 'ebib-windows
  :type '(choice (const :tag "Use standard mode line" nil)
                 (sexp :tag "Customize mode line")))

(defcustom ebib-index-display-fields nil
  "List of the fields to display in the index buffer."
  :group 'ebib
  :type '(repeat (symbol :tag "Index Field")))

(defcustom ebib-uniquify-keys nil
  "Create unique keys.
If set, Ebib will not complain about duplicate keys but will
instead create a unique key by adding an identifier to it.
Identifiers are created from consecutive letters of the
alphabet, starting with `b'."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-autogenerate-keys nil
  "If set, Ebib generates key automatically.
Uses the function `bibtex-generate-autokey', see there for
customization options."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-citation-commands '((any
                                     (("cite" "\\cite%<[%A]%>{%K}")))
                                    (org-mode
                                     (("ebib" "[[ebib:%K][%D]]")))
                                    (markdown-mode
                                     (("text" "@%K%< [%A]%>")
                                      ("paren" "[%(%<%A %>@%K%<, %A%>%; )]")
                                      ("year" "[-@%K%< %A%>]"))))
  "A list of format strings to insert a citation into a buffer.
These are used with `ebib-insert-bibtex-key' and
`ebib-push-bibtex-key'."
  :group 'ebib
  :type '(repeat (list :tag "Mode" (symbol :tag "Mode name")
                       (repeat (list :tag "Citation command"
                                     (string :tag "Identifier")
                                     (string :tag "Format string"))))))

(defcustom ebib-multiline-major-mode 'text-mode
  "The major mode of the multiline edit buffer."
  :group 'ebib
  :type '(function :tag "Mode function"))

(defcustom ebib-sort-order nil
  "The fields on which the BibTeX entries are to be sorted in the BibTeX file.
Sorting is done on different sort levels, and each sort level contains one
or more sort keys."
  :group 'ebib
  :type '(repeat (repeat :tag "Sort level" (symbol :tag "Sort field"))))

(defcustom ebib-save-xrefs-first nil
  "If true, entries with a crossref field will be saved first in the BibTeX-file.
Setting this option has unpredictable results for the sort order
of entries, so it is not compatible with setting the Sort Order option."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-use-timestamp nil
  "If true, new entries will get a time stamp.
The time stamp will be stored in a field \"timestamp\" that can
be made visible with the command \\[ebib-toggle-hidden] in the
index buffer."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-timestamp-format "%a %b %e %T %Y"
  "Format of the time string used in the timestamp.
The format is passed unmodified to `format-time-string', see the
documentation of that function for details."
  :group 'ebib
  :type 'string)

(defcustom ebib-standard-url-field 'url
  "Standard field to store URLs in.
In the index buffer, the command \\[ebib-browse-url] can be used to
send a URL to a browser. This option sets the field from which
this command extracts the URL."
  :group 'ebib
  :type 'symbol)

(defcustom ebib-url-regexp "\\\\url{\\(.*\\)}\\|https?://[^ ';<>\"\n\t\f]+"
  "Regular expression to extract URLs from a field."
  :group 'ebib
  :type 'string)

(defcustom ebib-browser-command nil
  "Command to call the browser with.
GNU/Emacs has a function call-browser, which is used if this
option is unset."
  :group 'ebib
  :type '(choice (const :tag "Use standard browser" nil)
                 (string :tag "Specify browser command")))

(defcustom ebib-standard-doi-field 'doi
  "Standard field to store a DOI (digital object identifier) in.
In the index buffer, the command ebib-browse-doi can be used to
send a suitable URL to a browser. This option sets the field from
which this command extracts the doi."
  :group 'ebib
  :type 'symbol)

(defcustom ebib-standard-file-field 'file
  "Standard field to store filenames in.
In the index buffer, the command ebib-view-file can be used to
view a file externally. This option sets the field from which
this command extracts the filename."
  :group 'ebib
  :type 'symbol)

(defcustom ebib-file-associations '(("pdf" . "xpdf")
                                    ("ps" . "gv"))
  "List of file associations.
Lists file extensions together with external programs to handle
files with those extensions. If the external program is left
blank, Ebib tries to handle the file internally in
Emacs (e.g. with doc-view-mode)."
  :group 'ebib
  :type '(repeat (cons :tag "File association"
                       (string :tag "Extension")
                       (choice (const :tag "Open in Emacs" nil)
                               (string :tag "Run external command")))))

(defcustom ebib-filename-separator "; "
  "Separator for filenames in `ebib-standard-file-field'.
The contents of the file field is split up using this separator,
each string is assumed to be a filename."
  :group 'ebib
  :type 'string)

(defcustom ebib-file-search-dirs '("~")
  "List of directories to search when viewing external files."
  :group 'ebib
  :type '(repeat :tag "Search directories" (string :tag "Directory")))

(defcustom ebib-print-preamble nil
  "Preamble used for the LaTeX file for printing the database.
Each string is added to the preamble on a separate line."
  :group 'ebib
  :type '(repeat (string :tag "Add to preamble")))

(defcustom ebib-print-newpage nil
  "If set, each entry is printed on a separate page."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-print-multiline nil
  "If set, multiline fields are included when printing the database."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-latex-preamble '("\\bibliographystyle{plain}")
  "Preamble used for the LaTeX file for BibTeXing the database.
Each string is added to the preamble on a separate line."
  :group 'ebib
  :type '(repeat (string :tag "Add to preamble")))

(defcustom ebib-print-tempfile ""
  "Temporary file for use with `ebib-print-database' and `ebib-latex-database'."
  :group 'ebib
  :type '(file))

(defcustom ebib-allow-identical-fields nil
  "If set, Ebib handles multiple occurrences of a field gracefully."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-field-separator "; "
  "String for separating elements in a field value.
This is primarily used for separating keywords, but can also be
used to separate elements in other fields. Note that the `file'
field has its own separator (although it has the same default
value) and the `url' field uses `ebib-url-regexp' to extract
URLs."
  :group 'ebib
  :type '(string :tag "Field separator:"))

(defcustom ebib-rc-file "~/.ebibrc"
  "Customization file for Ebib.
This file is read when Ebib is started. It can be used to define
custom keys or set customization variables (though the latter is
easier through Customize)."
  :group 'ebib
  :type '(file :tag "Customization file:"))

(defcustom ebib-bibtex-extensions '(".bib" ".bibtex")
  "List of possible filename extensions of BibTeX files.
When loading a BibTeX filename without extension, Ebib tries to
find a file by adding these extensions. When creating a new file,
the first extension is added if the filename provided does not
already have an extension. If you want to create BibTeX files
without extension, add the empty string \"\" to this list or
unset the option entirely."
  :group 'ebib
  :type '(repeat (string :tag "Extension")))

(defcustom ebib-biblatex-inheritance nil
  "Inheritance scheme for cross-referencing.
Inheritances are specified per entry type. The source is the
field name in the cross-referencing entry, the target the field
in the cross-referenced entry.

To define inheritances for all entry types, specify `all' as the
entry type. If you combine inheritances for `all' with
entry-specific inheritances, the latter override the former."
  :group 'ebib
  :type '(repeat (group (symbol :tag "Entry type")
                        (repeat :tag "Inherited fields"
                                (group (symbol :tag "Source")
                                       (symbol :tag "Target"))))))

(defvar ebib-unique-field-list nil
  "Holds a list of all field names.")

(defun ebib-set-unique-field-list (var value)
  "Sets `ebib-unique-field-list' on the basis of `ebib-entry-types'"
  (set-default var value)
  (setq ebib-unique-field-list nil)
  (mapc #'(lambda (entry)
            (mapc #'(lambda (field)
                      (add-to-list 'ebib-unique-field-list field t))
                  (cadr entry))
            (mapc #'(lambda (field)
                      (add-to-list 'ebib-unique-field-list field t))
                  (cl-caddr entry)))
        value))

(defcustom ebib-entry-types
  '((article                                              ;; name of entry type
     (author title journal year)                          ;; obligatory fields
     (volume number pages month note))                    ;; optional fields

    (book
     (author title publisher year)
     (editor volume number series address edition month note))

    (booklet
     (title)
     (author howpublished address month year note))

    (inbook
     (author title chapter pages publisher year)
     (editor volume series address edition month note))

    (incollection
     (author title booktitle publisher year)
     (editor volume number series type chapter pages address edition month note))

    (inproceedings
     (author title booktitle year)
     (editor pages organization publisher address month note))

    (manual
     (title)
     (author organization address edition month year note))

    (misc
     ()
     (title author howpublished month year note))

    (mastersthesis
     (author title school year)
     (address month note))

    (phdthesis
     (author title school year)
     (address month note))

    (proceedings
     (title year)
     (editor publisher organization address month note))

    (techreport
     (author title institution year)
     (type number address month note))

    (unpublished
     (author title note)
     (month year)))

  "List of entry type definitions for Ebib"
  :group 'ebib
  :type '(repeat (list :tag "Entry type" (symbol :tag "Name")
                       (repeat :tag "Obligatory fields" (symbol :tag "Field"))
                       (repeat :tag "Optional fields" (symbol :tag "Field"))))
  :set 'ebib-set-unique-field-list)

(defgroup ebib-faces nil "Faces for Ebib" :group 'ebib)

(defface ebib-crossref-face '((t (:inherit font-lock-comment-face)))
  "Face used to indicate values inherited from crossreferenced entries."
  :group 'ebib-faces)

(defface ebib-marked-face (if (featurep 'xemacs)
                              '((t (:foreground "white" :background "red")))
                            '((t (:inverse-video t))))
  "Face to indicate marked entries."
  :group 'ebib-faces)

(defface ebib-field-face '((t (:inherit font-lock-keyword-face)))
  "Face for field names."
  :group 'ebib-faces)

;; generic for all databases

;; constants and variables that are set only once
(defconst ebib-bibtex-identifier "[^^\"@\\&$#%',={}() \t\n\f]*" "Regexp describing a licit BibTeX identifier.")
(defconst ebib-key-regexp "[^][^\"@\\&$#%',={} \t\n\f]*" "Regexp describing a licit key.")
(defvar ebib-initialized nil "T if Ebib has been initialized.")

;; buffers and highlights
(defvar ebib-buffer-alist nil "Alist of Ebib buffers.")
(defvar ebib-index-highlight nil "Highlight to mark the current entry.")
(defvar ebib-fields-highlight nil "Highlight to mark the current field.")
(defvar ebib-strings-highlight nil "Highlight to mark the current string.")

;; general bookkeeping
(defvar ebib-field-history nil "Minibuffer field name history.")
(defvar ebib-filters-history nil "Minibuffer history for filters.")
(defvar ebib-cite-command-history nil "Minibuffer history for citation commands.")
(defvar ebib-key-history nil "Minibuffer history for entry keys.")
(defvar ebib-keywords-history nil "Minibuffer history for keywords.")

(defvar ebib-saved-window-config nil "Stores the window configuration when Ebib is called.")
(defvar ebib-window-before nil "The window that was active when Ebib was called.")
(defvar ebib-buffer-before nil "The buffer that was active when Ebib was called.")
;;(defvar ebib-pre-multiline-buffer nil "The buffer in the window before switching to the multiline edit buffer.")
(defvar ebib-export-filename nil "Filename to export entries to.")
(defvar ebib-push-buffer nil "Buffer to push entries to.")
(defvar ebib-search-string nil "Stores the last search string.")
(defvar ebib-editing nil "Indicates what the user is editing, either 'fields or 'preamble.")
(defvar ebib-multiline-unbraced nil "Indicates whether the multiline text being edited is braced.")
(defvar ebib-log-error nil "Indicates whether an error was logged.")
(defvar ebib-local-bibtex-filenames nil
  "A buffer-local variable holding a list of the name(s) of that buffer's .bib file(s)")
(make-variable-buffer-local 'ebib-local-bibtex-filenames)
(defvar ebib-syntax-table (make-syntax-table) "Syntax table used for reading .bib files.")
(modify-syntax-entry ?\[ "." ebib-syntax-table)
(modify-syntax-entry ?\] "." ebib-syntax-table)
(modify-syntax-entry ?\( "." ebib-syntax-table)
(modify-syntax-entry ?\) "." ebib-syntax-table)
(modify-syntax-entry ?\" "w" ebib-syntax-table)

;; The databases

;; the master list and the current database
(defvar ebib-databases nil "List of structs containing the databases.")
(defvar ebib-cur-db nil "The database that is currently active.")
(defvar ebib-cur-keys-list nil "Sorted list of entry keys in the current database.")

;; bookkeeping required when editing field values or @STRING definitions

(defvar ebib-hide-hidden-fields t "If set to T, hidden fields are not shown.")

;; these variables are set when the user enters the entry and strings buffer, respectively
(defvar ebib-cur-entry-fields nil "The fields of the type of the current entry.")
(defvar ebib-cur-strings-list nil "A sorted list of strings in the current database.")

;; this is set by `ebib-fill-entry-buffer'
(defvar ebib-current-field nil "The current field.")

;; and this one by `ebib-fill-strings-buffer'
(defvar ebib-current-string nil "The current @STRING definition.")

;; the prefix key and the multiline key are stored in a variable so that the
;; user can customise them.
(defvar ebib-prefix-key ?\;)
(defvar ebib-multiline-key ?\|)

;; this is an AucTeX variable, but we want to check its value, so let's
;; keep the compiler from complaining.
(eval-when-compile
  (defvar TeX-master))

;; this is to keep XEmacs from complaining.
(eval-when-compile
  (if (featurep 'xemacs)
      (defvar mark-active)))

;; XEmacs has line-number, not line-number-at-pos.
(eval-and-compile
  (if (featurep 'xemacs)
      (defalias 'line-number-at-pos 'line-number)))

(defun ebib-log (type format-string &rest args)
  "Writes a message to Ebib's log buffer.
TYPE (a symbol) is the type of message. It can be LOG, which
writes the message to the log buffer only; MESSAGE, which writes
the message to the log buffer and outputs it with the function
MESSAGE; WARNING, which logs the message and sets the variable
`ebib-log-error' to 0; or ERROR, which logs the message and sets
the variable `ebib-log-error' to 1. The latter two can be used to
signal the user to check the log for warnings or errors.

This function adds a newline to the message being logged."
  (with-current-buffer (cdr (assoc 'log ebib-buffer-alist))
    (cond
     ((eq type 'warning)
      (or ebib-log-error ; if ebib-error-log is already set to 1, we don't want to overwrite it!
          (setq ebib-log-error 0)))
     ((eq type 'error)
      (setq ebib-log-error 1))
     ((eq type 'message)
      (apply 'message format-string args)))
    (insert (apply 'format  (concat (if (eq type 'error)
                                        (propertize format-string 'face 'font-lock-warning-face)
                                      format-string)
                                    "\n")
                   args))))

(defun ebib-make-highlight (begin end buffer)
  (let (highlight)
    (if (featurep 'xemacs)
        (progn
          (setq highlight (make-extent begin end buffer))
          (set-extent-face highlight 'highlight))
      (progn
        (setq highlight (make-overlay begin end buffer))
        (overlay-put highlight 'face 'highlight)))
    highlight))

(defun ebib-move-highlight (highlight begin end buffer)
  (if (featurep 'xemacs)
      (set-extent-endpoints highlight begin end buffer)
    (move-overlay highlight begin end buffer)))

(defun ebib-highlight-start (highlight)
  (if (featurep 'xemacs)
      (extent-start-position highlight)
    (overlay-start highlight)))

(defun ebib-highlight-end (highlight)
  (if (featurep 'xemacs)
      (extent-end-position highlight)
    (overlay-end highlight)))

(defun ebib-delete-highlight (highlight)
  (if (featurep 'xemacs)
      (detach-extent highlight)
    (delete-overlay highlight)))

(defun ebib-set-index-highlight ()
  (with-current-buffer (cdr (assoc 'index ebib-buffer-alist))
    (beginning-of-line)
    (let ((beg (point)))
      (if ebib-index-display-fields
          (end-of-line)
        (skip-chars-forward "^ "))
      (ebib-move-highlight ebib-index-highlight beg (point) (cdr (assoc 'index ebib-buffer-alist)))
      (beginning-of-line))))

(defun ebib-set-fields-highlight ()
  (with-current-buffer (cdr (assoc 'entry ebib-buffer-alist))
    (beginning-of-line)
    (let ((beg (point)))
      (ebib-looking-at-goto-end "[^ \t\n\f]*")
      (ebib-move-highlight ebib-fields-highlight beg (point) (cdr (assoc 'entry ebib-buffer-alist)))
      (beginning-of-line))))

(defun ebib-set-strings-highlight ()
  (with-current-buffer (cdr (assoc 'strings ebib-buffer-alist))
    (beginning-of-line)
    (let ((beg (point)))
      (ebib-looking-at-goto-end "[^ \t\n\f]*")
      (ebib-move-highlight ebib-strings-highlight beg (point) (cdr (assoc 'strings ebib-buffer-alist)))
      (beginning-of-line))))

(defun ebib-display-buffer-reuse-window (buffer alist)
  "Display BUFFER in an existing Ebib buffer.
If BUFFER is the index buffer, simply switch to the window
displaying it. (This function should not be called if there is a
chance the index buffer is not visible.) For any other buffer,
find a window displaying an Ebib buffer other than the index
buffer, switch to that window and display BUFFER. If no window
can be found, return NIL.

Note, The argument ALIST has no function."
  (let (window)
    (cond
     ;; the index buffer can only be displayed in its dedicated window.
     ((eq buffer (cdr (assoc 'index ebib-buffer-alist)))
      (setq window (get-buffer-window buffer)))
     ;; if ebib-layout isn't full, the multiline buffer should not be
     ;; displayed in an Ebib buffer.
     ((and (eq buffer (cdr (assoc 'multiline ebib-buffer-alist)))
           (not (eq ebib-layout 'full)))
      (setq window nil))
     (t (let ((buffers (delq nil (mapcar #'(lambda (bf)
                                             (unless (eq (car bf) 'index)
                                               (cdr bf)))
                                         ebib-buffer-alist))))
          (while (and buffers
                      (not (get-buffer-window (car buffers))))
            (setq buffers (cdr buffers)))
          (setq window (get-buffer-window (car buffers))))))
    (when window
      (select-window window)
      (with-ebib-window-nondedicated
        (switch-to-buffer buffer))
      window)))

(defun ebib-display-buffer-largest-window (buffer alist)
  "Display BUFFER in the largest non-dedicated window.
The argument ALIST has no function."
  (unless ebib-popup-entry-window
    (let ((window (get-largest-window)))
      (select-window window)
      (switch-to-buffer buffer)
      window)))

(defun ebib-pop-to-buffer (buffer)
  "Select or create a window to display BUFFER and display it.
BUFFER is a symbol indicating the buffer to switch to. It can be
'index, 'entry, 'strings, 'log or 'multiline.

If BUFFER is 'index, switch to the index window or signal an
error if there is no window displaying the index buffer.

For any other buffer, if there is a visible Ebib buffer other
than the index buffer, switch to its window and display BUFFER.
If there is no Ebib window, use the largest non-dedicated window
or, if `ebib-layout' is set to `popup', pop up a new window. If
all else fails, pop up a new frame."
  (if (or (not (eq buffer 'index))
          (get-buffer-window (cdr (assoc 'index ebib-buffer-alist))))
      (pop-to-buffer (cdr (assoc buffer ebib-buffer-alist))
                     '((ebib-display-buffer-reuse-window
                        ebib-display-buffer-largest-window
                        display-buffer-pop-up-window
                        display-buffer-pop-up-frame))
                     t)
    (error "Ebib is lowered. Use `M-x ebib' to restart")))

(defun ebib-display-entry (entry-key)
  "Display ENTRY-KEY in the index buffer at POINT."
  (with-current-buffer (cdr (assoc 'index ebib-buffer-alist))
    (with-ebib-buffer-writable
      (insert (format "%-30s %s\n"
                      entry-key
                      (if ebib-index-display-fields
                          (mapconcat #'(lambda (field)
                                         (or (ebib-db-get-field-value field entry-key ebib-cur-db 'noerror 'unbraced)
                                             ""))
                                     ebib-index-display-fields
                                     "  ") ; separator for mapconcat
                        ""))))))

(defun ebib-redisplay-current-field ()
  "Redisplays the contents of the current field in the entry buffer."
  (with-current-buffer (cdr (assoc 'entry ebib-buffer-alist))
    ;; If the `crossref' field has changed, we need to redisplay the entire entry.
    (if (eq ebib-current-field 'crossref)
        (progn
          (ebib-fill-entry-buffer)
          (setq ebib-current-field 'crossref)
          (re-search-forward "^crossref")
          (ebib-set-fields-highlight))
      (with-ebib-buffer-writable
        (goto-char (ebib-highlight-start ebib-fields-highlight))
        (let ((beg (point)))
          (end-of-line)
          (delete-region beg (point)))
        (insert (propertize (format "%-17s " (symbol-name ebib-current-field)) 'face 'ebib-field-face)
                (ebib-get-field-highlighted ebib-current-field (ebib-cur-entry-key)))
        (ebib-set-fields-highlight)))))

(defun ebib-redisplay-current-string ()
  "Redisplays the current string definition in the strings buffer."
  (with-current-buffer (cdr (assoc 'strings ebib-buffer-alist))
    (with-ebib-buffer-writable
      (let ((val (ebib-db-get-string ebib-current-string ebib-cur-db nil 'unbraced)))
        (goto-char (ebib-highlight-start ebib-strings-highlight))
        (let ((beg (point)))
          (end-of-line)
          (delete-region beg (point)))
        (insert (format "%-18s %s" ebib-current-string
                        (if (ebib-multiline-p val)
                            (concat "+" (ebib-first-line val))
                          (concat " " val))))
        (ebib-set-strings-highlight)))))

(defun ebib-move-to-field (field direction)
  "Moves the fields overlay to the line containing FIELD.
If DIRECTION is positive, searches forward, if DIRECTION is
negative, searches backward. If DIRECTION is 1 or -1, searches
from POINT, if DIRECTION is 2 or -2, searches from beginning or
end of buffer.  If FIELD is not found in the entry buffer, the
overlay is not moved.  FIELD must be a symbol."

  ;;Note: this function does NOT change the value of `ebib-current-field'!

  (with-current-buffer (cdr (assoc 'entry ebib-buffer-alist))
    (if (eq field '=type=)
        (goto-char (point-min))
      (cl-multiple-value-bind (fn start limit) (if (>= direction 0)
                                                   (cl-values 're-search-forward (point-min) (point-max))
                                                 (cl-values 're-search-backward (point-max) (point-min)))
        ;; make sure we can get back to our original position, if the field
        ;; cannot be found in the buffer:
        (let ((current-pos (point)))
          (when (eq (logand direction 1) 0) ; if direction is even
            (goto-char start))
          (unless (funcall fn (concat "^" (symbol-name field)) limit t)
            (goto-char current-pos)))))
    (ebib-set-fields-highlight)))

(defun ebib-get-xref-field (field entry-type)
  "Return the field from which FIELD inherits in ENTRY-TYPE.
For BibTeX, a field simply inherits from the same field in the
cross-referenced entry. BibLaTeX, however, allows complex
inheritance relations, so that e.g., the field `booktitle' may
inherit from the field `title' in the cross-referenced entry.

The inheritance scheme is stored in `ebib-biblatex-inheritance'."
  (let ((inheritances (or (cadr (assq entry-type ebib-biblatex-inheritance))
                          (cadr (assq 'all ebib-biblatex-inheritance)))))
    (or (cadr (assq field inheritances))
        field)))

(defun ebib-get-field-highlighted (field key &optional match-str db)
  ;; Note: we need to work on a copy of the string, otherwise the highlights
  ;; are made to the string as stored in the database. Hence copy-sequence.
  (or db (setq db ebib-cur-db))
  (let* ((case-fold-search t)
         (value (ebib-db-get-field-value field key db 'noerror nil 'xref))
         (string (if (car value)
                     (copy-sequence (car value))))
         (xref (cadr value))
         (raw " ")
         (multiline " ")
         (matched nil))
    ;; we have to do a couple of things now:
    ;; - remove {} or "" around the string, if they're there
    ;; - search for match-str
    ;; - properly adjust the string if it's multiline
    ;; but all this is not necessary if there was no string
    (when string
      (if xref
          (setq string (propertize string 'face 'ebib-crossref-face 'fontified t)))
      (if (ebib-db-unbraced-p string)
          (setq raw "*")
        (setq string (ebib-db-unbrace string))) ; we have to make the string look nice
      (when match-str
        (cl-multiple-value-setq (string matched) (ebib-match-all-in-string match-str string)))
      (when (ebib-multiline-p string)
        ;; IIUC PROPERTIZE shouldn't be necessary here, as the variable
        ;; multiline is local and therefore the object it refers to should
        ;; be GC'ed when the function returns. But for some reason, the
        ;; plus sign is persistent, and if it's been highlighted as the
        ;; result of a search, it stays that way.
        (setq multiline (propertize "+" 'face nil))
        (setq string (ebib-first-line string)))
      (when (and matched
                 (string= multiline "+"))
        (add-text-properties 0 1 '(face highlight) multiline)))
    (concat raw multiline string)))

(defun ebib-format-fields (key &optional match-str db)
  (or db
      (setq db ebib-cur-db))
  (let* ((entry (ebib-db-get-entry key db))
         (entry-type (cdr (assoc '=type= entry)))
         (obl-fields (ebib-get-obl-fields entry-type))
         (opt-fields (ebib-get-opt-fields entry-type)))
    (insert (format "%-19s %s\n" (propertize "type" 'face 'ebib-field-face) entry-type))
    (mapc #'(lambda (fields)
              (insert "\n")
              (mapcar #'(lambda (field)
                          (unless (and (get field 'ebib-hidden)
                                       ebib-hide-hidden-fields)
                            (insert (propertize (format "%-17s " field) 'face 'ebib-field-face))
                            (insert (or
                                     (ebib-get-field-highlighted field key match-str)
                                     ""))
                            (insert "\n")))
                      fields))
          (list obl-fields opt-fields ebib-additional-fields))))

(defun ebib-redisplay ()
  "Redisplay the index and entry buffers."
  (ebib-fill-index-buffer)
  (ebib-fill-entry-buffer))

(defun ebib-fill-index-buffer ()
  "Fills the index buffer with the list of keys in `ebib-cur-db'.
If `ebib-cur-db' is nil, the buffer is just erased and its name set
to \"none\"."
  ;; Note: this function sets `ebib-cur-keys-list'.
  (with-current-buffer (cdr (assoc 'index ebib-buffer-alist))
    ;; First set the modification flag, so that it's still correct after
    ;; with-ebib-buffer-writable.
    (when ebib-cur-db
     (set-buffer-modified-p (ebib-db-modified-p ebib-cur-db)))
    (with-ebib-buffer-writable
      (erase-buffer)
      (if (not ebib-cur-db)
          (rename-buffer " none")
        (setq ebib-cur-keys-list
              (if (ebib-db-get-filter ebib-cur-db)
                  (ebib-filters-run-filter ebib-cur-db)
                (ebib-db-list-keys ebib-cur-db)))
        ;; Set a header line if there is a filter.
        (setq header-line-format (ebib-filters-pp-filter (ebib-db-get-filter ebib-cur-db)))
        ;; We may call this function when there are no entries in the
        ;; database. If so, we don't need to do this:
        (when (ebib-cur-entry-key)
          ;; It may be that no entry satisfies the filter.
          (if (not ebib-cur-keys-list)
              (message "No entries matching the filter")
            ;; Make sure the current entry is among the visible entries.
            (unless (member (ebib-cur-entry-key) ebib-cur-keys-list)
              (ebib-db-set-current-entry-key (car ebib-cur-keys-list) ebib-cur-db))
            (mapc #'(lambda (entry)
                      (ebib-display-entry entry)
                      (when (member entry (ebib-db-list-marked-entries ebib-cur-db 'nosort))
                        (save-excursion
                          (forward-line -1)
                          (ebib-display-mark t))))
                  ebib-cur-keys-list)
            (goto-char (point-min))
            (re-search-forward (format "^%s " (ebib-cur-entry-key)))
            (beginning-of-line)
            (ebib-set-index-highlight)))
        (rename-buffer (concat (format " %d:" (1+ (- (length ebib-databases)
                                                     (length (member ebib-cur-db ebib-databases)))))
                               (ebib-db-get-filename ebib-cur-db 'shortened)))))))

(defun ebib-display-mark (mark &optional beg end)
  "Mark/unmark an entry.
Add/remove `ebib-marked-face` to the region between BEG and END,
or to the entry point is on if these are omitted. If MARK is t,
`ebib-marked-face is added, if nil, it is removed. NB: if BEG and
END are omitted, this function changes point."
  (unless (and beg end)
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward "^ ")
    (setq end (point)))
  (if mark
      (add-text-properties beg end '(face ebib-marked-face))
    (remove-text-properties beg end '(face ebib-marked-face))))

(defun ebib-fill-entry-buffer (&optional match-str)
  "Fills the entry buffer with the fields of the current entry.
MATCH-STRING is a regexp that will be highlighted when it occurs in the
field contents."
  (with-current-buffer (cdr (assoc 'entry ebib-buffer-alist))
    (with-ebib-buffer-writable
      (erase-buffer)
      (when ebib-cur-keys-list         ; are there entries being displayed?
        (ebib-format-fields (ebib-cur-entry-key) match-str)
        (setq ebib-current-field '=type=)
        (goto-char (point-min))))))

(defun ebib-set-modified (mod &optional db)
  "Sets the modified flag of the database DB to MOD.
If DB is nil, it defaults to the current database, and the
modified flag of the index buffer is also (re)set. MOD must be
either T or NIL."
  (unless db
    (setq db ebib-cur-db))
  (ebib-db-set-modified mod db)
  (when (eq db ebib-cur-db)
    (with-current-buffer (cdr (assoc 'index ebib-buffer-alist))
      (set-buffer-modified-p mod))))

(defun ebib-modified-p ()
  "Checks if any of the databases in Ebib were modified.
Returns the first modified database, or NIL if none was modified."
  (let ((db (car ebib-databases)))
    (while (and db
                (not (ebib-db-modified-p db)))
      (setq db (ebib-next-elem db ebib-databases)))
    db))

(defun ebib-create-new-database ()
  "Creates a new database instance and returns it."
  (let ((new-db (ebib-db-new-database)))
    (setq ebib-databases (append ebib-databases (list new-db)))
    new-db))

(defun ebib-match-paren-forward (limit)
  "Moves forward to the closing parenthesis matching the opening parenthesis at POINT.
This function handles parentheses () and braces {}. Does not
search/move beyond LIMIT. Returns T if a matching parenthesis was
found, NIL otherwise. If point was not at an opening parenthesis
at all, NIL is returned and point is not moved. If point was at
an opening parenthesis but no matching closing parenthesis was
found, an error is logged and point is moved one character
forward to allow parsing to continue."
  (cond
   ((eq (char-after) ?\{)
    (ebib-match-brace-forward limit))
   ((eq (char-after) ?\()
    ;; we wrap this in a condition-case because we need to log the error
    ;; message outside of the save-restriction, otherwise we get the wrong
    ;; line number.
    (condition-case nil
        (save-restriction
          (narrow-to-region (point) limit)
          ;; this is really a hack. we want to allow unbalanced parentheses in
          ;; field values (bibtex does), so we cannot use forward-list
          ;; here. for the same reason, looking for the matching paren by hand
          ;; is pretty complicated. however, balanced parentheses can only be
          ;; used to enclose entire entries (or @STRINGs or @PREAMBLEs) so we
          ;; can be pretty sure we'll find it right before the next @ at the
          ;; start of a line, or right before the end of the file.
          (re-search-forward "^@" nil 0)
          (skip-chars-backward "@ \n\t\f")
          (forward-char -1)
          (if (eq (char-after) ?\))
              t
            (goto-char (1+ (point-min)))
            (error "")))
      (error (ebib-log 'error "Error in line %d: Matching closing parenthesis not found!" (line-number-at-pos))
             nil)))
   (t nil)))

(defun ebib-match-delim-forward (limit)
  "Moves forward to the closing delimiter matching the opening delimiter at POINT.
This function handles braces {} and double quotes \"\". Does not
search/move beyond LIMIT. Returns T if a matching delimiter was
found, NIL otherwise. If point was not at an opening delimiter at
all, NIL is returned and point is not moved. If point was at an
opening delimiter but no matching closing delimiter was found, an
error is logged and point is moved one character forward to allow
parsing to continue."
  (cond
   ((eq (char-after) ?\")
    (ebib-match-quote-forward limit))
   ((eq (char-after) ?\{)
    (ebib-match-brace-forward limit))
   (t nil)))

(defun ebib-match-brace-forward (limit)
  "Moves forward to the closing brace matching the opening brace at POINT.
Does not search/move beyond LIMIT. Returns T if a matching brace
was found, NIL otherwise. If point was not at an opening brace at
all, NIL is returned and point is not moved. If point was at an
opening brace but no matching closing brace was found, an error
is logged and point is moved one character forward to allow
parsing to continue."
  (when (eq (char-after) ?\{) ; make sure we're really on a brace, otherwise return nil
    (condition-case nil
        (save-restriction
          (narrow-to-region (point) limit)
          (progn
            (forward-list)
            ;; all of ebib expects that point moves to the closing
            ;; parenthesis, not right after it, so we adjust.
            (forward-char -1)
            t))               ; return t because a matching brace was found
      (error (progn
               (ebib-log 'error "Error in line %d: Matching closing brace not found!" (line-number-at-pos))
               (forward-char 1)
               nil)))))

(defun ebib-match-quote-forward (limit)
  "Moves to the closing double quote matching the quote at POINT.
Does not search/move beyond LIMIT. Returns T if a matching quote
was found, NIL otherwise. If point was not at a double quote at
all, NIL is returned and point is not moved. If point was at a
quote but no matching closing quote was found, an error is logged
and point is moved one character forward to allow parsing to
continue."
  (when (eq (char-after (point)) ?\")  ; make sure we're on a double quote.
    (condition-case nil
        (save-restriction
          (narrow-to-region (point) limit)
          (while (progn
                   (forward-char) ; move forward because we're on a double quote
                   (skip-chars-forward "^\"") ; search the next double quote
                   (eq (char-before) ?\\))) ; if it's preceded by a backslash, keep on searching
          (or (eq (char-after) ?\")
              (progn
                (goto-char (1+ (point-min)))
                (error ""))))
      (error (ebib-log 'error "Error in line %d: Matching closing quote not found!" (line-number-at-pos))
             nil))))

(defun ebib-store-entry (entry-key fields db &optional timestamp if-exists)
  "Store the entry defined by ENTRY-KEY and FIELDS into DB.
Optional argument TIMESTAMP indicates whether a timestamp is to
be added to the entry. Note that for a timestamp to be added,
`ebib-use-timestamp' must also be set to T. IF-EXISTS is as for
`ebib-db-set-entry'."
  (ebib-db-set-entry entry-key fields db if-exists)
  (when (and timestamp ebib-use-timestamp)
    (ebib-db-set-field-value 'timestamp (format-time-string ebib-timestamp-format) entry-key db 'overwrite))
  (ebib-set-modified t db))

(defun ebib-search-key-in-buffer (entry-key)
  "Searches ENTRY-KEY in the index buffer.
Moves point to the first character of the key and returns point."
  (goto-char (point-min))
  (re-search-forward (concat "^" entry-key))
  (beginning-of-line)
  (point))

(defvar ebib-info-flag nil "Flag to indicate whether Ebib called Info or not.")

(defadvice Info-exit (after ebib-info-exit activate)
  "Quit info and return to Ebib, if Info was called from there."
  (when ebib-info-flag
    (setq ebib-info-flag nil)
    (ebib)))

(defun read-file-to-list (filename)
  "Return a list of lines from file FILENAME."
  (if (and filename                               ; protect against 'filename' being 'nil'
           (file-readable-p filename))
      (with-temp-buffer
        (insert-file-contents filename)
        (split-string (buffer-string) "\n" t))))    ; 't' is omit nulls, blank lines in this case

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

(provide 'ebib-utils)

;;; ebib-utils ends here
