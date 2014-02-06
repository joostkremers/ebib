;;; ebib.el --- a BibTeX database manager

;; Copyright (c) 2003-2014 Joost Kremers
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 2003
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

;; Ebib is a BibTeX database manager that runs in GNU Emacs. With Ebib, you
;; can create and manage .bib-files, all within Emacs. It supports @string
;; and @preamble definitions, multi-line field values, searching, and
;; integration with Emacs' (La)TeX mode.

;; See the Ebib manual for usage and installation instructions.

;; The latest release version of Ebib, contact information and mailing list
;; can be found at <http://joostkremers.github.io/ebib>. Development
;; sources can be found at <https://github.com/joostkremers/ebib>.

;;; Code:

(eval-and-compile
  (if (string< (format "%d.%d" emacs-major-version emacs-minor-version) "24.3")
      (progn
        (require 'cl)
        (defalias 'cl-caddr 'caddr)
        (defalias 'cl-defstruct 'defstruct)
        (defalias 'cl-do 'do)
        (defalias 'cl-dolist 'dolist)
        (defalias 'cl-flet 'flet)
        (defalias 'cl-incf 'incf)
        (defalias 'cl-labels 'labels)
        (defalias 'cl-loop 'loop)
        (defalias 'cl-multiple-value-bind 'multiple-value-bind)
        (defalias 'cl-multiple-value-setq 'multiple-value-setq)
        (defalias 'cl-remove-if 'remove-if)
        (defalias 'cl-values 'values))
    (require 'cl-lib)))
(require 'easymenu)
(require 'bibtex)
(require 'pp)
(require 'ebib-db)
(require 'ebib-filters)
(require 'ebib-keywords)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful macros and functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main program execution ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun ebib (&optional file)
  "Ebib, a BibTeX database manager.
Optional argument FILE is a file to load. If FILE is already
loaded, switch to it."
  (interactive)
  ;; First do some stuff in the buffer from which Ebib was called.
  ;; Save the buffer from which Ebib is called.
  (setq ebib-buffer-before (current-buffer))
  ;; Set the push buffer to the current buffer.
  (setq ebib-push-buffer (current-buffer))
  ;; See if there are local databases.
  (or ebib-local-bibtex-filenames
      (setq ebib-local-bibtex-filenames (ebib-get-local-databases)))
  (let (key)
    (if file ;; Expand FILE if given.
        (setq file (ebib-locate-bibfile file (append ebib-bib-search-dirs (list default-directory))))
      ;; Otherwise see if there's a key at point.
      (setq key (ebib-read-string-at-point "][^\"@\\&$#%',={} \t\n\f")))
    ;; Initialize Ebib if required.
    (ebib-init)
    ;; Set up the windows.
    (ebib-setup-windows)
    ;; See if we have a file or a key.
    (cond
     (file (ebib-load-bibtex-file-internal file))
     (key (ebib-find-and-set-key key (buffer-local-value 'ebib-local-bibtex-filenames ebib-buffer-before)))))
  (ebib-redisplay))

(defun ebib-find-and-set-key (key files)
  "Make KEY the current entry.
FILES is a list of BibTeX files in which KEY is searched. If
FILES is `none', only the current database is searched."
  (when ebib-databases
    (if (eq files 'none)
        (unless (member key ebib-cur-keys-list)
          (setq key nil))
      (let ((database (catch 'found
                        (mapc #'(lambda (file)
                                  (let ((db (ebib-get-db-from-filename file)))
                                    (if (and db (member key (ebib-db-list-keys db 'nosort)))
                                        (throw 'found db))))
                              files)
                        nil))) ; We must return nil if the key wasn't found anywhere.
        (if (null database)
            (setq key nil)
          (setq ebib-cur-db database))))
    (if key 
        (ebib-db-set-current-entry-key key ebib-cur-db))))

(defun ebib-read-string-at-point (chars)
  "Reads a string at POINT delimited by CHARS and returns it.
CHARS is a string of characters that should not occur in the string."
  (save-excursion
    (skip-chars-backward (concat "^" chars))
    (let ((beg (point)))
      (ebib-looking-at-goto-end (concat "[^" chars "]*"))
      (buffer-substring-no-properties beg (point)))))

(defun ebib-init ()
  "Initialise Ebib.
This function sets all variables to their initial values, creates
the buffers, reads the rc file and loads the files in
`ebib-preload-bib-files'."
  (unless ebib-initialized
    (setq ebib-current-field nil
          ebib-saved-window-config nil)
    (put 'timestamp 'ebib-hidden t)
    (ebib-create-buffers)
    (if (and ebib-keywords-file
             (file-name-directory ebib-keywords-file)) ; returns nil if there is no directory part
        (add-to-list 'ebib-keywords-files-alist (list (file-name-directory ebib-keywords-file)
                                                      (read-file-to-list ebib-keywords-file) nil)))
    (setq ebib-keywords-list-per-session (copy-tree ebib-keywords-list))
    (ebib-filters-load-file ebib-filters-default-file)
    (setq ebib-index-highlight (ebib-make-highlight 1 1 (cdr (assoc 'index ebib-buffer-alist))))
    (setq ebib-fields-highlight (ebib-make-highlight 1 1 (cdr (assoc 'entry ebib-buffer-alist))))
    (setq ebib-strings-highlight (ebib-make-highlight 1 1 (cdr (assoc 'strings ebib-buffer-alist))))
    (add-hook 'kill-emacs-query-functions 'ebib-kill-emacs-query-function)
    (load ebib-rc-file t)
    (if ebib-preload-bib-files
        (mapc #'(lambda (file)
                  (ebib-load-bibtex-file-internal (or (locate-file file ebib-bib-search-dirs)
                                                      file)))
              ebib-preload-bib-files))
    (setq ebib-initialized t)))

(defun ebib-setup-windows ()
  "Create Ebib's window configuration in the current frame."
  ;; If the index buffer is visible, just switch to it.
  (let ((index-window (get-buffer-window (cdr (assoc 'index ebib-buffer-alist)))))
    (if index-window
        (select-window index-window)
      ;; Save the current window configuration.
      (setq ebib-saved-window-config (current-window-configuration))
      (cond
       ((eq ebib-layout 'full)
        (delete-other-windows))
       ((eq ebib-layout 'custom)
        (setq ebib-window-before (selected-window))
        (let ((ebib-window (split-window (selected-window) (- (window-width) ebib-width) t)))
          (select-window ebib-window))))
      (let* ((index-window (selected-window))
             (entry-window (split-window index-window ebib-index-window-size
                                         ebib-window-vertical-split)))
        (switch-to-buffer (cdr (assoc 'index ebib-buffer-alist)))
        (unless (eq ebib-layout 'index-only)
          (set-window-buffer entry-window (cdr (assoc 'entry ebib-buffer-alist))))
        (set-window-dedicated-p index-window t)
        (if (eq ebib-layout 'custom)
            (set-window-dedicated-p entry-window t))))))

(defun ebib-create-buffers ()
  "Creates the buffers for Ebib."
  ;; first we create a buffer for multiline editing.  this one does *not*
  ;; have a name beginning with a space, because undo-info is normally
  ;; present in an edit buffer.
  (add-to-list 'ebib-buffer-alist (cons 'multiline (get-buffer-create "*Ebib-edit*")))
  (with-current-buffer (cdr (assoc 'multiline ebib-buffer-alist))
    (funcall ebib-multiline-major-mode)
    (ebib-multiline-mode t))
  ;; then we create a buffer to hold the fields of the current entry.
  (add-to-list 'ebib-buffer-alist (cons 'entry (get-buffer-create "*Ebib-entry*")))
  (with-current-buffer (cdr (assoc 'entry ebib-buffer-alist))
    (ebib-entry-mode)
    (buffer-disable-undo))
  ;; then we create a buffer to hold the @STRING definitions
  (add-to-list 'ebib-buffer-alist (cons 'strings (get-buffer-create "*Ebib-strings*")))
  (with-current-buffer (cdr (assoc 'strings ebib-buffer-alist))
    (ebib-strings-mode)
    (buffer-disable-undo))
  ;; the log buffer
  (add-to-list 'ebib-buffer-alist (cons 'log (get-buffer-create "*Ebib-log*")))
  (with-current-buffer (cdr (assoc 'log ebib-buffer-alist))
    (erase-buffer)
    (insert "Ebib log messages\n\n(Press C-v or SPACE to scroll down, M-v or `b' to scroll up, `q' to quit.)\n\n")
    (ebib-log-mode)
    (buffer-disable-undo))
  ;; and lastly we create a buffer for the entry keys.
  (add-to-list 'ebib-buffer-alist (cons 'index (get-buffer-create " none")))
  (with-current-buffer (cdr (assoc 'index ebib-buffer-alist))
    (ebib-index-mode)
    (buffer-disable-undo)
    (if ebib-index-mode-line
        (setq mode-line-format ebib-index-mode-line))))

(defun ebib-quit ()
  "Quits Ebib.
The Ebib buffers are killed, all variables except the keymaps are set to nil."
  (interactive)
  (when (if (ebib-modified-p)
            (yes-or-no-p "There are modified databases. Quit anyway? ")
          (y-or-n-p "Quit Ebib? "))
    (ebib-keywords-save-all-new)
    (ebib-filters-update-filters-file)
    (mapc #'(lambda (buffer)
              (kill-buffer buffer))
          (mapcar #'cdr ebib-buffer-alist))
    (setq ebib-databases nil
          ebib-cur-db nil
          ebib-buffer-alist nil
          ebib-initialized nil
          ebib-index-highlight nil
          ebib-fields-highlight nil
          ebib-strings-highlight nil
          ebib-export-filename nil
          ebib-window-before nil
          ebib-buffer-before nil
          ebib-cur-keys-list nil
          ebib-keywords-files-alist nil
          ebib-keywords-list-per-session nil
          ebib-filters-alist nil)
    (set-window-configuration ebib-saved-window-config)
    (remove-hook 'kill-emacs-query-functions 'ebib-kill-emacs-query-function)
    (message "")))

(defun ebib-kill-emacs-query-function ()
  "Ask if the user wants to save any modified databases and added
keywords when Emacs is killed."
  (when (or (not (ebib-modified-p))
            (if (y-or-n-p "Save all unsaved Ebib databases? ")
                (progn
                  (ebib-save-all-databases)
                  t)
              (yes-or-no-p "Ebib holds modified databases. Kill anyway? ")))
    (ebib-keywords-save-all-new)
    t))

;;;;;;;;;;;;;;;;
;; index-mode ;;
;;;;;;;;;;;;;;;;

(eval-and-compile
  (define-prefix-command 'ebib-prefix-map)
  (suppress-keymap ebib-prefix-map))

;; macro to redefine key bindings.

(defmacro ebib-key (buffer key &optional command prefixed)
  (cond
   ((eq buffer 'index)
    (let ((one `(define-key ebib-index-mode-map ,key (quote ,command)))
          (two (when (or prefixed
                         (null command))
                 `(define-key ebib-prefix-map ,key (quote ,command)))))
      (if two
          `(progn ,one ,two)
        one)))
   ((eq buffer 'entry)
    `(define-key ebib-entry-mode-map ,key (quote ,command)))
   ((eq buffer 'strings)
    `(define-key ebib-strings-mode-map ,key (quote ,command)))
   ((eq buffer 'mark-prefix)
    `(progn
       (define-key ebib-index-mode-map (format "%c" ebib-prefix-key) nil)
       (define-key ebib-index-mode-map ,key 'ebib-prefix-map)
       (setq ebib-prefix-key (string-to-char ,key))))
   ((eq buffer 'multiline)
    `(progn
       (define-key ebib-multiline-mode-map "\C-c" nil)
       (mapc #'(lambda (command)
                 (define-key ebib-multiline-mode-map (format "\C-c%s%c" ,key (car command)) (cdr command)))
             '((?q . ebib-quit-multiline-edit-and-save)
               (?c . ebib-cancel-multiline-edit)
               (?s . ebib-save-from-multiline-edit)))
       (setq ebib-multiline-key (string-to-char ,key))))))

(defvar ebib-index-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    map)
  "Keymap for the ebib index buffer.")

;; we define the keys with ebib-key rather than with define-key, because
;; that automatically sets up ebib-prefix-map as well.
(ebib-key index [up] ebib-prev-entry)
(ebib-key index [down] ebib-next-entry)
(ebib-key index [right] ebib-next-database)
(ebib-key index [left] ebib-prev-database)
(ebib-key index [prior] ebib-index-scroll-down)
(ebib-key index [next] ebib-index-scroll-up)
(ebib-key index [home] ebib-goto-first-entry)
(ebib-key index [end] ebib-goto-last-entry)
(ebib-key index [return] ebib-select-and-popup-entry)
(ebib-key index " " ebib-index-scroll-up)
(ebib-key index "/" ebib-search)
(ebib-key index "&" ebib-filters-logical-and)
(ebib-key index "|" ebib-filters-logical-or)
(ebib-key index "~" ebib-filters-logical-not)
(ebib-key index ";" ebib-prefix-map)
(ebib-key index "?" ebib-info)
(ebib-key index "a" ebib-add-entry)
(ebib-key index "b" ebib-index-scroll-down)
(ebib-key index "c" ebib-index-c)
(ebib-key index "C" ebib-follow-crossref)
(ebib-key index "d" ebib-delete-entry t)
(ebib-key index "e" ebib-edit-entry)
(ebib-key index "E" ebib-edit-keyname)
(ebib-key index "f" ebib-view-file)
(ebib-key index "F" ebib-filters-map)
(ebib-key index "g" ebib-goto-first-entry)
(ebib-key index "G" ebib-goto-last-entry)
(ebib-key index "h" ebib-index-help)
(ebib-key index "i" ebib-browse-doi)
(ebib-key index "j" ebib-next-entry)
(ebib-key index "J" ebib-switch-to-database)
(ebib-key index "k" ebib-prev-entry)
(ebib-key index "K" ebib-generate-autokey)
(ebib-key index "l" ebib-show-log)
(ebib-key index "m" ebib-mark-entry t)
(ebib-key index "n" ebib-search-next)
(ebib-key index [(control n)] ebib-next-entry)
(ebib-key index [(meta n)] ebib-index-scroll-up)
(ebib-key index "o" ebib-load-bibtex-file)
(ebib-key index "p" ebib-push-bibtex-key t)
(ebib-key index [(control p)] ebib-prev-entry)
(ebib-key index [(meta p)] ebib-index-scroll-down)
(ebib-key index "P" ebib-edit-preamble)
(ebib-key index "q" ebib-quit)
(ebib-key index "r" ebib-reload-current-database)
(ebib-key index "R" ebib-reload-all-databases)
(ebib-key index "s" ebib-save-current-database)
(ebib-key index "S" ebib-edit-strings)
(ebib-key index "u" ebib-browse-url)
(ebib-key index "x" ebib-export-entry t)
(ebib-key index "\C-xb" ebib-leave-ebib-windows)
(ebib-key index "\C-xk" ebib-quit)
(ebib-key index "X" ebib-export-preamble)
(ebib-key index "z" ebib-leave-ebib-windows)
(ebib-key index "Z" ebib-lower)

(defun ebib-switch-to-database-nth (key)
  (interactive (list (if (featurep 'xemacs)
                         (event-key last-command-event)
                       last-command-event)))
  (ebib-switch-to-database (- (if (featurep 'xemacs)
                                  (char-to-int key)
                                key) 48)))

(mapc #'(lambda (key)
          (define-key ebib-index-mode-map (format "%d" key)
            'ebib-switch-to-database-nth))
      '(1 2 3 4 5 6 7 8 9))

(define-derived-mode ebib-index-mode
  fundamental-mode "Ebib-index"
  "Major mode for the Ebib index buffer."
  (setq buffer-read-only t)
  (setq cursor-type nil)
  (setq truncate-lines t))

(easy-menu-define ebib-index-menu ebib-index-mode-map "Ebib index menu"
  '("Ebib"
    ["Open Database..." ebib-load-bibtex-file t]
    ["Merge Database..." ebib-merge-bibtex-file (and ebib-cur-db (not (ebib-db-get-filter ebib-cur-db)))]
    ["Save Database" ebib-save-current-database (and ebib-cur-db
                                                     (ebib-db-modified-p ebib-cur-db))]
    ["Save All Databases" ebib-save-all-databases (ebib-modified-p)]
    ["Save Database As..." ebib-write-database ebib-cur-db]
    ["Close Database" ebib-close-database ebib-cur-db]
    "--"
    ["Save New Keywords For Database" ebib-keywords-save-cur-db (ebib-keywords-new-p ebib-cur-db)]
    ["Save All New Keywords" ebib-keywords-save-all-new (ebib-keywords-new-p)]
    "--"
    ("Entry"
     ["Add" ebib-add-entry (and ebib-cur-db (not (ebib-db-get-filter ebib-cur-db)))]
     ["Edit" ebib-edit-entry ebib-cur-keys-list]
     ["Delete" ebib-delete-entry (and ebib-cur-db
                                      (ebib-cur-entry-key)
                                      (not (ebib-db-get-filter ebib-cur-db)))])
    ["Edit Strings" ebib-edit-strings (and ebib-cur-db (not (ebib-db-get-filter ebib-cur-db)))]
    ["Edit Preamble" ebib-edit-preamble (and ebib-cur-db (not (ebib-db-get-filter ebib-cur-db)))]
    "--"
    ["Open URL" ebib-browse-url (ebib-db-get-field-value ebib-standard-url-field (ebib-cur-entry-key) ebib-cur-db 'noerror)]
    ["Open DOI" ebib-browse-doi (ebib-db-get-field-value ebib-standard-doi-field (ebib-cur-entry-key) ebib-cur-db 'noerror)]
    ["View File" ebib-view-file (ebib-db-get-field-value ebib-standard-file-field (ebib-cur-entry-key) ebib-cur-db 'noerror)]
    ("Print Entries"
     ["As Bibliography" ebib-latex-entries (and ebib-cur-db (not (ebib-db-get-filter ebib-cur-db)))]
     ["As Index Cards" ebib-print-entries ebib-cur-db]
     ["Print Multiline Fields" ebib-toggle-print-multiline :enable t
      :style toggle :selected ebib-print-multiline]
     ["Print Cards on Separate Pages" ebib-toggle-print-newpage :enable t
      :style toggle :selected ebib-print-newpage])
    "--"
    ("Options"
     ["Show Hidden Fields" ebib-toggle-hidden :enable t
      :style toggle :selected (not ebib-hide-hidden-fields)]
     ["Use Timestamp" ebib-toggle-timestamp :enable t
      :style toggle :selected ebib-use-timestamp]
     ["Save Cross-Referenced Entries First" ebib-toggle-xrefs-first :enable t
      :style toggle :selected ebib-save-xrefs-first]
     ["Allow Identical Fields" ebib-toggle-identical-fields :enable t
      :style toggle :selected ebib-allow-identical-fields]
     ["Full Layout" ebib-toggle-layout :enable t
      :style toggle :selected (eq ebib-layout 'full)]
     ["Modify Entry Types" ebib-customize-entry-types t]
     ["Customize Ebib" ebib-customize t])
    ["View Log Buffer" ebib-show-log t]
    ["Lower Ebib" ebib-lower t]
    ["Quit" ebib-quit t]
    ["Help on Ebib" ebib-info t]))

(easy-menu-add ebib-index-menu ebib-index-mode-map)

(defun ebib-customize ()
  "Switches to Ebib's customisation group."
  (interactive)
  (ebib-lower)
  (customize-group 'ebib))

(defun ebib-customize-entry-types ()
  "Customizes `ebib-entry-types'."
  (interactive)
  (ebib-lower)
  (customize-variable 'ebib-entry-types))

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

(defun ebib-load-bibtex-file (&optional file)
  "Open a BibTeX file."
  (interactive)
  (unless file
    (setq file (ebib-ensure-extension (read-file-name "File to open: " "~/") (car ebib-bibtex-extensions))))
  (ebib-load-bibtex-file-internal file)
  (ebib-redisplay))

(defun ebib-load-bibtex-file-internal (file)
  "Helper function for `ebib-load-bibtex-file'.
Note: it is assumed that FILE is a fully expanded filename."
  (let ((db (ebib-get-db-from-filename file)))
    (if db                              ; FILE is already open in Ebib.
        (setq ebib-cur-db db)
      (setq ebib-cur-db (ebib-create-new-database))
      (ebib-db-set-filename file ebib-cur-db)
      (setq ebib-log-error nil)         ; we haven't found any errors
      (ebib-log 'log "%s: Opening file %s" (format-time-string "%d %b %Y, %H:%M:%S") file)
      (if (file-exists-p file)
          (progn
            ;; load the entries in the file
            (ebib-load-entries file ebib-cur-db)
            ;; If the user makes any changes, we'll want to create a back-up.
            (ebib-db-set-backup t ebib-cur-db)
            (ebib-db-set-current-entry-key t ebib-cur-db)
            (ebib-set-modified nil))
        ;; if the file does not exist, we need to issue a message.
        (ebib-log 'message "(New file)"))
      ;; add keywords for the new database
      (ebib-keywords-load-keywords ebib-cur-db)
      (if ebib-keywords-files-alist
          (ebib-log 'log "Using keywords from %s." (ebib-keywords-get-file ebib-cur-db))
        (ebib-log 'log "Using general keyword list.")))))

(defun ebib-reload-current-database ()
  "Reload the current database from disk."
  (interactive)
  (ebib-execute-when
    ((entries)
     (when (yes-or-no-p "Reload current database from file ")
       (ebib-reload-database ebib-cur-db)
       (ebib-set-modified nil)
       (ebib-redisplay)
       (message "Database reloaded")))
    ((default) (beep))))

(defun ebib-reload-all-databases ()
  "Reload all databases from disk."
  (interactive)
  (when (yes-or-no-p "Reload all databases from file ")
    (mapc #'(lambda (db)
              (ebib-reload-database db)
              (ebib-set-modified nil db))
          ebib-databases)
    (ebib-redisplay)))

(defun ebib-reload-database (db)
  "Reload database DB from disk."
  (let ((file (ebib-db-get-filename db)))
    ;; first clear out some variables
    (ebib-db-clear-database db)
    ;; then load the file
    (ebib-log 'log "%s: Reloading file %s" (format-time-string "%d-%b-%Y: %H:%M:%S") file)
    (ebib-db-set-filename file db)
    (ebib-load-entries file db)
    (ebib-db-set-current-entry-key t db)))

(defun ebib-merge-bibtex-file ()
  "Merges a BibTeX file into the current database."
  (interactive)
  (ebib-execute-when
    ((real-db)
     (let ((file (expand-file-name (read-file-name "File to merge: "))))
       (if (not (file-readable-p file))
           (error "No such file: %s" file)
         (setq ebib-log-error nil)      ; we haven't found any errors (yet)
         (ebib-log 'log "%s: Merging file %s" (format-time-string "%d-%b-%Y: %H:%M:%S") (ebib-db-get-filename ebib-cur-db))
         (ebib-load-entries file ebib-cur-db)
         (unless (ebib-cur-entry-key)
           (ebib-db-set-current-entry-key t ebib-cur-db))
         (ebib-redisplay)
         (ebib-set-modified t))))
    ((default) (beep))))

(defun ebib-load-entries (file db)
  "Load BibTeX entries from FILE into DB."
  (with-temp-buffer
    (with-syntax-table ebib-syntax-table
      (insert-file-contents file)
      (let ((result (ebib-find-bibtex-entries db nil)))
        ;; Log the results.
        (ebib-log 'message "%d entries, %d @STRINGs and %s @PREAMBLE found in file."
                  (car result)
                  (cadr result)
                  (if (cl-caddr result)
                      "a"
                    "no"))
        (when ebib-log-error
          (message "%s found! Press `l' to check Ebib log buffer." (nth ebib-log-error '("Warnings" "Errors"))))))))

(defun ebib-find-bibtex-entries (db timestamp)
  "Find the BibTeX entries in the current buffer.
The search is started at the beginnig of the buffer. All entries
found are stored in DB. Return value is a three-element list: the
first element is the number of entries found, the second the
number of @STRING definitions, and the third is T or NIL,
indicating whether a @PREAMBLE was found.

TIMESTAMP indicates whether a timestamp is to be added to each
entry. Note that a timestamp is only added if `ebib-use-timestamp'
is set to T."
  (let ((n-entries 0)
        (n-strings 0)
        (preamble nil))
    (goto-char (point-min))
    (while (re-search-forward "^@" nil t) ; find the next entry
      (let ((beg (point)))
        (if (ebib-looking-at-goto-end (concat "\\(" ebib-bibtex-identifier "\\)[[:space:]]*[\(\{]") 1)
          (let ((entry-type (downcase (buffer-substring-no-properties beg (point)))))
            (ebib-looking-at-goto-end "[[:space:]]*[\(\{]")
            (cond
             ((equal entry-type "string") ; string and preamble must be treated differently
              (if (ebib-read-string db)
                  (setq n-strings (1+ n-strings))))
             ((equal entry-type "preamble")
              (when (ebib-read-preamble db)
                (setq preamble t)))
             ((equal entry-type "comment") ; ignore comments
              (ebib-log 'log "Comment at line %d ignored" (line-number-at-pos))
              (ebib-match-paren-forward (point-max)))
             ((assoc (intern-soft entry-type) ebib-entry-types) ; if the entry type has been defined
              (if (ebib-read-entry entry-type db timestamp)
                  (setq n-entries (1+ n-entries))))
             ;; anything else we report as an unknown entry type.
             (t (ebib-log 'warning "Line %d: Unknown entry type `%s'. Skipping." (line-number-at-pos) entry-type)
                (ebib-match-paren-forward (point-max)))))
          (ebib-log 'error "Error: illegal entry type at line %d. Skipping" (line-number-at-pos)))))
    (list n-entries n-strings preamble)))

(defun ebib-read-string (db)
  "Read the @STRING definition beginning at the line POINT is on.
If a proper abbreviation and string are found, they are stored in
DB. Return the string if one was read, NIL otherwise."
  (let ((limit (save-excursion       ; we find the matching end parenthesis
                 (backward-char)
                 (ebib-match-paren-forward (point-max))
                 (point))))
    (skip-chars-forward "\"#%'(),={} \n\t\f" limit)
    (let ((beg (point)))
      (if (ebib-looking-at-goto-end (concat "\\(" ebib-bibtex-identifier "\\)[ \t\n\f]*=") 1)
        (ebib-ifstring (abbr (buffer-substring-no-properties beg (point)))
            (progn
              (skip-chars-forward "^\"{" limit)
              (let ((beg (point)))
                (ebib-ifstring (string (if (ebib-match-delim-forward limit)
                                     (buffer-substring-no-properties beg (1+ (point)))
                                   nil))
                    (unless (ebib-db-set-string abbr string db 'noerror)
                      (ebib-log 'warning (format "Line %d: @STRING definition `%s' duplicated. Skipping."
                                                   (line-number-at-pos) abbr)))))))
        (ebib-log 'error "Error: illegal string identifier at line %d. Skipping" (line-number-at-pos))))))

(defun ebib-read-preamble (db)
  "Read the @PREAMBLE definition and stores it in DB.
If there was already another @PREAMBLE definition, the new one is
added to the existing one with a hash sign `#' between them."
  (let ((beg (point)))
    (forward-char -1)
    (when (ebib-match-paren-forward (point-max))
      (ebib-db-set-preamble (buffer-substring-no-properties beg (point)) db 'append))))

(defun ebib-read-entry (entry-type db &optional timestamp)
  "Read a BibTeX entry and store it in DB.
Returns the entry key if an entry was found, NIL otherwise.
Optional argument TIMESTAMP indicates whether a timestamp is to
be added. (Whether a timestamp is actually added, also depends on
`ebib-use-timestamp'.)"
  (setq entry-type (intern-soft entry-type))
  (let ((limit (save-excursion
                 (backward-char)
                 (ebib-match-paren-forward (point-max))
                 (point)))
        (beg (progn
               (skip-chars-forward " \n\t\f") ; note the space!
               (point))))
    (let (entry-key)
      (if (ebib-looking-at-goto-end (concat "\\("
                                            ebib-key-regexp
                                            "\\)[ \t\n\f]*,")
                                    1)  ; this delimits the entry key
          (progn                        ; if we found an entry key
            (setq entry-key (buffer-substring-no-properties beg (point)))
            (when (string= entry-key "") ; to be on the safe side
              (setq entry-key (ebib-generate-tempkey db))
              (ebib-log 'warning "Line %d: Temporary key generated for entry." (line-number-at-pos)))
            (skip-chars-forward "^,")) ; move to the comma after the entry key
        ;; if there is no legal entry key, we create a temporary key and try to read the entry anyway.
        (setq entry-key (ebib-generate-tempkey db))
        (ebib-log 'warning "Line %d: No entry key; generating temporary key." (line-number-at-pos)))
      (if (ebib-db-get-entry entry-key db 'noerror)
          (ebib-log 'warning "Line %d: Entry `%s' duplicated. Skipping." (line-number-at-pos) entry-key)
        (ebib-store-entry entry-key (list (cons '=type= entry-type)) db timestamp))
      (cl-loop for field = (ebib-find-bibtex-field limit)
               while field do
               ;; TODO We pass 'overwrite if `ebib-allow-identical-fields'
               ;; is nil in order to overwrite a possible timestamp. This
               ;; has to be handled better, though!
               (ebib-db-set-field-value (car field) (cdr field) entry-key db (if ebib-allow-identical-fields
                                                                                 ebib-field-separator 
                                                                               'overwrite)
                                        'as-is))
      entry-key)))                      ; Return the entry key.

(defun ebib-find-bibtex-field (limit)
  "Find the field after point.
Return a cons (FIELD . VALUE), or NIL if no field was found."
  (skip-chars-forward "\"#%'(),={} \n\t\f" limit) ; move to the first char of the field name
  (unless (>= (point) limit)   ; if we haven't reached the end of the entry
    (let ((beg (point)))
      (if (ebib-looking-at-goto-end (concat "\\(" ebib-bibtex-identifier "\\)[ \t\n\f]*=") 1)
          (let ((field-type (intern (downcase (buffer-substring-no-properties beg (point))))))
            (skip-chars-forward "#%'(),=} \n\t\f" limit) ; move to the field contents
            (let* ((beg (point))
                   (field-contents (buffer-substring-no-properties beg (ebib-find-end-of-field limit))))
              (cons field-type field-contents)))
        (ebib-log 'error "Error: illegal field name found at line %d. Skipping" (line-number-at-pos))))))

(defun ebib-find-end-of-field (limit)
  "Move POINT to the end of a field's contents and return POINT.
The contents of a field is delimited by a comma or by the closing brace of
the entry. The latter is at position LIMIT."
  (while (and (not (eq (char-after) ?\,))
              (< (point) limit))
    (ebib-match-delim-forward limit) ; check if we're on a delimiter and if so, jump to the matching closing delimiter
    (forward-char 1))
  (if (= (point) limit)
      (skip-chars-backward " \n\t\f"))
  (point))

(defun ebib-leave-ebib-windows ()
  "Leaves the Ebib windows, lowering them if necessary."
  (interactive)
  (ebib-lower t))

(defun ebib-lower (&optional soft)
  "Hides the Ebib windows.
If optional argument SOFT is non-NIL, just switch to a non-Ebib
buffer if Ebib is not occupying the entire frame."
  (interactive)
  (unless (member (window-buffer) (mapcar #'cdr ebib-buffer-alist))
    (error "Ebib is not active "))
  (cond
   ((and soft (eq ebib-layout 'custom))
    (select-window ebib-window-before))
   ((and soft (eq ebib-layout 'index-only))
    (other-window 1)
    (if (member (current-buffer) (mapcar #'cdr ebib-buffer-alist))
        (switch-to-buffer nil)))
   (t (set-window-configuration ebib-saved-window-config)))
  (mapc #'(lambda (buffer)
            (bury-buffer buffer))
        (mapcar #'cdr ebib-buffer-alist)))

(defun ebib-prev-entry ()
  "Moves to the previous BibTeX entry."
  (interactive)
  (ebib-execute-when
    ((entries)
     ;; if the current entry is the first entry,
     (let ((prev (ebib-prev-elem (ebib-cur-entry-key) ebib-cur-keys-list)))
       (if (not prev)                   ; if we're on the first entry
           (beep)                       ; just beep
         (ebib-db-set-current-entry-key prev ebib-cur-db)
         (goto-char (ebib-highlight-start ebib-index-highlight))
         (forward-line -1)
         (ebib-set-index-highlight)
         (ebib-fill-entry-buffer))))
    ((default)
     (beep))))

(defun ebib-next-entry ()
  "Moves to the next BibTeX entry."
  (interactive)
  (ebib-execute-when
    ((entries)
     (let ((next (ebib-next-elem (ebib-cur-entry-key) ebib-cur-keys-list)))
       (if (not next)                   ; if we're on the last entry,
           (beep)                       ; just beep
         (ebib-db-set-current-entry-key next ebib-cur-db)
         (goto-char (ebib-highlight-start ebib-index-highlight))
         (forward-line 1)
         (ebib-set-index-highlight)
         (ebib-fill-entry-buffer))))
    ((default)
     (beep))))

(defun ebib-add-entry-stub (&optional entry db)
  "Add ENTRY to DB in the form of a stub.
Returns the database key of the created entry. ENTRY is an
optional alist consisting of (FIELD . VALUE) pairs. The alist is
converted into a BibTeX entry stub and added to DB, which
defaults to the current database. If an entry alist doesn't
contain the `=type=' field, the entry type is set to the value of
`ebib-default-type'. If it doesn't contain a `=key=' field, a key
is created of the form \"<new-entry%d>\", where %d is replaced
with a number in ascending sequence."
  (unless db
    (setq db ebib-cur-db))
  (let ((fields ())
        entry-key)
    (cl-dolist (props entry)
      ;;aggregate properties, some require special handling
      (cond
       ((eq (car props) '=key=)
        (setq entry-key (cdr props)))
       ((eq (car props) '=type=)   ; the =type= field should not be braced.
        (push props fields))
       ((eq (car props) ebib-standard-file-field)
        (let ((short-file (ebib-file-relative-name (expand-file-name (cdr props)))))
          (push (cons ebib-standard-file-field (ebib-db-brace short-file)) fields)))
       (t
        (push (cons (car props) (ebib-db-brace (cdr props))) fields))))
    ;;check for required
    (unless entry-key
      (setq entry-key (ebib-generate-tempkey db)))
    (unless (assoc '=type= fields)
      (push (cons '=type= ebib-default-type) fields))
    ;; insert
    (ebib-store-entry entry-key fields db t ebib-uniquify-keys)
    entry-key))

(defun ebib-add-entry ()
  "Interactively adds a new entry to the database."
  (interactive)
  (ebib-execute-when
    ((real-db)
     (let ((entry-alist (list)))
       (unless ebib-autogenerate-keys
         (add-to-list 'entry-alist (cons '=key= (read-string "New entry key: " nil 'ebib-key-history))))
       (ebib-db-set-current-entry-key (ebib-add-entry-stub entry-alist ebib-cur-db) ebib-cur-db)
       (ebib-redisplay)
       (ebib-edit-entry-internal)))
    ((no-database)
     (error "No database open. Use `o' to open a database first"))
    ((default)
     (beep))))

(defun ebib-add-file-entry (&optional filepath allow-duplicates disable-prompt db)
  "Add an entry stub for an optional FILEPATH to DB.
If FILEPATH is a list, add entries for each file contained
within. If FILEPATH is a directory, add entries for all its
contents. And if FILEPATH is not given, prompt the user to browse
in the minibuffer, unless DISABLE-PROMPT is T. If a FILEPATH is
already referenced by an entry in the DB, then it is ignored by
default. If ALLOW-DUPLICATES is true, then add new entry stubs
for each file anyway."
  (interactive)
  (or db
      (setq db ebib-cur-db))
  (let (all-entry-files)
    (cl-labels
        ((file-exists-in-db-p (fp)
                              (if (member (locate-file fp ebib-file-search-dirs) all-entry-files)
                                  t))
         (add-file-entry (fp)
                         (cond
                          ((listp fp)
                           (cl-dolist (file fp) (add-file-entry file)))
                          ((file-directory-p fp)
                           (add-file-entry (directory-files fp t "^\\([^.]\\)"))) ;ignore hidden
                          ((file-exists-p fp)
                           (if (and (null allow-duplicates) (file-exists-in-db-p fp))
                               (message "File %s already exists in db, skipping" fp)
                             (ebib-add-entry-stub (list (cons ebib-standard-file-field fp)) db)
                             (message "Adding file %s" fp)))
                          (t
                           (error "Invalid file %s" fp)))))
      ;;prompt for file
      (if (and (null filepath) (null disable-prompt))
          (setq filepath (read-file-name "Add file or directory: " (file-name-as-directory (car ebib-file-search-dirs)))))
      ;;collect all file paths from db entries into single list
      (unless allow-duplicates
        (cl-dolist (entry-key (ebib-db-list-keys db 'nosort))
          (let ((entry-files (ebib-db-get-field-value ebib-standard-file-field entry-key db 'noerror 'unbraced)))
            (if entry-files
                (cl-dolist (fp (split-string entry-files ebib-filename-separator))
                  (add-to-list 'all-entry-files (locate-file fp ebib-file-search-dirs)))))))
      (add-file-entry filepath)
      (ebib-db-set-current-entry-key t ebib-cur-db)
      (ebib-redisplay))))

(defun ebib-generate-autokey ()
  "Automatically generate a key for the current entry.
This function uses the function BIBTEX-GENERATE-AUTOKEY to
generate the key, see that function's documentation for details."
  (interactive)
  (ebib-execute-when
    ((real-db entries)
     (let ((new-key
            (with-temp-buffer
              (ebib-format-entry (ebib-cur-entry-key) ebib-cur-db nil)
              (let ((x-ref (ebib-db-get-field-value 'crossref (ebib-cur-entry-key) ebib-cur-db 'noerror 'unbraced)))
                (if x-ref
                    (ebib-format-entry x-ref ebib-cur-db nil)))
              (goto-char (point-min))
              (bibtex-generate-autokey))))
       (if (equal new-key "")
           (error (format "Cannot create key"))
         (ebib-update-keyname new-key))))
    ((default)
     (beep))))

(defun ebib-generate-tempkey (&optional db)
  "Generate a unique temp key in DB or the current database.
Keys are in the form: <new-entry1>, <new-entry2>, ..."
  (unless db
    (setq db ebib-cur-db))
  (let ((key-list (ebib-db-list-keys db 'nosort))
        (entry-key "<new-entry1>")
        (key-count 2))
    (while (member entry-key key-list)
      (setq entry-key (format "<new-entry%d>" key-count))
      (cl-incf key-count))
    entry-key))

(defun ebib-index-c ()
  "Helper function for the `c' key in the index buffer."
  (interactive)
  (if (ebib-db-filtered-p ebib-cur-db)
      (ebib-filters-cancel-filter)
    (ebib-close-database)))

(defun ebib-close-database ()
  "Closes the current BibTeX database."
  (interactive)
  (ebib-execute-when
    ((database)
     (when (if (ebib-db-modified-p ebib-cur-db)
               (yes-or-no-p "Database modified. Close it anyway? ")
             (y-or-n-p "Close database? "))
       (ebib-keywords-save-new-keywords ebib-cur-db)
       (let ((to-be-deleted ebib-cur-db)
             (new-db (ebib-next-elem ebib-cur-db ebib-databases)))
         (setq ebib-databases (delq to-be-deleted ebib-databases))
         (if ebib-databases     ; do we still have another database loaded?
             (progn
               (setq ebib-cur-db (or new-db
                                     (ebib-last1 ebib-databases)))
               (ebib-redisplay))
           ;; otherwise, we have to clean up a little and empty all the buffers.
           (setq ebib-cur-db nil)
           (mapc #'(lambda (buf) ; this is just to avoid typing almost the same thing three times...
                     (with-current-buffer (car buf)
                       (with-ebib-buffer-writable
                         (erase-buffer))
                       (ebib-delete-highlight (cadr buf))))
                 (list (list (cdr (assoc 'entry ebib-buffer-alist)) ebib-fields-highlight)
                       (list (cdr (assoc 'index ebib-buffer-alist)) ebib-index-highlight)
                       (list (cdr (assoc 'strings ebib-buffer-alist)) ebib-strings-highlight)))
           ;; multiline edit buffer
           (with-current-buffer (cdr (assoc 'multiline ebib-buffer-alist))
             (with-ebib-buffer-writable
               (erase-buffer)))
           (with-current-buffer (cdr (assoc 'index ebib-buffer-alist))
             (rename-buffer " none"))
           (setq ebib-cur-keys-list nil))
         (message "Database closed."))))))

(defun ebib-goto-first-entry ()
  "Moves to the first BibTeX entry in the database."
  (interactive)
  (ebib-execute-when
    ((entries)
     (ebib-db-set-current-entry-key (car ebib-cur-keys-list) ebib-cur-db)
     (with-current-buffer (cdr (assoc 'index ebib-buffer-alist))
       (goto-char (point-min))
       (ebib-set-index-highlight)
       (ebib-fill-entry-buffer)))
    ((default)
     (beep))))

(defun ebib-goto-last-entry ()
  "Moves to the last entry in the BibTeX database."
  (interactive)
  (ebib-execute-when
    ((entries)
     (ebib-db-set-current-entry-key (ebib-last1 ebib-cur-keys-list) ebib-cur-db)
     (with-current-buffer (cdr (assoc 'index ebib-buffer-alist))
       (goto-char (point-min))
       (forward-line (1- (length ebib-cur-keys-list)))
       (ebib-set-index-highlight)
       (ebib-fill-entry-buffer)))
    ((default)
     (beep))))

(defun ebib-edit-entry ()
  "Edits the current BibTeX entry."
  (interactive)
  (ebib-execute-when
    ((entries)
     (ebib-edit-entry-internal))
    ((default)
     (beep))))

(defun ebib-edit-entry-internal ()
  "Helper function for `ebib-edit-entry'."
  (setq ebib-cur-entry-fields (ebib-get-all-fields (ebib-db-get-field-value '=type= (ebib-cur-entry-key) ebib-cur-db)))
  (ebib-pop-to-buffer 'entry)
  (ebib-set-fields-highlight))

(defun ebib-edit-keyname ()
  "Changes the key of a BibTeX entry."
  (interactive)
  (ebib-execute-when
    ((real-db entries)
     (let ((cur-keyname (ebib-cur-entry-key)))
       (ebib-ifstring (new-keyname (read-string (format "Change `%s' to: " cur-keyname)
                                         cur-keyname
                                         'ebib-key-history))
           (ebib-update-keyname new-keyname))))
    ((default)
     (beep))))

(defun ebib-update-keyname (new-key)
  "Change the key of the current BibTeX entry to NEW-KEY.
This function updates both the database and the buffer."
  (let ((marked (ebib-db-marked-p (ebib-cur-entry-key) ebib-cur-db))
        (actual-new-key (ebib-db-change-key (ebib-cur-entry-key) new-key ebib-cur-db (if ebib-uniquify-keys 'uniquify 'noerror))))
    (when actual-new-key
      (ebib-db-set-current-entry-key actual-new-key ebib-cur-db)
      (if marked (ebib-mark-entry))
      (ebib-redisplay)
      (ebib-set-modified t))))

(defun ebib-mark-entry ()
  "Marks or unmarks the current entry.
When used with the prefix key, mark all entries or unmark all
marked entries."
  (interactive)
  (if (ebib-called-with-prefix)
      (ebib-execute-when
        ((marked-entries)
         (ebib-db-unmark-entry 'all ebib-cur-db)
         (ebib-fill-index-buffer)
         (message "All entries unmarked"))
        ((entries)
         (ebib-db-mark-entry 'all ebib-cur-db)
         (ebib-fill-index-buffer)
         (message "All entries marked"))
        ((default)
         (beep)))
    (ebib-execute-when
      ((entries)
       (with-current-buffer (cdr (assoc 'index ebib-buffer-alist))
         (with-ebib-buffer-writable 
           (ebib-db-toggle-mark (ebib-cur-entry-key) ebib-cur-db)
           (ebib-display-mark (ebib-db-marked-p (ebib-cur-entry-key) ebib-cur-db)
                              (ebib-highlight-start ebib-index-highlight)
                              (ebib-highlight-end ebib-index-highlight)))))
      ((default)
       (beep)))))

(defun ebib-index-scroll-down ()
  "Moves one page up in the database."
  (interactive)
  (ebib-execute-when
    ((entries)
     (scroll-down)
     (ebib-select-entry))
    ((default)
     (beep))))

(defun ebib-index-scroll-up ()
  "Moves one page down in the database."
  (interactive)
  (ebib-execute-when
    ((entries)
     (scroll-up)
     (ebib-select-entry))
    ((default)
     (beep))))

(defun ebib-format-entry (key db timestamp)
  "Write entry KEY in DB into the current buffer in BibTeX format.
If TIMESTAMP is T and `ebib-use-timestamp' is set, a timestamp is
added to the entry, possibly overwriting an existing timestamp."
  (let ((entry (ebib-db-get-entry key db 'noerror)))
    (when entry
      (insert (format "@%s{%s,\n" (cdr (assoc '=type= entry)) key))
      (mapc #'(lambda (field)
                (unless (or (eq (car field) '=type=)
                            (and (eq (car field) 'timestamp) timestamp ebib-use-timestamp))
                  (insert (format "\t%s = %s,\n" (car field) (cdr field)))))
            entry)
      (if (and timestamp ebib-use-timestamp)
          (insert (format "\ttimestamp = {%s}" (format-time-string ebib-timestamp-format)))
        (delete-char -2))               ; the final ",\n" must be deleted
      (insert "\n}\n\n"))))

(defun ebib-format-strings (db)
  "Write the @STRINGs of DB into the current buffer in BibTeX format."
  (mapc #'(lambda (str)
            (insert (format "@STRING{%s = %s}\n" (car str) (cdr str))))
        (ebib-db-get-all-strings db))
  (insert "\n"))

(defun ebib-get-sortstring (entry-key sortkey-list db)
  "Return the field value on which the entry ENTRY-KEY is to be sorted.
DB is the database that contains the entry referred to by
ENTRY-KEY. SORTKEY-LIST is a list of fields that are considered
in order for the sort value."
  (let ((sort-string nil))
    (while (and sortkey-list
                (null (setq sort-string (ebib-db-get-field-value (car sortkey-list) entry-key db 'noerror 'unbraced))))
      (setq sortkey-list (cdr sortkey-list)))
    sort-string))

(defun ebib-format-database-as-bibtex (db)
  "Write database DB into the current buffer in BibTeX format."
  ;; We define two comparison functions for `sort'. These must simply
  ;; return non-NIL if the first element is to be sorted before the second.
  (cl-flet
      ;; The first one is simple: if X has a crossref field, it must be
      ;; sorted before Y (or at least *can* be, if Y also has a crossref
      ;; field).
      ((compare-xrefs (x y)
                      (ebib-db-get-field-value 'crossref x db 'noerror))
       ;; This one's a bit trickier. We iterate over the lists of fields in
       ;; `ebib-sort-order'. For each level, `ebib-get-sortstring' then
       ;; returns the string that can be used for sorting. If all fails,
       ;; sorting is done on the basis of the entry key.
       (entry< (x y)
               (let (sortstring-x sortstring-y)
                 (cl-loop for sort-list in ebib-sort-order do
                          (setq sortstring-x (ebib-get-sortstring x sort-list db))
                          (setq sortstring-y (ebib-get-sortstring y sort-list db))
                          while (string= sortstring-x sortstring-y))
                 (if (and sortstring-x sortstring-y)
                     (string< sortstring-x sortstring-y)
                   (string< x y))))) ; compare entry keys
    (when (ebib-db-get-preamble db)
      (insert (format "@PREAMBLE{%s}\n\n" (ebib-db-get-preamble db))))
    (ebib-format-strings db)
    ;; Only entries in `ebib-cur-keys-list' are saved, in case we're
    ;; writing a filtered db to a new file.
    (let ((sorted-list (copy-tree ebib-cur-keys-list)))
      (cond
       (ebib-save-xrefs-first
        (setq sorted-list (sort sorted-list #'compare-xrefs)))
       (ebib-sort-order
        (setq sorted-list (sort sorted-list #'entry<))))
      (mapc #'(lambda (key) (ebib-format-entry key db nil)) sorted-list))))

(defun ebib-make-backup (file)
  "Create a backup of FILE.
Honour `ebib-create-backups' and BACKUP-DIRECTORY-ALIST."
  (when ebib-create-backups
    (let ((backup-file (make-backup-file-name file)))
      (if (file-writable-p backup-file)
          (copy-file file backup-file t)
        (ebib-log 'error "Could not create backup file `%s'" backup-file)))))

(defun ebib-save-database (db)
  "Saves the database DB."
  (when (and (ebib-db-backup-p db)
             (file-exists-p (ebib-db-get-filename db)))
    (ebib-make-backup (ebib-db-get-filename db))
    (ebib-db-set-backup nil db))
  (with-temp-buffer
    (ebib-format-database-as-bibtex db)
    (write-region (point-min) (point-max) (ebib-db-get-filename db)))
  (ebib-set-modified nil db))

(defun ebib-write-database ()
  "Writes the current database to a different file.
If the current database is filtered, only the entries that match
the filter are saved. The original file is not deleted."
  (interactive)
  (ebib-execute-when
    ((database)
     (ebib-ifstring (new-filename (expand-file-name (read-file-name "Save to file: " "~/")))
         (when (or (not (file-exists-p new-filename))
                   (y-or-n-p (format (format "File %s already exists; overwrite " new-filename))))
           (with-temp-buffer
             (ebib-format-database-as-bibtex ebib-cur-db)
             (write-region (point-min) (point-max) new-filename nil nil nil))
           (if (ebib-db-get-filter ebib-cur-db)
               (message "Wrote filtered entries as new database to %s" new-filename)
             ;; If this wasn't a filtered db, we rename it.
             (ebib-db-set-filename new-filename ebib-cur-db 'overwrite)
             (rename-buffer (concat (format " %d:" (1+ (- (length ebib-databases)
                                                          (length (member ebib-cur-db ebib-databases)))))
                                    (file-name-nondirectory new-filename)))
             (ebib-set-modified nil)))))
    ((default)
     (beep))))

(defun ebib-save-current-database ()
  "Saves the current database."
  (interactive)
  (ebib-execute-when
    ((real-db)
     (if (not (ebib-db-modified-p ebib-cur-db))
         (message "No changes need to be saved.")
       (ebib-save-database ebib-cur-db)))
    ((filtered-db)
     ;; Saving a filtered db would result in saving only the entries that
     ;; match the filter.
     (error "Cannot save a filtered database. Use `w' to write to a file."))))

(defun ebib-save-all-databases ()
  "Saves all currently open databases if they were modified."
  (interactive)
  (mapc #'(lambda (db)
            (when (ebib-db-modified-p db)
              (ebib-save-database db)))
        ebib-databases)
  (message "All databases saved."))

(defun ebib-print-filename ()
  "Displays the filename of the current database in the minibuffer."
  (interactive)
  (message (ebib-db-get-filename ebib-cur-db)))

(defun ebib-follow-crossref ()
  "Follow the crossref field and jump to that entry.
If the current entry's crossref field is empty, search for the
first entry with the current entry's key in its crossref field."
  (interactive)
  (let ((new-cur-entry (ebib-db-get-field-value 'crossref (ebib-cur-entry-key) ebib-cur-db 'noerror 'unbraced)))
    (if new-cur-entry
        ;; If there is a cross-reference, see if we can find it.
        (cond
         ((member new-cur-entry ebib-cur-keys-list)
          (ebib-db-set-current-entry-key new-cur-entry ebib-cur-db)
          (ebib-redisplay))
         ((member new-cur-entry (ebib-db-list-keys ebib-cur-db 'nosort))
          (error "Crossreference `%s' not visible due to active filter" new-cur-entry))
         (t (error "Entry `%s' does not exist" new-cur-entry)))
      ;; Otherwise, we assume the user wants to search for entries
      ;; cross-referencing the current one.
      (setq ebib-search-string (ebib-cur-entry-key))
      (ebib-search-next))))

(defun ebib-toggle-hidden ()
  "Toggle viewing hidden fields."
  (interactive)
  (setq ebib-hide-hidden-fields (not ebib-hide-hidden-fields))
  (ebib-fill-entry-buffer))

(defun ebib-toggle-timestamp ()
  "Toggle using timestamp for new entries."
  (interactive)
  (setq ebib-use-timestamp (not ebib-use-timestamp)))

(defun ebib-toggle-xrefs-first ()
  "Toggle saving of crossreferenced entries first."
  (interactive)
  (setq ebib-save-xrefs-first (not ebib-save-xrefs-first)))

(defun ebib-toggle-identical-fields ()
  "Toggle whether Ebib allows identical fields when opening a .bib file."
  (interactive)
  (setq ebib-allow-identical-fields (not ebib-allow-identical-fields)))

(defun ebib-toggle-layout ()
  "Toggles the Ebib layout."
  (interactive)
  (if (eq ebib-layout 'full)
      (setq ebib-layout 'custom)
    (setq ebib-layout 'full))
  (ebib-lower)
  (ebib))

(defun ebib-toggle-print-newpage ()
  "Toggle whether index cards are printed with a newpage after each card."
  (interactive)
  (setq ebib-print-newpage (not ebib-print-newpage)))

(defun ebib-toggle-print-multiline ()
  "Toggle whether multiline fields are printed."
  (interactive)
  (setq ebib-print-multiline (not ebib-print-multiline)))

(defun ebib-delete-entry ()
  "Deletes the current entry from the database."
  (interactive)
  (cl-flet ((remove-entry (key)
                          (ebib-db-remove-entry key ebib-cur-db)
                          (ebib-db-unmark-entry key ebib-cur-db) ; This is harmless if key isn't marked.
                          (ebib-db-set-current-entry-key (or (ebib-next-elem key ebib-cur-keys-list)
                                                             (ebib-last1 ebib-cur-keys-list))
                                                         ebib-cur-db
                                                         'first)
                          (setq ebib-cur-keys-list (delete key ebib-cur-keys-list))))
    (ebib-execute-when
      ((entries) ; TODO this means we can delete an entry from a filtered db!
       (if (and (ebib-called-with-prefix) (ebib-db-marked-entries-p ebib-cur-db))
           (when (y-or-n-p "Delete all marked entries? ")
             (mapc #'remove-entry (ebib-db-list-marked-entries ebib-cur-db 'nosort))
             (message "Marked entries deleted."))
         (let ((cur-entry (ebib-cur-entry-key)))
           (when (y-or-n-p (format "Delete %s? " cur-entry))
             (remove-entry cur-entry)
             (message (format "Entry `%s' deleted." cur-entry)))))
       (ebib-set-modified t)
       (ebib-redisplay))
      ((default)
       (beep)))))

(defun ebib-select-and-popup-entry ()
  "Make the entry at point current and display it.
If `ebib-layout' is set to `index-only', also popup the entry
buffer and switch to it."
  (interactive)
  (ebib-execute-when
    ((entries)
     (ebib-select-entry)
     (when (eq ebib-layout 'index-only)
       ;; this makes the entry buffer visible but then switches to the
       ;; index buffer again.
       (ebib-pop-to-buffer 'entry)
       (ebib-pop-to-buffer 'index)))
    ((default)
     (beep))))

(defun ebib-select-entry ()
  "Make the entry at point current."
  (beginning-of-line)
  (let ((beg (point)))
    (let ((key (save-excursion
                 (skip-chars-forward "^ ")
                 (buffer-substring-no-properties beg (point)))))
      (ebib-db-set-current-entry-key key ebib-cur-db)
      (ebib-set-index-highlight)
      (ebib-fill-entry-buffer))))

;; the exporting functions will have to be redesigned completely. for now (1 Feb
;; 2012) we just define a new function ebib-export-entries. in the long run,
;; this should be the general exporting function, calling other functions as the
;; need arises.

(defun ebib-export-entries (entries &optional source-db filename)
  "Exports ENTRIES from SOURCE-DB to FILENAME.
ENTRIES is a list of entry keys. SOURCE-DB defaults to the
current database. If FILENAME is not provided, the user is asked
for one."
  (unless filename
    (setq filename (read-file-name
                    "File to export entries to:" "~/" nil nil ebib-export-filename)))
  (unless source-db
    (setq source-db ebib-cur-db))
  (with-temp-buffer
    (insert "\n")
    (mapc #'(lambda (key)
              (ebib-format-entry key ebib-cur-db nil))
          entries)
    (append-to-file (point-min) (point-max) filename)
    (setq ebib-export-filename filename)))

(defun ebib-export-entry (prefix)
  "Copies entries to another database.
The prefix argument indicates which database to copy the entry
to. If no prefix argument is present, a filename is asked to
which the entry is appended."
  (interactive "P")
  (let ((num (ebib-prefix prefix)))
    (if (ebib-called-with-prefix)
        (ebib-export-marked-entries num)
      (ebib-export-single-entry num))))

(defun ebib-export-single-entry (num)
  "Copies the current entry to another database.
NUM indicates which database to copy the entry to. If it is NIL,
a filename is asked to which the entry is appended."
  (ebib-execute-when
    ((entries)
     (if num
         (ebib-export-to-db num (format "Entry `%s' copied to database %%d." (ebib-cur-entry-key))
                            #'(lambda (db)
                                (let ((entry-key (ebib-cur-entry-key)))
                                  (if (member entry-key (ebib-db-list-keys db 'nosort))
                                      (error "Entry key `%s' already exists in database %d" entry-key num)
                                    (ebib-store-entry entry-key (copy-tree (ebib-db-get-entry entry-key ebib-cur-db)) db t)
                                    ;; if this is the first entry in the target DB,
                                    ;; its CUR-ENTRY must be set!
                                    (when (null (ebib-db-get-current-entry-key db))
                                      (ebib-db-set-current-entry-key t db))
                                    t)))) ; we must return T, WHEN does not always do this.
       (ebib-export-to-file (format "Export `%s' to file: " (ebib-cur-entry-key))
                            (format "Entry `%s' exported to %%s." (ebib-cur-entry-key))
                            #'(lambda ()
                                (insert "\n")
                                (ebib-format-entry (ebib-cur-entry-key) ebib-cur-db t)))))
    ((default)
     (beep))))

(defun ebib-export-marked-entries (num)
  "Copies the marked entries to another database.
NUM indicates which database to copy the entry to. If it is NIL,
a filename is asked to which the entry is appended."
  (ebib-execute-when
    ((marked-entries)
     (if num
         (ebib-export-to-db
          num "Entries copied to database %d."
          #'(lambda (db)
              (mapc #'(lambda (entry-key)
                        (if (member entry-key (ebib-db-list-keys db 'nosort))
                            (error "Entry key `%s' already exists in database %d" entry-key num)
                          (ebib-store-entry entry-key (copy-tree (ebib-db-get-entry entry-key ebib-cur-db)) db t)))
                    (ebib-db-list-marked-entries ebib-cur-db 'nosort))
              ;; if the target DB was empty before, its CUR-ENTRY must be set!
              (when (null (ebib-db-get-current-entry-key db))
                (ebib-db-set-current-entry-key t db))
              t))         ; we must return T, WHEN does not always do this.
       (ebib-export-to-file "Export to file: " "Entries exported to %s."
                            #'(lambda ()
                                (mapc #'(lambda (entry-key)
                                          (insert "\n")
                                          (ebib-format-entry entry-key ebib-cur-db t))
                                      (ebib-db-list-marked-entries ebib-cur-db 'nosort))))))
    ((default)
     (beep))))

(defun ebib-search ()
  "Search the current Ebib database.
The search is conducted with STRING-MATCH and can therefore be a
regexp. Searching starts with the current entry. In a filtered
database, only the visible entries are searched."
  (interactive)
  (ebib-execute-when
    ((entries)
     (ebib-ifstring (search-str (read-string "Search database for: "))
         (progn
           (setq ebib-search-string search-str)
           ;; first we search the current entry
           (if (ebib-search-in-entry ebib-search-string
                                     (ebib-db-get-entry (ebib-cur-entry-key) ebib-cur-db))
               (ebib-fill-entry-buffer ebib-search-string)
             ;; if the search string wasn't found in the current entry, we continue searching.
             (ebib-search-next)))))
    ((default)
     (beep))))

(defun ebib-search-next ()
  "Searches the next occurrence of `ebib-search-string'.
Searching starts at the entry following the current entry. If a
match is found, the matching entry is shown and becomes the new
current entry. If a filter is active, only the visible entries
are searched."
  (interactive)
  (ebib-execute-when
    ((entries)
     (if (null ebib-search-string)
         (message "No search string")
       (let ((cur-search-entry (cdr (member (ebib-cur-entry-key) ebib-cur-keys-list))))
         (while (and cur-search-entry
                     (null (ebib-search-in-entry ebib-search-string
                                                 (ebib-db-get-entry (car cur-search-entry) ebib-cur-db 'noerror))))
           (setq cur-search-entry (cdr cur-search-entry)))
         (if (null cur-search-entry)
             (message (format "`%s' not found" ebib-search-string))
           (ebib-db-set-current-entry-key (car cur-search-entry) ebib-cur-db)
           (with-current-buffer (cdr (assoc 'index ebib-buffer-alist))
             (goto-char (point-min))
             (re-search-forward (format "^%s " (ebib-cur-entry-key)))
             (beginning-of-line)
             (ebib-set-index-highlight)
             (ebib-fill-entry-buffer ebib-search-string))))))
    ((default)
     (beep))))

(defun ebib-search-in-entry (search-str entry &optional field)
  "Searches one entry of the ebib database.
Returns a list of fields in ENTRY that match the regexp
SEARCH-STR, or NIL if no matches were found. If FIELD is given,
only that field is searched. ENTRY is an alist of (FIELD . VALUE)
pairs.

Normally, the `=type=' field, which stores the entry type, is not
searched, but it is possible to seach for specific entry types by
specifying `=type=' for FIELD. In that case, the search string
can still be a string, but only exact matches will return a
result."
  (let ((case-fold-search t)  ; we want to ensure a case-insensitive search
        (result nil))
    (if field
        (let ((value (cdr (assoc field entry))))
          (when (or (and (stringp value)
                         (string-match search-str value))
                    (and (symbolp value)
                         (string= search-str (symbol-name value)))) ; The =type= field has a symbol as value.
            (setq result (list field))))
      (mapc #'(lambda (field)
                (when (and (stringp (cdr field)) ; We exlude the =type= field here.
                           (string-match search-str (cdr field)))
                  (setq result (cons (car field) result))))
            entry))
    result))

(defun ebib-edit-strings ()
  "Edits the @STRING definitions in the database."
  (interactive)
  (ebib-execute-when
    ((real-db)
     (setq ebib-cur-strings-list (ebib-db-list-strings ebib-cur-db))
     (ebib-fill-strings-buffer)
     (ebib-pop-to-buffer 'strings)
     (goto-char (point-min)))
    ((default)
     (beep))))

(defun ebib-edit-preamble ()
  "Edits the @PREAMBLE definition in the database."
  (interactive)
  (ebib-execute-when
    ((real-db)
     (ebib-multiline-edit 'preamble (ebib-db-get-preamble ebib-cur-db)))
    ((default)
     (beep))))

(defun ebib-export-preamble (prefix)
  "Exports the @PREAMBLE definition.
If a prefix argument is given, it is taken as the database to
export the preamble to. If the goal database already has a
preamble, the new preamble will be appended to it. If no prefix
argument is given, the user is asked to enter a filename to which
the preamble is appended."
  (interactive "P")
  (ebib-execute-when
    ((real-db)
     (if (null (ebib-db-get-preamble ebib-cur-db))
         (error "No @PREAMBLE defined")
       (let ((num (ebib-prefix prefix)))
         (if num
             (ebib-export-to-db num "@PREAMBLE copied to database %d"
                                #'(lambda (db)
                                    (ebib-db-set-preamble (ebib-db-get-preamble ebib-cur-db) db 'append)))
           (ebib-export-to-file "Export @PREAMBLE to file: "
                                "@PREAMBLE exported to %s"
                                #'(lambda ()
                                    (insert (format "\n@preamble{%s}\n\n" (ebib-db-get-preamble ebib-cur-db)))))))))
    ((default)
     (beep))))

(defun ebib-print-entries ()
  "Creates a LaTeX file listing the entries.
Either prints the entire database, or the marked entries."
  (interactive)
  (ebib-execute-when
    ((entries)
     (let ((entries (or (when (or (ebib-called-with-prefix)
                                  (equal '(menu-bar) (elt (this-command-keys-vector) 0)))
                          (ebib-db-list-marked-entries ebib-cur-db))
                        (ebib-db-list-keys ebib-cur-db))))
       (ebib-ifstring (tempfile (if (not (string= "" ebib-print-tempfile))
                             ebib-print-tempfile
                           (read-file-name "Use temp file: " "~/" nil nil)))
           (progn
             (with-temp-buffer
               (insert "\\documentclass{article}\n\n")
               (when ebib-print-preamble
                 (mapc #'(lambda (string)
                           (insert (format "%s\n" string)))
                       ebib-print-preamble))
               (insert "\n\\begin{document}\n\n")
               (mapc #'(lambda (entry-key)
                         ;; first create a table
                         (insert "\\begin{tabular}{p{0.2\\textwidth}p{0.8\\textwidth}}\n")
                         ;; insert the entry type
                         (let ((entry (ebib-db-get-entry entry-key ebib-cur-db)))
                           (insert (format "\\multicolumn{2}{l}{\\texttt{%s (%s)}}\\\\\n"
                                           entry-key (symbol-name (cdr (assoc '=type= entry)))))
                           (insert "\\hline\n")
                           ;; Then the other fields.
                           (mapc #'(lambda (field)
                                     (ebib-ifstring (value (cdr (assoc field entry)))
                                         (when (or (not (ebib-multiline-p value))
                                                   ebib-print-multiline)
                                           (insert (format "%s: & %s\\\\\n"
                                                           field (ebib-db-unbrace value))))))
                                 ;; Note: ebib-get-all-fields returns a list with `=type=' as its first element.
                                 (cdr (ebib-get-all-fields (cdr (assoc '=type= entry))))))
                         (insert "\\end{tabular}\n\n")
                         (insert (if ebib-print-newpage
                                     "\\newpage\n\n"
                                   "\\bigskip\n\n")))
                     entries)
               (insert "\\end{document}\n")
               (write-region (point-min) (point-max) tempfile))
             (ebib-lower)
             (find-file tempfile)))))
    ((default)
     (beep))))

(defun ebib-latex-entries ()
  "Creates a LaTeX file that \\nocites entries from the database.
Operates either on all entries or on the marked entries."
  (interactive)
  (ebib-execute-when
    ((real-db entries)
     (ebib-ifstring (tempfile (if (not (string= "" ebib-print-tempfile))
                           ebib-print-tempfile
                         (read-file-name "Use temp file: " "~/" nil nil)))
         (progn
           (with-temp-buffer
             (insert "\\documentclass{article}\n\n")
             (when ebib-latex-preamble
               (mapc #'(lambda (string)
                         (insert (format "%s\n" string)))
                     ebib-latex-preamble))
             (insert "\n\\begin{document}\n\n")
             (if (and (or (ebib-called-with-prefix)
                          (equal '(menu-bar) (elt (this-command-keys-vector) 0)))
                      (ebib-db-marked-entries-p ebib-cur-db))
                 (mapc #'(lambda (entry)
                           (insert (format "\\nocite{%s}\n" entry)))
                       (ebib-db-list-marked-entries ebib-cur-db))
               (insert "\\nocite{*}\n"))
             (insert (format "\n\\bibliography{%s}\n\n" (expand-file-name (ebib-db-get-filename ebib-cur-db))))
             (insert "\\end{document}\n")
             (write-region (point-min) (point-max) tempfile))
           (ebib-lower)
           (find-file tempfile))))
    ((default)
     (beep))))

(defun ebib-switch-to-database (num)
  (interactive "NSwitch to database number: ")
  (let ((new-db (nth (1- num) ebib-databases)))
    (if new-db
        (progn
          (setq ebib-cur-db new-db)
          (ebib-redisplay))
      (error "Database %d does not exist" num))))

(defun ebib-next-database ()
  (interactive)
  (ebib-execute-when
    ((database)
     (let ((new-db (ebib-next-elem ebib-cur-db ebib-databases)))
       (unless new-db
         (setq new-db (car ebib-databases)))
       (setq ebib-cur-db new-db)
       (ebib-redisplay)))))

(defun ebib-prev-database ()
  (interactive)
  (ebib-execute-when
    ((database)
     (let ((new-db (ebib-prev-elem ebib-cur-db ebib-databases)))
       (unless new-db
         (setq new-db (ebib-last1 ebib-databases)))
       (setq ebib-cur-db new-db)
       (ebib-redisplay)))))

(defun ebib-extract-urls (string)
  "Extract URLs from STRING.
What counts as a URL is defined by `ebib-url-regexp'. Return the
URLs as a list of strings. Parts of the string that are not
recognized as URLs are discarded."
  (let ((start 0)
        (result nil))
    (while (string-match ebib-url-regexp string start)
      (add-to-list 'result (match-string 0 string) t)
      (setq start (match-end 0)))
    result))

(defun ebib-browse-url (num)
  "Browse the URL in the standard URL field.
If this field contains more than one URL, ask the user which one
to open. Alternatively, the user can provide a numeric prefix
argument."
  (interactive "P")
  (ebib-execute-when
    ((entries)
     (let ((urls (ebib-db-get-field-value ebib-standard-url-field (ebib-cur-entry-key) ebib-cur-db 'noerror 'unbraced 'xref)))
       (if (listp urls)
           (setq urls (car urls)))
       (if urls
           (ebib-call-browser urls num)
         (error "Field `%s' is empty" ebib-standard-url-field))))
    ((default)
     (beep))))

(defun ebib-browse-doi ()
  "Open the DOI in the standard DOI field in a browser.
The stardard DOI field (see user option
`ebib-standard-doi-field') may contain only one DOI. The DOI is
combined with the URL \"http://dx.doi.org/\" before being sent to
the browser."
  (interactive)
  (ebib-execute-when
   ((entries)
    (let ((doi (ebib-db-get-field-value ebib-standard-doi-field (ebib-cur-entry-key) ebib-cur-db 'noerror 'unbraced 'xref)))
      (if (listp doi)
          (setq doi (car doi)))
      (if doi
          (ebib-call-browser (concat "http://dx.doi.org/" doi))
        (error "No DOI found in field `%s'" ebib-standard-doi-field))))
   ((default)
    (beep))))

(defun ebib-call-browser (string &optional n)
  "Call a browser with the Nth URL in STRING.
STRING is a string containing one or more URLs. If there is more
than one, N specifies which one to pass to the browser. If N
is NIL, the user is asked which URL to open."
  (let ((urls (ebib-extract-urls string)))
    (cond
     ((null (cdr urls))                 ; there's only one URL
      (setq n 1))
     ((not (integerp n)) ; the user didn't provide a numeric prefix argument
      (setq n (string-to-number (read-string (format "Select URL to open [1-%d]: " (length urls)))))))
    (if (or (< n 1)             ; if the user provide a number out of range
            (> n (length urls)))
        (setq n 1))
    (let ((url (nth (1- n) urls)))
      (when url
        (if (string-match "\\\\url{\\(.*?\\)}" url) ; see if the url is contained in \url{...}
            (setq url (match-string 1 url)))
        (if ebib-browser-command
            (progn
              (message "Executing `%s %s'" ebib-browser-command url)
              (start-process "Ebib-browser" nil ebib-browser-command url))
          (message "Opening `%s'" url)
          (browse-url url))))))

(defun ebib-view-file (num)
  "Views a file in the standard file field.
The standard file field (see option `ebib-standard-file-field') may
contain more than one filename. In that case, a numeric prefix
argument can be used to specify which file to choose."
  (interactive "P")
  (ebib-execute-when
    ((entries)
     (let ((filename (ebib-db-get-field-value ebib-standard-file-field (ebib-cur-entry-key) ebib-cur-db 'noerror 'unbraced 'xref)))
       (if (listp filename)
           (setq filename (car filename)))
       (if filename
           (ebib-call-file-viewer filename num)
         (error "Field `%s' is empty" ebib-standard-file-field))))
    ((default)
     (beep))))

(defun ebib-call-file-viewer (filename &optional n)
  "Open FILENAME with an external viewer.
FILENAME can also be a string of filenames separated by
`ebib-filename-separator', in which case the Nth file is
opened. If N is NIL, the user is asked to enter a number."
  (let ((files (split-string filename ebib-filename-separator t)))
    (cond
     ((null (cdr files))                ; there's only one file
      (setq n 1))
     ((not (integerp n))  ; the user did not pass a numeric prefix argument
      (setq n (string-to-number (read-string (format "Select file to open [1-%d]: " (length files)))))))
    (if (or (< n 1)    ; if the user provided a number that is out of range
            (> n (length files)))
        (setq n 1))
    (let* ((file (nth (1- n) files))
           (file-full-path
            (or (locate-file file ebib-file-search-dirs)
                (locate-file (file-name-nondirectory file) ebib-file-search-dirs)
                (expand-file-name file))))
      (if (file-exists-p file-full-path)
          (let ((ext (file-name-extension file-full-path)))
            (ebib-ifstring (viewer (cdr (assoc ext ebib-file-associations)))
                (progn
                  (message "Executing `%s %s'" viewer file-full-path)
                  (start-process (concat "ebib " ext " viewer process") nil viewer file-full-path))
              (message "Opening `%s'" file-full-path)
              (ebib-lower)
              (find-file file-full-path)))
        (error "File not found: `%s'" file)))))

(defun ebib-show-log ()
  "Displays the contents of the log buffer."
  (interactive)
  (ebib-pop-to-buffer 'log))

(defun ebib-create-citation-command (format-string &optional key)
  "Create a citation command using FORMAT-STRING.
If FORMAT-STRING contains a %K directive, it is replaced with
KEY. Furthermore, FORMAT-STRING may contain any number of %A
directives for additional arguments to the citation. The user is
asked to supply a string for each of them, which may be empty.

Each %A directive may be wrapped in a %<...%> pair, containing
optional material both before and after %A. If the user supplies
an empty string for such an argument, the optional material
surrounding it is not included in the citation command."
  (when (and (string-match "%K" format-string)
             key)
    (setq format-string (replace-match key t t format-string)))
  (let (arg-prompt)
    (cl-loop for n = 1 then (1+ n)
             until (null (string-match "%<\\(.*?\\)%A\\(.*?\\)%>\\|%A\\|%D" format-string)) do
             (setq arg-prompt
                   (if (string= (match-string 0 format-string) "%D")
                       "Description"
                     "Argument"))
             (setq format-string
                   (replace-match (ebib-ifstring (argument
                                                  (save-match-data
                                                    (read-from-minibuffer (format "%s%s%s: "
                                                                                  arg-prompt
                                                                                  (if (string= arg-prompt "Argument")
                                                                                      (format " %s" n)
                                                                                    "")
                                                                                  (if key
                                                                                      (concat " for " key)
                                                                                    "")))))
                                      (concat "\\1" argument "\\2")
                                    "")
                                  t nil format-string))
             finally return format-string)))

(defun ebib-split-citation-string (format-string)
  "Split up FORMAT-STRING.
The return value is a list of (BEFORE REPEATER SEPARATOR AFTER),
where BEFORE is the part before the repeating part of
FORMAT-STRING, REPEATER the repeating part, SEPARATOR the string
to be placed between each instance of REPEATER and AFTER the part
after the last instance of REPEATER."
  (let (before repeater separator after)
    ;; first check if the format string has a repeater and if so, separate each component
    (cond
     ((string-match "\\(.*?\\)%(\\(.*\\)%\\(.*?\\))\\(.*\\)" format-string)
      (setq before (match-string 1 format-string)
            repeater (match-string 2 format-string)
            separator (match-string 3 format-string)
            after (match-string 4 format-string)))
     ((string-match "\\(.*?\\)\\(%K\\)\\(.*\\)" format-string)
      (setq before (match-string 1 format-string)
            repeater (match-string 2 format-string)
            after (match-string 3 format-string))))
    (cl-values before repeater separator after)))

(defun ebib-push-bibtex-key ()
  "Pushes the current entry to a LaTeX buffer.
The user is prompted for the buffer to push the entry into."
  (interactive)
  (let ((called-with-prefix (ebib-called-with-prefix)))
    (ebib-execute-when
      ((entries)
       (let ((buffer (read-buffer (if called-with-prefix
                                      "Push marked entries to buffer: "
                                    "Push entry to buffer: ")
                                  ebib-push-buffer t)))
         (when buffer
           (setq ebib-push-buffer buffer)
           (let* ((format-list (or (cadr (assoc (buffer-local-value 'major-mode (get-buffer buffer)) ebib-citation-commands))
                                   (cadr (assoc 'any ebib-citation-commands))))
                  (citation-command
                   ;; Read a citation command from the user:
                   (ebib-ifstring (format-string (cadr (assoc
                                                        (completing-read "Command to use: " format-list nil nil nil 'ebib-cite-command-history)
                                                        format-list)))
                       (cl-multiple-value-bind (before repeater separator after) (ebib-split-citation-string format-string)
                         (cond
                          ((and called-with-prefix ; if there are marked entries and the user wants to push those
                                (ebib-db-marked-entries-p ebib-cur-db))
                           (concat (ebib-create-citation-command before)
                                   (mapconcat #'(lambda (key) ; then deal with the entries one by one
                                                  (ebib-create-citation-command repeater key))
                                              (ebib-db-list-marked-entries ebib-cur-db)
                                              (if separator separator (read-from-minibuffer "Separator: ")))
                                   (ebib-create-citation-command after)))
                          (t        ; otherwise just take the current entry
                           (ebib-create-citation-command (concat before repeater after) (ebib-cur-entry-key)))))
                     ;; If the user doesn't provide a command, we just insert the entry key or keys:
                     (if (ebib-db-marked-entries-p ebib-cur-db)
                         (mapconcat #'(lambda (key)
                                        key)
                                    (ebib-db-list-marked-entries ebib-cur-db)
                                    (read-from-minibuffer "Separator: "))
                       (ebib-cur-entry-key)))))
             (when citation-command
               (with-current-buffer buffer
                 (insert citation-command))
               (message "Pushed entries to buffer %s" buffer))))))
      ((default)
       (beep)))))

(defun ebib-index-help ()
  "Shows the info node of Ebib's index buffer."
  (interactive)
  (setq ebib-info-flag t)
  (ebib-lower)
  (info "(ebib) The Index Buffer"))

(defun ebib-info ()
  "Shows Ebib's info node."
  (interactive)
  (setq ebib-info-flag t)
  (ebib-lower)
  (info "(ebib)"))

;;;;;;;;;;;;;;;;
;; entry-mode ;;
;;;;;;;;;;;;;;;;

(defvar ebib-entry-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map [up] 'ebib-prev-field)
    (define-key map [down] 'ebib-next-field)
    (define-key map [prior] 'ebib-goto-prev-set)
    (define-key map [next] 'ebib-goto-next-set)
    (define-key map [home] 'ebib-goto-first-field)
    (define-key map [end] 'ebib-goto-last-field)
    (define-key map [return] 'ebib-edit-field)
    (define-key map " " 'ebib-goto-next-set)
    (define-key map "b" 'ebib-goto-prev-set)
    (define-key map "c" 'ebib-copy-field-contents)
    (define-key map "d" 'ebib-delete-field-contents)
    (define-key map "e" 'ebib-edit-field)
    (define-key map "f" 'ebib-view-file-in-field)
    (define-key map "g" 'ebib-goto-first-field)
    (define-key map "G" 'ebib-goto-last-field)
    (define-key map "h" 'ebib-entry-help)
    (define-key map "j" 'ebib-next-field)
    (define-key map "k" 'ebib-prev-field)
    (define-key map "l" 'ebib-edit-multiline-field)
    (define-key map [(control n)] 'ebib-next-field)
    (define-key map [(meta n)] 'ebib-goto-prev-set)
    (define-key map [(control p)] 'ebib-prev-field)
    (define-key map [(meta p)] 'ebib-goto-next-set)
    (define-key map "q" 'ebib-quit-entry-buffer)
    (define-key map "r" 'ebib-toggle-raw)
    (define-key map "s" 'ebib-insert-abbreviation)
    (define-key map "u" 'ebib-browse-url-in-field)
    (define-key map "x" 'ebib-cut-field-contents)
    (define-key map "y" 'ebib-yank-field-contents)
    (define-key map "\C-xb" 'ebib-quit-entry-buffer)
    (define-key map "\C-xk" 'ebib-quit-entry-buffer)
    map)
  "Keymap for the Ebib entry buffer.")

(define-derived-mode ebib-entry-mode
  fundamental-mode "Ebib-entry"
  "Major mode for the Ebib entry buffer."
  (setq buffer-read-only t)
  (setq cursor-type nil)
  (setq truncate-lines t))

(defun ebib-quit-entry-buffer ()
  "Quits editing the entry.
If the key of the current entry matches the pattern
<new-entry%d>, a new key is automatically generated using
`bibtex-generate-autokey'."
  (interactive)
  (cond
   ((and ebib-popup-entry-window
         (eq ebib-layout 'index-only))
    (delete-window))
   ((eq ebib-layout 'index-only)
    (switch-to-buffer nil t)))
  (ebib-pop-to-buffer 'index)
  (ebib-delete-highlight ebib-fields-highlight)
  ;; (select-window (get-buffer-window (cdr (assoc 'index ebib-buffer-alist))))
  (if (string-match "<new-entry[0-9]+>" (ebib-cur-entry-key))
      (ebib-generate-autokey)))

(defun ebib-find-visible-field (field direction)
  "Finds the first visible field before or after FIELD.
If DIRECTION is negative, search the preceding fields, otherwise
search the succeeding fields. If FIELD is visible itself, return
that. If there is no preceding/following visible field, return
NIL. If `ebib-hide-hidden-fields' is NIL, return FIELD."
  (when ebib-hide-hidden-fields
    (let ((fn (if (>= direction 0)
                  'ebib-next-elem
                'ebib-prev-elem)))
      (while (and field
                  (get field 'ebib-hidden))
        (setq field (funcall fn field ebib-cur-entry-fields)))))
  field)

(defun ebib-prev-field ()
  "Moves to the previous field."
  (interactive)
  (let ((new-field (ebib-find-visible-field (ebib-prev-elem ebib-current-field ebib-cur-entry-fields) -1)))
    (if (null new-field)
        (beep)
      (setq ebib-current-field new-field)
      (ebib-move-to-field ebib-current-field -1))))

(defun ebib-next-field ()
  "Moves to the next field."
  (interactive)
  (let ((new-field (ebib-find-visible-field (ebib-next-elem ebib-current-field ebib-cur-entry-fields) 1)))
    (if (null new-field)
        (when (ebib-called-interactively-p) ; I call this function after editing a field, and we don't want a beep then.
          (beep))
      (setq ebib-current-field new-field)
      (ebib-move-to-field ebib-current-field 1))))

(defun ebib-goto-first-field ()
  "Moves to the first field."
  (interactive)
  (let ((new-field (ebib-find-visible-field (car ebib-cur-entry-fields) 1)))
    (if (null new-field)
        (beep)
      (setq ebib-current-field new-field)
      (ebib-move-to-field ebib-current-field -1))))

(defun ebib-goto-last-field ()
  "Moves to the last field."
  (interactive)
  (let ((new-field (ebib-find-visible-field (ebib-last1 ebib-cur-entry-fields) -1)))
    (if (null new-field)
        (beep)
      (setq ebib-current-field new-field)
      (ebib-move-to-field ebib-current-field 1))))

(defun ebib-goto-next-set ()
  "Moves to the next set of fields."
  (interactive)
  (cond
   ((eq ebib-current-field '=type=) (ebib-next-field))
   ((member ebib-current-field ebib-additional-fields) (ebib-goto-last-field))
   (t (let* ((entry-type (ebib-db-get-field-value '=type= (ebib-cur-entry-key) ebib-cur-db))
             (obl-fields (ebib-get-obl-fields entry-type))
             (opt-fields (ebib-get-opt-fields entry-type))
             (new-field nil))
        (when (member ebib-current-field obl-fields)
          (setq new-field (ebib-find-visible-field (car opt-fields) 1)))
        ;; new-field is nil if there are no opt-fields
        (when (or (member ebib-current-field opt-fields)
                  (null new-field))
          (setq new-field (ebib-find-visible-field (car ebib-additional-fields) 1)))
        (if (null new-field)
            ;; if there was no further set to go to, go to the last field
            ;; of the current set
            (ebib-goto-last-field)
          (setq ebib-current-field new-field)
          (ebib-move-to-field ebib-current-field 1))))))

(defun ebib-goto-prev-set ()
  "Moves to the previous set of fields."
  (interactive)
  (unless (eq ebib-current-field '=type=)
    (let* ((entry-type (ebib-db-get-field-value '=type= (ebib-cur-entry-key) ebib-cur-db))
           (obl-fields (ebib-get-obl-fields entry-type))
           (opt-fields (ebib-get-opt-fields entry-type))
           (new-field nil))
      (if (member ebib-current-field obl-fields)
          (ebib-goto-first-field)
        (when (member ebib-current-field ebib-additional-fields)
          (setq new-field (ebib-find-visible-field (ebib-last1 opt-fields) -1)))
        (when (or (member ebib-current-field opt-fields)
                  (null new-field))
          (setq new-field (ebib-find-visible-field (ebib-last1 obl-fields) -1)))
        (if (null new-field)
            (ebib-goto-first-field)
          (setq ebib-current-field new-field)
          (ebib-move-to-field ebib-current-field -1))))))

(defun ebib-edit-entry-type ()
  "Edits the entry type."
  (ebib-ifstring (new-type (completing-read "type: " ebib-entry-types nil t))
      (progn
        (setq new-type (intern-soft new-type))
        (ebib-db-set-field-value '=type= new-type (ebib-cur-entry-key) ebib-cur-db 'overwrite 'unbraced)
        (ebib-fill-entry-buffer)
        (setq ebib-cur-entry-fields (ebib-get-all-fields new-type))
        (ebib-set-modified t))))

(defun ebib-edit-crossref ()
  "Edits the crossref field."
  (ebib-ifstring (key (completing-read "Key to insert in `crossref': " (ebib-db-list-keys ebib-cur-db 'nosort) nil t nil 'ebib-key-history))
      (progn
        (ebib-db-set-field-value 'crossref key (ebib-cur-entry-key) ebib-cur-db 'overwrite)
        ;; We now redisplay the entire entry buffer, so that the crossref'ed
        ;; fields show up. This also puts the cursor back on the =type= field,
        ;; though, so we need to readjust.
        (ebib-fill-entry-buffer)
        (setq ebib-current-field 'crossref)
        (re-search-forward "^crossref")
        (ebib-set-fields-highlight)
        (ebib-set-modified t))))

(defun ebib-sort-keywords (keywords)
  "Sort the KEYWORDS string, remove duplicates, and return it as a string."
  (mapconcat 'identity
             (sort (delete-dups (split-string keywords ebib-field-separator t))
                   'string<)
             ebib-field-separator))

(defun ebib-edit-keywords ()
  "Edit the keywords field."
  ;; we shadow the binding of `minibuffer-local-completion-map' so that we
  ;; can unbind <SPC>, since keywords may contain spaces. note also that in
  ;; emacs 24, we can use `make-composed-keymap' for this purpose, but in
  ;; emacs 23.1, this function is not available.
  (let ((minibuffer-local-completion-map `(keymap (keymap (32)) ,@minibuffer-local-completion-map))
        (collection (ebib-keywords-for-database ebib-cur-db)))
    (cl-loop for keyword = (completing-read "Add a new keyword (ENTER to finish): " collection nil nil nil 'ebib-keywords-history)
             until (string= keyword "")
             do (let* ((conts (ebib-db-get-field-value 'keywords (ebib-cur-entry-key) ebib-cur-db 'noerror 'unbraced))
                       (new-conts (if conts
                                      (concat conts ebib-field-separator keyword)
                                    keyword)))
                  (ebib-db-set-field-value 'keywords
                                           (if ebib-keywords-field-keep-sorted
                                               (ebib-sort-keywords new-conts)
                                             new-conts)
                                           (ebib-cur-entry-key)
                                           ebib-cur-db
                                           'overwrite) 
                  (ebib-redisplay-current-field)
                  (unless (member keyword collection)
                    (ebib-keywords-add-keyword keyword ebib-cur-db)))
             finally return (ebib-set-modified t))))

(defun ebib-edit-file-field ()
  "Edit the `ebib-standard-file-field'.
Filenames are added to the standard file field separated by
`ebib-filename-separator'. The first directory in
`ebib-file-search-dirs' is used as the start directory."
  (let ((start-dir (file-name-as-directory (car ebib-file-search-dirs))))
    (cl-loop for file = (read-file-name "Add file (ENTER to finish): " start-dir nil 'confirm-after-completion)
             until (or (string= file "")
                       (string= file start-dir))
             do (let* ((short-file (ebib-file-relative-name (expand-file-name file)))
                       (conts (ebib-db-get-field-value ebib-standard-file-field (ebib-cur-entry-key) ebib-cur-db 'noerror 'unbraced))
                       (new-conts (if conts
                                      (concat conts ebib-filename-separator short-file)
                                    short-file)))
                  (ebib-db-set-field-value ebib-standard-file-field new-conts (ebib-cur-entry-key) ebib-cur-db 'overwrite) 
                  (ebib-redisplay-current-field))
             finally return (ebib-set-modified t))))

(defun ebib-file-relative-name (file)
  "Return a name for FILE relative to `ebib-file-search-dirs'.
If FILE is not in (a subdirectory of) one of the directories in
`ebib-file-search-dirs', return FILE."
  ;; We first create a list of names relative to each dir in
  ;; ebib-file-search-dirs, discarding those that start with `..'
  (let* ((names (delq nil (mapcar #'(lambda (dir)
                                      (let ((rel-name (file-relative-name file dir)))
                                        (unless (string-prefix-p ".." rel-name)
                                          rel-name)))
                                  ebib-file-search-dirs)))
         ;; Then we take the shortest one...
         (name (car (sort names #'(lambda (x y)
                                    (< (length x) (length y)))))))
    ;; ...and return it, or the filename itself if it couldn't be
    ;; relativized.
    (or name file)))

(defun ebib-edit-normal-field ()
  "Edit a field that does not require special treatment."
    (let ((init-contents (ebib-db-get-field-value ebib-current-field (ebib-cur-entry-key) ebib-cur-db 'noerror))
          (brace? nil))
      (if (ebib-multiline-p init-contents)
          (ebib-edit-multiline-field) ; this always returns nil
        (when init-contents
          (setq brace? (ebib-db-unbraced-p init-contents))
          (setq init-contents (ebib-db-unbrace init-contents)))
        (ebib-ifstring (new-contents (read-string (format "%s: " (symbol-name ebib-current-field))
                                           (if init-contents
                                               (cons init-contents 0))))
            (ebib-db-set-field-value ebib-current-field new-contents (ebib-cur-entry-key) ebib-cur-db 'overwrite brace?)
          (ebib-db-remove-field-value ebib-current-field (ebib-cur-entry-key) ebib-cur-db))
        (ebib-redisplay-current-field)
        (ebib-set-modified t))))

(defun ebib-edit-field (&optional pfx)
  "Edit a field of a BibTeX entry.
Most fields are edited directly using the minibuffer, but a few
are handled specially: the `type' and `crossref` fields offer
completion, the `annote' field is edited as a multiline field,
the `keyword' field adds keywords one by one, also allowing
completion, and the field in `ebib-standard-file-field' uses
filename completion and shortens filenames if they are in (a
subdirectory of) one of the directories in
`ebib-file-search-dirs'.

With a prefix argument, the `keyword' field and the field in
`ebib-standard-file-field' can be edited directly. For other
fields, the prefix argument has no meaning."
  (interactive "P")
  (let ((result
         (cond
          ((eq ebib-current-field '=type=) (ebib-edit-entry-type))
          ((eq ebib-current-field 'crossref) (ebib-edit-crossref))
          ((and (eq ebib-current-field 'keywords)
                (not pfx))
           (ebib-edit-keywords))
          ((and (eq ebib-current-field ebib-standard-file-field)
                (not pfx))
           (ebib-edit-file-field))
          ((eq ebib-current-field 'annote) (ebib-edit-multiline-field))
          (t (ebib-edit-normal-field)))))
    ;; we move to the next field, but only if ebib-edit-field was
    ;; called interactively, otherwise we get a strange bug in
    ;; ebib-toggle-raw...
    (if (and result (ebib-called-interactively-p))
        (ebib-next-field))))

(defun ebib-browse-url-in-field (num)
  "Browse a URL in the current field.
If the field may contain multiple URLs (as defined by
`ebib-url-regexp'), the user is asked which one to open.
Altertanively, a numeric prefix argument can be passed."
  (interactive "P")
  (let ((urls (ebib-db-get-field-value ebib-current-field (ebib-cur-entry-key) ebib-cur-db 'noerror 'unbraced)))
    (if urls
        (ebib-call-browser urls num)
      (error "Field `%s' is empty" ebib-current-field))))

(defun ebib-view-file-in-field (num)
  "Views a file in the current field.
The field may contain multiple filenames, in which case the
prefix argument can be used to specify which file is to be
viewed."
  (interactive "P")
  (let ((files (ebib-db-get-field-value ebib-current-field (ebib-cur-entry-key) ebib-cur-db 'noerror 'unbraced)))
    (if files
        (ebib-call-file-viewer files num)
      (error "Field `%s' is empty" ebib-current-field))))

(defun ebib-copy-field-contents ()
  "Copies the contents of the current field to the kill ring."
  (interactive)
  (unless (eq ebib-current-field '=type=)
    (let ((contents (car (ebib-db-get-field-value ebib-current-field (ebib-cur-entry-key) ebib-cur-db 'noerror 'unbraced 'xref))))
      (when (stringp contents)
        (kill-new contents)
        (message "Field contents copied.")))))

(defun ebib-cut-field-contents ()
  "Kills the contents of the current field. The killed text is put in the kill ring."
  (interactive)
  (unless (eq ebib-current-field '=type=)
    (let ((contents (ebib-db-get-field-value ebib-current-field (ebib-cur-entry-key) ebib-cur-db 'noerror 'unbraced)))
      (when (stringp contents)
        (ebib-db-remove-field-value ebib-current-field (ebib-cur-entry-key) ebib-cur-db)
        (kill-new contents)
        (ebib-redisplay-current-field)
        (ebib-set-modified t)
        (message "Field contents killed.")))))

(defun ebib-yank-field-contents (arg)
  "Inserts the last killed text into the current field.
If the current field already has a contents, nothing is inserted,
unless the previous command was also `ebib-yank-field-contents',
then the field contents is replaced with the previous yank. That
is, multiple uses of this command function like the combination
of C-y/M-y. Prefix arguments also work the same as with C-y/M-y."
  (interactive "P")
  (if (or (eq ebib-current-field '=type=) ; we cannot yank into the =type= or crossref fields
          (eq ebib-current-field 'crossref)
          (unless (eq last-command 'ebib-yank-field-contents) ; nor into a field already filled
            (ebib-db-get-field-value ebib-current-field (ebib-cur-entry-key) ebib-cur-db 'noerror)))
      (progn
        (setq this-command t)
        (beep))
    (let ((new-contents (current-kill (cond
                                       ((listp arg)
                                        (if (eq last-command 'ebib-yank-field-contents) 1 0))
                                       ((eq arg '-) -2)
                                       (t (1- arg))))))
      (when new-contents
        (ebib-db-set-field-value ebib-current-field new-contents (ebib-cur-entry-key) ebib-cur-db 'overwrite)
        (ebib-redisplay-current-field)
        (ebib-set-modified t)))))

(defun ebib-delete-field-contents ()
  "Deletes the contents of the current field.
The deleted text is not put in the kill ring."
  (interactive)
  (if (eq ebib-current-field '=type=)
      (beep)
    (when (y-or-n-p "Delete field contents? ")
      (ebib-db-remove-field-value ebib-current-field (ebib-cur-entry-key) ebib-cur-db)
      (ebib-redisplay-current-field)
      (ebib-set-modified t)
      (message "Field contents deleted."))))

(defun ebib-toggle-raw ()
  "Toggles the raw status of the current field contents."
  (interactive)
  (unless (or (eq ebib-current-field '=type=)
              (eq ebib-current-field 'crossref)
              (eq ebib-current-field 'keywords))
    (let ((contents (ebib-db-get-field-value ebib-current-field (ebib-cur-entry-key) ebib-cur-db 'noerror)))
      (unless contents     ; If there is no value,
        (ebib-edit-field)  ; the user can enter one, which we must then store unbraced.
        (setq contents (ebib-db-get-field-value ebib-current-field (ebib-cur-entry-key) ebib-cur-db 'noerror)))
      (when contents ; We must check to make sure the user entered some value.
        (ebib-db-set-field-value ebib-current-field contents (ebib-cur-entry-key) ebib-cur-db 'overwrite (not (ebib-db-unbraced-p contents)))
        (ebib-redisplay-current-field)
        (ebib-set-modified t)))))

(defun ebib-edit-multiline-field ()
  "Edits the current field in multiline-mode."
  (interactive)
  (unless (or (eq ebib-current-field '=type=)
              (eq ebib-current-field 'crossref))
    (let ((text (ebib-db-get-field-value ebib-current-field (ebib-cur-entry-key) ebib-cur-db 'noerror)))
      (if (ebib-db-unbraced-p text)
          (setq ebib-multiline-unbraced t)
        (setq text (ebib-db-unbrace text))
        (setq ebib-multiline-unbraced nil))
      (ebib-multiline-edit 'fields text))))

(defun ebib-insert-abbreviation ()
  "Inserts an abbreviation from the ones defined in the database."
  (interactive)
  (if (ebib-db-get-field-value ebib-current-field (ebib-cur-entry-key) ebib-cur-db 'noerror)
      (beep)
    (let ((strings (ebib-db-list-strings ebib-cur-db 'nosort)))
      (when strings
        (unwind-protect
            (progn
              (other-window 1)
              (let ((string (completing-read "Abbreviation to insert: " strings nil t)))
                (when string
                  (ebib-db-set-field-value ebib-current-field string (ebib-cur-entry-key) ebib-cur-db 'overwrite 'unbraced)
                  (ebib-set-modified t))))
          (other-window 1)
          ;; we can't do this earlier, because we would be writing to the index buffer...
          (ebib-redisplay-current-field)
          (ebib-next-field))))))

(defun ebib-entry-help ()
  "Shows the info node for Ebib's entry buffer."
  (interactive)
  (setq ebib-info-flag t)
  (ebib-lower)
  (info "(ebib) The Entry Buffer"))

;;;;;;;;;;;;;;;;;;
;; strings-mode ;;
;;;;;;;;;;;;;;;;;;

(defvar ebib-strings-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map [up] 'ebib-prev-string)
    (define-key map [down] 'ebib-next-string)
    (define-key map [prior] 'ebib-strings-page-up)
    (define-key map [next] 'ebib-strings-page-down)
    (define-key map [home] 'ebib-goto-first-string)
    (define-key map [end] 'ebib-goto-last-string)
    (define-key map " " 'ebib-strings-page-down)
    (define-key map "a" 'ebib-add-string)
    (define-key map "b" 'ebib-strings-page-up)
    (define-key map "c" 'ebib-copy-string-contents)
    (define-key map "d" 'ebib-delete-string)
    (define-key map "e" 'ebib-edit-string)
    (define-key map "g" 'ebib-goto-first-string)
    (define-key map "G" 'ebib-goto-last-string)
    (define-key map "h" 'ebib-strings-help)
    (define-key map "j" 'ebib-next-string)
    (define-key map "k" 'ebib-prev-string)
    (define-key map "l" 'ebib-edit-multiline-string)
    (define-key map [(control n)] 'ebib-next-string)
    (define-key map [(meta n)] 'ebib-strings-page-down)
    (define-key map [(control p)] 'ebib-prev-string)
    (define-key map [(meta p)] 'ebib-strings-page-up)
    (define-key map "q" 'ebib-quit-strings-buffer)
    (define-key map "x" 'ebib-export-string)
    (define-key map "X" 'ebib-export-all-strings)
    (define-key map "\C-xb" 'ebib-quit-strings-buffer)
    (define-key map "\C-xk" 'ebib-quit-strings-buffer)
    map)
  "Keymap for the ebib strings buffer.")

(define-derived-mode ebib-strings-mode
  fundamental-mode "Ebib-strings"
  "Major mode for the Ebib strings buffer."
  (setq buffer-read-only t)
  (setq cursor-type nil)
  (setq truncate-lines t))

(defun ebib-quit-strings-buffer ()
  "Quits editing the @STRING definitions."
  (interactive)
  (if (and (eq ebib-layout 'index-only)
           ebib-popup-entry-window)
      (delete-window)
    (with-ebib-window-nondedicated
      (switch-to-buffer nil t)))
  (ebib-pop-to-buffer 'index))

(defun ebib-prev-string ()
  "Moves to the previous string."
  (interactive)
  (if (equal ebib-current-string (car ebib-cur-strings-list))  ; if we're on the first string
      (beep)
    ;; go to the beginnig of the highlight and move upward one line.
    (goto-char (ebib-highlight-start ebib-strings-highlight))
    (forward-line -1)
    (setq ebib-current-string (ebib-prev-elem ebib-current-string ebib-cur-strings-list))
    (ebib-set-strings-highlight)))

(defun ebib-next-string ()
  "Moves to the next string."
  (interactive)
  (if (equal ebib-current-string (ebib-last1 ebib-cur-strings-list))
      (when (ebib-called-interactively-p) (beep))
    (goto-char (ebib-highlight-start ebib-strings-highlight))
    (forward-line 1)
    (setq ebib-current-string (ebib-next-elem ebib-current-string ebib-cur-strings-list))
    (ebib-set-strings-highlight)))

(defun ebib-goto-first-string ()
  "Moves to the first string."
  (interactive)
  (setq ebib-current-string (car ebib-cur-strings-list))
  (goto-char (point-min))
  (ebib-set-strings-highlight))

(defun ebib-goto-last-string ()
  "Moves to the last string."
  (interactive)
  (setq ebib-current-string (ebib-last1 ebib-cur-strings-list))
  (goto-char (point-max))
  (forward-line -1)
  (ebib-set-strings-highlight))

(defun ebib-strings-page-up ()
  "Moves 10 strings up."
  (interactive)
  (let ((number-of-strings (length ebib-cur-strings-list))
        (remaining-number-of-strings (length (member ebib-current-string ebib-cur-strings-list))))
    (if (<= (- number-of-strings remaining-number-of-strings) 10)
        (ebib-goto-first-string)
      (setq ebib-current-string (nth
                                 (- number-of-strings remaining-number-of-strings 10)
                                 ebib-cur-strings-list))
      (goto-char (ebib-highlight-start ebib-strings-highlight))
      (forward-line -10)
      (ebib-set-strings-highlight))))

(defun ebib-strings-page-down ()
  "Moves 10 strings down."
  (interactive)
  (let ((number-of-strings (length ebib-cur-strings-list))
        (remaining-number-of-strings (length (member ebib-current-string ebib-cur-strings-list))))
    (if (<= remaining-number-of-strings 10)
        (ebib-goto-last-string)
      (setq ebib-current-string (nth
                                 (- number-of-strings remaining-number-of-strings -10)
                                 ebib-cur-strings-list))
      (goto-char (ebib-highlight-start ebib-strings-highlight))
      (forward-line 10)
      (ebib-set-strings-highlight))))

(defun ebib-fill-strings-buffer ()
  "Fills the strings buffer with the @STRING definitions."
  (with-current-buffer (cdr (assoc 'strings ebib-buffer-alist))
    (with-ebib-buffer-writable
      (erase-buffer)
      (cl-dolist (elem ebib-cur-strings-list)
        (let ((str (ebib-db-get-string elem ebib-cur-db 'noerror 'unbraced)))
          (insert (format "%-18s %s\n" elem
                          (if (ebib-multiline-p str)
                              (concat "+" (ebib-first-line str))
                            (concat " " str)))))))
    (goto-char (point-min))
    (setq ebib-current-string (car ebib-cur-strings-list))
    (ebib-set-strings-highlight)
    (set-buffer-modified-p nil)))

(defun ebib-edit-string ()
  "Edits the value of an @STRING definition
When the user enters an empty string, the value is not changed."
  (interactive)
  (let ((init-contents (ebib-db-get-string ebib-current-string ebib-cur-db 'noerror 'unbraced)))
    (ebib-ifstring (new-contents (read-string (format "%s: " ebib-current-string)
                                              (if init-contents
                                                  (cons init-contents 0)
                                                nil)))
        (progn
          (ebib-db-set-string ebib-current-string new-contents ebib-cur-db 'overwrite)
          (ebib-redisplay-current-string)
          (ebib-next-string)
          (ebib-set-modified t))
      (error "@STRING definition cannot be empty"))))

(defun ebib-copy-string-contents ()
  "Copies the contents of the current string to the kill ring."
  (interactive)
  (let ((contents (ebib-db-get-string ebib-current-string ebib-cur-db nil 'unbraced)))
    (kill-new contents)
    (message "String value copied.")))

(defun ebib-delete-string ()
  "Deletes the current @STRING definition from the database."
  (interactive)
  (when (y-or-n-p (format "Delete @STRING definition %s? " ebib-current-string))
    (ebib-db-remove-string ebib-current-string ebib-cur-db)
    (with-ebib-buffer-writable
      (let ((beg (progn
                   (goto-char (ebib-highlight-start ebib-strings-highlight))
                   (point))))
        (forward-line 1)
        (delete-region beg (point))))
    (let ((new-cur-string (ebib-next-elem ebib-current-string ebib-cur-strings-list)))
      (setq ebib-cur-strings-list (delete ebib-current-string ebib-cur-strings-list))
      (when (null new-cur-string)       ; deleted the last string
        (setq new-cur-string (ebib-last1 ebib-cur-strings-list))
        (forward-line -1))
      (setq ebib-current-string new-cur-string))
    (ebib-set-strings-highlight)
    (ebib-set-modified t)
    (message "@STRING definition deleted.")))

(defun ebib-add-string ()
  "Creates a new @STRING definition."
  (interactive)
  (ebib-ifstring (new-abbr (read-string "New @STRING abbreviation: " nil 'ebib-key-history))
      (progn
        (if (member new-abbr ebib-cur-strings-list)
            (error (format "%s already exists" new-abbr)))
        (ebib-ifstring (new-string (read-string (format "Value for %s: " new-abbr)))
            (progn
              (ebib-db-set-string new-abbr new-string ebib-cur-db)
              (ebib-sort-in-buffer (length ebib-cur-strings-list) new-abbr)
              (with-ebib-buffer-writable
                (insert (format "%-19s %s\n" new-abbr new-string)))
              (forward-line -1)
              (ebib-set-strings-highlight)
              (setq ebib-current-string new-abbr)
              (setq ebib-cur-strings-list (ebib-db-list-strings ebib-cur-db))
              (ebib-set-modified t))))))

(defun ebib-export-string (prefix)
  "Exports the current @STRING.
The prefix argument indicates which database to copy the string
to. If no prefix argument is present, a filename is asked to
which the string is appended."
  (interactive "P")
  (let ((num (ebib-prefix prefix)))
    (if num
        (ebib-export-to-db num (format "@STRING definition `%s' copied to database %%d" ebib-current-string)
                           #'(lambda (db)
                               (ebib-db-set-string ebib-current-string (ebib-db-get-string ebib-current-string ebib-cur-db) db)))
      (ebib-export-to-file (format "Export @STRING definition `%s' to file: " ebib-current-string)
                           (format "@STRING definition `%s' exported to %%s" ebib-current-string)
                           #'(lambda ()
                               (insert (format "\n@string{%s = %s}\n"
                                               ebib-current-string
                                               (ebib-db-get-string ebib-current-string ebib-cur-db))))))))

(defun ebib-export-all-strings (prefix)
  "Exports all @STRING definitions.
If a prefix argument is given, it is taken as the database to
copy the definitions to. Without prefix argument, asks for a file
to append them to."
  (interactive "P")
  (when ebib-current-string ; there is always a current string, unless there are no strings
    (let ((num (ebib-prefix prefix)))
      (if num
          (ebib-export-to-db
           num "All @STRING definitions copied to database %d"
           #'(lambda (db)
               (mapc #'(lambda (abbr)
                         (ebib-db-set-string abbr (ebib-db-get-string abbr ebib-cur-db) db 'noerror))
                     ebib-cur-strings-list)))
        (ebib-export-to-file "Export all @STRING definitions to file: "
                             "All @STRING definitions exported to %s"
                             #'(lambda ()
                                 (insert (format "\n")) ; to keep things tidy.
                                 (ebib-format-strings ebib-cur-db)))))))

(defun ebib-strings-help ()
  "Shows the info node on Ebib's strings buffer."
  (interactive)
  (setq ebib-info-flag t)
  (ebib-lower)
  (info "(ebib) The Strings Buffer"))

;;;;;;;;;;;;;;;;;;;;
;; multiline edit ;;
;;;;;;;;;;;;;;;;;;;;

(define-minor-mode ebib-multiline-mode
  "Minor mode for Ebib's multiline edit buffer."
  :init-value nil :lighter nil :global nil
  :keymap '(("\C-c|q" . ebib-quit-multiline-edit-and-save)
            ("\C-c|c" . ebib-cancel-multiline-edit)
            ("\C-c|s" . ebib-save-from-multiline-edit)
            ("\C-c|h" . ebib-multiline-help)))

(easy-menu-define ebib-multiline-menu ebib-multiline-mode-map "Ebib multiline menu"
  '("Ebib"
    ["Store Text and Exit" ebib-quit-multiline-edit-and-save t]
    ["Cancel Edit" ebib-cancel-multiline-edit t]
    ["Save Text" ebib-save-from-multiline-edit t]
    ["Help" ebib-multiline-help t]))

(easy-menu-add ebib-multiline-menu ebib-multiline-mode-map)

(defun ebib-multiline-edit (type &optional starttext)
  "Switches to Ebib's multiline edit buffer.
STARTTEXT is a string that contains the initial text of the buffer."
  ;; (setq ebib-pre-multiline-buffer (current-buffer))
  (ebib-pop-to-buffer 'multiline)
  (set-buffer-modified-p nil)
  (erase-buffer)
  (setq ebib-editing type)
  (when starttext
    (insert starttext)
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun ebib-quit-multiline-edit-and-save ()
  "Quits the multiline edit buffer, saving the text."
  (interactive)
  (ebib-store-multiline-text)
  (ebib-leave-multiline-edit-buffer) 
  (message "Text stored."))

(defun ebib-cancel-multiline-edit ()
  "Quits the multiline edit buffer and discards the changes."
  (interactive)
  (catch 'no-cancel
    (when (buffer-modified-p)
      (unless (y-or-n-p "Text has been modified. Abandon changes? ")
        (throw 'no-cancel nil)))
    (ebib-leave-multiline-edit-buffer)))

(defun ebib-leave-multiline-edit-buffer ()
  "Leaves the multiline edit buffer.
Restores the previous buffer in the window that the multiline
edit buffer was shown in."
  (if (and (eq ebib-layout 'index-only)
           ebib-popup-entry-window)
      (delete-window)
    (switch-to-buffer nil t))
  (cond
   ((eq ebib-editing 'preamble)
    (ebib-pop-to-buffer 'index))
   ((eq ebib-editing 'fields)
    (ebib-pop-to-buffer 'entry)
    (ebib-redisplay-current-field))))

(defun ebib-save-from-multiline-edit ()
  "Saves the database from within the multiline edit buffer.
The text being edited is stored before saving the database."
  (interactive)
  (ebib-store-multiline-text)
  (ebib-save-database ebib-cur-db)
  (set-buffer-modified-p nil))

(defun ebib-store-multiline-text ()
  "Stores the text being edited in the multiline edit buffer."
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (cond
     ((eq ebib-editing 'preamble)
      (if (equal text "")
          (ebib-db-remove-preamble ebib-cur-db)
        (ebib-db-set-preamble text ebib-cur-db 'overwrite)))
     ((eq ebib-editing 'fields)
      (if (equal text "")
          (ebib-db-remove-field-value ebib-current-field (ebib-cur-entry-key) ebib-cur-db) 
        (ebib-db-set-field-value ebib-current-field text (ebib-cur-entry-key) ebib-cur-db 'overwrite ebib-multiline-unbraced)))))
  (ebib-set-modified t))

(defun ebib-multiline-help ()
  "Show the info node on Ebib's multiline edit buffer."
  (interactive)
  (setq ebib-info-flag t)
  (ebib-lower)
  (info "(ebib) The Multiline Edit Buffer"))

;;;;;;;;;;;;;;;;;;;
;; ebib-log-mode ;;
;;;;;;;;;;;;;;;;;;;

(defvar ebib-log-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map " " 'scroll-up)
    (define-key map "b" 'scroll-down)
    (define-key map "q" 'ebib-quit-log-buffer)
    map)
  "Keymap for the ebib log buffer.")

(define-derived-mode ebib-log-mode
  fundamental-mode "Ebib-log"
  "Major mode for the Ebib log buffer."
  (local-set-key "\C-xb" 'ebib-quit-log-buffer)
  (local-set-key "\C-xk" 'ebib-quit-log-buffer))

(defun ebib-quit-log-buffer ()
  "Exits the log buffer."
  (interactive)
  (if (and (eq ebib-layout 'index-only)
           ebib-popup-entry-window)
      (delete-window)
    (with-ebib-window-nondedicated
      (switch-to-buffer nil t)))
  (ebib-pop-to-buffer 'index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions for non-Ebib buffers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ebib-import ()
  "Searches for BibTeX entries in the current buffer.
The entries are added to the current database (i.e. the database
that was active when Ebib was lowered. Works on the whole buffer,
or on the region if it is active."
  (interactive)
  (if (not ebib-cur-db)
      (error "No database loaded")
    (with-syntax-table ebib-syntax-table
      (save-excursion
        (save-restriction
          (if (use-region-p)
              (narrow-to-region (region-beginning)
                                (region-end)))
          (let ((buffer (current-buffer)))
            (with-temp-buffer
              (insert-buffer-substring buffer)
              (let ((result (ebib-find-bibtex-entries ebib-cur-db t)))
                (unless (ebib-cur-entry-key)
                  (ebib-db-set-current-entry-key t ebib-cur-db))
                (ebib-redisplay)
                (ebib-set-modified t)
                (message (format "%d entries, %d @STRINGs and %s @PREAMBLE found in buffer."
                                 (car result)
                                 (cadr result)
                                 (if (caddr result) "a" "no")))))))))))

(defun ebib-get-db-from-filename (filename)
  "Returns the database struct associated with FILENAME."
  (when filename
    (if (file-name-absolute-p filename)
        (setq filename (expand-file-name filename))) ; expand ~, . and ..
    (catch 'found
      (mapc #'(lambda (db)
                ;; If filename is absolute, we want to compare to the
                ;; absolute filename of the database, otherwise we should use
                ;; only the non-directory component.
                (if (string= filename (ebib-db-get-filename db (not (file-name-absolute-p filename))))
                    (throw 'found db)))
            ebib-databases)
      nil)))

(defun ebib-get-local-databases ()
  "Return a list of .bib files associated with the file in the current LaTeX buffer.
Each element in the list is a string holding the name of the .bib
file. This function simply searches the current LaTeX file or its
master file for a `\\bibliography' or `\\addbibresource' command
and returns the file(s) given in its argument. If no such command
is found, return the symbol `none'."
  ;; This only makes sense in LaTeX buffers
  (if (not (eq major-mode 'latex-mode))
      'none
    (let ((texfile-buffer (current-buffer))
          texfile)
      ;; if AucTeX's TeX-master is used and set to a string, we must
      ;; search that file for a \bibliography command, as it's more
      ;; likely to be in there than in the file we're in.
      (and (boundp 'TeX-master)
           (stringp TeX-master)
           (setq texfile (ebib-ensure-extension TeX-master ".tex")))
      (with-temp-buffer
        (if (and texfile (file-readable-p texfile))
            (insert-file-contents texfile)
          (insert-buffer-substring texfile-buffer))
        (save-excursion
          (save-match-data
            (let (files)
              (goto-char (point-min))
              ;; First search for a \bibliography command:
              (if (re-search-forward "\\\\\\(?:no\\)*bibliography{\\(.*?\\)}" nil t)
                  (setq files (mapcar #'(lambda (file)
                                          (ebib-ensure-extension file ".bib"))
                                      (split-string (buffer-substring-no-properties (match-beginning 1) (match-end 1)) ",[ ]*")))
                ;; If we didn't find a \bibliography command, search for \addbibresource commands:
                (while (re-search-forward "\\\\addbibresource\\(\\[.*?\\]\\)?{\\(.*?\\)}" nil t)
                  (let ((option (match-string 1))
                        (file (match-string-no-properties 2)))
                    ;; If this isn't a remote resource, add it to the list.
                    (unless (and option (string-match "location=remote" option))
                      (push file files)))))
              (or files 'none))))))))

(defun ebib-create-collection-from-db ()
  "Create a collection of BibTeX keys.
The source of the collection is either curent database or, if the
current buffer is a LaTeX file containing a \\bibliography
command, the BibTeX files in that command (if they are open in
Ebib)."
  (or ebib-local-bibtex-filenames
      (setq ebib-local-bibtex-filenames (ebib-get-local-databases)))
  (let (collection)
    (if (eq ebib-local-bibtex-filenames 'none)
        (if (null ebib-cur-keys-list)
            (error "No entries found in current database")
          (setq collection ebib-cur-keys-list))
      (mapc #'(lambda (file)
                (let ((db (ebib-get-db-from-filename file)))
                  (cond
                   ((null db)
                    (message "Database %s not loaded" file))
                   ((null (ebib-db-get-current-entry-key db))
                    (message "No entries in database %s" file))
                   (t (setq collection (append (ebib-db-list-keys db 'nosort)
                                               collection))))))
            ebib-local-bibtex-filenames))
    collection))

(defun ebib-insert-bibtex-key ()
  "Inserts a BibTeX key at POINT.
The user is prompted for a BibTeX key and has to choose one from
the database(s) associated with the current LaTeX file, or from
the current database if there is no \\bibliography command. Tab
completion works."
  (interactive)
  (ebib-execute-when
    ((database)
     (let ((collection (ebib-create-collection-from-db)))
       (when collection
         (let* ((key (completing-read "Key to insert: " collection nil t nil 'ebib-key-history))
                (format-list (or (cadr (assoc (buffer-local-value 'major-mode (current-buffer)) ebib-citation-commands))
                                 (cadr (assoc 'any ebib-citation-commands))))
                (citation-command
                 (ebib-ifstring (format-string (cadr (assoc
                                               (completing-read "Command to use: " format-list nil nil nil 'ebib-cite-command-history)
                                               format-list)))
                     (cl-multiple-value-bind (before repeater separator after) (ebib-split-citation-string format-string)
                       (concat (ebib-create-citation-command before)
                               (ebib-create-citation-command repeater key)
                               (ebib-create-citation-command after)))
                   key))) ; if the user didn't provide a command, we insert just the entry key
           (when citation-command
             (insert (format "%s" citation-command)))))))
    ((default)
     (error "No database loaded"))))

(defun ebib-create-bib-from-bbl ()
  "Create a .bib file for the current LaTeX document.
The LaTeX document must have a .bbl file associated with it. All
bibitems are extracted from this file and a new .bib file is
created containing only these entries."
  (interactive)
  (ebib-execute-when
    ((database)
     (or ebib-local-bibtex-filenames
         (setq ebib-local-bibtex-filenames (ebib-get-local-databases)))
     (let* ((filename-sans-extension (file-name-sans-extension (buffer-file-name)))
            (bbl-file (concat filename-sans-extension ".bbl"))
            (bib-file (concat filename-sans-extension (car ebib-bibtex-extensions))))
       (unless (file-exists-p bbl-file)
         (error "No .bbl file exists. Run BibTeX first"))
       (when (or (not (file-exists-p bib-file))
                 (y-or-n-p (format "%s already exists. Overwrite? " (file-name-nondirectory bib-file))))
         (when (file-exists-p bib-file)
           (delete-file bib-file)) ; accepts additional arg `delete-by-moving-to-trash' in Emacs 24
         (let ((databases
                (delq nil (mapcar #'(lambda (file)
                                      (ebib-get-db-from-filename file))
                                  ebib-local-bibtex-filenames))))
           (with-temp-buffer
             (insert-file-contents bbl-file)
             (ebib-export-entries (ebib-read-entries-from-bbl) databases bib-file))))))
    ((default)
     (beep))))

(defun ebib-read-entries-from-bbl ()
  (interactive)
  (goto-char (point-min))
  (let (entries)
    (while (re-search-forward "\\\\bibitem\\[\\(?:.\\|\n[^\n]\\)*\\]{\\(.*?\\)}" nil t)
      (add-to-list 'entries (match-string 1) t))
    entries))

(provide 'ebib)

;;; ebib ends here
