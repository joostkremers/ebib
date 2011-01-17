;; Ebib v==VERSION==
;;
;; Copyright (c) 2003-2010 Joost Kremers
;; All rights reserved.
;;
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

(eval-when-compile
  (require 'cl))
(require 'easymenu)

;;;;;;;;;;;;;;;;;;;;;;
;; global variables ;;
;;;;;;;;;;;;;;;;;;;;;;

;; user customisation

(defgroup ebib nil "Ebib: a BibTeX database manager" :group 'Tex)

(defcustom ebib-default-type 'article
  "*The default type for a newly created BibTeX entry."
  :group 'ebib
  :type 'symbol)

(defcustom ebib-preload-bib-files nil
  "*List of .bib files to load automatically when Ebib starts."
  :group 'ebib
  :type '(repeat (file :must-match t)))

(defcustom ebib-additional-fields '(crossref url annote abstract
					     keywords file timestamp)
  "*List of the additional fields."
  :group 'ebib
  :type '(repeat (symbol :tag "Field")))

(defcustom ebib-layout 'full
  "*Ebib window layout.
Full width: Ebib occupies the entire Emacs frame.
Custom width: Ebib occupies the right side of the Emacs frame,
with the left side free for another window."
  :group 'ebib
  :type '(choice (const :tag "Full width" full)
		 (const :tag "Custom width" custom)))

(defcustom ebib-width 80
  "*Width of the Ebib windows.
Only takes effect if EBIB-LAYOUT is set to CUSTOM."
  :group 'ebib
  :type 'integer)

(defcustom ebib-index-window-size 10
  "*The number of lines used for the index buffer window."
  :group 'ebib
  :type 'integer)

(defcustom ebib-index-display-fields nil
  "*List of the fields to display in the index buffer."
  :group 'ebib
  :type '(repeat (symbol :tag "Index Field")))

(defcustom ebib-insertion-commands '(("cite" 1 nil))
  "*A list of commands that can be used to insert an entry into a (La)TeX buffer.
For use with EBIB-INSERT-BIBTEX-KEY and EBIB-PUSH-BIBTEX-KEY."
  :group 'ebib
  :type '(repeat (list :tag "Citation command" (string :tag "Command")
		       (integer :tag "Optional arguments")
		       (choice (const :tag "Standard command" nil)
			       (const :tag "Multicite command" t)))))

(defcustom ebib-multiline-major-mode 'text-mode
  "*The major mode of the multiline edit buffer."
  :group 'ebib
  :type '(function :tag "Mode function"))

(defcustom ebib-sort-order nil
  "*The fields on which the BibTeX entries are to be sorted in the .bib file.
Sorting is done on different sort levels, and each sort level contains one
or more sort keys."
  :group 'ebib
  :type '(repeat (repeat :tag "Sort level" (symbol :tag "Sort field"))))

(defcustom ebib-save-xrefs-first nil
  "*If true, entries with a crossref field will be saved first in the .bib-file.
Setting this option has unpredictable results for the sort order
of entries, so it is not compatible with setting the Sort Order option."
  :group 'ebib
  :type 'boolean)

(defface ebib-crossref-face '((t (:foreground "red")))
  "*Face used to indicate values inherited from crossreferenced entries."
  :group 'ebib)

(defface ebib-marked-face (if (featurep 'xemacs)
			      '((t (:foreground "white" :background "red")))
			    '((t (:inverse-video t))))
  "*Face to indicate marked entries."
  :group 'ebib)

(defcustom ebib-use-timestamp nil
  "*If true, new entries will get a time stamp.
The time stamp will be stored in a field \"timestamp\" that can
be made visible with the `H' command in the index buffer."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-timestamp-format "%a %b %e %T %Y"
  "*Format of the time string used in the timestamp.
The format is passed unmodified to FORMAT-TIME-STRING, see the
documentation of that function for details."
  :group 'ebib
  :type 'string)

(defcustom ebib-standard-url-field 'url
  "*Standard field to store urls in.
In the index buffer, the command ebib-browse-url can be used to
send a url to a browser. This option sets the field from which
this command extracts the url."
  :group 'ebib
  :type 'symbol)

(defcustom ebib-url-regexp "\\\\url{\\(.*\\)}\\|https?://[^ '<>\"\n\t\f]+"
  "*Regular expression to extract urls."
  :group 'ebib
  :type 'string)

(defcustom ebib-browser-command ""
  "*Command to call the browser with.
GNU/Emacs has a function call-browser, which is used if this
option is unset."
  :group 'ebib
  :type '(string :tag "Browser command"))

(defcustom ebib-standard-file-field 'file
  "*Standard field to store filenames in.
In the index buffer, the command ebib-view-file can be used to
view a file externally. This option sets the field from which
this command extracts the filename."
  :group 'ebib
  :type 'symbol)

(defcustom ebib-file-associations '(("pdf" . "xpdf")
				    ("ps" . "gv"))
  "*List of file associations.
Lists file extensions together with external programs to handle
files with those extensions. If the external program is left
blank, Ebib tries to handle the file internally in
Emacs (e.g. with doc-view-mode)."
  :group 'ebib
  :type '(repeat (cons :tag "File association"
		       (string :tag "Extension") (string :tag "Command"))))

(defcustom ebib-file-regexp "[^?|\\:*<>\" \n\t\f]+"
  "*Regular expression to match filenames."
  :group 'ebib
  :type 'string)

(defcustom ebib-file-search-dirs '("~")
  "*List of directories to search for files."
  :group 'ebib
  :type '(repeat :tag "Search directories" (string :tag "Directory")))

(defcustom ebib-print-preamble nil
  "*Preamble used for the LaTeX file for printing the database.
Each string is added to the preamble on a separate line."
  :group 'ebib
  :type '(repeat (string :tag "Add to preamble")))

(defcustom ebib-print-newpage nil
  "*If set, each entry is printed on a separate page."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-print-multiline nil
  "*If set, multiline fields are included when printing the database."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-latex-preamble '("\\bibliographystyle{plain}")
  "*Preamble used for the LaTeX file for BibTeXing the database.
Each string is added to the preamble on a separate line."
  :group 'ebib
  :type '(repeat (string :tag "Add to preamble")))

(defcustom ebib-print-tempfile ""
  "*Temporary file for use with EBIB-PRINT-DATABASE and EBIB-LATEX-DATABASE."
  :group 'ebib
  :type '(file))

(defcustom ebib-allow-identical-fields nil
  "*If set, Ebib handles multiple occurrences of a field gracefully."
  :group 'ebib
  :type 'boolean)

(defvar ebib-unique-field-list nil
  "Holds a list of all field names.")

(defmacro add-to-listq (listvar element &optional append fn)
  (if (or (featurep 'xemacs)
	  (string< emacs-version "22"))
      `(add-to-list (quote ,listvar) ,element ,append)
    `(add-to-list (quote ,listvar) ,element ,append ,fn)))

(defun ebib-set-unique-field-list (var value)
  "Sets EBIB-UNIQUE-FIELD-LIST on the basis of EBIB-ENTRY-TYPES"
  (set-default var value)
  (setq ebib-unique-field-list nil)
  (mapc #'(lambda (entry)
	    (mapc #'(lambda (field)
		      (add-to-listq ebib-unique-field-list field t 'eq))
		  (cadr entry))
	    (mapc #'(lambda (field)
		      (add-to-listq ebib-unique-field-list field t 'eq))
		  (caddr entry)))
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

;; generic for all databases

;; constants and variables that are set only once
(defconst ebib-bibtex-identifier "[^\"#%'(),={} \t\n\f]*" "Regexp describing a licit BibTeX identifier.")
(defconst ebib-version "==VERSION==")
(defvar ebib-initialized nil "T if Ebib has been initialized.")

;; buffers and highlights
(defvar ebib-index-buffer nil "The index buffer.")
(defvar ebib-entry-buffer nil "The entry buffer.")
(defvar ebib-strings-buffer nil "The strings buffer.")
(defvar ebib-multiline-buffer nil "Buffer for editing multiline strings.")
(defvar ebib-log-buffer nil "Buffer showing warnings and errors during loading of .bib files")
(defvar ebib-index-highlight nil "Highlight to mark the current entry.")
(defvar ebib-fields-highlight nil "Highlight to mark the current field.")
(defvar ebib-strings-highlight nil "Highlight to mark the current string.")

;; general bookkeeping
(defvar ebib-minibuf-hist nil "Holds the minibuffer history for Ebib")
(defvar ebib-saved-window-config nil "Stores the window configuration when Ebib is called.")
(defvar ebib-pre-ebib-window nil "The window that was active when Ebib was called.")
(defvar ebib-pre-multiline-buffer nil "The buffer in the window before switching to the multiline edit buffer.")
(defvar ebib-export-filename nil "Filename to export entries to.")
(defvar ebib-push-buffer nil "Buffer to push entries to.")
(defvar ebib-search-string nil "Stores the last search string.")
(defvar ebib-editing nil "Indicates what the user is editing.
Its value can be 'strings, 'fields, or 'preamble.")
(defvar ebib-multiline-raw nil "Indicates whether the multiline text being edited is raw.")
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

;; the databases

;; each database is represented by a struct
(defstruct edb
  (database (make-hash-table :test 'equal)) ; hashtable containing the database itself
  (keys-list nil)		            ; sorted list of the keys in the database
  (cur-entry nil)                           ; sublist of KEYS-LIST that starts with the current entry
  (marked-entries nil)                      ; list of marked entries
  (n-entries 0)			            ; number of entries stored in this database
  (strings (make-hash-table :test 'equal))  ; hashtable with the @STRING definitions
  (strings-list nil)                        ; sorted list of the @STRING abbreviations
  (preamble nil)                            ; string with the @PREAMBLE definition
  (filename nil)                            ; name of the BibTeX file that holds this database
  (name nil)                                ; name of the database
  (modified nil)                            ; has this database been modified?
  (make-backup nil)                         ; do we need to make a backup of the .bib file?
  (virtual nil))                            ; is this a virtual database?

;; the master list and the current database
(defvar ebib-databases nil "List of structs containing the databases.")
(defvar ebib-cur-db nil "The database that is currently active.")

;;;;;; bookkeeping required when editing field values or @STRING definitions

(defvar ebib-hide-hidden-fields t "If set to T, hidden fields are not shown.")

;; this variable is set when the user enters the entry buffer
(defvar ebib-cur-entry-fields nil "The fields of the type of the current entry.")

;; these two are set by EBIB-FILL-ENTRY-BUFFER
(defvar ebib-cur-entry-hash nil "The hash table containing the data of the current entry.")
(defvar ebib-current-field nil "The current field.")

;; and this one by EBIB-FILL-STRINGS-BUFFER
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
;; useful macros and functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro nor (&rest args)
  "Returns T if none of its arguments are true."
  `(not (or ,@args)))

;; we sometimes (often, in fact ;-) need to do something with a string, but
;; take special action (or do nothing) if that string is empty. IF-STR
;; makes that easier:

(defmacro if-str (bindvar then &rest else)
  "Execute THEN only if STRING is nonempty.
Format: (if-str (var value) then-form [else-forms]) VAR is bound
to VALUE, which is evaluated. If VAR is a nonempty string,
THEN-FORM is executed. If VAR is either \"\" or nil, ELSE-FORM is
executed. Returns the value of THEN or of ELSE."
  (declare (indent 2))
  `(let ,(list bindvar)
     (if (nor (null ,(car bindvar))
	      (equal ,(car bindvar) ""))
	 ,then
       ,@else)))

(defmacro last1 (lst &optional n)
  "Returns the last (or Nth last) element of LST."
  `(car (last ,lst ,n)))

;; we sometimes need to walk through lists.  these functions yield the
;; element directly preceding or following ELEM in LIST. in order to work
;; properly, ELEM must be unique in LIST, obviously. if ELEM is the
;; first/last element of LIST, or if it is not contained in LIST at all,
;; the result is nil.
(defun next-elem (elem list)
  (cadr (member elem list)))

(defun prev-elem (elem list)
  (if (or (equal elem (car list))
	  (not (member elem list)))
      nil
    (last1 list (1+ (length (member elem list))))))

(defun read-string-at-point (chars)
  "Reads a string at POINT delimited by CHARS and returns it.
CHARS is a string of characters that should not occur in the string."
  (save-excursion
    (skip-chars-backward (concat "^" chars))
    (let ((beg (point)))
      (looking-at-goto-end (concat "[^" chars "]*"))
      (buffer-substring-no-properties beg (point)))))

(defun remove-from-string (string remove)
  "Returns a copy of STRING with all the occurrences of REMOVE taken out.
REMOVE can be a regex."
  (apply 'concat (split-string string remove)))

(defun in-string (char string)
  "Returns T if CHAR is in STRING, otherwise NIL."
  (catch 'found
    (do ((len (length string))
	 (i 0 (1+ i)))
	((= i len) nil)
      (if (eq char (aref string i))
	  (throw 'found t)))))

(defun ensure-extension (string ext)
  "Makes sure STRING has the extension EXT, by appending it if necessary.
EXT should be an extension without the dot."
  (if (string-match (concat "\\." ext "$") string)
      string
    (concat string "." ext)))

(defmacro with-buffer-writable (&rest body)
  "Makes the current buffer writable and executes the commands in BODY.
After BODY is executed, the buffer modified flag is unset."
  (declare (indent defun))
  `(unwind-protect
       (let ((buffer-read-only nil))
	 ,@body)
     (set-buffer-modified-p nil)))

(defmacro safe-write-region (start end filename &optional append visit lockname mustbenew)
  "XEmacs does not have the MUSTBENEW argument, so this is a way to implement it."
  (if (featurep 'xemacs)
      `(if (and (file-exists-p ,filename)
		(not (y-or-n-p (format "File %s already exists; overwrite anyway? " ,filename))))
	   (error "File %s exist" ,filename)
	 (write-region ,start ,end ,filename ,append ,visit ,lockname))
    `(write-region ,start ,end ,filename ,append ,visit ,lockname ,mustbenew)))

(defun symbol-or-string (x)
  "Returns the symbol-name of X if X is a symbol, otherwise return X.
Much like SYMBOL-NAME, except it does not throw an error if X is
not a symbol."
  (if (symbolp x)
       (symbol-name x)
     x))

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

(defun region-active ()
  (if (featurep 'xemacs)
      (region-active-p)
    mark-active))

;; RAW-P determines if STRING is raw. note that we cannot do this by
;; simply checking whether STRING begins with { and ends with } (or
;; begins and ends with "), because something like "{abc} # D # {efg}"
;; would then be incorrectly recognised as non-raw. so we need to do
;; the following: take out everything that is between braces or
;; quotes, and see if anything is left. if there is, the original
;; string was raw, otherwise it was not.
;;
;; so i first check whether the string begins with { or ". if not, we
;; certainly have a raw string. (RAW-P recognises this through the default
;; clause of the COND.) if the first character is { or ", we first take out
;; every occurrence of backslash-escaped { and } or ", so that the rest of
;; the function does not get confused over them.
;;
;; then, if the first character is {, i use REMOVE-FROM-STRING to take out
;; every occurrence of the regex "{[^{]*?}", which translates to "the
;; smallest string that starts with { and ends with }, and does not contain
;; another {. IOW, it takes out the innermost braces and their
;; contents. because braces may be embedded, we have to repeat this step
;; until no more balanced braces are found in the string. (note that it
;; would be unwise to check for just the occurrence of { or }, because that
;; would throw RAW-P in an infinite loop if a string contains an unbalanced
;; brace.)
;;
;; for strings beginning with " i do the same, except that it is not
;; necessary to repeat this in a WHILE loop, for the simple reason that
;; strings surrounded with double quotes cannot be embedded; i.e.,
;; "ab"cd"ef" is not a valid (BibTeX) string, while {ab{cd}ef} is.
;;
;; note: because these strings are to be fed to BibTeX and ultimately
;; (La)TeX, it might seem that we don't need to worry about strings
;; containing unbalanced braces, because (La)TeX would choke on them. but
;; the user may inadvertently enter such a string, and we therefore need to
;; be able to handle it. (alternatively, we could perform a check on
;; strings and warn the user.)

(defun raw-p (string)
  "Non-nil if STRING is raw."
  (when (stringp string)
    (cond
     ((eq (string-to-char string) ?\{)
      ;; we remove all occurrences of `\{' and of `\}' from the string:
      (let ((clear-str (remove-from-string (remove-from-string string "[\\][{]")
					   "[\\][}]")))
	(while (and (in-string ?\{ clear-str) (in-string ?\} clear-str))
	  (setq clear-str (remove-from-string clear-str "{[^{]*?}")))
	(> (length clear-str) 0)))
     ((eq (string-to-char string) ?\")
      (let ((clear-str (remove-from-string string "[\\][\"]")))	; remove occurrences of `\"'
	(setq clear-str (remove-from-string clear-str "\"[^\"]*?\""))
	(> (length clear-str) 0)))
     (t t))))

(defun to-raw (string)
  "Converts a string to its raw counterpart."
  (if (and (stringp string)
           (not (raw-p string)))
      (substring string 1 -1)
    string))

(defun from-raw (string)
  "Converts a raw string to a non-raw one."
  (if (raw-p string)
      (concat "{" string "}")
    string))

(defun multiline-p (string)
  "True if STRING is multiline."
  (if (stringp string)
      (string-match "\n" string)))

(defun first-line (string)
  "Returns the first line of a multi-line string."
  (string-match "\n" string)
  (substring string 0 (match-beginning 0)))

(defun sort-in-buffer (limit str)
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
		 (> (- upper lower) 1))     ; if upper and lower differ by only 1, we have found the
					    ; position to insert the entry in.
	  (save-excursion
	    (let ((beg (point)))
	      (end-of-line)
	      (if (string< (buffer-substring-no-properties beg (point)) str)
		  (setq lower middle)
		(setq upper middle)))))))))

(defun match-all (match-str string)
  "Highlights all the matches of MATCH-STR in STRING.
The return value is a list of two elements: the first is the
modified string, the second either t or nil, indicating whether a
match was found at all."
  (do ((counter 0 (match-end 0)))
      ((not (string-match match-str string counter)) (values string (not (= counter 0))))
    (add-text-properties (match-beginning 0) (match-end 0) '(face highlight) string)))

(defun looking-at-goto-end (str &optional match)
  "Like LOOKING-AT but moves point to the end of the matching string.
MATCH acts just like the argument to MATCH-END, and defaults to 0."
  (or match (setq match 0))
  (let ((case-fold-search t))
    (if (looking-at str)
	(goto-char (match-end match)))))

;; this needs to be wrapped in an eval-and-compile, to keep Emacs from
;; complaining that ebib-execute-helper isn't defined when it compiles
;; ebib-execute-when.
(eval-and-compile
  (defun ebib-execute-helper (env)
    "Helper function for EBIB-EXECUTE-WHEN."
    (cond
     ((eq env 'entries)
      '(and ebib-cur-db
	    (edb-cur-entry ebib-cur-db)))
     ((eq env 'marked-entries)
      '(and ebib-cur-db
	    (edb-marked-entries ebib-cur-db)))
     ((eq env 'database)
      'ebib-cur-db)
     ((eq env 'real-db)
      '(and ebib-cur-db
	    (not (edb-virtual ebib-cur-db))))
     ((eq env 'virtual-db)
      '(and ebib-cur-db
	    (edb-virtual ebib-cur-db)))
     ((eq env 'no-database)
      '(not ebib-cur-db))
     (t t))))

(defmacro ebib-execute-when (&rest forms)
  "Macro to facilitate writing Ebib functions.
This functions essentially like a COND clause: the basic format
is (ebib-execute-when FORMS ...), where each FORM is built up
as (ENVIRONMENTS BODY). ENVIRONMENTS is a list of symbols (not
quoted) that specify under which conditions BODY is to be
executed. Valid symbols are:

entries: execute when there are entries in the database,
marked-entries: execute when there are marked entries in the database,
database: execute if there is a database,
no-database: execute if there is no database,
real-db: execute when there is a database and it is real,
virtual-db: execute when there is a database and it is virtual,
default: execute if all else fails.

Just like with COND, only one form is actually executed, the
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
       (cond
	((not ,goal-db)
	 (error "Database %d does not exist" ,num))
	((edb-virtual ,goal-db)
	 (error "Database %d is virtual" ,num))
	(t (when (funcall ,copy-fn ,goal-db)
	     (ebib-set-modified t ,goal-db)
	     (message ,message ,num)))))))

(defmacro ebib-cur-entry-key ()
  "Returns the key of the current entry in EBIB-CUR-DB."
  `(car (edb-cur-entry ebib-cur-db)))

(defmacro ebib-export-to-file (prompt-string message insert-fn)
  "Exports data to a file.
PROMPT-STRING is the string that is used to ask for the filename
to export to. INSERT-FN must insert the data to be exported into
the current buffer: it is called within a WITH-TEMP-BUFFER, whose
contents is appended to the file the user enters.

MESSAGE is shown in the echo area when the export was
successful. It must contain a %s directive, which is used to
display the actual filename."
  (let ((filename (make-symbol "filename")))
    `(let ((insert-default-directory (not ebib-export-filename)))
       (if-str (,filename (read-file-name
			   ,prompt-string "~/" nil nil ebib-export-filename))
	   (with-temp-buffer
	     (funcall ,insert-fn)
	     (append-to-file (point-min) (point-max) ,filename)
	     (setq ebib-export-filename ,filename))))))

(defun ebib-temp-window ()
  "Returns a window to be used for temporary use."
  (if (eq ebib-layout 'full)
      (get-buffer-window ebib-entry-buffer)
    ebib-pre-ebib-window))

(defun ebib-get-obl-fields (entry-type)
  "Returns the obligatory fields of ENTRY-TYPE."
  (nth 1 (assoc entry-type ebib-entry-types)))

(defun ebib-get-opt-fields (entry-type)
  "Returns the optional fields of ENTRY-TYPE."
  (nth 2 (assoc entry-type ebib-entry-types)))

(defun ebib-get-all-fields (entry-type)
  "Returns all the fields of ENTRY-TYPE."
  (cons 'type* (append (ebib-get-obl-fields entry-type)
		       (ebib-get-opt-fields entry-type)
		       ebib-additional-fields)))

(defmacro ebib-retrieve-entry (entry-key db)
  "Returns the hash table of the fields stored in DB under ENTRY-KEY."
  `(gethash ,entry-key (edb-database ,db)))

(defun ebib-erase-buffer (buffer)
  (set-buffer buffer)
  (with-buffer-writable
    (erase-buffer)))

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
  (set-buffer ebib-index-buffer)
  (beginning-of-line)
  (let ((beg (point)))
    (if ebib-index-display-fields
	(end-of-line)
      (skip-chars-forward "^ "))
    (ebib-move-highlight ebib-index-highlight beg (point) ebib-index-buffer)
    (beginning-of-line)))

(defun ebib-set-fields-highlight ()
  (set-buffer ebib-entry-buffer)
  (beginning-of-line)
  (let ((beg (point)))
    (looking-at-goto-end "[^ \t\n\f]*")
    (ebib-move-highlight ebib-fields-highlight beg (point) ebib-entry-buffer)
    (beginning-of-line)))

(defun ebib-set-strings-highlight ()
  (set-buffer ebib-strings-buffer)
  (beginning-of-line)
  (let ((beg (point)))
    (looking-at-goto-end "[^ \t\n\f]*")
    (ebib-move-highlight ebib-strings-highlight beg (point) ebib-strings-buffer)
    (beginning-of-line)))

(defun ebib-display-entry (entry-key)
  "Displays ENTRY-KEY in the index buffer at POINT."
  (set-buffer ebib-index-buffer)
  (insert (format "%-30s %s\n"
		  entry-key
		  (if ebib-index-display-fields
		      (let ((cur-entry-hash (ebib-retrieve-entry entry-key ebib-cur-db)))
			(mapconcat #'(lambda (field)
				       (or
					(to-raw (gethash field cur-entry-hash))
					""))
				   ebib-index-display-fields
				   "; "))
		    ""))))

(defun ebib-redisplay-current-field ()
  "Redisplays the contents of the current field in the entry buffer."
  (set-buffer ebib-entry-buffer)
  (if (eq ebib-current-field 'crossref)
      (progn
	(ebib-fill-entry-buffer)
	(setq ebib-current-field 'crossref)
	(re-search-forward "^crossref")
	(ebib-set-fields-highlight))
    (with-buffer-writable
      (goto-char (ebib-highlight-start ebib-fields-highlight))
      (let ((beg (point)))
	(end-of-line)
	(delete-region beg (point)))
      (insert (format "%-17s " (symbol-name ebib-current-field))
	      (ebib-get-field-highlighted ebib-current-field ebib-cur-entry-hash))
      (ebib-set-fields-highlight))))

(defun ebib-redisplay-current-string ()
  "Redisplays the current string definition in the strings buffer."
  (set-buffer ebib-strings-buffer)
  (with-buffer-writable
    (let ((str (to-raw (gethash ebib-current-string (edb-strings ebib-cur-db)))))
      (goto-char (ebib-highlight-start ebib-strings-highlight))
      (let ((beg (point)))
	(end-of-line)
	(delete-region beg (point)))
      (insert (format "%-18s %s" ebib-current-string
		      (if (multiline-p str)
			  (concat "+" (first-line str))
			(concat " " str))))
      (ebib-set-strings-highlight))))

(defun ebib-move-to-field (field direction)
  "Moves the fields overlay to the line containing FIELD.
If DIRECTION is positive, searches forward, if DIRECTION is
negative, searches backward. If DIRECTION is 1 or -1, searches
from POINT, if DIRECTION is 2 or -2, searches from beginning or
end of buffer.  If FIELD is not found in the entry buffer, the
overlay is not moved.  FIELD must be a symbol."

  ;;Note: this function does NOT change the value of EBIB-CURRENT-FIELD!

  (set-buffer ebib-entry-buffer)
  (if (eq field 'type*)
      (goto-char (point-min))
    (multiple-value-bind (fn start limit) (if (>= direction 0)
					      (values 're-search-forward (point-min) (point-max))
					    (values 're-search-backward (point-max) (point-min)))
      ;; make sure we can get back to our original position, if the field
      ;; cannot be found in the buffer:
      (let ((current-pos (point)))
	(when (eq (logand direction 1) 0) ; if direction is even
	  (goto-char start))
	(unless (funcall fn (concat "^" (symbol-name field)) limit t)
	  (goto-char current-pos)))))
  (ebib-set-fields-highlight))

(defun ebib-create-collection (hashtable)
  "Creates a list from the keys in HASHTABLE that can be used as COLLECTION in COMPLETING-READ.
The keys of HASHTABLE must be either symbols or strings."
  (let ((result nil))
    (maphash #'(lambda (x y)
		 (setq result (cons (cons (symbol-or-string x)
					  0)
				    result)))
	     hashtable)
    result))

(defun ebib-get-field-highlighted (field current-entry &optional match-str)
  ;; note: we need to work on a copy of the string, otherwise the highlights
  ;; are made to the string as stored in the database. hence copy-sequence.
  (let ((case-fold-search t)
	(string (copy-sequence (gethash field current-entry)))
	(raw " ")
	(multiline " ")
	(matched nil))
    ;; we have to do a couple of things now:
    ;; - remove {} or "" around the string, if they're there
    ;; - search for match-str
    ;; - properly adjust the string if it's multiline
    ;; but all this is not necessary if there was no string
    (if (null string)
	(let* ((xref (to-raw (gethash 'crossref current-entry)))
	       (xref-entry (ebib-retrieve-entry xref ebib-cur-db)))
	  (when xref-entry
	    (setq string (copy-sequence (gethash field xref-entry)))
	    (if string
		(setq string (propertize (to-raw string) 'face 'ebib-crossref-face 'fontified t))
	      (setq string ""))))
      (if (raw-p string)
	  (setq raw "*")
	(setq string (to-raw string)))	; we have to make the string look nice
      (when match-str
	(multiple-value-setq (string matched) (match-all match-str string))))
    (when (multiline-p string)
      ;; IIUC PROPERTIZE shouldn't be necessary here, as the variable
      ;; multiline is local and therefore the object it refers to should
      ;; be GC'ed when the function returns. but for some reason, the
      ;; plus sign is persistent, and if it's been highlighted as the
      ;; result of a search, it stays that way.
      (setq multiline (propertize "+" 'face nil))
      (setq string (first-line string)))
    (when (and matched
	       (string= multiline "+"))
      (add-text-properties 0 1 '(face highlight) multiline))
    (concat raw multiline string)))

(defun ebib-format-fields (entry fn &optional match-str)
  (let* ((entry-type (gethash 'type* entry))
	 (obl-fields (ebib-get-obl-fields entry-type))
	 (opt-fields (ebib-get-opt-fields entry-type)))
    (funcall fn (format "%-19s %s\n" "type" entry-type))
    (mapc #'(lambda (fields)
	      (funcall fn "\n")
	      (mapcar #'(lambda (field)
			  (unless (and (get field 'ebib-hidden)
				       ebib-hide-hidden-fields)
			    (funcall fn (format "%-17s " field))
			    (funcall fn (or
					 (ebib-get-field-highlighted field entry match-str)
					 ""))
			    (funcall fn "\n")))
		      fields))
	  (list obl-fields opt-fields ebib-additional-fields))))

(defun ebib-fill-entry-buffer (&optional match-str)
  "Fills the entry buffer with the fields of the current entry.
MATCH-STRING is a regexp that will be highlighted when it occurs in the
field contents."
  (set-buffer ebib-entry-buffer)
  (with-buffer-writable
    (erase-buffer)
    (when (and ebib-cur-db ; do we have a database?
	       (edb-keys-list ebib-cur-db) ; does it contain entries?
	       (gethash (car (edb-cur-entry ebib-cur-db))
			(edb-database ebib-cur-db))) ; does the current entry exist?
      (ebib-format-fields (gethash (car (edb-cur-entry ebib-cur-db))
				   (edb-database ebib-cur-db)) 'insert match-str)
      (setq ebib-current-field 'type*)
      (setq ebib-cur-entry-hash (ebib-retrieve-entry (ebib-cur-entry-key) ebib-cur-db))
      (goto-char (point-min))
      (ebib-set-fields-highlight))))
;;      (skip-chars-forward "^ "))))

(defun ebib-set-modified (mod &optional db)
  "Sets the modified flag of the database DB to MOD.
If DB is nil, it defaults to the current database, and the
modified flag of the index buffer is also (re)set. MOD must be
either T or NIL."
  (unless db
    (setq db ebib-cur-db))
  (setf (edb-modified db) mod)
  (when (eq db ebib-cur-db)
    (with-current-buffer ebib-index-buffer
      (set-buffer-modified-p mod))))

(defun ebib-modified-p ()
  "Checks if any of the databases in Ebib were modified.
Returns the first modified database, or NIL if none was modified."
  (let ((db (car ebib-databases)))
    (while (and db
		(not (edb-modified db)))
      (setq db (next-elem db ebib-databases)))
    db))

(defun ebib-create-new-database (&optional db)
  "Creates a new database instance and returns it.
If DB is set to a database, the new database is a copy of DB."
  (let ((new-db
	 (if (edb-p db)
	     (copy-edb db)
	   (make-edb))))
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
	    t))		      ; return t because a matching brace was found
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

(defun ebib-insert-entry (entry-key fields db &optional sort timestamp)
  "Stores the entry defined by ENTRY-KEY and FIELDS into DB.
Optional argument SORT indicates whether the KEYS-LIST must be
sorted after insertion. Default is NIL. Optional argument
TIMESTAMP indicates whether a timestamp is to be added to the
entry. Note that for a timestamp to be added, EBIB-USE-TIMESTAMP
must also be set to T."
  (when (and timestamp ebib-use-timestamp)
    (puthash 'timestamp (from-raw (format-time-string ebib-timestamp-format)) fields))
  (puthash entry-key fields (edb-database db))
  (ebib-set-modified t db)
  (setf (edb-n-entries db) (1+ (edb-n-entries db)))
  (setf (edb-keys-list db)
	(if sort
	    (sort (cons entry-key (edb-keys-list db)) 'string<)
	  (cons entry-key (edb-keys-list db)))))

(defun ebib-insert-string (abbr string db &optional sort)
  "Stores the @STRING definition defined by ABBR and STRING into DB.
Optional argument SORT indicates whether the STRINGS-LIST must be sorted
after insertion. When loading or merging a file, for example, it is more
economic to sort KEYS-LIST manually after all entries in the file have been
added."
  (puthash abbr (from-raw string) (edb-strings db))
  (ebib-set-modified t db)
  (setf (edb-strings-list db)
	(if sort
	    (sort (cons abbr (edb-strings-list db)) 'string<)
	  (cons abbr (edb-strings-list db)))))

(defun ebib-search-key-in-buffer (entry-key)
  "Searches ENTRY-KEY in the index buffer.
Moves point to the first character of the key and returns point."
  (goto-char (point-min))
  (re-search-forward (concat "^" entry-key))
  (beginning-of-line)
  (point))

;; when we sort entries, we either use string< on the entry keys, or
;; ebib-entry<, if the user has defined a sort order.

(defun ebib-entry< (x y)
  "Returns T if entry X is smaller than entry Y.
The entries are compared based on the fields listed in EBIB-SORT-ORDER. X
and Y should be keys of entries in the current database."
  (let* ((sort-list ebib-sort-order)
	 (sortstring-x (to-raw (ebib-get-sortstring x (car sort-list))))
	 (sortstring-y (to-raw (ebib-get-sortstring y (car sort-list)))))
    (while (and sort-list
		(string= sortstring-x sortstring-y))
      (setq sort-list (cdr sort-list))
      (setq sortstring-x (to-raw (ebib-get-sortstring x (car sort-list))))
      (setq sortstring-y (to-raw (ebib-get-sortstring y (car sort-list)))))
    (if (and sortstring-x sortstring-y)
	(string< sortstring-x sortstring-y)
      (string< x y))))

(defun ebib-get-sortstring (entry-key sortkey-list)
  "Returns the field value on which the entry ENTRY-KEY is to be sorted.
ENTRY-KEY must be the key of an entry in the current database. SORTKEY-LIST
is a list of fields that are considered in order for the sort value."
  (let ((sort-string nil))
    (while (and sortkey-list
		(null (setq sort-string (gethash (car sortkey-list)
						 (ebib-retrieve-entry entry-key ebib-cur-db)))))
      (setq sortkey-list (cdr sortkey-list)))
    sort-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main program execution ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun ebib (&optional key)
  "Ebib, a BibTeX database manager.
Optional argument key specifies the entry of the current database
that is to be displayed."
  (interactive)
  (if (or (equal (window-buffer) ebib-index-buffer)
	  (equal (window-buffer) ebib-entry-buffer))
      (error "Ebib already active")
    ;; we save the buffer from which ebib is called
    (setq ebib-push-buffer (current-buffer))
    ;; initialize ebib if required
    (unless ebib-initialized
      (ebib-init)
      (if ebib-preload-bib-files
	  (mapc #'(lambda (file)
		    (ebib-load-bibtex-file file))
		ebib-preload-bib-files)))
    ;; if ebib is visible, we just switch to the index buffer
    (let ((index-window (get-buffer-window ebib-index-buffer)))
      (if index-window
          (select-window index-window nil)
        (ebib-setup-windows)))
    ;; if ebib is called with an argument, we look for it
    (when key
      (let ((exists? (member key (edb-keys-list ebib-cur-db))))
	(if exists?
	    (progn
	      (setf (edb-cur-entry ebib-cur-db) exists?)
	      (set-buffer ebib-index-buffer)
	      (goto-char (point-min))
	      (re-search-forward (format "^%s " (ebib-cur-entry-key)))
	      (ebib-select-entry))
	  (message "No entry `%s' in current database " key))))))

(defun ebib-setup-windows ()
  "Creates the window configuration we want for Ebib in the
current window."
  ;; we save the current window configuration.
  (setq ebib-saved-window-config (current-window-configuration))
  (if (eq ebib-layout 'full)
      (delete-other-windows)
    (setq ebib-pre-ebib-window (selected-window))
    (let ((ebib-window (split-window (selected-window) (- (window-width) ebib-width) t)))
      (select-window ebib-window nil)))
  (let* ((index-window (selected-window))
	 (entry-window (split-window index-window ebib-index-window-size)))
    (switch-to-buffer ebib-index-buffer)
    (set-window-buffer entry-window ebib-entry-buffer)
    (unless (eq ebib-layout 'full)
      (set-window-dedicated-p index-window t)
      (set-window-dedicated-p entry-window t))))

(defun ebib-init ()
  "Initialises Ebib.
This function sets all variables to their initial values, creates the
buffers and reads the rc file."
  (setq ebib-cur-entry-hash nil
	ebib-current-field nil
	ebib-minibuf-hist nil
	ebib-saved-window-config nil)
  (put 'timestamp 'ebib-hidden t)
  (load "~/.ebibrc" t)
  (ebib-create-buffers)
  (setq ebib-index-highlight (ebib-make-highlight 1 1 ebib-index-buffer))
  (setq ebib-fields-highlight (ebib-make-highlight 1 1 ebib-entry-buffer))
  (setq ebib-strings-highlight (ebib-make-highlight 1 1 ebib-strings-buffer))
  (setq ebib-initialized t))

(defun ebib-create-buffers ()
  "Creates the buffers for Ebib."
  ;; first we create a buffer for multiline editing.  this one does *not*
  ;; have a name beginning with a space, because undo-info is normally
  ;; present in an edit buffer.
  (setq ebib-multiline-buffer (get-buffer-create "*Ebib-edit*"))
  (set-buffer ebib-multiline-buffer)
  (funcall ebib-multiline-major-mode)
  (ebib-multiline-mode t)
  ;; then we create a buffer to hold the fields of the current entry.
  (setq ebib-entry-buffer (get-buffer-create " *Ebib-entry*"))
  (set-buffer ebib-entry-buffer)
  (ebib-entry-mode)
  ;; then we create a buffer to hold the @STRING definitions
  (setq ebib-strings-buffer (get-buffer-create " *Ebib-strings*"))
  (set-buffer ebib-strings-buffer)
  (ebib-strings-mode)
  ;; the log buffer
  (setq ebib-log-buffer (get-buffer-create " *Ebib-log*"))
  (set-buffer ebib-log-buffer)
  (erase-buffer)
  (insert "Ebib log messages\n\n(Press C-v or SPACE to scroll down, M-v or `b' to scroll up, `q' to quit.)\n\n\n")
  (ebib-log-mode)
  ;; and lastly we create a buffer for the entry keys.
  (setq ebib-index-buffer (get-buffer-create " none"))
  (set-buffer ebib-index-buffer)
  (ebib-index-mode))

(defun ebib-quit ()
  "Quits Ebib.
The Ebib buffers are killed, all variables except the keymaps are set to nil."
  (interactive)
  (when (if (ebib-modified-p)
	    (yes-or-no-p "There are modified databases. Quit anyway? ")
	  (y-or-n-p "Quit Ebib? "))
    (mapc #'(lambda (buffer)
	      (kill-buffer buffer))
	  (list ebib-entry-buffer
		ebib-index-buffer
		ebib-strings-buffer
		ebib-multiline-buffer
		ebib-log-buffer))
    (setq ebib-databases nil
	  ebib-index-buffer nil
	  ebib-entry-buffer nil
	  ebib-initialized nil
	  ebib-index-highlight nil
	  ebib-fields-highlight nil
	  ebib-strings-highlight nil
	  ebib-export-filename nil
	  ebib-pre-ebib-window nil)
    (set-window-configuration ebib-saved-window-config)
    (message "")))

(defun ebib-kill-emacs-query-function ()
  "Ask if the user wants to save the database loaded in Ebib when Emacs is
killed and the database has been modified."
  (if (not (ebib-modified-p))
      t
    (if (y-or-n-p "Save all unsaved Ebib databases? ")
	(progn
	  (ebib-save-all-databases)
	  t)
      (yes-or-no-p "Ebib database was modified. Kill anyway? "))))

(add-hook 'kill-emacs-query-functions 'ebib-kill-emacs-query-function)

;;;;;;;;;;;;;;;;
;; index-mode ;;
;;;;;;;;;;;;;;;;

(eval-and-compile
  (define-prefix-command 'ebib-prefix-map)
  (suppress-keymap ebib-prefix-map)
  (defvar ebib-prefixed-functions '(ebib-delete-entry
				    ebib-latex-entries
				    ebib-mark-entry
				    ebib-print-entries
				    ebib-push-bibtex-key
				    ebib-export-entry)))

;; macro to redefine key bindings.

(defmacro ebib-key (buffer key &optional command)
  (cond
   ((eq buffer 'index)
    (let ((one `(define-key ebib-index-mode-map ,key (quote ,command)))
	  (two (when (or (null command)
			 (member command ebib-prefixed-functions))
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
	     '((?q . ebib-quit-multiline-edit)
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
(ebib-key index [return] ebib-select-entry)
(ebib-key index " " ebib-index-scroll-up)
(ebib-key index "/" ebib-search)
(ebib-key index "&" ebib-virtual-db-and)
(ebib-key index "|" ebib-virtual-db-or)
(ebib-key index "~" ebib-virtual-db-not)
(ebib-key index ";" ebib-prefix-map)
(ebib-key index "a" ebib-add-entry)
(ebib-key index "b" ebib-index-scroll-down)
(ebib-key index "c" ebib-close-database)
(ebib-key index "d" ebib-delete-entry)
(ebib-key index "e" ebib-edit-entry)
(ebib-key index "E" ebib-edit-keyname)
(ebib-key index "f" ebib-view-file)
(ebib-key index "F" ebib-follow-crossref)
(ebib-key index "g" ebib-goto-first-entry)
(ebib-key index "G" ebib-goto-last-entry)
(ebib-key index "h" ebib-index-help)
(ebib-key index "j" ebib-next-entry)
(ebib-key index "J" ebib-switch-to-database)
(ebib-key index "k" ebib-prev-entry)
(ebib-key index "l" ebib-show-log)
(ebib-key index "m" ebib-mark-entry)
(ebib-key index "n" ebib-search-next)
(ebib-key index "N" ebib-search-crossref)
(ebib-key index [(control n)] ebib-next-entry)
(ebib-key index [(meta n)] ebib-index-scroll-up)
(ebib-key index "o" ebib-load-bibtex-file)
(ebib-key index "p" ebib-push-bibtex-key)
(ebib-key index [(control p)] ebib-prev-entry)
(ebib-key index [(meta p)] ebib-index-scroll-down)
(ebib-key index "P" ebib-edit-preamble)
(ebib-key index "q" ebib-quit)
(ebib-key index "s" ebib-save-current-database)
(ebib-key index "S" ebib-edit-strings)
(ebib-key index "u" ebib-browse-url)
(ebib-key index "V" ebib-print-filter)
(ebib-key index "x" ebib-export-entry)
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
  (setq truncate-lines t))

(easy-menu-define ebib-index-menu ebib-index-mode-map "Ebib index menu"
  '("Ebib"
    ["Open Database..." ebib-load-bibtex-file t]
    ["Merge Database..." ebib-merge-bibtex-file (and ebib-cur-db (not (edb-virtual ebib-cur-db)))]
    ["Save Database" ebib-save-current-database (and ebib-cur-db
						     (edb-modified ebib-cur-db))]
    ["Save All Databases" ebib-save-all-databases (ebib-modified-p)]
    ["Save Database As..." ebib-write-database ebib-cur-db]
    ["Close Database" ebib-close-database ebib-cur-db]
    "--"
    ("Entry"
     ["Add" ebib-add-entry (and ebib-cur-db (not (edb-virtual ebib-cur-db)))]
     ["Edit" ebib-edit-entry (and ebib-cur-db
				  (edb-cur-entry ebib-cur-db)
				  (not (edb-virtual ebib-cur-db)))]
     ["Delete" ebib-delete-entry (and ebib-cur-db
				      (edb-cur-entry ebib-cur-db)
				      (not (edb-virtual ebib-cur-db)))])
    ["Edit Strings" ebib-edit-strings (and ebib-cur-db (not (edb-virtual ebib-cur-db)))]
    ["Edit Preamble" ebib-edit-preamble (and ebib-cur-db (not (edb-virtual ebib-cur-db)))]
    "--"
    ["Open URL" ebib-browse-url (gethash ebib-standard-url-field ebib-cur-entry-hash)]
    ["View File" ebib-view-file (gethash ebib-standard-file-field ebib-cur-entry-hash)]
    ("Print Entries"
     ["As Bibliography" ebib-latex-entries (and ebib-cur-db (not (edb-virtual ebib-cur-db)))]
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

(defun ebib-fill-index-buffer ()
  "Fills the index buffer with the list of keys in EBIB-CUR-DB.
If EBIB-CUR-DB is nil, the buffer is just erased and its name set
to \"none\"."
  (set-buffer ebib-index-buffer)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (if ebib-cur-db
	(progn
	  ;; we may call this function when there are no entries in the
	  ;; database. if so, we don't need to do this:
	  (when (edb-cur-entry ebib-cur-db)
	    (mapc #'(lambda (entry)
		      (ebib-display-entry entry)
		      (when (member entry (edb-marked-entries ebib-cur-db))
			(save-excursion
			  (let ((beg (progn
				       (beginning-of-line)
				       (point))))
			    (skip-chars-forward "^ ")
			    (add-text-properties beg (point) '(face ebib-marked-face))))))
		  (edb-keys-list ebib-cur-db))
	    (goto-char (point-min))
	    (re-search-forward (format "^%s " (ebib-cur-entry-key)))
	    (beginning-of-line)
	    (ebib-set-index-highlight))
	  (set-buffer-modified-p (edb-modified ebib-cur-db))
	  (rename-buffer (concat (format " %d:" (1+ (- (length ebib-databases)
						       (length (member ebib-cur-db ebib-databases)))))
				 (edb-name ebib-cur-db))))
      (rename-buffer " none"))))

(defun ebib-customize ()
  "Switches to Ebib's customisation group."
  (interactive)
  (ebib-lower)
  (customize-group 'ebib))

(defun ebib-customize-entry-types ()
  "Customizes EBIB-ENTRY-TYPES."
  (interactive)
  (ebib-lower)
  (customize-variable 'ebib-entry-types))

(defun ebib-log (type format-string &rest args)
  "Writes a message to Ebib's log buffer.
TYPE (a symbol) is the type of message. It can be LOG, which
writes the message to the log buffer only; MESSAGE, which writes
the message to the log buffer and outputs it with the function
MESSAGE; WARNING, which logs the message and sets the variable
EBIB-LOG-ERROR to 0; or ERROR, which logs the message and sets
the variable EBIB-LOG-ERROR to 1. The latter two can be used to
signal the user to check the log for warnings or errors."
  (with-current-buffer ebib-log-buffer
    (cond
     ((eq type 'warning)
      (or ebib-log-error ; if ebib-error-log is already set to 1, we don't want to overwrite it!
	  (setq ebib-log-error 0)))
     ((eq type 'error)
      (setq ebib-log-error 1))
     ((eq type 'message)
      (apply 'message format-string args)))
    (insert (apply 'format  (concat format-string "\n") args))))

(defun ebib-load-bibtex-file (&optional file)
  "Loads a BibTeX file into Ebib."
  (interactive)
  (unless file
    (setq file (ensure-extension (read-file-name "File to open: " "~/") "bib")))
  (setq ebib-cur-db (ebib-create-new-database))
  (setf (edb-filename ebib-cur-db) (expand-file-name file))
  (setf (edb-name ebib-cur-db) (file-name-nondirectory (edb-filename ebib-cur-db)))
  (setq ebib-log-error nil) ; we haven't found any errors
  (ebib-log 'log "%s: Opening file %s" (format-time-string "%d %b %Y, %H:%M:%S") (edb-filename ebib-cur-db))
  ;; first, we empty the buffers
  (ebib-erase-buffer ebib-index-buffer)
  (ebib-erase-buffer ebib-entry-buffer)
  (if (file-readable-p file)
      ;; if the user entered the name of an existing file, we load it
      ;; by putting it in a buffer and then parsing it.
      (with-temp-buffer
	(with-syntax-table ebib-syntax-table
	  (insert-file-contents file)
	  ;; if the user makes any changes, we'll want to create a back-up.
	  (setf (edb-make-backup ebib-cur-db) t)
	  (let ((result (ebib-find-bibtex-entries nil)))
	    (setf (edb-n-entries ebib-cur-db) (car result))
	    (when (edb-keys-list ebib-cur-db)
	      (setf (edb-keys-list ebib-cur-db) (sort (edb-keys-list ebib-cur-db) 'string<)))
	    (when (edb-strings-list ebib-cur-db)
	      (setf (edb-strings-list ebib-cur-db) (sort (edb-strings-list ebib-cur-db) 'string<)))
	    (setf (edb-cur-entry ebib-cur-db) (edb-keys-list ebib-cur-db))
	    ;; and fill the buffers. note that filling a buffer also makes
	    ;; that buffer active. therefore we do EBIB-FILL-INDEX-BUFFER
	    ;; later.
	    (ebib-set-modified nil)
	    (ebib-fill-entry-buffer)
	    ;; and now we tell the user the result
	    (ebib-log 'message "%d entries, %d @STRINGs and %s @PREAMBLE found in file."
		      (car result)
		      (cadr result)
		      (if (caddr result)
			  "a"
			"no")))))
    ;; if the file does not exist, we need to issue a message.
    (ebib-log 'message "(New file)"))
  ;; what we have to do in *any* case, is fill the index buffer. (this
  ;; even works if there are no keys in the database, e.g. when the
  ;; user opened a new file or if no BibTeX entries were found.
  (ebib-fill-index-buffer)
  (when ebib-log-error
    (message "%s found! Press `l' to check Ebib log buffer." (nth ebib-log-error '("Warnings" "Errors"))))
  (ebib-log 'log "\n\f\n"))

(defun ebib-merge-bibtex-file ()
  "Merges a BibTeX file into the database."
  (interactive)
  (unless (edb-virtual ebib-cur-db)
    (if (not ebib-cur-db)
	(error "No database loaded. Use `o' to open a database")
      (let ((file (read-file-name "File to merge: ")))
	(setq ebib-log-error nil)	; we haven't found any errors
	(ebib-log 'log "%s: Merging file %s" (format-time-string "%d-%b-%Y: %H:%M:%S") (edb-filename ebib-cur-db))
	(with-temp-buffer
	  (with-syntax-table ebib-syntax-table
	    (insert-file-contents file)
	    (let ((n (ebib-find-bibtex-entries t)))
	      (setf (edb-keys-list ebib-cur-db) (sort (edb-keys-list ebib-cur-db) 'string<))
	      (setf (edb-n-entries ebib-cur-db) (length (edb-keys-list ebib-cur-db)))
	      (when (edb-strings-list ebib-cur-db)
		(setf (edb-strings-list ebib-cur-db) (sort (edb-strings-list ebib-cur-db) 'string<)))
	      (setf (edb-cur-entry ebib-cur-db) (edb-keys-list ebib-cur-db))
	      (ebib-fill-entry-buffer)
	      (ebib-fill-index-buffer)
	      (ebib-set-modified t)
	      (ebib-log 'message "%d entries, %d @STRINGs and %s @PREAMBLE found in file."
			(car n)
			(cadr n)
			(if (caddr n)
			    "a"
			  "no"))
	      (when ebib-log-error
		(message "%s found! Press `l' to check Ebib log buffer." (nth ebib-log-error '("Warnings" "Errors"))))
	      (ebib-log 'log "\n\f\n"))))))))

(defun ebib-find-bibtex-entries (timestamp)
  "Finds the BibTeX entries in the current buffer.
The search is started at the beginnig of the buffer. All entries
found are stored in the hash table DATABASE of
EBIB-CUR-DB. Returns a three-element list: the first element is
the number of entries found, the second the number of @STRING
definitions, and the third is T or NIL, indicating whether a
@PREAMBLE was found.

TIMESTAMP indicates whether a timestamp is to be added to each
entry. Note that a timestamp is only added if EBIB-USE-TIMESTAMP
is set to T."
  (let ((n-entries 0)
	(n-strings 0)
	(preamble nil))
    (goto-char (point-min))
    (while (re-search-forward "^@" nil t) ; find the next entry
      (let ((beg (point)))
	(when (looking-at-goto-end (concat "\\(" ebib-bibtex-identifier "\\)[[:space:]]*[\(\{]") 1)
	  (let ((entry-type (downcase (buffer-substring-no-properties beg (point)))))
            (looking-at-goto-end "[[:space:]]*[\(\{]")
	    (cond
	     ((equal entry-type "string") ; string and preamble must be treated differently
	      (if (ebib-read-string)
		  (setq n-strings (1+ n-strings))))
	     ((equal entry-type "preamble")
	      (when (ebib-read-preamble)
		(setq preamble t)))
	     ((equal entry-type "comment") ; ignore comments
	      (ebib-log 'log "Comment at line %d ignored" (line-number-at-pos))
	      (ebib-match-paren-forward (point-max)))
	     ((assoc (intern-soft entry-type) ebib-entry-types) ; if the entry type has been defined
	      (if (ebib-read-entry entry-type timestamp)
		  (setq n-entries (1+ n-entries))))
	     ;; anything else we report as an unknown entry type.
	     (t (ebib-log 'warning "Line %d: Unknown entry type `%s'. Skipping." (line-number-at-pos) entry-type)
		(ebib-match-paren-forward (point-max))))))))
  (list n-entries n-strings preamble)))

(defun ebib-read-string ()
  "Reads the @STRING definition beginning at the line POINT is on.
If a proper abbreviation and string are found, they are stored in the
database. Returns the string if one was read, nil otherwise."
  (let ((limit (save-excursion	     ; we find the matching end parenthesis
		 (backward-char)
		 (ebib-match-paren-forward (point-max))
		 (point))))
    (skip-chars-forward "\"#%'(),={} \n\t\f" limit)
    (let ((beg (point)))
      (when (looking-at-goto-end (concat "\\(" ebib-bibtex-identifier "\\)[ \t\n\f]*=") 1)
	(if-str (abbr (buffer-substring-no-properties beg (point)))
	    (progn
	      (skip-chars-forward "^\"{" limit)
	      (let ((beg (point)))
		(if-str (string  (if (ebib-match-delim-forward limit)
				    (buffer-substring-no-properties beg (1+ (point)))
				  nil))
		    (if (member abbr (edb-strings-list ebib-cur-db))
			(ebib-log 'warning (format "Line %d: @STRING definition `%s' duplicated. Skipping."
						   (line-number-at-pos) abbr))
		      (ebib-insert-string abbr string ebib-cur-db))))))))))

(defun ebib-read-preamble ()
  "Reads the @PREAMBLE definition and stores it in EBIB-PREAMBLE.
If there was already another @PREAMBLE definition, the new one is
added to the existing one with a hash sign `#' between them."
  (let ((beg (point)))
    (forward-char -1)
    (when (ebib-match-paren-forward (point-max))
      (let ((text (buffer-substring-no-properties beg (point))))
	(if (edb-preamble ebib-cur-db)
	    (setf (edb-preamble ebib-cur-db) (concat (edb-preamble ebib-cur-db) "\n# " text))
	  (setf (edb-preamble ebib-cur-db) text))))))

(defun ebib-read-entry (entry-type &optional timestamp)
  "Reads a BibTeX entry and stores it in DATABASE of EBIB-CUR-DB.
Returns the new EBIB-KEYS-LIST if an entry was found, nil
otherwise. Optional argument TIMESTAMP indicates whether a
timestamp is to be added. (Whether a timestamp is actually added,
also depends on EBIB-USE-TIMESTAMP.)"
  (let ((entry-limit (save-excursion
		       (backward-char)
		       (ebib-match-paren-forward (point-max))
		       (point)))
	(beg (progn
	       (skip-chars-forward " \n\t\f") ; note the space!
	       (point))))
    (when (looking-at-goto-end (concat "\\("
				       ebib-bibtex-identifier
				       "\\)[ \t\n\f]*,")
			       1)	; this delimits the entry key
      (let ((entry-key (buffer-substring-no-properties beg (point))))
	(if (member entry-key (edb-keys-list ebib-cur-db))
	    (ebib-log 'warning "Line %d: Entry `%s' duplicated. Skipping." (line-number-at-pos) entry-key)
	  (let ((fields (ebib-find-bibtex-fields (intern-soft entry-type) entry-limit)))
	    (when fields	     ; if fields were found, we store them, and return T.
	      (ebib-insert-entry entry-key fields ebib-cur-db nil timestamp)
	      t)))))))

(defun ebib-find-bibtex-fields (entry-type limit)
  "Finds the fields of the BibTeX entry that starts on the line POINT is on.
Returns a hash table containing all the fields and values, or NIL
if none were found. ENTRY-TYPE is the type of the entry, which
will be recorded in the hash table. Before the search starts,
POINT is moved back to the beginning of the line."
  (beginning-of-line)
  ;; we assign a function to fn in order to avoid putting the test on
  ;; ebib-allow-identical-fields in the while loop, where it would get
  ;; tested with every field being read.
  (let ((fn (if (not ebib-allow-identical-fields)
		(symbol-function 'puthash)
	      #'(lambda (field-type field-contents fields)
		  (let ((existing-contents (gethash field-type fields)))
		    (puthash field-type (if existing-contents
					    (from-raw (concat (to-raw existing-contents)
							      "; "
							      (to-raw field-contents)))
					  field-contents)
			     fields))))))
    (let ((fields (make-hash-table :size 15)))
      (while (progn
	       (skip-chars-forward "^," limit) ; we must move to the next comma,
	       (eq (char-after) ?,)) ; and make sure we are really on a comma.
	(skip-chars-forward "\"#%'(),={} \n\t\f" limit)
	(let ((beg (point)))
	  (when (looking-at-goto-end (concat "\\(" ebib-bibtex-identifier "\\)[ \t\n\f]*=") 1)
	    (let ((field-type (intern (downcase (buffer-substring-no-properties beg (point))))))
	      (unless (eq field-type 'type*) ; the 'type*' key holds the entry type, so we can't use it
		(let ((field-contents (ebib-get-field-contents limit)))
		  (when field-contents
		    (funcall fn field-type field-contents fields))))))))
      (when (> (hash-table-count fields) 0)
	(puthash 'type* entry-type fields)
	fields))))

(defun ebib-get-field-contents (limit)
  "Gets the contents of a BibTeX field.
LIMIT indicates the end of the entry, beyond which the function will not
search."
  (skip-chars-forward "#%'(),=} \n\t\f" limit)
  (let ((beg (point)))
    (buffer-substring-no-properties beg (ebib-find-end-of-field limit))))

(defun ebib-find-end-of-field (limit)
  "Moves POINT to the end of a field's contents and returns POINT.
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
If optional argument SOFT is non-nil, just switch to a non-Ebib
buffer if Ebib is not occupying the entire frame."
  (interactive)
  (unless (member (window-buffer) (list ebib-index-buffer
					ebib-entry-buffer
					ebib-strings-buffer
					ebib-multiline-buffer
					ebib-log-buffer))
    (error "Ebib is not active "))
  (if (and soft
	   (not (eq ebib-layout 'full)))
      (select-window ebib-pre-ebib-window nil)
    (set-window-configuration ebib-saved-window-config))
  (mapc #'(lambda (buffer)
	    (bury-buffer buffer))
	(list ebib-index-buffer
	      ebib-entry-buffer
	      ebib-strings-buffer
	      ebib-multiline-buffer
	      ebib-log-buffer)))

(defun ebib-prev-entry ()
  "Moves to the previous BibTeX entry."
  (interactive)
  (ebib-execute-when
    ((entries)
     ;; if the current entry is the first entry,
     (if (eq (edb-cur-entry ebib-cur-db) (edb-keys-list ebib-cur-db))
	 (beep)				; just beep.
       (setf (edb-cur-entry ebib-cur-db) (last (edb-keys-list ebib-cur-db)
					       (1+ (length (edb-cur-entry ebib-cur-db)))))
       (goto-char (ebib-highlight-start ebib-index-highlight))
       (forward-line -1)
       (ebib-set-index-highlight)
       (ebib-fill-entry-buffer)))
    ((default)
     (beep))))

(defun ebib-next-entry ()
  "Moves to the next BibTeX entry."
  (interactive)
  (ebib-execute-when
    ((entries)
     (if (= (length (edb-cur-entry ebib-cur-db)) 1) ; if we're on the last entry,
	 (beep)					    ; just beep.
       (setf (edb-cur-entry ebib-cur-db)
	     (last (edb-keys-list ebib-cur-db) (1- (length (edb-cur-entry ebib-cur-db)))))
       (goto-char (ebib-highlight-start ebib-index-highlight))
       (forward-line 1)
       (ebib-set-index-highlight)
       (ebib-fill-entry-buffer)))
    ((default)
     (beep))))

(defun ebib-add-entry ()
  "Adds a new entry to the database."
  (interactive)
  (ebib-execute-when
    ((real-db)
     (if-str (entry-key (read-string "New entry key: "))
	 (progn
	   (if (member entry-key (edb-keys-list ebib-cur-db))
	       (error "Key already exists")
	     (set-buffer ebib-index-buffer)
	     (sort-in-buffer (1+ (edb-n-entries ebib-cur-db)) entry-key)
	     ;; we create the hash table *before* the call to
	     ;; ebib-display-entry, because that function refers to the
	     ;; hash table if ebib-index-display-fields is set.
	     (let ((fields (make-hash-table)))
	       (puthash 'type* ebib-default-type fields)
	       (ebib-insert-entry entry-key fields ebib-cur-db t t))
	     (with-buffer-writable
	       (ebib-display-entry entry-key))
	     (forward-line -1) ; move one line up to position the cursor on the new entry.
	     (ebib-set-index-highlight)
	     (setf (edb-cur-entry ebib-cur-db) (member entry-key (edb-keys-list ebib-cur-db)))
	     (ebib-fill-entry-buffer)
	     (ebib-edit-entry)
	     (ebib-set-modified t)))))
    ((no-database)
     (error "No database open. Use `o' to open a database first"))
    ((default)
     (beep))))

(defun ebib-close-database ()
  "Closes the current BibTeX database."
  (interactive)
  (ebib-execute-when
    ((database)
     (when (if (edb-modified ebib-cur-db)
	       (yes-or-no-p "Database modified. Close it anyway? ")
	     (y-or-n-p "Close database? "))
       (let ((to-be-deleted ebib-cur-db)
	     (new-db (next-elem ebib-cur-db ebib-databases)))
	 (setq ebib-databases (delete to-be-deleted ebib-databases))
	 (if ebib-databases	; do we still have another database loaded?
	     (progn
	       (setq ebib-cur-db (or new-db
				     (last1 ebib-databases)))
	       (unless (edb-cur-entry ebib-cur-db)
		 (setf (edb-cur-entry ebib-cur-db) (edb-keys-list ebib-cur-db)))
	       (ebib-fill-entry-buffer)
	       (ebib-fill-index-buffer))
	   ;; otherwise, we have to clean up a little and empty all the buffers.
	   (setq ebib-cur-db nil)
	   (mapc #'(lambda (buf) ; this is just to avoid typing almost the same thing three times...
		     (set-buffer (car buf))
		     (with-buffer-writable
		       (erase-buffer))
		     (ebib-delete-highlight (cadr buf)))
		 (list (list ebib-entry-buffer ebib-fields-highlight)
		       (list ebib-index-buffer ebib-index-highlight)
		       (list ebib-strings-buffer ebib-strings-highlight)))
	   ;; multiline edit buffer
	   (set-buffer ebib-multiline-buffer)
	   (with-buffer-writable
	     (erase-buffer))
	   (set-buffer ebib-index-buffer)
	   (rename-buffer " none"))
	 (message "Database closed."))))))

(defun ebib-goto-first-entry ()
  "Moves to the first BibTeX entry in the database."
  (interactive)
  (ebib-execute-when
    ((entries)
     (setf (edb-cur-entry ebib-cur-db) (edb-keys-list ebib-cur-db))
     (set-buffer ebib-index-buffer)
     (goto-char (point-min))
     (ebib-set-index-highlight)
     (ebib-fill-entry-buffer))
    ((default)
     (beep))))

(defun ebib-goto-last-entry ()
  "Moves to the last entry in the BibTeX database."
  (interactive)
  (ebib-execute-when
    ((entries)
     (setf (edb-cur-entry ebib-cur-db) (last (edb-keys-list ebib-cur-db)))
     (set-buffer ebib-index-buffer)
     (goto-char (point-min))
     (forward-line (1- (edb-n-entries ebib-cur-db)))
     (ebib-set-index-highlight)
     (ebib-fill-entry-buffer))
    ((default)
     (beep))))

(defun ebib-edit-entry ()
  "Edits the current BibTeX entry."
  (interactive)
  (ebib-execute-when
    ((real-db entries)
     (setq ebib-cur-entry-fields (ebib-get-all-fields (gethash 'type* ebib-cur-entry-hash)))
     (select-window (get-buffer-window ebib-entry-buffer) nil))
    ((default)
     (beep))))

(defun ebib-edit-keyname ()
  "Changes the key of a BibTeX entry."
  (interactive)
  (ebib-execute-when
    ((real-db entries)
     (let ((cur-keyname (ebib-cur-entry-key)))
       (if-str (new-keyname (read-string (format "Change `%s' to: " cur-keyname)
					 cur-keyname))
	   (if (member new-keyname (edb-keys-list ebib-cur-db))
	       (error (format "Key `%s' already exists" new-keyname))
	     (unless (string= cur-keyname new-keyname)
	       (let ((fields (ebib-retrieve-entry cur-keyname ebib-cur-db))
		     (marked (member cur-keyname (edb-marked-entries ebib-cur-db))))
		 (ebib-remove-entry-from-db cur-keyname ebib-cur-db)
		 (ebib-remove-key-from-buffer cur-keyname)
		 (ebib-insert-entry new-keyname fields ebib-cur-db t nil)
		 (setf (edb-cur-entry ebib-cur-db) (member new-keyname (edb-keys-list ebib-cur-db)))
		 (sort-in-buffer (edb-n-entries ebib-cur-db) new-keyname)
		 (with-buffer-writable
		   (ebib-display-entry new-keyname))
		 (forward-line -1) ; move one line up to position the cursor on the new entry.
		 (ebib-set-index-highlight)
		 (ebib-set-modified t)
		 (when marked (ebib-mark-entry))))))))
    ((default)
     (beep))))

(defun ebib-mark-entry ()
  "Marks or unmarks the current entry."
  (interactive)
  (if (ebib-called-with-prefix)
      (ebib-execute-when
	((marked-entries)
	 (setf (edb-marked-entries ebib-cur-db) nil)
	 (ebib-fill-index-buffer))
	((entries)
	 (setf (edb-marked-entries ebib-cur-db) (copy-sequence (edb-keys-list ebib-cur-db)))
	 (ebib-fill-index-buffer))
	((default)
	 (beep)))
    (ebib-execute-when
      ((entries)
       (set-buffer ebib-index-buffer)
       (with-buffer-writable
	 (if (member (ebib-cur-entry-key) (edb-marked-entries ebib-cur-db))
	     (progn
	       (setf (edb-marked-entries ebib-cur-db)
		     (delete (ebib-cur-entry-key) (edb-marked-entries ebib-cur-db)))
	       (remove-text-properties (ebib-highlight-start ebib-index-highlight)
				       (ebib-highlight-end ebib-index-highlight)
				       '(face ebib-marked-face)))
	   (setf (edb-marked-entries ebib-cur-db) (sort (cons (ebib-cur-entry-key)
							      (edb-marked-entries ebib-cur-db))
							'string<))
	   (add-text-properties (ebib-highlight-start ebib-index-highlight)
				(ebib-highlight-end ebib-index-highlight)
				'(face ebib-marked-face)))))
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
  "Formats entry KEY from database DB into the current buffer in BibTeX format.
If TIMESTAMP is T, a timestamp is added to the entry if
EBIB-USE-TIMESTAMP is T."
  (let ((entry (ebib-retrieve-entry key db)))
    (when entry
      (insert (format "@%s{%s,\n" (gethash 'type* entry) key))
      (maphash #'(lambda (key value)
		   (unless (or (eq key 'type*)
			       (and (eq key 'timestamp) timestamp ebib-use-timestamp))
		     (insert (format "\t%s = %s,\n" key value))))
	       entry)
      (if (and timestamp ebib-use-timestamp)
	  (insert (format "\ttimestamp = {%s}" (format-time-string ebib-timestamp-format)))
	(delete-char -2))		; the final ",\n" must be deleted
      (insert "\n}\n\n"))))

(defun ebib-format-strings (db)
  "Formats the @STRING commands in database DB."
  (maphash #'(lambda (key value)
	       (insert (format "@STRING{%s = %s}\n" key value)))
	   (edb-strings db))
  (insert "\n"))

(defun ebib-compare-xrefs (x y)
  (gethash 'crossref (ebib-retrieve-entry x ebib-cur-db)))

(defun ebib-format-database (db)
  "Writes database DB into the current buffer in BibTeX format."
  (when (edb-preamble db)
    (insert (format "@PREAMBLE{%s}\n\n" (edb-preamble db))))
  (ebib-format-strings db)
  (let ((sorted-list (copy-tree (edb-keys-list db))))
    (cond
     (ebib-save-xrefs-first
      (setq sorted-list (sort sorted-list 'ebib-compare-xrefs)))
     (ebib-sort-order
      (setq sorted-list (sort sorted-list 'ebib-entry<))))
    (mapc #'(lambda (key) (ebib-format-entry key db nil)) sorted-list)))

(defun ebib-save-database (db)
  "Saves the database DB."
  (ebib-execute-when
    ((real-db)
     (when (and (edb-make-backup db)
		(file-exists-p (edb-filename db)))
       (rename-file (edb-filename db) (concat (edb-filename db) "~") t)
       (setf (edb-make-backup db) nil))
     (with-temp-buffer
       (ebib-format-database db)
       (write-region (point-min) (point-max) (edb-filename db)))
     (ebib-set-modified nil db))))

(defun ebib-write-database ()
  "Writes the current database to a different file.
Can also be used to change a virtual database into a real one."
  (interactive)
  (ebib-execute-when
    ((database)
     (if-str (new-filename (read-file-name "Save to file: " "~/"))
	 (progn
	   (with-temp-buffer
	     (ebib-format-database ebib-cur-db)
	     (safe-write-region (point-min) (point-max) new-filename nil nil nil t))
	   ;; if SAFE-WRITE-REGION was cancelled by the user because he
	   ;; didn't want to overwrite an already existing file with his
	   ;; new database, it throws an error, so the next lines will not
	   ;; be executed. hence we can safely set (EDB-FILENAME DB) and
	   ;; (EDB-NAME DB).
	   (setf (edb-filename ebib-cur-db) new-filename)
	   (setf (edb-name ebib-cur-db) (file-name-nondirectory new-filename))
	   (rename-buffer (concat (format " %d:" (1+ (- (length ebib-databases)
							(length (member ebib-cur-db ebib-databases)))))
				  (edb-name ebib-cur-db)))
	   (ebib-execute-when
	     ((virtual-db)
	      (setf (edb-virtual ebib-cur-db) nil)
	      (setf (edb-database ebib-cur-db)
		    (let ((new-db (make-hash-table :test 'equal)))
		      (mapc #'(lambda (key)
				(let ((entry (gethash key (edb-database ebib-cur-db))))
				  (when entry
				    (puthash key (copy-hash-table entry) new-db))))
			    (edb-keys-list ebib-cur-db))
		      new-db))))
	   (ebib-set-modified nil))))
    ((default)
     (beep))))

(defun ebib-save-current-database ()
  "Saves the current database."
  (interactive)
  (ebib-execute-when
    ((real-db)
      (if (not (edb-modified ebib-cur-db))
	  (message "No changes need to be saved.")
	(ebib-save-database ebib-cur-db)))
    ((virtual-db)
     (error "Cannot save a virtual database. Use `w' to write to a file."))))

(defun ebib-save-all-databases ()
  "Saves all currently open databases if they were modified."
  (interactive)
  (ebib-execute-when
    ((database)
     (mapc #'(lambda (db)
	       (when (edb-modified db)
		 (ebib-save-database db)))
	   ebib-databases)
     (message "All databases saved."))))

(defun ebib-print-filename ()
  "Displays the filename of the current database in the minibuffer."
  (interactive)
  (message (edb-filename ebib-cur-db)))

(defun ebib-follow-crossref ()
  "Goes to the entry mentioned in the crossref field of the current entry."
  (interactive)
  (let ((new-cur-entry (to-raw (gethash	'crossref
					(ebib-retrieve-entry (ebib-cur-entry-key) ebib-cur-db)))))
    (setf (edb-cur-entry ebib-cur-db)
          (or (member new-cur-entry (edb-keys-list ebib-cur-db))
              (edb-cur-entry ebib-cur-db))))
  (ebib-fill-entry-buffer)
  (ebib-fill-index-buffer))

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
  (if (ebib-called-with-prefix)
      (ebib-execute-when
	((real-db marked-entries)
	 (when (y-or-n-p "Delete all marked entries? ")
	   (mapc #'(lambda (entry)
		     (ebib-remove-entry-from-db entry ebib-cur-db (not (string= entry (ebib-cur-entry-key)))))
		 (edb-marked-entries ebib-cur-db))
	   (message "Marked entries deleted.")
	   (ebib-set-modified t)
	   (ebib-fill-entry-buffer)
	   (ebib-fill-index-buffer)))
	((default)
	 (beep)))
    (ebib-execute-when
      ((real-db entries)
       (let ((cur-entry (ebib-cur-entry-key)))
	 (when (y-or-n-p (format "Delete %s? " cur-entry))
	   (ebib-remove-entry-from-db cur-entry ebib-cur-db)
	   (ebib-remove-key-from-buffer cur-entry)
	   (ebib-fill-entry-buffer)
	   (ebib-set-modified t)
	   (message (format "Entry `%s' deleted." cur-entry)))))
      ((default)
       (beep)))))

(defun ebib-remove-entry-from-db (entry-key db &optional new-cur-entry)
  "Removes ENTRY-KEY from DB.
Optional argument NEW-CUR-ENTRY is the key of the entry that is
to become the new current entry. It it is NIL, the entry after
the deleted one becomes the new current entry. If it is T, the
current entry is not changed."
  (remhash entry-key (edb-database db))
  (setf (edb-n-entries db) (1- (edb-n-entries db)))
  (cond
   ((null new-cur-entry) (setq new-cur-entry (cadr (edb-cur-entry db))))
   ((stringp new-cur-entry) t)
   (t (setq new-cur-entry (ebib-cur-entry-key))))
  (setf (edb-keys-list db) (delete entry-key (edb-keys-list db)))
  (setf (edb-marked-entries db) (delete entry-key (edb-marked-entries db)))
  (setf (edb-cur-entry db) (member new-cur-entry (edb-keys-list db)))
  (unless (edb-cur-entry db) ; if (edb-cur-entry db) is nil, we deleted the last entry.
    (setf (edb-cur-entry db) (last (edb-keys-list db)))))

(defun ebib-remove-key-from-buffer (entry-key)
  "Removes ENTRY-KEY from the index buffer and highlights the current entry."
  (with-buffer-writable
    (let ((beg (ebib-search-key-in-buffer entry-key)))
      (forward-line 1)
      (delete-region beg (point))))
  (ebib-execute-when
    ((entries)
     (ebib-search-key-in-buffer (ebib-cur-entry-key))
     (ebib-set-index-highlight))))

(defun ebib-select-entry ()
  "Makes the entry at POINT the current entry."
  (interactive)
  (ebib-execute-when
    ((entries)
     (beginning-of-line)
     (let ((beg (point)))
       (let* ((key (save-excursion
		     (skip-chars-forward "^ ")
		     (buffer-substring-no-properties beg (point))))
	      (new-cur-entry (member key (edb-keys-list ebib-cur-db))))
	 (when new-cur-entry
	   (setf (edb-cur-entry ebib-cur-db) new-cur-entry)
	   (ebib-set-index-highlight)
	   (ebib-fill-entry-buffer)))))
    ((default)
     (beep))))

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
    ((real-db entries)
     (if num
	 (ebib-export-to-db num (format "Entry `%s' copied to database %%d." (ebib-cur-entry-key))
			    #'(lambda (db)
				(let ((entry-key (ebib-cur-entry-key)))
				  (if (member entry-key (edb-keys-list db))
				      (error "Entry key `%s' already exists in database %d" entry-key num)
				    (ebib-insert-entry entry-key
						       (copy-hash-table (ebib-retrieve-entry entry-key
											     ebib-cur-db))
						       db t t)
				    ;; if this is the first entry in the target DB,
				    ;; its CUR-ENTRY must be set!
				    (when (null (edb-cur-entry db))
				      (setf (edb-cur-entry db) (edb-keys-list db)))
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
    ((real-db marked-entries)
     (if num
	 (ebib-export-to-db
	  num "Entries copied to database %d."
	  #'(lambda (db)
	      (mapc #'(lambda (entry-key)
			(if (member entry-key (edb-keys-list db))
			    (error "Entry key `%s' already exists in database %d" entry-key num)
			  (ebib-insert-entry entry-key
					     (copy-hash-table (ebib-retrieve-entry entry-key
										   ebib-cur-db))
					     db t t)))
		    (edb-marked-entries ebib-cur-db))
	      ;; if the target DB was empty before, its CUR-ENTRY must be set!
	      (when (null (edb-cur-entry db))
		(setf (edb-cur-entry db) (edb-keys-list db)))
	      t))	  ; we must return T, WHEN does not always do this.
       (ebib-export-to-file "Export to file: " "Entries exported to %s."
			    #'(lambda ()
				(mapc #'(lambda (entry-key)
					  (insert "\n")
					  (ebib-format-entry entry-key ebib-cur-db t))
				      (edb-marked-entries ebib-cur-db))))))
    ((default)
     (beep))))

(defun ebib-search ()
  "Search the current Ebib database.
The search is conducted with STRING-MATCH and can therefore be a
regexp. Searching starts with the current entry."
  (interactive)
  (ebib-execute-when
    ((entries)
     (if-str (search-str (read-string "Search database for: "))
	 (progn
	   (setq ebib-search-string search-str)
	   ;; first we search the current entry
	   (if (ebib-search-in-entry ebib-search-string
				     (ebib-retrieve-entry (ebib-cur-entry-key) ebib-cur-db))
	       (ebib-fill-entry-buffer ebib-search-string)
	     ;; if the search string wasn't found in the current entry, we continue searching.
	     (ebib-search-next)))))
    ((default)
     (beep))))

(defun ebib-search-next ()
  "Searches the next occurrence of EBIB-SEARCH-STRING.
Searching starts at the entry following the current entry. If a
match is found, the matching entry is shown and becomes the new
current entry."
  (interactive)
  (ebib-execute-when
    ((entries)
     (if (null ebib-search-string)
	 (message "No search string")
       (let ((cur-search-entry (cdr (edb-cur-entry ebib-cur-db))))
	 (while (and cur-search-entry
		     (null (ebib-search-in-entry ebib-search-string
						 (gethash (car cur-search-entry)
							  (edb-database ebib-cur-db)))))
	   (setq cur-search-entry (cdr cur-search-entry)))
	 (if (null cur-search-entry)
	     (message (format "`%s' not found" ebib-search-string))
	   (setf (edb-cur-entry ebib-cur-db) cur-search-entry)
	   (set-buffer ebib-index-buffer)
	   (goto-char (point-min))
	   (re-search-forward (format "^%s " (ebib-cur-entry-key)))
	   (beginning-of-line)
	   (ebib-set-index-highlight)
	   (ebib-fill-entry-buffer ebib-search-string)))))
    ((default)
     (beep))))

(defun ebib-search-in-entry (search-str entry &optional field)
  "Searches one entry of the ebib database.
Returns a list of fields in ENTRY that match the regexp
SEARCH-STR, or NIL if no matches were found. If FIELD is given,
only that field is searched."
  (let ((case-fold-search t)  ; we want to ensure a case-insensitive search
	(result nil))
    (if field
	(let ((value (gethash field entry)))
	  (when (and (stringp value) ; the type* field has a symbol as value
		     (string-match search-str value))
	    (setq result (list field))))
      (maphash #'(lambda (field value)
		   (when (and (stringp value) ; the type* field has a symbol as value
			      (string-match search-str value))
		     (setq result (cons field result))))
	       entry))
    result))

(defun ebib-search-crossref ()
  "Searches the database for the key of the current entry.
This is useful in searching the entries that cross-reference the
current entry."
  (interactive)
  (setq ebib-search-string (car (edb-cur-entry ebib-cur-db)))
  (ebib-search-next))

(defun ebib-edit-strings ()
  "Edits the @STRING definitions in the database."
  (interactive)
  (ebib-execute-when
    ((real-db)
     (ebib-fill-strings-buffer)
     (select-window (get-buffer-window ebib-entry-buffer) nil)
     (set-window-dedicated-p (selected-window) nil)
     (switch-to-buffer ebib-strings-buffer)
     (unless (eq ebib-layout 'full)
       (set-window-dedicated-p (selected-window) t))
     (goto-char (point-min)))
    ((default)
     (beep))))

(defun ebib-edit-preamble ()
  "Edits the @PREAMBLE definition in the database."
  (interactive)
  (ebib-execute-when
    ((real-db)
     (select-window (ebib-temp-window) nil)
     (ebib-multiline-edit 'preamble (edb-preamble ebib-cur-db)))
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
     (if (null (edb-preamble ebib-cur-db))
	 (error "No @PREAMBLE defined")
       (let ((num (ebib-prefix prefix)))
	 (if num
	     (ebib-export-to-db num "@PREAMBLE copied to database %d"
				#'(lambda (db)
				    (let ((text (edb-preamble ebib-cur-db)))
				      (if (edb-preamble db)
					  (setf (edb-preamble db) (concat (edb-preamble db) "\n# " text))
					(setf (edb-preamble db) text)))))
	   (ebib-export-to-file "Export @PREAMBLE to file: "
				"@PREAMBLE exported to %s"
				#'(lambda ()
				    (insert (format "\n@preamble{%s}\n\n" (edb-preamble ebib-cur-db)))))))))
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
			  (edb-marked-entries ebib-cur-db))
			(edb-keys-list ebib-cur-db))))
       (if-str (tempfile (if (not (string= "" ebib-print-tempfile))
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
			 (insert "\\begin{tabular}{p{0.2\\textwidth}p{0.8\\textwidth}}\n")
			 (let ((entry (ebib-retrieve-entry entry-key ebib-cur-db)))
			   (insert (format "\\multicolumn{2}{l}{\\texttt{%s (%s)}}\\\\\n"
					   entry-key (symbol-name (gethash 'type* entry))))
			   (insert "\\hline\n")
			   (mapc #'(lambda (field)
				     (if-str (value (gethash field entry))
					 (when (or (not (multiline-p value))
						   ebib-print-multiline)
					   (insert (format "%s: & %s\\\\\n"
							   field (to-raw value))))))
				 (cdr (ebib-get-all-fields (gethash 'type* entry)))))
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
     (if-str (tempfile (if (not (string= "" ebib-print-tempfile))
			   ebib-print-tempfile
			 (read-file-name "Use temp file: " "~/" nil nil)))
	 (progn
	   (with-temp-buffer
	     (insert "\\documentclass{article}\n\n")
	     (when ebib-print-preamble
	       (mapc #'(lambda (string)
			 (insert (format "%s\n" string)))
		     ebib-latex-preamble))
	     (insert "\n\\begin{document}\n\n")
	     (if (and (or (ebib-called-with-prefix)
			  (equal '(menu-bar) (elt (this-command-keys-vector) 0)))
		      (edb-marked-entries ebib-cur-db))
		 (mapc #'(lambda (entry)
			   (insert (format "\\nocite{%s}\n" entry)))
		       (edb-marked-entries ebib-cur-db))
	       (insert "\\nocite{*}\n"))
	     (insert (format "\n\\bibliography{%s}\n\n" (expand-file-name (edb-filename ebib-cur-db))))
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
	  (ebib-fill-entry-buffer)
	  (ebib-fill-index-buffer))
      (error "Database %d does not exist" num))))

(defun ebib-next-database ()
  (interactive)
  (ebib-execute-when
    ((database)
     (let ((new-db (next-elem ebib-cur-db ebib-databases)))
       (unless new-db
	 (setq new-db (car ebib-databases)))
       (setq ebib-cur-db new-db)
       (ebib-fill-entry-buffer)
       (ebib-fill-index-buffer)))))

(defun ebib-prev-database ()
  (interactive)
  (ebib-execute-when
    ((database)
     (let ((new-db (prev-elem ebib-cur-db ebib-databases)))
       (unless new-db
	 (setq new-db (last1 ebib-databases)))
       (setq ebib-cur-db new-db)
       (ebib-fill-entry-buffer)
       (ebib-fill-index-buffer)))))

(defun ebib-browse-url (num)
  "Asks a browser to load the URL in the standard URL field.
The standard URL field may contain more than one URL, if they're
whitespace-separated. In that case, a numeric prefix argument
specifies which URL to choose.

By \"standard URL field\" is meant the field defined in the
customisation variable EBIB-STANDARD-URL-FIELD. Its default value
is `url'."
  (interactive "p")
  (ebib-execute-when
    ((entries)
     (let ((url (to-raw (gethash ebib-standard-url-field
				 (ebib-retrieve-entry (ebib-cur-entry-key) ebib-cur-db)))))
       (when (not (and url
		       (ebib-call-browser url num)))
	 (error "No url found in field `%s'" ebib-standard-url-field))))
    ((default)
     (beep))))

(defun ebib-call-browser (urls n)
  "Passes the Nth url in URLS to a browser.
URLs must be a string of whitespace-separated urls."
  (let ((url (nth (1- n)
		  (let ((start 0)
			(result nil))
		    (while (string-match ebib-url-regexp urls start)
		      (add-to-list 'result (match-string 0 urls) t)
		      (setq start (match-end 0)))
		    result))))
    (cond
     ((string-match "\\\\url{\\(.*?\\)}" url)
      (setq url (match-string 1 url)))
     ((string-match ebib-url-regexp url)
      (setq url (match-string 0 url)))
     (t (setq url nil)))
    (when url
      (if (not (string= ebib-browser-command ""))
	  (start-process "Ebib-browser" nil ebib-browser-command url)
	(browse-url url)))))

(defun ebib-view-file (num)
  "Views a file in the standard file field.
The standard file field may contain more than one filename, if
they're whitespace-separated. In that case, a numeric prefix
argument specifies which file to choose.

By \"standard file field\" is meant the field defined in the
customisation variable EBIB-STANDARD-FILE-FIELD. Its default
value is `file'."
  (interactive "p")
  (ebib-execute-when
    ((entries)
     (let ((filename (to-raw (gethash ebib-standard-file-field
				      (ebib-retrieve-entry (ebib-cur-entry-key) ebib-cur-db)))))
       (when (not (and filename
		       (ebib-call-file-viewer filename num)))
	 (error "No valid filename found in field `%s'" ebib-standard-file-field))))
    ((default)
     (beep))))

(defun ebib-call-file-viewer (files n)
  "Passes the Nth file in FILES to an external viewer.
FILES must be a string of whitespace-separated filenames."
  (let* ((file (nth (1- n)
		    (let ((start 0)
			  (result nil))
		      (while (string-match ebib-file-regexp files start)
			(add-to-list 'result (match-string 0 files) t)
			(setq start (match-end 0)))
		      result)))
	 (ext (file-name-extension file)))
    (let ((file-full-path (locate-file file ebib-file-search-dirs)))
      (when file-full-path
	(if-str (viewer (cdr (assoc ext ebib-file-associations)))
	    (start-process (concat "ebib " ext " viewer process") nil viewer file-full-path)
	  (ebib-lower)
	  (find-file file-full-path))))))

(defun ebib-virtual-db-and (not)
  "Filters entries into a virtual database.
If the current database is a virtual database already, perform a
logical AND on the entries."
  (interactive "p")
  (ebib-execute-when
    ((entries)
     (ebib-filter-to-virtual-db 'and not))
    ((default)
     (beep))))

(defun ebib-virtual-db-or (not)
  "Filters entries into a virtual database.
If the current database is a virtual database already, perform a
logical OR with the entries in the original database."
  (interactive "p")
  (ebib-execute-when
    ((entries)
     (ebib-filter-to-virtual-db 'or not))
    ((default)
     (beep))))

(defun ebib-virtual-db-not ()
  "Negates the current virtual database."
  (interactive)
  (ebib-execute-when
    ((virtual-db)
     (setf (edb-virtual ebib-cur-db)
	   (if (eq (car (edb-virtual ebib-cur-db)) 'not)
	       (cadr (edb-virtual ebib-cur-db))
	     `(not ,(edb-virtual ebib-cur-db))))
     (ebib-run-filter (edb-virtual ebib-cur-db) ebib-cur-db)
     (ebib-fill-entry-buffer)
     (ebib-fill-index-buffer))
    ((default)
     (beep))))

(defun ebib-filter-to-virtual-db (bool not)
  "Filters the current database to a virtual database.
BOOL is the operator to be used, either `and' or `or'. If NOT<0,
a logical `not' is applied to the selection."
  (let ((field (completing-read (format "Filter: %s(contains <field> <regexp>)%s. Enter field: "
					(if (< not 0) "(not " "")
					(if (< not 0) ")" ""))
				(cons '("any" 0)
				      (mapcar #'(lambda (x)
						  (cons (symbol-name x) 0))
					      (append ebib-unique-field-list ebib-additional-fields)))
				nil t)))
    (setq field (intern-soft field))
    (let ((regexp (read-string (format "Filter: %s(contains %s <regexp>)%s. Enter regexp: "
					(if (< not 0) "(not " "")
					field
					(if (< not 0) ")" "")))))
      (ebib-execute-when
	((virtual-db)
	 (setf (edb-virtual ebib-cur-db) `(,bool ,(edb-virtual ebib-cur-db)
						 ,(if (>= not 0)
						      `(contains ,field ,regexp)
						    `(not (contains ,field ,regexp))))))
	((real-db)
	 (setq ebib-cur-db (ebib-create-virtual-db field regexp not))))
      (ebib-run-filter (edb-virtual ebib-cur-db) ebib-cur-db)
      (ebib-fill-entry-buffer)
      (ebib-fill-index-buffer))))

(defun ebib-create-virtual-db (field regexp not)
  "Creates a virtual database based on EBIB-CUR-DB."
  ;; a virtual database is a database whose edb-virtual field contains an
  ;; expression that selects entries. this function only sets that
  ;; expression, it does not actually filter the entries.
  (let ((new-db (ebib-create-new-database ebib-cur-db)))
    (setf (edb-virtual new-db) (if (>= not 0)
				   `(contains ,field ,regexp)
				 `(not (contains ,field ,regexp))))
    (setf (edb-filename new-db) nil)
    (setf (edb-name new-db) (concat "V:" (edb-name new-db)))
    (setf (edb-modified new-db) nil)
    (setf (edb-make-backup new-db) nil)
    new-db))

(defmacro contains (field regexp)
  ;; Note: the variable ENTRY is *not* bound in this macro! The function
  ;; calling CONTAINS *must* set ENTRY to an actual Ebib entry. The point
  ;; of this macro is to facilitate defining filters for virtual databases.
  ;; It enables us to define filters of the form:

  ;; (and (not (contains author "Chomsky")) (contains year "1995"))

  `(ebib-search-in-entry ,regexp entry ,(unless (eq field 'any) `(quote ,field))))

(defun ebib-run-filter (filter db)
  "Runs FILTER on DB"
  (setf (edb-keys-list db)
	(sort (let ((result nil))
		(maphash #'(lambda (key value)
			     (let ((entry value)) ; this is necessary for actually running the filter
			       (when (eval filter)
				 (setq result (cons key result)))))
			 (edb-database db))
		result)
	      'string<))
  (setf (edb-n-entries db) (length (edb-keys-list db)))
  (setf (edb-cur-entry db) (edb-keys-list db)))

(defun ebib-print-filter (num)
  "Displays the filter of the current virtual database.
With any prefix argument, reapplies the filter to the
database. This can be useful when the source database was
modified."
  (interactive "P")
  (ebib-execute-when
    ((virtual-db)
     (when num
       (ebib-run-filter (edb-virtual ebib-cur-db) ebib-cur-db)
       (ebib-fill-entry-buffer)
       (ebib-fill-index-buffer))
     (message "%S" (edb-virtual ebib-cur-db)))
    ((default)
     (beep))))

(defun ebib-show-log ()
  "Displays the contents of the log buffer."
  (interactive)
  (select-window (get-buffer-window ebib-entry-buffer) nil)
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer ebib-log-buffer)
  (unless (eq ebib-layout 'full)
    (set-window-dedicated-p (selected-window) t)))

(defun ebib-create-citation-command (command n-opt-args key)
  "Create a LaTeX citation command with COMMAND and KEY.
N-OPT-ARGS is the number of optional arguments COMMAND takes. A
backslash is prefixed to COMMAND if none is present (and COMMAND
is non-empty)."
  (let ((opt-args (loop for i from 1 to n-opt-args
			collect (read-from-minibuffer (format "Cite %s with optional argument %d: " key i)))))
    (while (equal (car opt-args) "") ; empty args at the beginning of the list don't need
      (setq opt-args (cdr opt-args)))	; to be included.
    (format "%s%s%s{%s}"
	    (if (or (string= command "")
		    (= (aref command 0) 92))
		"" "\\") ; add a backslash if the user didn't type one.
	    command
	    (mapconcat #'(lambda (str)
			   (format "[%s]" str))
		       opt-args "")
	    key)))

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
	   (let (insert-string)
	     (if-str (command (completing-read "Command to use: " ebib-insertion-commands
					       nil nil nil ebib-minibuf-hist))
		 (let* ((n-opt-args (or (cadr (assoc command ebib-insertion-commands))
					1))
			(allow-mult-args (caddr (assoc command ebib-insertion-commands)))
			(key (if (and called-with-prefix ; if there are marked entries...
				      (edb-marked-entries ebib-cur-db))
				 (if allow-mult-args ; and the command allows multiple separate args
				     (car (edb-marked-entries ebib-cur-db)) ; just take the first one here
				   (mapconcat #'(lambda (x) x) ; otherwise take all the marked entries
					      (edb-marked-entries ebib-cur-db)
					      ","))
			       (car (edb-cur-entry ebib-cur-db)))) ; if there are no marked entries, take the current entry
			(first (ebib-create-citation-command command n-opt-args key))
			(rest (when (and called-with-prefix ; if we have marked entries...
					 (edb-marked-entries ebib-cur-db)
					 allow-mult-args) ; and a command that allows multiple separate arguments
				(mapcar #'(lambda (key) ; we need to process the rest of the entries
					    (ebib-create-citation-command "" n-opt-args key))
					(cdr (edb-marked-entries ebib-cur-db))))))
		   (setq insert-string (apply #'concat first rest)))
	       ;; if the user did not provide a comand, we just insert the
	       ;; current entry, or the marked entries, separated by commas:
	       (setq insert-string
		     (if (and called-with-prefix
			      (edb-marked-entries ebib-cur-db))
			 (mapconcat #'(lambda (x) x) ; otherwise take all the marked entries
				    (edb-marked-entries ebib-cur-db)
				    ",")
		       (car (edb-cur-entry ebib-cur-db)))))
	     (when insert-string
	       (with-current-buffer buffer
		 (insert insert-string))
	       (message "Pushed entries to buffer %s" buffer))))))
      ((default)
       (beep)))))

(defun ebib-index-help ()
  "Shows the info node of Ebib's index buffer."
  (interactive)
  (ebib-lower)
  (info "(ebib) The Index Buffer"))

(defun ebib-info ()
  "Shows Ebib's info node."
  (interactive)
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
    (define-key map "\C-xb" 'undefined)
    (define-key map "\C-xk" 'undefined)
    (define-key map "y" 'ebib-yank-field-contents)
    map)
  "Keymap for the Ebib entry buffer.")

(define-derived-mode ebib-entry-mode
  fundamental-mode "Ebib-entry"
  "Major mode for the Ebib entry buffer."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defun ebib-quit-entry-buffer ()
  "Quits editing the entry."
  (interactive)
  (select-window (get-buffer-window ebib-index-buffer) nil))

(defun ebib-find-visible-field (field direction)
  "Finds the first visible field before or after FIELD.
If DIRECTION is negative, search the preceding fields, otherwise
search the succeeding fields. If FIELD is visible itself, return
that. If there is no preceding/following visible field, return
NIL. If EBIB-HIDE-HIDDEN-FIELDS is NIL, return FIELD."
  (when ebib-hide-hidden-fields
    (let ((fn (if (>= direction 0)
		  'next-elem
		'prev-elem)))
      (while (and field
		  (get field 'ebib-hidden))
	(setq field (funcall fn field ebib-cur-entry-fields)))))
  field)

(defun ebib-prev-field ()
  "Moves to the previous field."
  (interactive)
  (let ((new-field (ebib-find-visible-field (prev-elem ebib-current-field ebib-cur-entry-fields) -1)))
    (if (null new-field)
	(beep)
      (setq ebib-current-field new-field)
      (ebib-move-to-field ebib-current-field -1))))

(defun ebib-next-field ()
  "Moves to the next field."
  (interactive)
  (let ((new-field (ebib-find-visible-field (next-elem ebib-current-field ebib-cur-entry-fields) 1)))
    (if (null new-field)
	(when (interactive-p) ; i call this function after editing a field,
			      ; and we don't want a beep then
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
    (let ((new-field (ebib-find-visible-field (last1 ebib-cur-entry-fields) -1)))
    (if (null new-field)
	(beep)
      (setq ebib-current-field new-field)
      (ebib-move-to-field ebib-current-field 1))))

(defun ebib-goto-next-set ()
  "Moves to the next set of fields."
  (interactive)
  (cond
   ((eq ebib-current-field 'type*) (ebib-next-field))
   ((member ebib-current-field ebib-additional-fields) (ebib-goto-last-field))
   (t (let* ((entry-type (gethash 'type* ebib-cur-entry-hash))
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
	    (ebib-goto-last-field)  ; if there was no further set to go to,
				    ; go to the last field of the current set
	  (setq ebib-current-field new-field)
	  (ebib-move-to-field ebib-current-field 1))))))

(defun ebib-goto-prev-set ()
  "Moves to the previous set of fields."
  (interactive)
  (unless (eq ebib-current-field 'type*)
    (let* ((entry-type (gethash 'type* ebib-cur-entry-hash))
	   (obl-fields (ebib-get-obl-fields entry-type))
	   (opt-fields (ebib-get-opt-fields entry-type))
	   (new-field nil))
      (if (member ebib-current-field obl-fields)
	  (ebib-goto-first-field)
	(when (member ebib-current-field ebib-additional-fields)
	  (setq new-field (ebib-find-visible-field (last1 opt-fields) -1)))
	(when (or (member ebib-current-field opt-fields)
		  (null new-field))
	  (setq new-field (ebib-find-visible-field (last1 obl-fields) -1)))
	(if (null new-field)
	    (ebib-goto-first-field)
	  (setq ebib-current-field new-field)
	  (ebib-move-to-field ebib-current-field -1))))))

(defun ebib-edit-entry-type ()
  "Edits the entry type."
  ;; we don't want the completion buffer to be shown in the index window,
  ;; so we need to switch to an appropriate window first. we do this in an
  ;; unwind-protect to make sure we always get back to the entry buffer.
  (unwind-protect
      (progn
	(if (eq ebib-layout 'full)
	    (other-window 1)
	  (select-window ebib-pre-ebib-window) nil)
	(if-str (new-type (completing-read "type: " ebib-entry-types nil t))
	    (progn
	      (puthash 'type* (intern-soft new-type) ebib-cur-entry-hash)
	      (ebib-fill-entry-buffer)
	      (setq ebib-cur-entry-fields (ebib-get-all-fields (gethash 'type* ebib-cur-entry-hash)))
	      (ebib-set-modified t))))
    (select-window (get-buffer-window ebib-entry-buffer) nil)))

(defun ebib-edit-crossref ()
  "Edits the crossref field."
  ;; we don't want the completion buffer to be shown in the index window,
  ;; so we need to switch to an appropriate window first. we do this in an
  ;; unwind-protect to make sure we always get back to the entry buffer.
  (unwind-protect
      (progn
	(if (eq ebib-layout 'full)
	    (other-window 1)
	  (select-window ebib-pre-ebib-window) nil)
	(let ((collection (ebib-create-collection (edb-database ebib-cur-db))))
	  (if-str (key (completing-read "Key to insert in `crossref': " collection nil t))
	      (progn
		(puthash 'crossref (from-raw key) ebib-cur-entry-hash)
		(ebib-set-modified t)))))
    (select-window (get-buffer-window ebib-entry-buffer) nil)
    ;; we now redisplay the entire entry buffer, so that the crossref'ed
    ;; fields show up. this also puts the cursor back on the type field.
    (ebib-fill-entry-buffer)
    (setq ebib-current-field 'crossref)
    (re-search-forward "^crossref")
    (ebib-set-fields-highlight)))

;; we should modify ebib-edit-field, so that it calls the appropriate
;; helper function, which asks the user for the new value and just returns
;; that. storing it should then be done by ebib-edit-field, no matter what
;; sort of field the user edits.

(defun ebib-edit-field ()
  "Edits a field of a BibTeX entry."
  (interactive)
  (cond
   ((eq ebib-current-field 'type*) (ebib-edit-entry-type))
   ((eq ebib-current-field 'crossref) (ebib-edit-crossref))
   ((eq ebib-current-field 'annote) (ebib-edit-multiline-field))
   (t
    (let ((init-contents (gethash ebib-current-field ebib-cur-entry-hash))
	  (raw nil))
      (if (multiline-p init-contents)
	  (ebib-edit-multiline-field)
	(when init-contents
	  (if (raw-p init-contents)
	      (setq raw t)
	    (setq init-contents (to-raw init-contents))))
	(if-str (new-contents (read-string (format "%s: " (symbol-name ebib-current-field))
					   (if init-contents
					       (cons init-contents 0)
					     nil)
					   ebib-minibuf-hist))
	    (puthash ebib-current-field (if raw
					    new-contents
					  (concat "{" new-contents "}"))
		     ebib-cur-entry-hash)
	  (remhash ebib-current-field ebib-cur-entry-hash))
	(ebib-redisplay-current-field)
	;; we move to the next field, but only if ebib-edit-field was
	;; called interactively, otherwise we get a strange bug in
	;; ebib-toggle-raw...
	(if (interactive-p) (ebib-next-field))
	(ebib-set-modified t))))))

(defun ebib-browse-url-in-field (num)
  "Browses a URL in the current field.
The field may contain a whitespace-separated set of URLs. The
prefix argument indicates which URL is to be sent to the
browser."
  (interactive "p")
  (let ((urls (to-raw (gethash ebib-current-field ebib-cur-entry-hash))))
    (if (not (and urls
		  (ebib-call-browser urls num)))
	(error "No url found in field `%s'" ebib-current-field))))

(defun ebib-view-file-in-field (num)
  "Views a file in the current field.
The field may contain a whitespace-separated set of
filenames. The prefix argument indicates which file is to be
viewed."
  (interactive "p")
  (let ((files (to-raw (gethash ebib-current-field ebib-cur-entry-hash))))
    (if (not (and files
		  (ebib-call-file-viewer files num)))
	(error "No valid filename found in field `%s'" ebib-current-field))))

(defun ebib-copy-field-contents ()
  "Copies the contents of the current field to the kill ring."
  (interactive)
  (unless (eq ebib-current-field 'type*)
    (let ((contents (gethash ebib-current-field ebib-cur-entry-hash)))
      (when (stringp contents)
	(kill-new contents)
	(message "Field contents copied.")))))

(defun ebib-cut-field-contents ()
  "Kills the contents of the current field. The killed text is put in the kill ring."
  (interactive)
  (unless (eq ebib-current-field 'type*)
    (let ((contents (gethash ebib-current-field ebib-cur-entry-hash)))
      (when (stringp contents)
	(remhash ebib-current-field ebib-cur-entry-hash)
	(kill-new contents)
	(ebib-redisplay-current-field)
	(ebib-set-modified t)
	(message "Field contents killed.")))))

(defun ebib-yank-field-contents (arg)
  "Inserts the last killed text into the current field.
If the current field already has a contents, nothing is inserted,
unless the previous command was also ebib-yank-field-contents,
then the field contents is replaced with the previous yank. That
is, multiple uses of this command function like the combination
of C-y/M-y. Prefix arguments also work the same as with C-y/M-y."
  (interactive "P")
  (if (or (eq ebib-current-field 'type*) ; we cannot yank into the type* or crossref fields
	  (eq ebib-current-field 'crossref)
	  (unless (eq last-command 'ebib-yank-field-contents)
	    (gethash ebib-current-field ebib-cur-entry-hash))) ; nor into a field already filled
      (progn
	(setq this-command t)
	(beep))
    (let ((new-contents (current-kill (cond
				       ((listp arg) (if (eq last-command 'ebib-yank-field-contents)
							1
						      0))
				       ((eq arg '-) -2)
				       (t (1- arg))))))
      (when new-contents
	(puthash ebib-current-field new-contents ebib-cur-entry-hash)
	(ebib-redisplay-current-field)
	(ebib-set-modified t)))))

(defun ebib-delete-field-contents ()
  "Deletes the contents of the current field.
The deleted text is not put in the kill ring."
  (interactive)
  (if (eq ebib-current-field 'type*)
      (beep)
    (when (y-or-n-p "Delete field contents? ")
      (remhash ebib-current-field ebib-cur-entry-hash)
      (ebib-redisplay-current-field)
      (ebib-set-modified t)
      (message "Field contents deleted."))))

(defun ebib-toggle-raw ()
  "Toggles the raw status of the current field contents."
  (interactive)
  (unless (or (eq ebib-current-field 'type*)
	      (eq ebib-current-field 'crossref))
    (let ((contents (gethash ebib-current-field ebib-cur-entry-hash)))
      (if (not contents)     ; if there is no value,
	  (progn
	    (ebib-edit-field)  ; the user can enter one, which we must then make raw
	    (let ((new-contents (gethash ebib-current-field ebib-cur-entry-hash)))
	      (when new-contents
		;; note: we don't have to check for empty string, since that is
		;; already done in ebib-edit-field
		(puthash ebib-current-field (to-raw new-contents) ebib-cur-entry-hash))))
	(if (raw-p contents)
	    (puthash ebib-current-field (from-raw contents) ebib-cur-entry-hash)
	  (puthash ebib-current-field (to-raw contents) ebib-cur-entry-hash)))
      (ebib-redisplay-current-field)
      (ebib-set-modified t))))

(defun ebib-edit-multiline-field ()
  "Edits the current field in multiline-mode."
  (interactive)
  (unless (or (eq ebib-current-field 'type*)
	      (eq ebib-current-field 'crossref))
    (let ((text (gethash ebib-current-field ebib-cur-entry-hash)))
      (if (raw-p text)
	  (setq ebib-multiline-raw t)
	(setq text (to-raw text))
	(setq ebib-multiline-raw nil))
      (select-window (ebib-temp-window) nil)
      (ebib-multiline-edit 'fields text))))

(defun ebib-insert-abbreviation ()
  "Inserts an abbreviation from the ones defined in the database."
  (interactive)
  (if (gethash ebib-current-field ebib-cur-entry-hash)
      (beep)
    (when (edb-strings-list ebib-cur-db)
      (unwind-protect
	  (progn
	    (other-window 1)
	    (let* ((collection (ebib-create-collection (edb-strings ebib-cur-db)))
		   (string (completing-read "Abbreviation to insert: " collection nil t)))
	      (when string
		(puthash ebib-current-field string ebib-cur-entry-hash)
		(ebib-set-modified t))))
	(other-window 1)
	;; we can't do this earlier, because we would be writing to the index buffer...
	(ebib-redisplay-current-field)
	(ebib-next-field)))))

(defun ebib-entry-help ()
  "Shows the info node for Ebib's entry buffer."
  (interactive)
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
    (define-key map "\C-xb" 'disabled)
    (define-key map "\C-xk" 'disabled)
    map)
  "Keymap for the ebib strings buffer.")

(define-derived-mode ebib-strings-mode
  fundamental-mode "Ebib-strings"
  "Major mode for the Ebib strings buffer."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defun ebib-quit-strings-buffer ()
  "Quits editing the @STRING definitions."
  (interactive)
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer ebib-entry-buffer)
  (unless (eq ebib-layout 'full)
    (set-window-dedicated-p (selected-window) t))
  (select-window (get-buffer-window ebib-index-buffer) nil))

(defun ebib-prev-string ()
  "Moves to the previous string."
  (interactive)
  (if (equal ebib-current-string (car (edb-strings-list ebib-cur-db)))  ; if we're on the first string
      (beep)
    ;; go to the beginnig of the highlight and move upward one line.
    (goto-char (ebib-highlight-start ebib-strings-highlight))
    (forward-line -1)
    (setq ebib-current-string (prev-elem ebib-current-string (edb-strings-list ebib-cur-db)))
    (ebib-set-strings-highlight)))

(defun ebib-next-string ()
  "Moves to the next string."
  (interactive)
  (if (equal ebib-current-string (last1 (edb-strings-list ebib-cur-db)))
      (when (interactive-p) (beep))
    (goto-char (ebib-highlight-start ebib-strings-highlight))
    (forward-line 1)
    (setq ebib-current-string (next-elem ebib-current-string (edb-strings-list ebib-cur-db)))
    (ebib-set-strings-highlight)))

(defun ebib-goto-first-string ()
  "Moves to the first string."
  (interactive)
  (setq ebib-current-string (car (edb-strings-list ebib-cur-db)))
  (goto-char (point-min))
  (ebib-set-strings-highlight))

(defun ebib-goto-last-string ()
  "Moves to the last string."
  (interactive)
  (setq ebib-current-string (last1 (edb-strings-list ebib-cur-db)))
  (goto-char (point-max))
  (forward-line -1)
  (ebib-set-strings-highlight))

(defun ebib-strings-page-up ()
  "Moves 10 entries up in the database."
  (interactive)
  (let ((number-of-strings (length (edb-strings-list ebib-cur-db)))
	(remaining-number-of-strings (length (member ebib-current-string (edb-strings-list ebib-cur-db)))))
    (if (<= (- number-of-strings remaining-number-of-strings) 10)
	(ebib-goto-first-string)
      (setq ebib-current-string (nth
				 (- number-of-strings remaining-number-of-strings 10)
				 (edb-strings-list ebib-cur-db)))
      (goto-char (ebib-highlight-start ebib-strings-highlight))
      (forward-line -10)
      (ebib-set-strings-highlight)))
  (message ebib-current-string))

(defun ebib-strings-page-down ()
  "Moves 10 entries down in the database."
  (interactive)
  (let ((number-of-strings (length (edb-strings-list ebib-cur-db)))
	(remaining-number-of-strings (length (member ebib-current-string (edb-strings-list ebib-cur-db)))))
    (if (<= remaining-number-of-strings 10)
      (ebib-goto-last-string)
    (setq ebib-current-string (nth
			      (- number-of-strings remaining-number-of-strings -10)
			      (edb-strings-list ebib-cur-db)))
    (goto-char (ebib-highlight-start ebib-strings-highlight))
    (forward-line 10)
    (ebib-set-strings-highlight)))
  (message ebib-current-string))

(defun ebib-fill-strings-buffer ()
  "Fills the strings buffer with the @STRING definitions."
  (set-buffer ebib-strings-buffer)
  (with-buffer-writable
    (erase-buffer)
    (dolist (elem (edb-strings-list ebib-cur-db))
      (let ((str (to-raw (gethash elem (edb-strings ebib-cur-db)))))
	(insert (format "%-18s %s\n" elem
			(if (multiline-p str)
			    (concat "+" (first-line str))
			  (concat " " str)))))))
  (goto-char (point-min))
  (setq ebib-current-string (car (edb-strings-list ebib-cur-db)))
  (ebib-set-strings-highlight)
  (set-buffer-modified-p nil))

(defun ebib-edit-string ()
  "Edits the value of an @STRING definition
When the user enters an empty string, the value is not changed."
  (interactive)
  (let ((init-contents (to-raw (gethash ebib-current-string (edb-strings ebib-cur-db)))))
    (if (multiline-p init-contents)
	(ebib-edit-multiline-string)
      (if-str (new-contents (read-string (format "%s: " ebib-current-string)
					 (if init-contents
					     (cons init-contents 0)
					   nil)
					 ebib-minibuf-hist))
	  (progn
	    (puthash ebib-current-string (from-raw new-contents) (edb-strings ebib-cur-db))
	    (ebib-redisplay-current-string)
	    (ebib-next-string)
	    (ebib-set-modified t))
	(error "@STRING definition cannot be empty")))))

(defun ebib-copy-string-contents ()
  "Copies the contents of the current string to the kill ring."
  (interactive)
  (let ((contents (gethash ebib-current-string (edb-strings ebib-cur-db))))
    (kill-new contents)
    (message "String value copied.")))

(defun ebib-delete-string ()
  "Deletes the current @STRING definition from the database."
  (interactive)
  (when (y-or-n-p (format "Delete @STRING definition %s? " ebib-current-string))
    (remhash ebib-current-string (edb-strings ebib-cur-db))
    (with-buffer-writable
      (let ((beg (progn
		   (goto-char (ebib-highlight-start ebib-strings-highlight))
		   (point))))
	(forward-line 1)
	(delete-region beg (point))))
    (let ((new-cur-string (next-elem ebib-current-string (edb-strings-list ebib-cur-db))))
      (setf (edb-strings-list ebib-cur-db) (delete ebib-current-string (edb-strings-list ebib-cur-db)))
      (when (null new-cur-string)	; deleted the last string
	(setq new-cur-string (last1 (edb-strings-list ebib-cur-db)))
	(forward-line -1))
      (setq ebib-current-string new-cur-string))
    (ebib-set-strings-highlight)
    (ebib-set-modified t)
    (message "@STRING definition deleted.")))

(defun ebib-add-string ()
  "Creates a new @STRING definition."
  (interactive)
  (if-str (new-abbr (read-string "New @STRING abbreviation: "))
      (progn
	(if (member new-abbr (edb-strings-list ebib-cur-db))
	    (error (format "%s already exists" new-abbr)))
	(if-str (new-string (read-string (format "Value for %s: " new-abbr)))
	    (progn
	      (ebib-insert-string new-abbr new-string ebib-cur-db t)
	      (sort-in-buffer (length (edb-strings-list ebib-cur-db)) new-abbr)
	      (with-buffer-writable
		(insert (format "%-19s %s\n" new-abbr new-string)))
	      (forward-line -1)
	      (ebib-set-strings-highlight)
	      (setq ebib-current-string new-abbr)
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
			       (let ((abbr ebib-current-string)
				     (string (gethash ebib-current-string (edb-strings ebib-cur-db))))
				 (if (member abbr (edb-strings-list db))
				     (error "@STRING definition already exists in database %d" num)
				   (ebib-insert-string abbr string db t)))))
      (ebib-export-to-file (format "Export @STRING definition `%s' to file: " ebib-current-string)
			   (format "@STRING definition `%s' exported to %%s" ebib-current-string)
			   #'(lambda ()
			       (insert (format "\n@string{%s = %s}\n"
					       ebib-current-string
					       (gethash ebib-current-string (edb-strings ebib-cur-db)))))))))

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
			 (if (member abbr (edb-strings-list db))
			     (message "@STRING definition `%s' already exists in database %d" abbr num)
			   (ebib-insert-string abbr (gethash abbr (edb-strings ebib-cur-db)) db t)))
		     (edb-strings-list ebib-cur-db))))
	(ebib-export-to-file "Export all @STRING definitions to file: "
			     "All @STRING definitions exported to %s"
			     #'(lambda ()
				 (insert (format "\n"))	; to keep things tidy.
				 (ebib-format-strings ebib-cur-db)))))))

(defun ebib-edit-multiline-string ()
  "Edits the current string in multiline-mode."
  (interactive)
  (select-window (ebib-temp-window) nil)
  (ebib-multiline-edit 'string (to-raw (gethash ebib-current-string (edb-strings ebib-cur-db)))))

(defun ebib-strings-help ()
  "Shows the info node on Ebib's strings buffer."
  (interactive)
  (ebib-lower)
  (info "(ebib) The Strings Buffer"))

;;;;;;;;;;;;;;;;;;;;
;; multiline edit ;;
;;;;;;;;;;;;;;;;;;;;

(define-minor-mode ebib-multiline-mode
  "Minor mode for Ebib's multiline edit buffer."
  :init-value nil :lighter nil :global nil
  :keymap '(("\C-c|q" . ebib-quit-multiline-edit)
	    ("\C-c|c" . ebib-cancel-multiline-edit)
	    ("\C-c|s" . ebib-save-from-multiline-edit)
	    ("\C-c|h" . ebib-multiline-help)))

(easy-menu-define ebib-multiline-menu ebib-multiline-mode-map "Ebib multiline menu"
  '("Ebib"
    ["Store Text and Exit" ebib-quit-multiline-edit t]
    ["Cancel Edit" ebib-cancel-multiline-edit t]
    ["Save Text" ebib-save-from-multiline-edit t]
    ["Help" ebib-multiline-help t]))

(easy-menu-add ebib-multiline-menu ebib-multiline-mode-map)

(defun ebib-multiline-edit (type &optional starttext)
  "Switches to Ebib's multiline edit buffer.
STARTTEXT is a string that contains the initial text of the buffer."
  ;; note: the buffer is put in the currently active window!
  (setq ebib-pre-multiline-buffer (current-buffer))
  (switch-to-buffer ebib-multiline-buffer)
  (set-buffer-modified-p nil)
  (erase-buffer)
  (setq ebib-editing type)
  (when starttext
    (insert starttext)
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun ebib-quit-multiline-edit ()
  "Quits the multiline edit buffer, saving the text."
  (interactive)
  (ebib-store-multiline-text)
  (ebib-leave-multiline-edit-buffer)
  (cond
   ((eq ebib-editing 'fields)
    (ebib-next-field))
   ((eq ebib-editing 'strings)
    (ebib-next-string)))
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
  (switch-to-buffer ebib-pre-multiline-buffer)
  (cond
   ((eq ebib-editing 'preamble)
    (select-window (get-buffer-window ebib-index-buffer) nil))
   ((eq ebib-editing 'fields)
    ;; in full-frame layout, select-window isn't necessary, but it doesn't hurt either.
    (select-window (get-buffer-window ebib-entry-buffer) nil)
    (ebib-redisplay-current-field))
   ((eq ebib-editing 'strings)
    ;; in full-frame layout, select-window isn't necessary, but it doesn't hurt either.
    (select-window (get-buffer-window ebib-strings-buffer) nil)
    (ebib-redisplay-current-string))))

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
	    (setf (edb-preamble ebib-cur-db) nil)
	  (setf (edb-preamble ebib-cur-db) text)))
       ((eq ebib-editing 'fields)
	(if (equal text "")
	    (remhash ebib-current-field ebib-cur-entry-hash)
	  (when (not ebib-multiline-raw)
	    (setq text (from-raw text)))
	  (puthash ebib-current-field text ebib-cur-entry-hash)))
       ((eq ebib-editing 'strings)
	(if (equal text "")
	    ;; with ERROR, we avoid execution of EBIB-SET-MODIFIED and
	    ;; MESSAGE, but we also do not switch back to the strings
	    ;; buffer. this may not be so bad, actually, because the user
	    ;; may want to change his edit.
	    (error "@STRING definition cannot be empty ")
	  (setq text (from-raw text))  ; strings cannot be raw
	  (puthash ebib-current-string text (edb-strings ebib-cur-db))))))
    (ebib-set-modified t))

(defun ebib-multiline-help ()
  "Show the info node on Ebib's multiline edit buffer."
  (interactive)
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
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer ebib-entry-buffer)
  (unless (eq ebib-layout 'full)
    (set-window-dedicated-p (selected-window) t))
  (select-window (get-buffer-window ebib-index-buffer) nil))

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
      (error "No database loaded. Use `o' to open a database")
    (if (edb-virtual ebib-cur-db)
	(error "Cannot import to a virtual database")
      (with-syntax-table ebib-syntax-table
	(save-excursion
	  (save-restriction
	    (if (region-active)
		(narrow-to-region (region-beginning)
				  (region-end)))
	    (let ((buffer (current-buffer)))
	      (with-temp-buffer
		(insert-buffer-substring buffer)
		(let ((n (ebib-find-bibtex-entries t)))
		  (setf (edb-keys-list ebib-cur-db) (sort (edb-keys-list ebib-cur-db) 'string<))
		  (setf (edb-n-entries ebib-cur-db) (length (edb-keys-list ebib-cur-db)))
		  (when (edb-strings-list ebib-cur-db)
		    (setf (edb-strings-list ebib-cur-db) (sort (edb-strings-list ebib-cur-db) 'string<)))
		  (setf (edb-cur-entry ebib-cur-db) (edb-keys-list ebib-cur-db))
		  (ebib-fill-entry-buffer)
		  (ebib-fill-index-buffer)
		  (ebib-set-modified t)
		  (message (format "%d entries, %d @STRINGs and %s @PREAMBLE found in buffer."
				   (car n)
				   (cadr n)
				   (if (caddr n)
				       "a"
				     "no"))))))))))))

(defun ebib-get-db-from-filename (filename)
  "Returns the database struct associated with FILENAME."
  (when (file-name-absolute-p filename)
    (setq filename (expand-file-name filename))) ; expand ~, . and ..
  (catch 'found
    (mapc #'(lambda (db)
	      (if (string= filename
			   ; if filename is absolute, we want to compare to the
			   ; absolute filename of the database, otherwise we
			   ; should use only the non-directory component.
			   (if (file-name-absolute-p filename)
			       (edb-filename db)
			     (file-name-nondirectory (edb-filename db))))
		 (throw 'found db)))
	  ebib-databases)
    nil))

(defun ebib-get-local-databases ()
  "Returns a list of .bib files associated with the file in the current LaTeX buffer.
Each element in the list is a string holding the name of the .bib
file. This function simply searches the current LaTeX file or its
master file for a \\bibliography command and returns the file(s)
given in its argument. If no \\bibliography command is found,
returns the symbol NONE."
  (let ((texfile-buffer (current-buffer))
	texfile)
    ;; if AucTeX's TeX-master is used and set to a string, we must
    ;; search that file for a \bibliography command, as it's more
    ;; likely to be in there than in the file we're in.
    (and (boundp 'TeX-master)
	 (stringp TeX-master)
	 (setq texfile (ensure-extension TeX-master "tex")))
    (with-temp-buffer
      (if (and texfile (file-readable-p texfile))
	  (insert-file-contents texfile)
	(insert-buffer-substring texfile-buffer))
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward "\\\\bibliography{\\(.*?\\)}" nil t)
	    (mapcar #'(lambda (file)
			(ensure-extension file "bib"))
		    (split-string (buffer-substring-no-properties (match-beginning 1) (match-end 1)) ",[ ]*"))
	  'none)))))

(defun ebib-insert-bibtex-key ()
  "Inserts a BibTeX key at POINT.
The user is prompted for a BibTeX key and has to choose one from
the database(s) associated with the current LaTeX file, or from
the current database if there is no \\bibliography command. Tab
completion works."
  (interactive)
  (ebib-execute-when
    ((database)
     (or ebib-local-bibtex-filenames
	 (setq ebib-local-bibtex-filenames (ebib-get-local-databases)))
     (let (collection)
       (if (eq ebib-local-bibtex-filenames 'none)
	   (if (null (edb-cur-entry ebib-cur-db))
	       (error "No entries found in current database")
	     (setq collection (ebib-create-collection (edb-database ebib-cur-db))))
	 (mapc #'(lambda (file)
		   (let ((db (ebib-get-db-from-filename file)))
		     (cond
		      ((null db)
		       (message "Database %s not loaded" file))
		      ((null (edb-cur-entry db))
		       (message "No entries in database %s" file))
		      (t (setq collection (append (ebib-create-collection (edb-database db))
						  collection))))))
	       ebib-local-bibtex-filenames))
       (when collection
	 (let* ((key (completing-read "Key to insert: " collection nil t nil ebib-minibuf-hist))
		(insert-string (if-str (command (completing-read "Command to use: " ebib-insertion-commands
								 nil nil nil ebib-minibuf-hist))
				   (ebib-create-citation-command command
								 (or
								  (cadr (assoc command ebib-insertion-commands))
								  1)
								 key)
				 key)))
	   (when insert-string
	     (insert insert-string))))))
    ((default)
     (error "No database loaded"))))

(defun ebib-entry-summary ()
  "Shows the fields of the key at POINT.
The key is searched in the database associated with the LaTeX
file, or in the current database if no \\bibliography command can
be found."
  (interactive)
  (ebib-execute-when
    ((database)
     (or ebib-local-bibtex-filenames
	 (setq ebib-local-bibtex-filenames (ebib-get-local-databases)))
     (let ((key (read-string-at-point "\"#%'(),={} \n\t\f"))
	   entry
	   database)
       (if (eq ebib-local-bibtex-filenames 'none)
	   (if (not (member key (edb-keys-list ebib-cur-db)))
	       (error "Entry `%s' is not in the current database" key)
	     (setq entry (gethash key (edb-database ebib-cur-db)))
	     (setq database ebib-cur-db))
	 (multiple-value-setq (entry database)
	   (catch 'found
	     (mapc #'(lambda (file)
		       (let ((db (ebib-get-db-from-filename file)))
			 (if (null db)
			     (message "Database %s not loaded" file)
			   (if (member key (edb-keys-list db))
			       (throw 'found (values (gethash key (edb-database db)) db))))))
		   ebib-local-bibtex-filenames)
	     (list nil nil))))
       (if (null entry)
	   (error "Entry `%s' not found" key)
	 (let ((index-window (get-buffer-window ebib-index-buffer)))
	   (if (not index-window)
	       (with-help-window "*Help*"
		 (ebib-format-fields entry 'princ))
	     (with-selected-window index-window
	       (setq ebib-cur-db database)
	       (ebib-fill-index-buffer)
	       (setf (edb-cur-entry ebib-cur-db) (member key (edb-keys-list ebib-cur-db)))
	       (goto-char (point-min))
	       (re-search-forward (format "^%s " key))
	       (beginning-of-line)
	       (ebib-set-index-highlight))
	     (ebib-fill-entry-buffer))))))
    ((default)
     (error "No database(s) loaded"))))

(provide 'ebib)

;;; ebib ends here
