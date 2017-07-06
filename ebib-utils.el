;;; ebib-utils.el --- Part of Ebib, a BibTeX database manager  -*- lexical-binding: t -*-

;; Copyright (c) 2003-2016 Joost Kremers
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
;; general macros, utilities and variables.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'bibtex)
(require 'ebib-db)

;; Make a bunch of variables obsolete.
(make-obsolete-variable 'ebib-entry-types "The variabale `ebib-entry-types' is obsolete; see the manual for details." "24.4")
(make-obsolete-variable 'ebib-default-entry 'ebib-default-entry-type "24.4")
(make-obsolete-variable 'ebib-additional-fields 'ebib-extra-fields "24.4")
(make-obsolete-variable 'ebib-biblatex-inheritance 'ebib-biblatex-inheritances "24.4")
(make-obsolete-variable 'ebib-standard-url-field 'ebib-url-field "24.4")
(make-obsolete-variable 'ebib-standard-file-field 'ebib-file-field "24.4")
(make-obsolete-variable 'ebib-standard-doi-field 'ebib-doi-field "24.4")

;; Make sure we can call bibtex-generate-autokey
(declare-function bibtex-generate-autokey "bibtex" nil)

;;;;;;;;;;;;;;;;;;;;;;
;; global variables ;;
;;;;;;;;;;;;;;;;;;;;;;

;; user customisation

(defgroup ebib nil "Ebib: a BibTeX database manager" :group 'tex)

(defgroup ebib-windows nil "Ebib window management" :group 'ebib)

(defcustom ebib-default-entry-type "Article"
  "The default entry type.
This is the entry type assigned to newly created entries."
  :group 'ebib
  :type 'string)

(defcustom ebib-preload-bib-files nil
  "List of BibTeX files to load automatically when Ebib starts.
This option allows you to specify which `.bib' file(s) Ebib
should load automatically when it starts up.  Specify one file per
line.  You can complete a partial filename with `M-TAB`."
  :group 'ebib
  :type '(repeat (file :must-match t)))

(defcustom ebib-bib-search-dirs '("~")
  "List of directories to search for BibTeX files.
This is a list of directories Ebib searches for `.bib' files to
be preloaded.  Note that only the directories themselves are
searched, not their subdirectories.  The directories in this list
are also searched when the function `ebib' is passed a file
name (e.g., from an Eshell command line)."
  :group 'ebib
  :type '(repeat :tag "Search directories for BibTeX files" (string :tag "Directory")))

(defcustom ebib-create-backups t
  "Create a backup file.
The first time a BibTeX file is saved, a backup file is created
when it is first saved.  Note that Ebib uses
`make-backup-file-name' to create the name for the backup file."
  :group 'ebib
  :type '(choice (const :tag "Create backups" t)
                 (const :tag "Do not create backups" nil)))

(defcustom ebib-extra-fields '((BibTeX "crossref"
                                   "annote"
                                   "abstract"
                                   "keywords"
                                   "file"
                                   "timestamp"
                                   "url"
                                   "doi")
                           (biblatex "crossref"
                                     "annotation"
                                     "abstract"
                                     "keywords"
                                     "file"
                                     "timestamp"))
  "List of the extra fields for BibTeX entries.
Extra fields are fields that are available for all entry types.
Depending on the bibliography style, the value of these fields
may appear in the bibliography, but you may also define fields
that are just for personal use.

Note, before adding fields to this list, check if the field you
want to add is among the fields that are hidden by default.  See
the option \"Hidden Fields\" (`ebib--hidden-fields') for details."
  :group 'ebib
  :type '(repeat (cons (choice :tag "Choose BibTeX dialect"
                               (const BibTeX)
                               (const biblatex)
                               (symbol :tag "Other"))
                       (repeat :tag "Extra fields" (string :tag "Field")))))

(defcustom ebib-hidden-fields '("addendum" "afterword" "annotator" "bookauthor"
                            "booksubtitle" "booktitleaddon" "chapter" "commentator"
                            "edition" "editora" "editorb" "editorc" "eid" "eprint"
                            "eprintclass" "eprinttype" "eventdate" "eventtitle"
                            "foreword" "holder" "howpublished" "introduction" "isbn"
                            "isrn" "issn" "issue" "issuesubtitle" "issuetitle"
                            "journalsubtitle" "language" "location" "mainsubtitle"
                            "maintitle" "maintitleaddon" "month" "origlanguage"
                            "pagetotal" "part" "remark" "subtitle" "timestamp"
                            "titleaddon" "translator" "urldate" "venue" "version"
                            "volumes")
  "List of fields that are not displayed.
The fields in this list are not displayed by default.  Since
Biblatex defines a large number of fields, it is not practical to
display them all in the entry buffer.  You can make these fields
temporarily visible with the command `\\<ebib-index-mode-map>\\[ebib-toggle-hidden]'
or through the menu."
  :group 'ebib
  :type '(repeat (string :tag "Field")))

(defcustom ebib-layout 'full
  "Ebib window layout.
This option defines how Ebib displays the buffers its uses.  By
default, Ebib takes over the entire frame and creates two windows
to display the index and the entry buffer.  Alternatively, Ebib
can use just the right part of the frame (the width can be
specified with the option `ebib-width').  A third option is to
display only the index window upon startup.  The entry buffer will
be displayed when you edit an entry of if you press
\\[ebib-select-and-popup-entry]."
  :group 'ebib-windows
  :type '(choice (const :tag "Use full frame" full)
                 (const :tag "Use current window" window)
                 (const :tag "Use right part of the frame" custom)
                 (const :tag "Display only index window" index-only)))

(defcustom ebib-width 80
  "Width of the Ebib windows.
The width can be absolute or relative; if it is absolute, it
specifies the number of columns that the Ebib windows occupies.
If it is relative, the with must be a value between 0 and 1
specifying the width relative to the width of the window that is
selected when Ebib is started.

This option only takes effect if `ebib-layout' is set to `custom'."
  :group 'ebib-windows
  :type '(choice (integer :tag "Absolute width")
                 (float :tag "Relative width" :value 0.3)))

(defcustom ebib-popup-entry-window nil
  "Create a popup window to display the entry window.
If `ebib-layout' is set to `index-only', Ebib will use an
existing window to display the entry buffer when needed.  By
setting this option, however, you can tell Ebib to use the
function `display-buffer-pop-up-window' to show the entry buffer,
which (usually) means that a new window will be created.

Note that setting this option has no effect unless `ebib-layout'
is set to `index-only'."
  :group 'ebib-windows
  :type 'boolean)

(defcustom ebib-window-vertical-split nil
  "Display the index buffer at the left of the frame.
Setting this option makes Ebib display the index buffer at the
left side of the frame rather than at the top.  The width of the
window will be `ebib-index-window-size', which you will probably
have to set to a larger value."
  :group 'ebib-windows
  :type 'boolean)

(defcustom ebib-index-window-size 10
  "The size of the index buffer window.
This is either the height of the window, or, if
`ebib-window-vertical-split' is set, the width of the window.
The rest of the frame is used for the entry buffer, unless
`ebib-layout' is set to `index-only'."
  :group 'ebib-windows
  :type 'integer)

(defcustom ebib-index-mode-line '("%e"
                              mode-line-front-space
                              ebib--mode-line-modified
                              mode-line-buffer-identification
                              (:eval (format "  (%s)" (ebib--get-dialect ebib--cur-db)))
                              (:eval (if (and ebib--cur-db (ebib--get-key-at-point)) "     Entry %l" "     No Entries"))
                              (:eval (if (and ebib--cur-db (ebib-db-get-filter ebib--cur-db)) (format "  |%s|" (ebib--filters-pp-filter (ebib-db-get-filter ebib--cur-db))) "")))
  "The mode line for the index window.
The mode line of the index window shows some Ebib-specific
information.  You can customize this information if you wish, or
disable the Ebib-specific mode line altogether.  Note that the
mode line of the entry buffer is not changed."
  :group 'ebib-windows
  :type '(choice (const :tag "Use standard mode line" nil)
                 (sexp :tag "Customize mode line")))

(defvar ebib--mode-line-modified '(:eval (ebib--mode-line-modified-p))
  "Mode line construct for database's modified status.")
(put 'ebib--mode-line-modified 'risky-local-variable t)

(defcustom ebib-modified-char "M"
  "Character indicating the modified status in the mode line."
  :group 'ebib-windows
  :type 'string)

(defcustom ebib-index-columns '(("Entry Key" 40 t)
                            ("Author/Editor" 40 t)
                            ("Year" 6 t)
                            ("Title" 50 t))
  "Columns to display in the index buffer.
Each column consists of the BibTeX field to be displayed, which
is also the column's label, the column's maximum width and a flag
indicating whether sorting on this column is possible.

Any field BibTeX or biblatex field can be used.  There are two
special labels: \"Entry Key\" and \"Author\".  The label \"Entry
Key\" displays the entry's BibTeX key, and the label
\"Author/Editor\" displays the contents of the Author field, or,
if that is empty, the contents of the Editor field.

Note that the default sort field is the entry key, even if the
\"Entry Key\" field is absent from the index buffer.  You can
remove the \"Entry Key\" field from this option, but that will
not change the default sort."
  :group 'ebib
  :type '(repeat (list  (string :tag "Field")
                        (integer :tag "Width")
                        (boolean :tag "Sort"))))

(defcustom ebib-index-column-separator "  "
  "Separator between columns in the index buffer."
  :group 'ebib
  :type 'string)

(defcustom ebib-field-transformation-functions '(("Title" . ebib-clean-TeX-markup)
                                             ("Doi" . ebib-display-www-link)
                                             ("Url" . ebib-display-www-link)
                                             ("Note" . ebib-display-note-symbol))
  "Functions transforming field contents to appropriate forms.
Each function should accept three arguments, the field to be
displayed, the key of the entry being displayed, and the database
that contains the entry, and should return a string to be
displayed in the ebib index buffer.  In principle, this string
should be the contents of the field transformed in some way, but
it may actually be anything.  In fact, it is not necessary for
the field to be an actual Bib(La)TeX field, as long as the
transformation function returns something that can be displayed."
  :group 'ebib
  :type '(repeat (cons (string :tag "Field")
                       (function :tag "Transform function"))))

(defcustom ebib-uniquify-keys nil
  "Create unique keys.
When adding new entries to the database, Ebib does not allow
duplicate keys.  By setting this option, you can tell Ebib to
automatically create a unique key by adding `b', `c', etc. to it.
This applies when Ebib automatically generates keys for new
entries (see `ebib-autogenerate-keys'), when merging `.bib'
files, and when changing a key."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-autogenerate-keys nil
  "Automatically generate keys for new entries.
With this option set, Ebib does not ask for a key when you add a
new entry.  Instead, it gives the entry a temporary key and
assigns a proper key when you finish editing the entry.  This
option uses the function `bibtex-generate-autokey', which has a
number of user-customizable options.  See that function's
documentation for details."
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
  "A list of format strings used to insert citations into text buffers.
Each item in this option consists of a major mode and a list of
identifier + format strings pairs.  The identifiers (which can be
any string) are used to select the citation command in
`ebib-insert-citation' and `ebib-push-citation'.  The format
strings are used to construct the citation command that is
inserted in the buffer.

The major mode can also be specified as `any', which defines
citation commands that are available in buffers that do not have
any of the major modes listed in this option.  By default, this
is used for LaTeX citations, so as to cover all TeX and LaTeX
modes.

The format string template can contain a number of formatting
directives:

%K: the key of the entry.
%A: an argument; prompts the user.
%D: a description; prompts the user.
%<...%>: optional material surrounding %A.
%(...%): a repeater, which must contain %K.

%A is used for arguments to the citation command, which are
elements such as page numbers, etc.  %A accommodates optional
arguments in LaTeX-based citations and, similarly, optional
material in Pandoc Markdown citations.  %D can be used to provide
a description as used in Org-mode links.  The user is prompted
for this description, but if possible a default is provided,
which can be accepted by hitting RET.

Optional material around %A is only included if the user provides
some string for %A.  If not, the optional material is omitted.

The command `ebib-push-citation' can be used on multiple
entries (by marking them in the index buffer).  If the template
contains a repeater, the material inside this repeater is
processed for each key individually.  If there is no repeater,
all keys are substituted for %K using a separator for which the
user is prompted.

The repeater can optionally contain a separator, which must be
placed between % and ); to use comma as a separator, the format
shring should contain \"%(%K%,)\".  If the separator is not
provided, the user is prompted to supply one."
  :group 'ebib
  :type '(repeat (list :tag "Mode" (symbol :tag "Mode name")
                       (repeat (list :tag "Citation command"
                                     (string :tag "Identifier")
                                     (string :tag "Format string"))))))

(defcustom ebib-citation-description-function 'ebib-author-year-description
  "Function for creating a description to be used in citations.
This function is called to provide a description to substitute
for the %D directive in `ebib-citation-commands', and also when
creating Org links with `org-store-link', provided the library
`org-ebib' is loaded.

The default value of this option provides an author/year
description composed of the author or editor field of the entry
and the year field, combined as \"Author (Year)\".  A second
option is to use the Title field on an entry for the link
description.

It is also possible to specify a user-defined function.  This
function should take two arguments: the key of the entry for
which a description is to be created, and the database that
contains the entry."
  :group 'ebib
  :type '(choice (function-item :tag "Author/Year" ebib-author-year-description)
                 (function-item :tag "Title" ebib-title-description)
                 (function :tag "Custom function")))

(defun ebib-author-year-description (key db)
  "Provide an author/year description for an Org Ebib link.
KEY is the key of the entry to provide the link for, DB the
database that contains the entry."
  (format "%s (%s)"
          (ebib--get-field-value-for-display "Author/Editor" key db)
          (ebib--get-field-value-for-display "Year" key db)))

(defun ebib-title-description (key db)
  "Provide a title description for an Org Ebib link.
KEY is the key of the entry to provide the link for, DB the
database that contains the entry."
  (ebib-db-get-field-value "Title" key db "(Untitled)" 'unbraced 'xref))

(defcustom ebib-multiline-major-mode 'text-mode
  "The major mode of the multiline edit buffer."
  :group 'ebib
  :type '(function :tag "Mode function"))

(defcustom ebib-sort-order nil
  "The fields on which the BibTeX entries are to be sorted in the BibTeX file.
This option is described in the manual/info file in the section
\"Sorting the .bib file\"."
  :group 'ebib
  :type '(repeat (repeat :tag "Sort level" (string :tag "Sort field"))))

(defcustom ebib-save-xrefs-first t
  "Save entries with a crossref field first in the BibTeX-file.
For BibTeX's cross-referencing to work, the cross-referencing
entries must appear in the `.bib` file before the
cross-referenced entries.  This option tells Ebib to save all
entries with a `crossref` field first, so that BibTeX's
crossreferencing options work as intended.

Note: this option is not compatible with setting the option
`ebib-sort-order'.  If you want to use the latter, unset this
one."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-use-timestamp nil
  "Add a timestamp to new entries.
If this option is set, Ebib will add a `timestamp` field to every
new entry, recording the date and time it was added to the
database.  See the section \"Timestamps\" in the manual/info file for
details.

Note that the `timestamp' field is normally hidden.  You can make
it visible with \\[ebib--toggle-hidden] in the index buffer or by
customizing the option `ebib--hidden-fields'."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-timestamp-format "%a %b %e %T %Y"
  "Format of the time string used in the timestamp.
This option specifies the format string that is used to create
the timestamp.  The default value produces a timestamp of the form
\"Mon Mar 12 01:03:26 2007\".  This option uses the Emacs function
`format-time-string` to create the timestamp.  See that function's
documentation for details on customizing the format string."
  :group 'ebib
  :type 'string)

(defcustom ebib-url-field "url"
  "Standard field to store URLs in.
In the index buffer, the command \\[ebib--browse-url] can be used to
send a URL to a browser.  This option sets the field from which
this command extracts the URL."
  :group 'ebib
  :type 'string)

(defcustom ebib-url-regexp "\\\\url{\\(.*\\)}\\|https?://[^ ';<>\"\n\t\f]+"
  "Regular expression to extract URLs from a field.
This is the regular expression that Ebib uses to search for URLs
in a field.  With the default value, Ebib considers everything
that is in a LaTeX `\\url{...}' command as a URL, and furthermore
every string of text that starts with `http://' or `https://' and
does not contain whitespace or one of the characters ' \" ; < or >.

Note that the semicolon is added for consistency: it makes it
possible to use the same separator in the `url' field as in the
`file' field."
  :group 'ebib
  :type 'string)

(defcustom ebib-browser-command nil
  "Command to call the browser with.
If this option is unset, Ebib uses the Emacs function
`browse-url' to start a browser.  If you prefer not to use this,
you can set this option to the executable name of your preferred
browser.  For this to work, the browser that you use must be able
to handle a URL on the command line."
  :group 'ebib
  :type '(choice (const :tag "Use standard browser" nil)
                 (string :tag "Specify browser command")))

(defcustom ebib-doi-field "doi"
  "Standard field to store a DOI (digital object identifier) in.
In the index buffer, the command ebib--browse-doi can be used to
send a suitable URL to a browser.  This option sets the field from
which this command extracts the doi."
  :group 'ebib
  :type 'string)

(defcustom ebib-file-field "file"
  "Standard field to store filenames in.
In the index buffer, the command ebib--view-file can be used to
view a file externally.  This option sets the field from which
this command extracts the filename."
  :group 'ebib
  :type 'string)

(defcustom ebib-file-associations '(("pdf" . "xpdf")
                                ("ps" . "gv"))
  "List of file associations.
Lists file extensions together with external programs to handle
files with those extensions.  If the program string contains a
literal `%s', it is replaced with the name of the file being
opened, allowing the use of command-line options.  Otherwise, the
string is treated as an executable, searched for in `exec-path'.

When you open a file for which no external program is defined,
the file is opened in Emacs."
  :group 'ebib
  :type '(repeat (cons :tag "File association"
                       (string :tag "Extension")
                       (choice (const :tag "Open in Emacs" nil)
                               (string :tag "Run external command")))))

(defcustom ebib-filename-separator "; "
  "Separator for filenames in `ebib-file-field'.
The contents of the file field is split up using this separator,
each chunk is assumed to be a filename.

Note that the default value of this option consists of
semicolon-space.  This means you can have semicolons in your file
names, as long as they're not followed by a space."
  :group 'ebib
  :type 'string)

(defcustom ebib-file-search-dirs '("~")
  "List of directories to search when viewing external files.
Note that searching is not recursive: only the files listed here
are searched, not their subdirectories."
  :group 'ebib
  :type '(repeat :tag "Search directories" (string :tag "Directory")))

(defcustom ebib-truncate-file-names t
  "Truncate file names in the file field.
If t, file names entered in the file field are truncated relative
to the directories in `ebib-file-search-dirs'."
  :group 'ebib
  :type '(choice (const :tag "Truncate File Names" t)
                 (const :tag "Do not Truncate File Names" nil)))

(defcustom ebib-name-transform-function 'identity
  "Function for transforming keys into file names.
When `ebib-view-file' is called but no filename is listed in the
file field, the entry key is converted to a filename using this
function."
  :group 'ebib
  :type '(choice (const :tag "Do not apply any function" identity)
                 (function :tag "Apply function")))

(defcustom ebib-file-name-mod-function 'ebib-dont-change-file-name
  "Function to modify a file name in the file field.
This function should take two arguments, the first being the file
name (absolute or relative), the second either t or nil.  If t,
the file name should be modified for storing, if nil the
modifications should be undone so that the file name can be
passed to an external viewer."
  :group 'ebib
  :type '(choice (const :tag "Do not modify file names" ebib-dont-change-file-name)
                 (function :tag "Modification function")))

(defun ebib-dont-change-file-name (file _)
  "Return FILE unchanged.
This function is the default value for `ebib-file-name-mod-function'."
  file)

(defcustom ebib-local-variable-indentation ""
  "Indentation of the local variable block."
  :group 'ebib
  :type '(string :tag "Indentation"))

(defcustom ebib-print-preamble '("\\documentclass{article}")
  "Preamble used for the LaTeX file for printing the database.
This option specifies the preamble that is to be added to the
LaTeX file Ebib creates for printing the database as index cards.
You can set your own `\\usepackage' commands, or anything else
you may need.  See the section \"Printing the Database\" in the
manual/info file for details."
  :group 'ebib
  :type '(repeat (string :tag "Add to preamble")))

(defcustom ebib-print-newpage nil
  "Print each entry on a separate page.
With this option set, Ebib puts every entry on a separate page
when printing index cards.  Otherwise the entries are separated by
a small amount of whitespace only."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-print-multiline nil
  "Include multiline field values when printing the database.
When this options is set, Ebib includes multiline field values
when you print index cards.  Otherwise multiline values are
excluded, which saves space."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-latex-preamble '("\\documentclass{article}" "\\bibliographystyle{plain}")
  "Preamble for the LaTeX file for BibTeXing the database."
  :group 'ebib
  :type '(repeat (string :tag "Add to preamble")))

(defcustom ebib-print-tempfile ""
  "Temporary file for printing the database.
If set, the commands to print the database (`ebib--print-database'
and `ebib--latex-database') write to this file.  Otherwise you are
asked for a file name."
  :group 'ebib
  :type '(file))

(defcustom ebib-allow-identical-fields nil
  "Handle multiple occurrences of a single field gracefully.
Sometimes BibTeX entries from external sources use multiple
identical fields for some reason (e.g., multiple `keyword'
fields).  Normally, only the last value is read by Ebib, but with
this option set, all values are combined into a single field.  See
the section \"Multiple Identical Fields\" in the manual/info
file."
  :group 'ebib
  :type 'boolean)

(defcustom ebib-bibtex-extensions '(".bib" ".bibtex")
  "List of possible filename extensions of BibTeX files.
When loading a BibTeX filename without extension, Ebib tries to
find a file by adding these extensions.  When creating a new file,
the first extension is added if the filename provided does not
already have an extension.  If you want to create BibTeX files
without extension, add the empty string \"\" to this list or
unset the option entirely."
  :group 'ebib
  :type '(repeat (string :tag "Extension")))

(defcustom ebib-hide-cursor t
  "Hide the cursor in the Ebib buffers.
Normally, the cursor is hidden in Ebib buffers, with the
highlight indicating which entry, field or string is active.  By
unsetting this option, you can make the cursor visible.  Note
that changing this option does not take effect until you restart
Ebib (not Emacs)."
  :group 'ebib
  :type '(choice (const :tag "Hide the cursor" t)
                 (const :tag "Show the cursor" nil)))

(defgroup ebib-faces nil "Faces for Ebib" :group 'ebib)

(defface ebib-highlight-face '((t (:inherit highlight)))
  "Face used for the highlights."
  :group 'ebib-faces)

(defface ebib-crossref-face '((t (:inherit font-lock-comment-face)))
  "Face used to indicate values inherited from crossreferenced entries."
  :group 'ebib-faces)

(defface ebib-alias-face '((t (:inherit warning)))
  "Face used to indicate values inherited from crossreferenced entries."
  :group 'ebib-faces)

(defface ebib-marked-face '((t (:inverse-video t)))
  "Face to indicate marked entries."
  :group 'ebib-faces)

(defface ebib-modified-face '((t (:inherit error)))
  "Face indicating modified status."
  :group 'ebib-faces)

(defface ebib-field-face '((t (:inherit font-lock-keyword-face)))
  "Face for field names."
  :group 'ebib-faces)

(defface ebib-warning-face '((t (:inherit error)))
  "Face for marking potential problems with the database.
Currently, the following problems are marked:

* Crossreferences to entry keys that do not exist.
* Keywords that have not been saved."
  :group 'ebib-faces)

;; generic for all databases

;; constants and variables that are set only once
(defvar ebib--initialized nil "T if Ebib has been initialized.")

(defvar ebib--buffer-alist nil "Alist of Ebib buffers.")

;; general bookkeeping
(defvar ebib--field-history nil "Minibuffer field name history.")
(defvar ebib--filters-history nil "Minibuffer history for filters.")
(defvar ebib--citation-history nil "Minibuffer history for citation commands.")
(defvar ebib--key-history nil "Minibuffer history for entry keys.")
(defvar ebib--keywords-history nil "Minibuffer history for keywords.")

(defvar ebib--saved-window-config nil "Stores the window configuration when Ebib is called.")
(defvar ebib--window-before nil "The window that was active when Ebib was called.")
(defvar ebib--buffer-before nil "The buffer that was active when Ebib was called.")
(defvar ebib--frame-before nil "The frame that was active when Ebib was called.")
(defvar ebib--export-filename nil "Filename to export entries to.")
(defvar ebib--push-buffer nil "Buffer to push entries to.")
(defvar ebib--search-string nil "Stores the last search string.")
(defvar ebib--multiline-buffer-list nil "List of multiline edit buffers.")
(defvar-local ebib--multiline-info nil "Information about the multiline text being edited.")
(defvar ebib--log-error nil "Indicates whether an error was logged.")
(defvar-local ebib--local-bibtex-filenames nil "A list of a buffer's .bib file(s)")
(put 'ebib--local-bibtex-filenames 'safe-local-variable (lambda (v) (null (--remove (stringp it) v))))

;; The databases

;; the master list and the current database
(defvar ebib--databases nil "List of structs containing the databases.")
(defvar ebib--cur-db nil "The database that is currently active.")

;; bookkeeping required when editing field values or @STRING definitions

(defvar ebib--hide-hidden-fields t "If set to T, hidden fields are not shown.")

;; the prefix key and the multiline key are stored in a variable so that the
;; user can customise them.
(defvar ebib--prefix-key ?\;)
(defvar ebib--multiline-key ?\|)

;; this is an AucTeX variable, but we want to check its value, so let's
;; keep the compiler from complaining.
(eval-when-compile
  (defvar TeX-master))

;; General functions

(defun ebib--buffer (buffer)
  "Return the buffer object referred to by BUFFER.
BUFFER is a symbol referring to a buffer in
`ebib--buffer-alist'."
  (cdr (assq buffer ebib--buffer-alist)))

(defmacro with-current-ebib-buffer (buffer &rest body)
  "Make BUFFER current and execute BODY.
BUFFER is a symbol referring to a buffer in
`ebib--buffer-alist'."
  (declare (indent defun)
           (debug t))
  `(with-current-buffer (cdr (assq ,buffer ebib--buffer-alist))
     ,@body))

(defmacro with-ebib-window-nondedicated (&rest body)
  "Execute BODY with the current window non-dedicated.
Restore the dedicated status after executing BODY."
  (declare (indent defun)
           (debug t))
  `(let ((dedicated (window-dedicated-p)))
     (unwind-protect
         (progn
           (set-window-dedicated-p (selected-window) nil)
           ,@body)
       (if dedicated
           (set-window-dedicated-p (selected-window) t)))))

;; We sometimes (often, in fact ;-) need to do something with a string, but
;; take special action (or do nothing) if that string is empty.
;; `ebib--ifstring' makes that easier:

(defmacro ebib--ifstring (bindvar then &rest else)
  "Create a string and test its value.

BINDVAR should be of the form (<var> <value>), where <var> is a
variable name (unquoted symbol) which will be let-bound to the
result of evaluating <value>.  If VALUE is a nonempty string,
THEN (a single sexpr) is executed and its return value returned.
If VALUE is either \"\" or nil, the forms in ELSE are executed
and the return value of its last form is returned."
  (declare (indent 2)
           (debug ((symbolp form) form body)))
  `(let ,(list bindvar)
     (if (not (or (null ,(car bindvar))
                  (equal ,(car bindvar) "")))
         ,then
       ,@else)))

(eval-and-compile
  (defun ebib--execute-helper (env)
    "Helper function for `ebib--execute-when'."
    (cond
     ((eq env 'entries)
      '(ebib-db-list-keys ebib--cur-db))
     ((eq env 'marked-entries)
      '(and ebib--cur-db
            (ebib-db-marked-entries-p ebib--cur-db)))
     ((eq env 'database)
      'ebib--cur-db)
     ((eq env 'real-db)
      '(and ebib--cur-db
            (not (ebib-db-get-filter ebib--cur-db))))
     ((eq env 'filtered-db)
      '(and ebib--cur-db
            (ebib-db-get-filter ebib--cur-db)))
     ((eq env 'no-database)
      '(not ebib--cur-db))
     (t t))))

(defmacro ebib--execute-when (&rest forms)
  "Macro to facilitate writing Ebib functions.
This functions essentially like a `cond' clause: the basic format
is (ebib--execute-when FORMS ...), where each FORM is built up
as (ENVIRONMENTS BODY).  ENVIRONMENTS is a list of symbols (not
quoted) that specify under which conditions BODY is to be
executed.  Valid symbols are:

`entries': execute when there are entries in the database,
`marked-entries': execute when there are marked entries in the database,
`database': execute if there is a database,
`no-database': execute if there is no database,
`real-db': execute when there is a database and it is not filtered,
`filtered-db': execute when there is a database and it is filtered,
`default': execute if all else fails.

Just like with `cond', only one form is actually executed, the
first one that matches.  If ENVIRONMENT contains more than one
condition, BODY is executed if they all match (i.e., the
conditions are AND'ed.)"
  (declare (indent defun)
           (debug (&rest (sexp form))))
  `(cond
    ,@(mapcar (lambda (form)
                (cons (if (= 1 (length (car form)))
                          (ebib--execute-helper (caar form))
                        `(and ,@(mapcar (lambda (env)
                                          (ebib--execute-helper env))
                                        (car form))))
                      (cdr form)))
              forms)))

(defun ebib--mode-line-modified-p (&optional db)
  "Return a string describing the modified status of DB.
DB defaults to the current database."
  (or db (setq db ebib--cur-db))
  (when db  ; Note that `ebib--cur-db' may also be nil!
    (if (not (ebib-db-modified-p db))
        " "
      (propertize ebib-modified-char
                  'face 'ebib-modified-face
                  'help-echo "Database modified\nmouse-1: Save database"
                  'local-map '(keymap (mode-line keymap (mouse-1 . ebib-save-current-database)))))))

(defun ebib--log (type format-string &rest args)
  "Write a message to Ebib's log buffer.
TYPE (a symbol) is the type of message: `log' writes the message
to the log buffer only; `message' writes the message to the log
buffer and outputs it with the function `message'; `warning' logs
the message and sets the variable `ebib--log-error' to 0; finally,
`error' logs the message and sets the variable `ebib--log-error'
to 1. The latter two can be used to signal the user to check the
log for warnings or errors.

FORMAT-STRING and ARGS function as in `format'.  Note that this
function adds a newline to the message being logged."
  (with-current-ebib-buffer 'log
    (cond
     ((eq type 'warning)
      (or ebib--log-error ; If ebib--error-log is already set to 1, we don't want to overwrite it!
          (setq ebib--log-error 0)))
     ((eq type 'error)
      (setq ebib--log-error 1))
     ((eq type 'message)
      (apply #'message format-string args)))
    (insert (apply #'format (concat (if (eq type 'error)
                                        (propertize format-string 'face 'font-lock-warning-face)
                                      format-string)
                                    "\n")
                   args))))

(defun ebib--read-file-to-list (filename)
  "Return the contents of FILENAME as a list of lines."
  (if (and filename                               ; protect against 'filename' being 'nil'
           (file-readable-p filename))
      (with-temp-buffer
        (insert-file-contents filename)
        (split-string (buffer-string) "\n" t))))    ; 't' is omit nulls, blank lines in this case

;; we sometimes need to walk through lists.  these functions yield the
;; element directly preceding or following ELEM in LIST. in order to work
;; properly, ELEM must be unique in LIST, obviously. if ELEM is the
;; first/last element of LIST, or if it is not contained in LIST at all,
;; the result is nil.
(defun ebib--next-elem (elem list)
  "Return the element following ELEM in LIST.
If ELEM is the last element, return nil."
  (cadr (member elem list)))

(defun ebib--prev-elem (elem list)
  "Return the element preceding ELEM in LIST.
If ELEM is the first element, return nil."
  (if (or (equal elem (car list))
          (not (member elem list)))
      nil
    (car (last list (1+ (length (member elem list)))))))

(defun ebib--locate-bibfile (file &optional dirs)
  "Locate and/or expand FILE to an absolute filename in DIRS.
First try to locate BibTeX file FILE with `locate-file' and with
`ebib-bibtex-extensions' as possible suffixes.  If this does not
yield a result, expand FILE with `expand-file-name', adding the
first extension in `ebib-bibtex-extensions' if FILE has no
filename suffix."
  (or (locate-file file (or dirs "/") (append '("") ebib-bibtex-extensions))
      (expand-file-name (if (file-name-extension file)
                            file
                          (concat file (car ebib-bibtex-extensions))))))

(defun ebib--get-file-modtime (file)
  "Return the modification time of FILE.
If FILE cannot be read, return nil."
  (if (file-readable-p file)
      (nth 5 (file-attributes file))))

(defun ebib--ensure-extension (filename ext)
  "Ensure FILENAME has an extension.
Return FILENAME if it alread has an extension, otherwise return
FILENAME appended with EXT.  Note that EXT should start with a
dot."
  (if (file-name-extension filename)
      filename
    (concat filename ext)))

(defun ebib--create-file-name-from-key (key ext)
  "Create a filename from KEY and EXT.
KEY is modified as per `ebib-name-transform-function'.  EXT is
the extension and should not contain a dot."
  (concat (funcall ebib-name-transform-function key)
          "."
          ext))

(defun ebib--expand-file-name (file)
  "Search and expand FILE.
FILE is a file name, possibly with a partial file path.  It is
expanded relative to `ebib-file-search-dirs'.  If the file cannot
be found, the non-directory part is searched for as well.  As a
last resort, FILE is expanded relative to `default-directory'.
If FILE is an absolute file name, expand it with
`expand-file-name' and return the result."
  (if (file-name-absolute-p file)
      (expand-file-name file)
    (let* ((unmod-file (funcall ebib-file-name-mod-function file nil)))
      (or (locate-file unmod-file ebib-file-search-dirs)
          (locate-file (file-name-nondirectory unmod-file) ebib-file-search-dirs)
          (expand-file-name unmod-file)))))

(defun ebib--select-file (files n key)
  "Split FILES into separate files and return the Nth.
FILES should be a string of file names separated by
`ebib-filename-separator'.  If there is only one file name in
FILES, it is returned regardless of the value of N.  If N is nil,
the user is asked to enter a number, unless there is only one
file in FILES, in which case that one is chosen automatically.
If FILES is nil, a file name is created on the basis of KEY.  See
the function `ebib--create-file-name-from-key' for details."
  (if (not files)
      (ebib--create-file-name-from-key key "pdf")
    (let ((file-list (split-string files (regexp-quote ebib-filename-separator) t)))
      (cond
       ((= (length file-list) 1)
        (setq n 1))
       ((null n)
        (setq n (string-to-number (read-string (format "Select file [1-%d]: " (length file-list)))))))
      (unless (<= 1 n (length file-list))  ; unless n is within range
        (error "[Ebib] No such file (%d)" n))
      (nth (1- n) file-list))))

(defun ebib--select-url (urls n)
  "Select a URL from URLS.
URLS is a string containing one or more URLs.  URLS is split
using `ebib-url-regexp' and the Nth URL is returned.  If N is
nil, the user is asked which URL to select, unless there is only
one.  If URLS is nil or does not contain any valid URLs, raise an
error."
  (unless urls
    (error "[Ebib] No URLs found"))
  (setq urls (let ((start 0)
                   (result nil))
               (while (string-match ebib-url-regexp urls start)
                 (push (match-string 0 urls) result)
                 (setq start (match-end 0)))
               (nreverse result)))
  (unless urls
    (error "[Ebib] No valid URLs found"))
  (cond
   ((= (length urls) 1)
    (setq n 1))
   ((null n) ; the user didn't provide a numeric prefix argument
    (setq n (string-to-number (read-string (format "Select URL to open [1-%d]: " (length urls)))))))
  (unless (<= 1 n (length urls))  ; unless n is within range
    (error "[Ebib] No such URL (%d)" n))
  (let ((url (nth (1- n) urls)))
    (if (string-match "\\\\url{\\(.*?\\)}" url) ; see if the url is contained in \url{...}
        (setq url (match-string 1 url)))
    url))

(defun ebib-create-org-identifier (key _)
  "Create a unique identifier for KEY for use in an org file.
The key is prepended with the string \"Custom_id:\", so that it
can be used in a :PROPERTIES: block."
  (format ":Custom_id: %s" key))

(defun ebib-create-org-title (key db)
  "Return a title for an orgmode note for KEY in DB.
The title is formed from the author(s) or editor(s) of the entry,
its year and its title.  Newlines are removed from the resulting
string."
  (let ((author (or (ebib-db-get-field-value "author" key db 'noerror 'unbraced 'xref)
                    (ebib-db-get-field-value "editor" key db 'noerror 'unbraced 'xref)
                    "(No Author)"))
        (year (or (ebib-db-get-field-value "year" key db 'noerror 'unbraced 'xref)
                  "????"))
        (title (or (ebib-db-get-field-value "title" key db 'noerror 'unbraced 'xref)
                   "(No Title)")))
    (remove ?\n (format "%s (%s): %s" author year title))))

(defun ebib-create-org-link (key db)
  "Create an org link for KEY in DB.
Check the entry designated by KEY whether it has a file, a doi or
a URL (in that order) and use the first element found to create
an org link.  If none of these elements is found, return the
empty string."
  (cond
   ((ebib-db-get-field-value ebib-file-field key db 'noerror 'unbraced 'xref)
    (ebib-create-org-file-link key db))
   ((ebib-db-get-field-value ebib-doi-field key db 'noerror 'unbraced 'xref)
    (ebib-create-org-doi-link key db))
   ((ebib-db-get-field-value ebib-url-field key db 'noerror 'unbraced 'xref)
    (ebib-create-org-url-link key db))
   (t "")))

(defun ebib-create-org-file-link (key db)
  "Create an org link to the file in entry KEY in DB.
The file is taken from `ebib-file-field' in the entry designated
by KEY in the current database.  If that field contains more than
one file name, the user is asked to select one.  If
`ebib-file-field' is empty, create a file name based on KEY,
using the function `ebib--create-file-name-from-key'."
  (let ((files (ebib-db-get-field-value ebib-file-field key db 'noerror 'unbraced 'xref)))
    (format "[[file:%s]]" (ebib--expand-file-name (ebib--select-file files nil key)))))

(defun ebib-create-org-doi-link (key db)
  "Create an org link to the DOI in entry KEY in DB.
The file is taken from `ebib-doi-field' in the entry designated
by KEY in the current database.  If `ebib-doi-field' is empty,
return the empty string."
  (let ((doi (ebib-db-get-field-value ebib-doi-field key db 'noerror 'unbraced 'xref)))
    (if doi (format "[[doi:%s]]" doi) "")))

(defun ebib-create-org-url-link (key db)
  "Create an org link to the URL in entry KEY in DB.
The URL is taken from `ebib-url-field' in the entry designated by
KEY in the current database.  If that field contains more than
one url, the user is asked to select one.  If `ebib-url-field' is
empty, return the empty string."
  (let* ((urls (ebib-db-get-field-value ebib-url-field key db 'noerror 'unbraced 'xref))
         (url (ebib--select-url urls nil)))
    (if url (format "[[%s]]" url) "")))

(defun ebib-format-template (template specifiers &rest args)
  "Format TEMPLATE using SPECIFIERS.
SPECIFIERS is an alist of characters and symbols.  Each symbol
should be the name of a function that takes ARGS as arguments and
returns a string which is substituted for the specifier in
TEMPLATE.  Specs in SPECIFIERS that do not occur in TEMPLATE are
ignored."
  ;; First remove specs that do not occur in TEMPLATE.  In principle,
  ;; `format-spec' ignores all specs that do not occur in the template, but we
  ;; do not want to apply the functions of specs that are not needed.
  (setq specifiers (cl-remove-if-not (lambda (elt)
                                       (string-match-p (format "%%%c" (car elt)) template))
                                     specifiers))
  (format-spec template (delq nil (mapcar (lambda (spec)
                                            (let* ((replacer (cdr spec))
                                                   (replacement (if (fboundp replacer)
                                                                    (ignore-errors (apply (cdr spec) args))
                                                                  (symbol-value replacer))))
                                              (if replacement
                                                  (cons (car spec) replacement))))
                                          specifiers))))

(defun ebib--multiline-p (string)
  "Return non-nil if STRING is multiline."
  (if (stringp string)
      (string-match-p "\n" string)))

(defsubst ebib--first-line (string)
  "Return the first line of a multiline STRING."
  (car (split-string string "\n")))

(defun ebib--match-all-in-string (match-str string)
  "Highlight all the matches of MATCH-STR in STRING.
The return value is a list of two elements: the first is the
modified string, the second either t or nil, indicating whether a
match was found at all."
  (cl-do ((counter 0 (match-end 0)))
      ((not (string-match match-str string counter)) (cl-values string (not (= counter 0))))
    (add-text-properties (match-beginning 0) (match-end 0) '(face highlight) string)))

(defun ebib--looking-at-goto-end (regexp &optional match)
  "Return t if text after point matches REGEXP and move point.
MATCH acts just like the argument to MATCH-END, and defaults to
0."
  (or match (setq match 0))
  (let ((case-fold-search t))
    (if (looking-at regexp)
        (goto-char (match-end match)))))

(defun ebib--special-field-p (field)
  "Return t if FIELD is a special field.
Speciald fields are those whose names start and end with an equal sign."
  (string-match-p "\\`=[[:alpha:]]*=\\'" field))

(defun ebib--local-vars-to-list (str)
  "Convert STR to a list of local variables.
STR must start with \"Local Variables:\" and end with \"End:\".
The return value is a list of lists, where each sublist has the
form (\"<variable>\" \"<value>\"). If STR is not a local variable
block, the return value is nil."
  (let ((vars (split-string str "[{}\n]+" t "[{} \t\r]+")))
    (when (and (string= (car vars) "Local Variables:")
               (string= (-last-item vars) "End:"))
      (--map (split-string it ": " t "[ \t]+") (-slice vars 1 -1)))))

(defun ebib--get-dialect (db)
  "Get the dialect of DB.
If DB has no dialect, return the default dialect, as stored in
`ebib-bibtex-dialect'."
  (or (and db (ebib-db-get-dialect db))
      ebib-bibtex-dialect))

(defun ebib--local-vars-add-dialect (vars dialect &optional overwrite)
  "Expand local variable block VARS with DIALECT.
VARS is a list as returned by `ebib--local-vars-to-list'.
DIALECT must be a symbol, possible values are listed in
`bibtex-dialect-list'.  If OVERWRITE is non-nil, overwrite an
existing dialect variable, otherwise do nothing.  The return
value is the (un)modified list."
  (let ((ind (--find-index (string= (car it) "bibtex-dialect") vars)))
    (if ind
        (when overwrite
          (setq vars (-replace-at ind (list "bibtex-dialect" (symbol-name dialect)) vars)))
      (setq vars (push (list "bibtex-dialect" (symbol-name dialect)) vars)))
    vars))

(defun ebib--local-vars-delete-dialect (vars)
  "Delete the dialect definition from VARS.
VARS is a list as returned by `ebib--local-vars-to-list'.  VARS is
not modified, instead the new list is returned."
  (--remove (string= (car it) "bibtex-dialect") vars))

;; The numeric prefix argument is 1 if the user gave no prefix argument at
;; all. The raw prefix argument is not always a number. So we need to do
;; our own conversion.
(defun ebib--prefix (num)
  "Return NUM if it is a number, otherwise return nil.
This can be used to check if the user provided a numeric prefix
argument to a function or not."
  (when (numberp num)
    num))

(defun ebib--list-fields (entry-type type dialect)
  "List the fields of ENTRY-TYPE.
TYPE specifies which fields to list.  It is a symbol and can be
one of the following: `required' means to list only required
fields; `optional' means to list optional fields; `extra' means
to list extra fields (i.e., fields defined in `ebib--extra-fields'
and not present in ENTRY-TYPE); finally, `all' means to list all
fields.  DIALECT is the BibTeX dialect; possible values are those
listed in `bibtex-dialect-list' or NIL, in which case the value
of `ebib-bibtex-dialect' is used.

If DIALECT is `biblatex' and ENTRY-TYPE is a type alias as
defined by Biblatex, return the fields of the entry type for
which ENTRY-TYPE is an alias."
  (or dialect (setq dialect ebib-bibtex-dialect))
  (if (eq dialect 'biblatex)
      (setq entry-type (or (cdr (assoc-string entry-type ebib--type-aliases 'case-fold))
                           entry-type)))
  (let (required optional extra)
    (when (memq type '(required extra all))
      (setq required (mapcar #'car (append (nth 2 (assoc-string entry-type (bibtex-entry-alist dialect) 'case-fold))
                                           (nth 3 (assoc-string entry-type (bibtex-entry-alist dialect) 'case-fold))))))
    (when (memq type '(optional extra all))
      (setq optional (mapcar #'car (nth 4 (assoc-string entry-type (bibtex-entry-alist dialect) 'case-fold)))))
    (when (memq type '(all extra))
      (setq extra (--remove (member-ignore-case it (append required optional)) (cdr (assq dialect ebib-extra-fields)))))
    (cond
     ((eq type 'required) required)
     ((eq type 'optional) optional)
     ((eq type 'extra) extra)
     ((eq type 'all) (append required optional extra)))))

(defun ebib--list-undefined-fields (entry dialect)
  "Return an alist of fields of ENTRY that are not predefined.
ENTRY is an alist representing a BibTeX entry.  The return value
is an alist of (field . value) pairs of those fields that are not
part of the definition of ENTRY's type and also not part of the
extra fields.

DIALECT is the BibTeX dialect; possible values are those listed
in `bibtex-dialect-list' or NIL, in which case the value of
`ebib-bibtex-dialect' is used."
  (or dialect (setq dialect ebib-bibtex-dialect))
  (let ((fields (ebib--list-fields (cdr (assoc "=type=" entry)) 'all dialect)))
    (--remove (member-ignore-case (car it) (cons "=type=" fields)) entry)))

(defun ebib--list-entry-types (&optional dialect include-aliases)
  "Return a list of entry types.
This list depends on the value of DIALECT, which can have the
values in `bibtex-dialect-list' or NIL, in which case the value
of `ebib-bibtex-dialect' is used.  If INCLUDE-ALIASES is non-NIL,
include entry type aliases as defined by `ebib--type-aliases'."
  (or dialect (setq dialect ebib-bibtex-dialect))
  (append (mapcar #'car (bibtex-entry-alist dialect))
          (if (and include-aliases (eq dialect 'biblatex))
              (mapcar #'car ebib--type-aliases))))

(defvar ebib--unique-field-alist nil
  "Alist of BibTeX dialects and their fields.
This variable is initialized by `ebib--list-field-uniquely'.")

(defun ebib--list-fields-uniquely (dialect)
  "Return a list of all fields of BibTeX DIALECT.
Possible values for DIALECT are those listed in
`bibtex-dialect-list' or nil, in which case the value of
`ebib-bibtex-dialect' is used."
  (or dialect (setq dialect ebib-bibtex-dialect))
  (or (cdr (assq dialect ebib--unique-field-alist))
      (let (fields)
        (mapc (lambda (entry)
                (setq fields (-union fields (ebib--list-fields (car entry) 'all dialect))))
              (bibtex-entry-alist dialect))
        (push (cons dialect fields) ebib--unique-field-alist)
        fields)))

(defun ebib--get-field-value-for-display (field key db)
  "Return the value of FIELD in entry KEY in DB for display.
This function returns a value for FIELD in such a way that it can
be used to display to the user.  If FIELD is found in
`ebib-field-transformation-functions', return the field content
transformed by the associated function.

Otherwise, If FIELD is \"Entry Key\", KEY is returned; if FIELD
is \"Author/Editor\", the contents of the Author field is
returned or, if the Author field is empty, the contents of the
Editor field; if the Editor field is empty as well, return the
string \"(No Author/Editor)\".  These defaults can be overridden
by adding an appropriate transformation function to
`ebib-field-transformation-functions'.

If FIELD is not found in `ebib-field-transformation-functions'
and is not one of the special fields listed above, the field
value is returned.  If a field value is empty, the return value
is the string \"(No <FIELD>)\", or, if FIELD is \"Year\", the
string \"(XXXX)\"."
  (cond
   ((assoc-string field ebib-field-transformation-functions 'case-fold)
    (funcall (cdr (assoc field ebib-field-transformation-functions)) field key db))
   ((cl-equalp field "Entry Key")
    key)
   ((cl-equalp field "Author/Editor")
    (or (ebib-db-get-field-value "Author" key db 'noerror 'unbraced 'xref)
        (ebib-db-get-field-value "Editor" key db "(No Author/Editor)" 'unbraced 'xref)))
   ((cl-equalp field "Year")
    (ebib-db-get-field-value "Year" key db "XXXX" 'unbraced 'xref))
   (t (ebib-db-get-field-value field key db (format "(No %s)" (capitalize field)) 'unbraced 'xref))))

(defun ebib-clean-TeX-markup (field key db)
  "Return the contents of FIELD from KEY in DB without TeX markups."
  (let ((str (ebib-db-get-field-value field key db "" 'unbraced 'xref)))
    ;; First replace TeX commands with their arguments, i.e., \textsc{CLS} ==> CLS.
    ;; Note that optional arguments are also removed.
    (setq str (replace-regexp-in-string "\\\\[a-zA-Z*]*\\(?:\\[.*?\\]\\)*{\\(.*?\\)}" "\\1" str))
    ;; Now replace all remaining braces. This also takes care of nested braces.
    (replace-regexp-in-string "[{}]" "" str)))

(defun ebib-abbreviate-journal-title (field key db)
  "Abbreviate the content of FIELD from KEY in database DB.
This function is intended to abbreviate journal titles.  Short
function words (specifically \"a, an, at, of, on, and\" and
\"for\") are removed, the initial letters of the remaining words
are returned as a string."
  (let ((str (ebib-db-get-field-value field key db "" 'unbraced 'xref)))
    (if (string-match-p "\s+" str)
        (apply 'concat
               (mapcar (lambda (str) (substring str 0 1))
                       (seq-difference
                        (mapcar 'capitalize (split-string str "\\Sw+" t))
                        '("A" "An" "At" "Of" "On" "And" "For"))))
      str)))

(defun ebib-display-note-symbol (_field key _db)
  "Return the note symbol for displaying if a note exists for KEY."
  (if (ebib--notes-exists-note key)
      (propertize ebib-notes-symbol
                  'face '(:height 0.8 :inherit link)
                  'mouse-face 'highlight)
    (propertize (make-string (string-width ebib-notes-symbol) ?\s)
                'face '(:height 0.8))))

(defun ebib-display-www-link (field key db)
  "Return the content of FIELD from KEY in DB as a link.
This function is mainly intended for the DOI and URL fields."
  (let ((str (ebib-db-get-field-value field key db 'noerror 'unbraced 'xref)))
    (if (and str (string-match-p "^[0-9]" str))
        (setq str (concat "https://dx.doi.org/" str)))
    (if str
        (propertize "www"
                    'face '(:height 0.8 :inherit link)
                    'mouse-face 'highlight
                    'help-echo str)
      (propertize "   " 'face '(:height 0.8)))))

(defun ebib--sort-keys-list (keys db)
  "Sort KEYS according to the sort info of DB.
First, the keys are sorted themselves, then the list is stably
sorted on the sort info of DB.  Thus if two entries have the same
value for the sort field, their keys determine the order in which
they appear.

Sorting on the sort field is done with `string-collate-lessp', so
that the order in which the entries appear depends on the user's
locale.  This is only relevant if one uses BibLaTeX and UTF-8
characters in fields."
  ;; First sort the keys themselves.
  (setq keys (sort keys #'string<))
  ;; And then stably sort on the sort field.
  (let* ((field (if (ebib-db-custom-sorted-p db)
                    (ebib-db-get-sort-field db)
                  (caar ebib-index-columns)))
         ;; We use a temp list for sorting, so that the :key argument to
         ;; `cl-stable-sort' can simply be `car' rather than (a much
         ;; heavier) `ebib-db-get-field-value'. Sorting is much faster
         ;; that way.
         (list (mapcar (lambda (key)
                         (cons (ebib--get-field-value-for-display field key db) key))
                       keys)))
    (setq list (cl-stable-sort list (if (fboundp 'string-collate-lessp)
                                        #'string-collate-lessp
                                      #'string-lessp)
                               :key #'car))
    (setq keys (mapcar #'cdr list)))
  ;; Reverse the list if necessary.
  (if (eq (ebib-db-get-sort-order db) 'descend)
      (setq keys (nreverse keys)))
  ;; Now return the list of keys.
  keys)

(provide 'ebib-utils)

;;; ebib-utils.el ends here
