;;; ebib-test.el --- Tests for ebib.el

;;; Commentary:
;; Tests for the ebib package.

;;; Code:

(require 'ebib)
(require 'with-simulated-input)

;;; Tests for creating citations in source documents (LaTeX, Markdown)

;; Test `ebib--split-citation-string'
(ert-deftest ebib--split-citation-without-repeater ()
  (should (equal (ebib--split-citation-string "\\textcite%<[%A]%>%<[%A]%>{%K}")
                 '("\\textcite%<[%A]%>%<[%A]%>{"
                   "%K"
                   nil
                   "}"))))

(ert-deftest ebib--split-citation-with-repeater ()
  (should (equal (ebib--split-citation-string "\\textcite[%A][%A]{%(%K%,)}")
                 '("\\textcite[%A][%A]{"
                   "%K"
                   ","
                   "}"))))

(ert-deftest ebib--split-citation-with-repeater-with-arguments ()
  (should (equal (ebib--split-citation-string  "\\footcites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
                 '("\\footcites%<(%A)%>(%A)"
                   "%<[%A]%>[%A]{%K}"
                   ""
                   ""))))

;; Test `ebib--create-citation'
(ert-deftest ebib--create-citation-with-repeater ()
  (should (equal (with-simulated-input "cite RET cf. RET RET"
                   (ebib--create-citation 'latex-mode '("Chomsky1965" "Abney1987")))
                 "\\cite[cf.][]{Chomsky1965,Abney1987}")))

(ert-deftest ebib--create-citation-with-repeater-with-empty-arguments ()
  (should (equal (with-simulated-input "foots RET RET RET RET RET cf. RET a.o. RET"
                   (ebib--create-citation 'latex-mode '("Chomsky1965" "Abney1987")))
                 "\\footcites(cf.)(a.o.)[]{Chomsky1965}[]{Abney1987}")))

(ert-deftest ebib--create-citation-with-repeater-with-arguments ()
  (should (equal (with-simulated-input "foots RET RET p. SPC 40 RET RET RET cf. RET a.o. RET"
                   (ebib--create-citation 'latex-mode '("Chomsky1965" "Abney1987")))
                 "\\footcites(cf.)(a.o.)[p. 40]{Chomsky1965}[]{Abney1987}")))

(ert-deftest ebib--create-citation-org-mode ()
  (should (equal (with-simulated-input "ebib RET RET"
                   (let ((ebib-citation-description-function (lambda (_ _)
                                                               "Abney (1987)")))
                     (ebib--create-citation 'org-mode '("Abney1987") t)))
                 "[[ebib:Abney1987][Abney (1987)]]")))

;; Test `ebib--process-citation'.
(ert-deftest ebib-citation-with-empty-template-should-come-through ()
  (should (equal (ebib--process-citation-template "" "Abney1987")
                 "")))

(ert-deftest ebib-citation-with-no-directives-in-template-should-come-through ()
  (should (equal (ebib--process-citation-template "nodirectiveshere" "Abney1987")
                 "nodirectiveshere")))

(ert-deftest ebib-citation-with-key-directive-should-catch-the-key ()
  (should (equal (ebib--process-citation-template "%K" "Abney1987")
                 "Abney1987")))

(ert-deftest ebib-citation-with-key-directive-and-surrounding-material-should-get-key-with-the-surrounded-material ()
  (should (equal (ebib--process-citation-template "before %K after" "Abney1987")
                 "before Abney1987 after")))

(ert-deftest ebib-citation-with-argument-directive-should-prompt-the-user ()
  (should (equal (with-simulated-input "cf. RET"
                   (ebib--process-citation-template "%A" "Abney1987"))
                 "cf.")))

(ert-deftest ebib-citation-with-argument-directive-should-accept-spaces-in-args ()
  (should (equal (with-simulated-input "cf. SPC also RET"
                   (ebib--process-citation-template "%A" "Abney1987"))
                 "cf. also")))

(ert-deftest ebib-citation-with-key-and-argument-directives-should-prompt-the-user ()
  (should (equal (with-simulated-input "p. SPC 20 RET"
                   (ebib--process-citation-template "%K %A" "Abney1987"))
                 "Abney1987 p. 20")))

(ert-deftest ebib-citation-with-argument-and-key-directives-should-prompt-the-user ()
  (should (equal (with-simulated-input "cf. RET"
                   (ebib--process-citation-template "%A %K" "Abney1987"))
                 "cf. Abney1987")))

(ert-deftest ebib-citation-with-argument-and-key-and-argument-directives-should-prompt-the-user ()
  (should (equal (with-simulated-input "cf. RET p. SPC 20 RET"
                   (ebib--process-citation-template "%A %K %A" "Abney1987"))
                 "cf. Abney1987 p. 20")))


;; TODO: Add tests for
;; `ebib-citation-prompt-with-format-string`
;; which is what this branch is about anyway.

;; TODO: A bit of integration testing here would be nice, say to grab some
;; of the defined templates from e.g. MarkDown format and pushing a bit more
;; realistic arguments through them, e.g. "c.f." and "pp. 22-25".

;;; Tests for `ebib-clean-TeX-markup'

(ert-deftest ebib-clean-TeX-markup-dashes ()
  (should (equal (ebib-clean-TeX-markup "---") "—"))
  (should (equal (ebib-clean-TeX-markup "\\textemdash") "—"))
  (should (equal (ebib-clean-TeX-markup "\\textemdash and") "—and"))
  (should (equal (ebib-clean-TeX-markup "\\textemdash  and") "—and"))
  (should (equal (ebib-clean-TeX-markup "\\textemdash{}") "—"))
  (should (equal (ebib-clean-TeX-markup "\\textemdash{}and") "—and"))
  (should (equal (ebib-clean-TeX-markup "\\textemdash{} and") "— and"))
  (should (equal (ebib-clean-TeX-markup "\\textemdash{}  and") "— and"))

  (should (equal (ebib-clean-TeX-markup "--") "–"))
  (should (equal (ebib-clean-TeX-markup "\\textendash") "–"))
  (should (equal (ebib-clean-TeX-markup "\\textendash{}") "–")))

(ert-deftest ebib-clean-TeX-markup-math-and-text-mode-commands ()
  (should (equal (ebib-clean-TeX-markup "\\ddag{} \\textdaggerdbl") "‡ ‡"))
  (should (equal (ebib-clean-TeX-markup "10\\textpertenthousand") "10‱"))
  (should (equal (ebib-clean-TeX-markup "200\\textperthousand.") "200‰."))
  (should (equal (ebib-clean-TeX-markup "\\textquestiondown") "¿"))
  (should (equal (ebib-clean-TeX-markup "\\P 3.2") "¶3.2"))
  (should (equal (ebib-clean-TeX-markup "\\$ \\textdollar") "$$"))
  (should (equal (ebib-clean-TeX-markup "\\S 5.2") "§5.2"))
  (should (equal (ebib-clean-TeX-markup "\\ldots{} [\\dots] \\textellipsis and")
                 "… […] …and")))

(ert-deftest ebib-clean-TeX-markup-nonletter-diacritics-without-braces ()
  ;; No space is needed after a nonletter diacritic commands.
  (should (equal (ebib-clean-TeX-markup "\\\"a") "a\N{COMBINING DIAERESIS}"))
  (should (equal (ebib-clean-TeX-markup "\\'a")  "a\N{COMBINING ACUTE ACCENT}"))
  (should (equal (ebib-clean-TeX-markup "\\.a")  "a\N{COMBINING DOT ABOVE}"))
  (should (equal (ebib-clean-TeX-markup "\\=a")  "a\N{COMBINING MACRON}"))
  (should (equal (ebib-clean-TeX-markup "\\^a")  "a\N{COMBINING CIRCUMFLEX ACCENT}"))
  (should (equal (ebib-clean-TeX-markup "\\`a")  "a\N{COMBINING GRAVE ACCENT}"))
  (should (equal (ebib-clean-TeX-markup "\\~a")  "a\N{COMBINING TILDE}"))
  (should (equal (ebib-clean-TeX-markup "\\|a")  "a\N{COMBINING COMMA ABOVE}"))
  ;; Spaces are possible, though:
  (should (equal (ebib-clean-TeX-markup "\\' a")  "a\N{COMBINING ACUTE ACCENT}"))
  (should (equal (ebib-clean-TeX-markup "\\'  a")  "a\N{COMBINING ACUTE ACCENT}")))

(ert-deftest ebib-clean-TeX-markup-letter-diacritics-without-braces ()
  ;; Diacritic commands that consist of a single letter require a space.
  (should (equal (ebib-clean-TeX-markup "\\b a") "a\N{COMBINING MACRON BELOW}"))
  (should (equal (ebib-clean-TeX-markup "\\c c") "c\N{COMBINING CEDILLA}"))
  (should (equal (ebib-clean-TeX-markup "\\d a") "a\N{COMBINING DOT BELOW}"))
  (should (equal (ebib-clean-TeX-markup "\\H a") "a\N{COMBINING DOUBLE ACUTE ACCENT}"))
  (should (equal (ebib-clean-TeX-markup "\\k a") "a\N{COMBINING OGONEK}"))
  (should (equal (ebib-clean-TeX-markup "\\U a") "a\N{COMBINING DOUBLE VERTICAL LINE ABOVE}"))
  (should (equal (ebib-clean-TeX-markup "\\u a") "a\N{COMBINING BREVE}"))
  (should (equal (ebib-clean-TeX-markup "\\v a") "a\N{COMBINING CARON}"))
  (should (equal (ebib-clean-TeX-markup "\\f a") "a\N{COMBINING INVERTED BREVE}"))
  (should (equal (ebib-clean-TeX-markup "\\G a") "a\N{COMBINING DOUBLE GRAVE ACCENT}"))
  (should (equal (ebib-clean-TeX-markup "\\h a") "a\N{COMBINING HOOK ABOVE}"))
  (should (equal (ebib-clean-TeX-markup "\\C a") "a\N{COMBINING DOUBLE GRAVE ACCENT}"))
  (should (equal (ebib-clean-TeX-markup "\\r a") "a\N{COMBINING RING ABOVE}"))
  ;; More than one space should also work:
  (should (equal (ebib-clean-TeX-markup "\\b  a") "a\N{COMBINING MACRON BELOW}"))
  (should (equal (ebib-clean-TeX-markup "\\b   a") "a\N{COMBINING MACRON BELOW}"))
  ;; It shouldn't work without space. Since something like "\ba after" is
  ;; essentially a command without an (explicit) argument, it should remain
  ;; unchanged.
  (should (equal (ebib-clean-TeX-markup "before \\ba after") "before \\ba after")))

(ert-deftest ebib-clean-TeX-markup-diacritics-with-braces ()
  ;; Diacritic commands may use braces to mark the argument.
  (should (equal (ebib-clean-TeX-markup "\\\"{a}") "a\N{COMBINING DIAERESIS}"))
  (should (equal (ebib-clean-TeX-markup "\\'{a}")  "a\N{COMBINING ACUTE ACCENT}"))
  (should (equal (ebib-clean-TeX-markup "\\.{a}")  "a\N{COMBINING DOT ABOVE}"))
  (should (equal (ebib-clean-TeX-markup "\\={a}")  "a\N{COMBINING MACRON}"))
  (should (equal (ebib-clean-TeX-markup "\\^{a}")  "a\N{COMBINING CIRCUMFLEX ACCENT}"))
  (should (equal (ebib-clean-TeX-markup "\\`{a}")  "a\N{COMBINING GRAVE ACCENT}"))
  (should (equal (ebib-clean-TeX-markup "\\b{a}")  "a\N{COMBINING MACRON BELOW}"))
  (should (equal (ebib-clean-TeX-markup "\\c{c}")  "c\N{COMBINING CEDILLA}"))
  (should (equal (ebib-clean-TeX-markup "\\d{a}")  "a\N{COMBINING DOT BELOW}"))
  (should (equal (ebib-clean-TeX-markup "\\H{a}")  "a\N{COMBINING DOUBLE ACUTE ACCENT}"))
  (should (equal (ebib-clean-TeX-markup "\\k{a}")  "a\N{COMBINING OGONEK}"))
  (should (equal (ebib-clean-TeX-markup "\\U{a}")  "a\N{COMBINING DOUBLE VERTICAL LINE ABOVE}"))
  (should (equal (ebib-clean-TeX-markup "\\u{a}")  "a\N{COMBINING BREVE}"))
  (should (equal (ebib-clean-TeX-markup "\\v{a}")  "a\N{COMBINING CARON}"))
  (should (equal (ebib-clean-TeX-markup "\\~{a}")  "a\N{COMBINING TILDE}"))
  (should (equal (ebib-clean-TeX-markup "\\|{a}")  "a\N{COMBINING COMMA ABOVE}"))
  (should (equal (ebib-clean-TeX-markup "\\f{a}")  "a\N{COMBINING INVERTED BREVE}"))
  (should (equal (ebib-clean-TeX-markup "\\G{a}")  "a\N{COMBINING DOUBLE GRAVE ACCENT}"))
  (should (equal (ebib-clean-TeX-markup "\\h{a}")  "a\N{COMBINING HOOK ABOVE}"))
  (should (equal (ebib-clean-TeX-markup "\\C{a}")  "a\N{COMBINING DOUBLE GRAVE ACCENT}"))
  (should (equal (ebib-clean-TeX-markup "\\r{a}")  "a\N{COMBINING RING ABOVE}"))
  ;; There may be spaces between the command and the argument.
  (should (equal (ebib-clean-TeX-markup "\\' {a}")  "a\N{COMBINING ACUTE ACCENT}"))
  (should (equal (ebib-clean-TeX-markup "\\'  {a}")  "a\N{COMBINING ACUTE ACCENT}")))

(ert-deftest ebib-clean-TeX-markup-escapable-characters ()
  (should (equal (ebib-clean-TeX-markup "percent: \\%  ampersand: \\&  hash: \\#")
                 "percent: %  ampersand: &  hash: #")))

(ert-deftest ebib-clean-TeX-markup-quotes ()
  (should (equal (ebib-clean-TeX-markup "``double'' quotes") "\N{LEFT DOUBLE QUOTATION MARK}double\N{RIGHT DOUBLE QUOTATION MARK} quotes"))
  (should (equal (ebib-clean-TeX-markup "`single' quotes") "\N{LEFT SINGLE QUOTATION MARK}single\N{RIGHT SINGLE QUOTATION MARK} quotes")))

(ert-deftest ebib-clean-TeX-markup-textit ()
  (should (equal-including-properties
           (ebib-clean-TeX-markup "The verb \\textit{krijgen}")
           #("The Verb krijgen" 9 16
             (face italic)))))

(ert-deftest ebib-clean-TeX-markup-emph ()
  (should (equal-including-properties
           (ebib-clean-TeX-markup "The verb \\emph{krijgen}")
           #("The Verb krijgen" 9 16
             (face italic)))))

(ert-deftest ebib-clean-TeX-markup-textbf ()
  (should (equal-including-properties
           (ebib-clean-TeX-markup "The verb \\textbf{krijgen}")
           #("The Verb krijgen" 9 16
             (face bold)))))

(ert-deftest ebib-clean-TeX-markup-textsc ()
  (should (equal
           (ebib-clean-TeX-markup "The verb \\textsc{krijgen}")
           "The verb KRIJGEN")))

(ert-deftest ebib-clean-TeX-markup-nonascii-letters-with-braces ()
  ;; The braces should be removed and the space after it retained.
  (should (equal (ebib-clean-TeX-markup "\\AA{} and") "\N{LATIN CAPITAL LETTER A WITH RING ABOVE} and"))
  (should (equal (ebib-clean-TeX-markup "\\AE{} and") "\N{LATIN CAPITAL LETTER AE} and"))
  (should (equal (ebib-clean-TeX-markup "\\DH{} and") "\N{LATIN CAPITAL LETTER ETH} and"))
  (should (equal (ebib-clean-TeX-markup "\\DJ{} and") "\N{LATIN CAPITAL LETTER ETH} and"))
  (should (equal (ebib-clean-TeX-markup "\\L{} and")  "\N{LATIN CAPITAL LETTER L WITH STROKE} and"))
  (should (equal (ebib-clean-TeX-markup "\\SS{} and") "\N{LATIN CAPITAL LETTER SHARP S} and"))
  (should (equal (ebib-clean-TeX-markup "\\NG{} and") "\N{LATIN CAPITAL LETTER ENG} and"))
  (should (equal (ebib-clean-TeX-markup "\\OE{} and") "\N{LATIN CAPITAL LIGATURE OE} and"))
  (should (equal (ebib-clean-TeX-markup "\\O{} and")  "\N{LATIN CAPITAL LETTER O WITH STROKE} and"))
  (should (equal (ebib-clean-TeX-markup "\\TH{} and") "\N{LATIN CAPITAL LETTER THORN} and"))
  (should (equal (ebib-clean-TeX-markup "\\aa{} and") "\N{LATIN SMALL LETTER A WITH RING ABOVE} and"))
  (should (equal (ebib-clean-TeX-markup "\\ae{} and") "\N{LATIN SMALL LETTER AE} and"))
  (should (equal (ebib-clean-TeX-markup "\\dh{} and") "\N{LATIN SMALL LETTER ETH} and"))
  (should (equal (ebib-clean-TeX-markup "\\dj{} and") "\N{LATIN SMALL LETTER ETH} and"))
  (should (equal (ebib-clean-TeX-markup "\\l{} and")  "\N{LATIN SMALL LETTER L WITH STROKE} and"))
  (should (equal (ebib-clean-TeX-markup "\\ss{} and") "\N{LATIN SMALL LETTER SHARP S} and"))
  (should (equal (ebib-clean-TeX-markup "\\ng{} and") "\N{LATIN SMALL LETTER ENG} and"))
  (should (equal (ebib-clean-TeX-markup "\\oe{} and") "\N{LATIN SMALL LIGATURE OE} and"))
  (should (equal (ebib-clean-TeX-markup "\\o{} and")  "\N{LATIN SMALL LETTER O WITH STROKE} and"))
  (should (equal (ebib-clean-TeX-markup "\\th{} and") "\N{LATIN SMALL LETTER THORN} and"))
  (should (equal (ebib-clean-TeX-markup "\\ij{} and") "ij and"))
  (should (equal (ebib-clean-TeX-markup "\\i{} and")  "\N{LATIN SMALL LETTER DOTLESS I} and"))
  (should (equal (ebib-clean-TeX-markup "\\j{} and")  "\N{LATIN SMALL LETTER DOTLESS J} and"))
  ;; More than one space should work as well.
  (should (equal (ebib-clean-TeX-markup "\\AA{}  and")  "\N{LATIN CAPITAL LETTER A WITH RING ABOVE} and"))
  (should (equal (ebib-clean-TeX-markup "\\AA{}   and") "\N{LATIN CAPITAL LETTER A WITH RING ABOVE} and")))

(ert-deftest ebib-clean-TeX-markup-nonascii-letters-without-braces ()
  ;; The space should be removed.
  (should (equal (ebib-clean-TeX-markup "\\AA n") "\N{LATIN CAPITAL LETTER A WITH RING ABOVE}n"))
  (should (equal (ebib-clean-TeX-markup "\\AE n") "\N{LATIN CAPITAL LETTER AE}n"))
  (should (equal (ebib-clean-TeX-markup "\\DH n") "\N{LATIN CAPITAL LETTER ETH}n"))
  (should (equal (ebib-clean-TeX-markup "\\DJ n") "\N{LATIN CAPITAL LETTER ETH}n"))
  (should (equal (ebib-clean-TeX-markup "\\L n")  "\N{LATIN CAPITAL LETTER L WITH STROKE}n"))
  (should (equal (ebib-clean-TeX-markup "\\SS n") "\N{LATIN CAPITAL LETTER SHARP S}n"))
  (should (equal (ebib-clean-TeX-markup "\\NG n") "\N{LATIN CAPITAL LETTER ENG}n"))
  (should (equal (ebib-clean-TeX-markup "\\OE n") "\N{LATIN CAPITAL LIGATURE OE}n"))
  (should (equal (ebib-clean-TeX-markup "\\O n")  "\N{LATIN CAPITAL LETTER O WITH STROKE}n"))
  (should (equal (ebib-clean-TeX-markup "\\TH n") "\N{LATIN CAPITAL LETTER THORN}n"))
  (should (equal (ebib-clean-TeX-markup "\\aa n") "\N{LATIN SMALL LETTER A WITH RING ABOVE}n"))
  (should (equal (ebib-clean-TeX-markup "\\ae n") "\N{LATIN SMALL LETTER AE}n"))
  (should (equal (ebib-clean-TeX-markup "\\dh n") "\N{LATIN SMALL LETTER ETH}n"))
  (should (equal (ebib-clean-TeX-markup "\\dj n") "\N{LATIN SMALL LETTER ETH}n"))
  (should (equal (ebib-clean-TeX-markup "\\l n")  "\N{LATIN SMALL LETTER L WITH STROKE}n"))
  (should (equal (ebib-clean-TeX-markup "\\ss n") "\N{LATIN SMALL LETTER SHARP S}n"))
  (should (equal (ebib-clean-TeX-markup "\\ng n") "\N{LATIN SMALL LETTER ENG}n"))
  (should (equal (ebib-clean-TeX-markup "\\oe n") "\N{LATIN SMALL LIGATURE OE}n"))
  (should (equal (ebib-clean-TeX-markup "\\o n")  "\N{LATIN SMALL LETTER O WITH STROKE}n"))
  (should (equal (ebib-clean-TeX-markup "\\th n") "\N{LATIN SMALL LETTER THORN}n"))
  (should (equal (ebib-clean-TeX-markup "\\ij n") "ijn"))
  (should (equal (ebib-clean-TeX-markup "\\i n")  "\N{LATIN SMALL LETTER DOTLESS I}n"))
  (should (equal (ebib-clean-TeX-markup "\\j n")  "\N{LATIN SMALL LETTER DOTLESS J}n"))
  ;; More than one space should work as well.
  (should (equal (ebib-clean-TeX-markup "\\AA  n")  "\N{LATIN CAPITAL LETTER A WITH RING ABOVE}n"))
  (should (equal (ebib-clean-TeX-markup "\\AA   n") "\N{LATIN CAPITAL LETTER A WITH RING ABOVE}n"))
  ;; If there is no space, treat it as an unknown command.
  (should (equal (ebib-clean-TeX-markup "\\AAn")  "\\AAn")))

(ert-deftest ebib-clean-TeX-markup-other-commands ()
  ;; Do not change commands with no arguments.
  (should (equal (ebib-clean-TeX-markup "\\LaTeX and") "\\LaTeX and"))
  ;; Commands with an empty set of braces should remain, the braces should be removed.
  (should (equal (ebib-clean-TeX-markup "\\LaTeX{} and") "\\LaTeX and"))
  ;; Obligatory arguments should replace the command.
  (should (equal (ebib-clean-TeX-markup "\\foo{bar} and") "bar and"))
  ;; Optional arguments should be removed, even empty ones.
  (should (equal (ebib-clean-TeX-markup "\\foo[]{bar} and") "bar and"))
  (should (equal (ebib-clean-TeX-markup "\\foo[bar]{baz} and") "baz and"))
  (should (equal (ebib-clean-TeX-markup "\\foo[bar][baz]{boo} and") "boo and"))
  (should (equal (ebib-clean-TeX-markup "\\foo[bar][baz]{} and") "foo and")))

(ert-deftest ebib-clean-TeX-markup-braces ()
  ;; Braces not part of a command should be removed.
  (should (equal (ebib-clean-TeX-markup "The {UN} should be all-caps.") "The UN should be all-caps.")))

;;; ebib-test.el ends here
