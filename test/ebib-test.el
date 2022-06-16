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

;;; ebib-test.el ends here
