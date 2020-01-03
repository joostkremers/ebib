;;; ebib-test.el --- Tests for ebib.el

;;; Commentary:
;; Tests for the ebib package.

;;; Code:

(require 'ebib)

;; Test the citation template processing logic.
(ert-deftest ebib-citation-with-empty-template-should-come-through ()
  (should (string= (ebib--process-citation-template "" "key")
                   "")))

(ert-deftest ebib-citation-with-no-directives-in-template-should-come-through ()
  (should (string= (ebib--process-citation-template "nodirectiveshere" "key")
                   "nodirectiveshere")))

(ert-deftest ebib-citation-with-key-directive-should-catch-the-key ()
  (should (string= (ebib--process-citation-template "%K" "key")
                   "key")))

(ert-deftest ebib-citation-with-key-directive-and-surrounding-material-should-get-key-with-the-surrounded-material ()
  (should (string= (ebib--process-citation-template "before%Kafter" "key")
                   "beforekeyafter")))

(ert-deftest ebib-citation-with-argument-directive-should-prompt-the-user ()
  (let ((unread-command-events (listify-key-sequence (kbd "lizard RET"))))
    (should (string= (ebib--process-citation-template "%A" "key")
                     "lizard"))))

(ert-deftest ebib-citation-with-argument-directive-should-accept-spaces-in-args ()
  (let ((unread-command-events (listify-key-sequence (kbd "lizard SPC snake RET"))))
    (should (string= (ebib--process-citation-template "%A" "key")
                     "lizard snake"))))

(ert-deftest ebib-citation-with-key-and-argument-directives-should-prompt-the-user ()
  (let ((unread-command-events (listify-key-sequence (kbd "lizard RET"))))
    (should (string= (ebib--process-citation-template "%k %A" "key")
                     "key lizard"))))

(ert-deftest ebib-citation-with-argument-and-key-directives-should-prompt-the-user ()
  (let ((unread-command-events (listify-key-sequence (kbd "lizard RET"))))
    (should (string= (ebib--process-citation-template "%A%k" "key")
                     "lizardkey"))))

(ert-deftest ebib-citation-with-argument-and-key-and-argument-directives-should-prompt-the-user ()
  (let ((unread-command-events (listify-key-sequence (kbd "lizard RET snake RET"))))
    (should (string= (ebib--process-citation-template "%A%k%A" "key")
                     "lizardkeysnake"))))

(ert-deftest ebib-citation-with-a-key-and-arguments-should-accept-spaces-in-args ()
  (let ((unread-command-events (listify-key-sequence (kbd "liz SPC ard RET sna SPC ke RET"))))
    (should (string= (ebib--process-citation-template "%A%k%A" "key")
                     "liz ardkeysna ke"))))

;; TODO: A bit of integration testing here would be nice, say to grab some
;; of the defined templates from the format and pushing a bit more
;; realistic arguments through them, e.g. "c.f." and "pp. 22-25".

;;; ebib-test.el ends here
