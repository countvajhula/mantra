;; Note: we want to retain dynamic binding for these tests because the
;; ERT "fixtures" rely on it.

;; To run the tests from within Emacs, you must `eval-buffer` this test
;; buffer first. Then, run tests using `ert-run-tests-interactively`.
;; But, to avoid having to evaluate the changes (which may affect the live
;; environment), it may be preferable to `make test` at the shell, instead.

;; Notes:
;; - If you see "lisp nesting exceeds max-lisp-eval-depth"
;;   while running these tests, it could be that you have a duplicate
;;   "body" invocation within one of the nested fixtures. Since these
;;   are dynamically bound, every fixture needs to have a distinct
;;   name for the body argument.
;; - If you see errors like "(void-function t)", "(void-function nil)"
;;   and "invalid function nil . 0"
;;   then you probably are using a fixture without wrapping the body
;;   in a lambda

;; Add source paths to load path so the tests can find the source files
;; Adapted from:
;; https://github.com/Lindydancer/cmake-font-lock/blob/47687b6ccd0e244691fb5907aaba609e5a42d787/test/cmake-font-lock-test-setup.el#L20-L27
(defvar mantra-test-setup-directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(dolist (dir '("." ".."))
  (add-to-list 'load-path
               (concat mantra-test-setup-directory dir)))
;;

(require 'mantra)

;;
;; Fixtures
;;


;; fixture recipe from:
;; https://www.gnu.org/software/emacs/manual/html_node/ert/Fixtures-and-Test-Suites.html


(defmacro with-fixture (fixture &rest test)
  "Run TEST using FIXTURE."
  (declare (indent 1))
  `(,fixture
    (lambda ()
      ,@test)))

(defconst fixture-parser-accept-all-name "all")

(defconst fixture-parser-accept-none-name "none")

(defconst fixture-parser-abort-all-name "abort")

(defun fixture-parser-accept-all (body)
  (let ((parser nil))
    (unwind-protect
        (progn (setq parser (mantra-make-parser fixture-parser-accept-all-name
                                                (lambda (_key-seq) t)
                                                (lambda (_key-seq) t)
                                                (lambda (_key-seq) nil)))
               (funcall body))
      ;; perhaps aid garbage collection
      (setq parser nil))))

(defun fixture-parser-accept-none (body)
  (let ((parser nil))
    (unwind-protect
        (progn (setq parser (mantra-make-parser fixture-parser-accept-none-name
                                                (lambda (_key-seq) nil)
                                                (lambda (_key-seq) nil)
                                                (lambda (_key-seq) nil)))
               (funcall body))
      ;; perhaps aid garbage collection
      (setq parser nil))))

(defun fixture-parser-abort-all (body)
  (let ((parser nil))
    (unwind-protect
        (progn (setq parser (mantra-make-parser fixture-parser-abort-all-name
                                                (lambda (_key-seq) t)
                                                (lambda (_key-seq) t)
                                                (lambda (_key-seq) t)))
               (funcall body))
      ;; perhaps aid garbage collection
      (setq parser nil))))

(defvar fixture-single-key [108]
  "A sequence representing a single key press (the letter l).")

(defvar fixture-multi-key [3 102]
  "A sequence representing a multi key event (C-c f).")

(defvar fixture-key-sequence [108 3 102]
  "A sequence representing a composed key sequence (l C-c f).")

(defun fixture-parser-with-state (body-1)
  (with-fixture fixture-parser-accept-all
    (mantra-parser-set-state parser fixture-single-key)
    (funcall body-1)))

(defun fixture-abort-parser-with-state (body-2)
  (with-fixture fixture-parser-abort-all
    (mantra-parser-set-state parser fixture-single-key)
    (funcall body-2)))

(defmacro with-key-listening (&rest test)
  (declare (indent 0))
  `(unwind-protect
        (progn (mantra-register parser)
               ,@test)
      (pop mantra-parsers)))

(defun fixture-subscriber (body-2)
  (let* ((result nil)
         (subscriber (lambda (parsed-keys)
                       (setq result parsed-keys))))
    (pubsub-subscribe (mantra-parser-name parser)
                      subscriber)
    (funcall body-2)))

;;
;; Tests
;;

(ert-deftest mantra-parser-test ()
  ;; null constructor
  (should (vectorp (mantra-make-parser "test"
                                       (lambda (_key-seq) t)
                                       (lambda (_key-seq) t)
                                       (lambda (_key-seq) nil))))

  ;; mantra-parser-name
  (with-fixture fixture-parser-accept-all
    (should (stringp (mantra-parser-name parser))))

  ;; mantra-parser-start
  (with-fixture fixture-parser-accept-all
    (should (functionp (mantra-parser-start parser))))

  ;; mantra-parser-stop
  (with-fixture fixture-parser-accept-all
    (should (functionp (mantra-parser-stop parser))))

  ;; mantra-parser-abort
  (with-fixture fixture-parser-accept-all
    (should (functionp (mantra-parser-abort parser))))

  ;; mantra-parser-state
  (with-fixture fixture-parser-accept-all
    (should (vectorp (mantra-parser-state parser)))))

(ert-deftest mantra-state-test ()
  (with-fixture fixture-parser-accept-all
    (should (seq-empty-p (mantra-parser-state parser))))
  (with-fixture fixture-parser-with-state
    (should-not (seq-empty-p (mantra-parser-state parser))))
  (with-fixture fixture-parser-with-state
    (mantra-parser-append-state parser [1 2 3])
    (should (equal (vconcat fixture-single-key [1 2 3])
                   (mantra-parser-state parser))))
  (with-fixture fixture-parser-with-state
    (mantra-parser-clear-state parser)
    (should (seq-empty-p (mantra-parser-state parser)))))

(ert-deftest mantra-key-listening-test ()
  (with-fixture fixture-parser-accept-all
    (with-key-listening
     (should (member parser mantra-parsers))))
  (with-fixture fixture-parser-accept-all
    (with-key-listening
     (mantra-listen-start fixture-single-key)
     (should (mantra-parsing-in-progress-p parser))))
  (with-fixture fixture-parser-accept-all
    (with-key-listening
     (mantra-listen-start fixture-single-key)
     (mantra-listen-end fixture-single-key)
     (should-not (mantra-parsing-in-progress-p parser)))))

(ert-deftest mantra-parsing-in-progress-test ()
  (with-fixture fixture-parser-accept-all
    (should-not (mantra-parsing-in-progress-p parser)))
  (with-fixture fixture-parser-with-state
    (should (mantra-parsing-in-progress-p parser))))

(ert-deftest mantra-parse-test ()
  (with-fixture fixture-parser-accept-none
    (mantra-parse parser
                  fixture-single-key)
    (should-not (mantra-parsing-in-progress-p parser)))
  (with-fixture fixture-parser-accept-all
    ;; parsing not already in progress but start condition passes
    (mantra-parse parser
                  fixture-single-key)
    (should (mantra-parsing-in-progress-p parser)))
  (with-fixture fixture-parser-with-state
    ;; parsing already in progress AND start condition passes
    (mantra-parse parser
                  fixture-single-key)
    (should (mantra-parsing-in-progress-p parser)))
  (with-fixture fixture-parser-accept-none
    ;; condition to start parsing fails BUT parsing already in progress
    (mantra-parser-set-state parser fixture-single-key)
    (mantra-parse parser
                  fixture-single-key)
    (should (mantra-parsing-in-progress-p parser))))

(ert-deftest mantra-accept-test ()
  (with-fixture fixture-parser-with-state
    ;; publishes key sequence
    (with-fixture fixture-subscriber
     (mantra-accept parser)
     (should (equal fixture-single-key
                    result))))
  (with-fixture fixture-parser-with-state
    ;; clears state
    (mantra-accept parser)
    (should-not (mantra-parsing-in-progress-p parser)))
  (with-fixture fixture-parser-accept-all
    (should-error (mantra-accept parser))))

(ert-deftest mantra-parse-finish-test ()
  ;; does not accept if parsing is not in progress
  (with-fixture fixture-parser-accept-all
    (with-fixture fixture-subscriber
      (mantra-parse-finish parser fixture-single-key)
      (should-not (equal fixture-single-key
                         result))))
  ;; accepts if parsing is in progress
  (with-fixture fixture-parser-with-state
    (with-fixture fixture-subscriber
      (mantra-parse-finish parser fixture-single-key)
      (should (equal fixture-single-key
                     result))
      (should-not (mantra-parsing-in-progress-p parser))))
  ;; aborts if abort condition is met
  (with-fixture fixture-abort-parser-with-state
    (with-fixture fixture-subscriber
      (mantra-parse-finish parser fixture-single-key)
      (should-not result)
      (should-not (mantra-parsing-in-progress-p parser)))))
