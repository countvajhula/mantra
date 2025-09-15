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

(require 'seq)
(require 'mantra-parse)

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

(defconst fixture-parser-basic-name "basic")

(defconst fixture-parser-accept-all-name "all")

(defconst fixture-parser-accept-none-name "none")

(defconst fixture-parser-abort-all-name "abort")

(defconst fixture-parser-accept-all-abort-all-name "accept-abort")

(defconst fixture-parser-nondefault-name "nondefault")

(defconst fixture-subscriber-name "hks")

(defun fixture-parser-basic (body)
  (let ((parser nil))
    (unwind-protect
        (progn (setq parser (mantra-make-parser fixture-parser-basic-name
                                                (lambda (_input) t)
                                                (lambda (_input _state) t)))
               (funcall body))
      ;; perhaps aid garbage collection
      (setq parser nil))))

(defun fixture-parser-accept-all (body)
  (let ((parser nil))
    (unwind-protect
        (progn (setq parser (mantra-make-parser fixture-parser-accept-all-name
                                                (lambda (_input) t)
                                                (lambda (_input _state) t)
                                                (lambda (_input _state) nil)))
               (funcall body))
      ;; perhaps aid garbage collection
      (setq parser nil))))

(defun fixture-parser-accept-none (body)
  (let ((parser nil))
    (unwind-protect
        (progn (setq parser (mantra-make-parser fixture-parser-accept-none-name
                                                (lambda (_input) nil)
                                                (lambda (_input _state) nil)
                                                (lambda (_input _state) nil)))
               (funcall body))
      ;; perhaps aid garbage collection
      (setq parser nil))))

(defun fixture-parser-abort-all (body)
  (let ((parser nil))
    (unwind-protect
        (progn (setq parser (mantra-make-parser fixture-parser-abort-all-name
                                                (lambda (_input) t)
                                                (lambda (_input _state) nil)
                                                (lambda (_input _state) t)))
               (funcall body))
      ;; perhaps aid garbage collection
      (setq parser nil))))

(defun fixture-parser-accept-all-abort-all (body)
  (let ((parser nil))
    (unwind-protect
        (progn (setq parser (mantra-make-parser fixture-parser-accept-all-abort-all-name
                                                (lambda (_input) t)
                                                (lambda (_input _state) t)
                                                (lambda (_input _state) t)))
               (funcall body))
      ;; perhaps aid garbage collection
      (setq parser nil))))

;; parser with nondefault map and compose predicates
(defun fixture-parser-nondefault (body)
  (let ((parser nil))
    (unwind-protect
        (progn (setq parser (mantra-make-parser fixture-parser-nondefault-name
                                                (lambda (_key-seq) t)
                                                (lambda (_key-seq _state) t)
                                                (lambda (_key-seq _state) nil)
                                                #'key-description
                                                #'concat))
               (funcall body))
      ;; perhaps aid garbage collection
      (setq parser nil))))

;; nondefault parser with nondefault state
(defun fixture-parser-nondefault-state (body)
  (let ((parser nil))
    (unwind-protect
        (progn (setq parser (mantra-make-parser fixture-parser-nondefault-name
                                                (lambda (_key-seq) t)
                                                (lambda (_key-seq _state) t)
                                                (lambda (_key-seq _state) nil)
                                                #'key-description
                                                #'concat
                                                "abc"))
               (funcall body))
      ;; perhaps aid garbage collection
      (setq parser nil))))

;; parser with nondefault finish
(defun fixture-parser-nondefault-finish (body)
  (let ((parser nil))
    (unwind-protect
        (progn (setq parser (mantra-make-parser fixture-parser-nondefault-name
                                                nil
                                                nil
                                                nil
                                                nil
                                                nil
                                                nil
                                                (lambda (_state) "hello")))
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

(defun fixture-nondefault-parser-with-state (body-3)
  (with-fixture fixture-parser-nondefault
    (mantra-parser-set-state parser
                             (key-description fixture-single-key))
    (funcall body-3)))

(defun fixture-nondefault-state-parser-with-state (body-3)
  (with-fixture fixture-parser-nondefault
    (mantra-parser-set-state parser
                             (concat (mantra-parser-state parser)
                                     (key-description fixture-single-key)))
    (funcall body-3)))

(defun fixture-multi-level-parsers (body-4)
  (with-fixture fixture-parser-with-state
    (let* ((my-parser (mantra-make-parser "my-parser"
                                          (lambda (_i) t)
                                          (lambda (_i _s) t)
                                          nil
                                          (lambda (input)
                                            (list (list 'my-parser input)))
                                          #'append))
           ;; subscriber for the final result (of the second parser)
           (my-subscriber (lambda (input)
                            ;; note: expects `result' to be
                            ;; (dynamically) bound in the calling test
                            (setq result input))))
      (unwind-protect
          (progn (mantra-subscribe (mantra-parser-name parser)
                                   my-parser)
                 (pubsub-subscribe (mantra-parser-name my-parser)
                                   "my-subscriber"
                                   my-subscriber)
                 (funcall body-4))
        (mantra-unsubscribe (mantra-parser-name parser)
                            my-parser)
        (pubsub-unsubscribe (mantra-parser-name my-parser)
                            "my-subscriber")))))

(defun fixture-subscriber (body-2)
  (let* ((result nil)
         (subscriber (lambda (parsed-keys)
                       (setq result parsed-keys))))
    (unwind-protect
        (progn (pubsub-subscribe (mantra-parser-name parser)
                                 fixture-subscriber-name
                                 subscriber)
               (funcall body-2))
      ;; it's especially important to unsubscribe because due to
      ;; dynamic scope, this "zombie" listener would otherwise remain
      ;; active and would be capable of overwriting dynamic local
      ;; variables like `result' in other tests
      (pubsub-unsubscribe (mantra-parser-name parser)
                          fixture-subscriber-name))))

;;
;; Tests
;;

(ert-deftest parser-test ()
  ;; null constructor
  (with-fixture fixture-parser-basic
    (should (vectorp parser)))

  (with-fixture fixture-parser-basic
    (should (vectorp parser)))

  ;; uses default abort predicate if none provided
  (with-fixture fixture-parser-basic
    (should-not (funcall (mantra-parser-abort parser) "abc" [97 98 99])))

  ;; mantra-parser-name
  (with-fixture fixture-parser-basic
    (should (equal fixture-parser-basic-name
                   (mantra-parser-name parser))))

  ;; mantra-parser-start
  (with-fixture fixture-parser-accept-all
    (should (funcall (mantra-parser-start parser) "abc")))

  ;; mantra-parser-stop
  (with-fixture fixture-parser-accept-all
    (should (funcall (mantra-parser-stop parser) "abc" [97 98 99])))

  ;; mantra-parser-abort
  (with-fixture fixture-parser-accept-all
    (should-not (funcall (mantra-parser-abort parser) "abc" [97 98 99])))

  ;; mantra-parser-state
  (with-fixture fixture-parser-basic
    (should (vectorp (mantra-parser-state parser))))

  ;; state is initialized by applying map to the empty vector
  (with-fixture fixture-parser-nondefault
    (should (equal "" (mantra-parser-state parser))))

  ;; provided initial state overrides default
  (with-fixture fixture-parser-nondefault-state
    (should (equal "abc" (mantra-parser-state parser))))

  ;; initial value of nil may be provided via mantra-initial-value
  (let ((parser (mantra-make-parser "nil-parser"
                                    nil
                                    nil
                                    nil
                                    nil
                                    nil
                                    (mantra-initial-value nil))))
    (should (equal nil (mantra-parser-init parser))))

  ;; mantra-parser-map
  (with-fixture fixture-parser-basic
    ;; defaults to identity function
    (should (equal "abc"
                   (funcall (mantra-parser-map parser)
                            "abc"))))

  ;; mantra-parser-compose
  (with-fixture fixture-parser-accept-all
    ;; defaults to vconcat
    (should (equal [1 2 3]
                   (funcall (mantra-parser-compose parser)
                            []
                            [1 2 3])))))

(ert-deftest state-test ()
  (with-fixture fixture-parser-accept-all
    (should (seq-empty-p (mantra-parser-state parser))))
  (with-fixture fixture-parser-with-state
    (should-not (seq-empty-p (mantra-parser-state parser))))
  (with-fixture fixture-parser-with-state
    (mantra-parser-clear-state parser)
    (should (seq-empty-p (mantra-parser-state parser))))
  (with-fixture fixture-parser-nondefault
    ;; clear resets to null state
    (mantra-parser-clear-state parser)
    (should (equal "" (mantra-parser-state parser))))
  (with-fixture fixture-parser-nondefault-state
    ;; clear resets to initial state set
    (mantra-parser-clear-state parser)
    (should (equal "abc" (mantra-parser-state parser))))
  ;; null state is determined using map on the empty vector
  (with-fixture fixture-parser-basic
    (should (equal [] (mantra-parser-init parser))))
  (with-fixture fixture-parser-nondefault
    (should (equal "" (mantra-parser-init parser))))
  (with-fixture fixture-parser-nondefault-state
    (should (equal "abc" (mantra-parser-init parser)))))

(ert-deftest mantra-parsing-in-progress-test ()
  (with-fixture fixture-parser-accept-all
    (should-not (mantra-parsing-in-progress-p parser)))
  (with-fixture fixture-parser-with-state
    (should (mantra-parsing-in-progress-p parser)))
  (with-fixture fixture-parser-nondefault
    (should-not (mantra-parsing-in-progress-p parser)))
  (with-fixture fixture-nondefault-parser-with-state
    (should (mantra-parsing-in-progress-p parser)))
  (with-fixture fixture-parser-nondefault-state
    (should-not (mantra-parsing-in-progress-p parser)))
  (with-fixture fixture-nondefault-state-parser-with-state
    (should (mantra-parsing-in-progress-p parser))))

(ert-deftest mantra-feed-parser-test ()
  (with-fixture fixture-parser-accept-none
    (mantra-feed-parser parser
                        fixture-single-key)
    (should-not (mantra-parsing-in-progress-p parser)))
  (with-fixture fixture-parser-accept-all
    ;; parsing not already in progress but start condition passes
    (mantra-feed-parser parser
                        fixture-single-key)
    (should (mantra-parsing-in-progress-p parser)))
  (with-fixture fixture-parser-with-state
    ;; parsing already in progress AND start condition passes
    (mantra-feed-parser parser
                        fixture-single-key)
    (should (mantra-parsing-in-progress-p parser)))
  (with-fixture fixture-parser-accept-none
    ;; condition to start parsing fails BUT parsing already in progress
    (mantra-parser-set-state parser fixture-single-key)
    (mantra-feed-parser parser
                        fixture-single-key)
    (should (mantra-parsing-in-progress-p parser)))
  (with-fixture fixture-parser-nondefault
    ;; values are mapped and composed with state
    (mantra-parser-set-state parser "a b c ")
    (mantra-feed-parser parser
                        [100 101 102])
    (should (equal "a b c d e f"
                   (mantra-parser-state parser))))
  (with-fixture fixture-abort-parser-with-state
    (mantra-feed-parser parser
                        fixture-single-key)
    (should-not (mantra-parsing-in-progress-p parser))))

(ert-deftest mantra-accept-test ()
  (with-fixture fixture-parser-with-state
    ;; publishes key sequence by default
    (with-fixture fixture-subscriber
     (mantra-accept parser)
     (should (equal fixture-single-key
                    result))))
  (with-fixture fixture-parser-with-state
    ;; clears state
    (mantra-accept parser)
    (should-not (mantra-parsing-in-progress-p parser)))

  ;; produces finished parsing state
  (with-fixture fixture-parser-nondefault-finish
    (with-fixture fixture-subscriber
      (mantra-accept parser)
      (should (equal "hello" result)))))

(ert-deftest mantra-parse-test ()
  ;; does not accept if parsing is not in progress
  (with-fixture fixture-parser-accept-all
    (with-fixture fixture-subscriber
      (mantra-parse parser fixture-single-key)
      (should-not (equal fixture-single-key
                         result))))
  ;; accepts if parsing is in progress
  (with-fixture fixture-parser-with-state
    (with-fixture fixture-subscriber
      (mantra-parse parser fixture-single-key)
      (should (equal fixture-single-key
                     result))
      (should-not (mantra-parsing-in-progress-p parser)))))

(ert-deftest mantra-subscribe-test ()
  ;; subscriber receives parsed tokens
  (with-fixture fixture-multi-level-parsers
    (let ((result nil))
      (mantra-parse parser fixture-single-key)
      (should (equal '((my-parser []) (my-parser [108]))
                     result))))
  ;; subscriber stops receiving parsed tokens upon unsubscribing
  (with-fixture fixture-multi-level-parsers
    (let ((result nil))
      (mantra-unsubscribe (mantra-parser-name parser)
                          my-parser)
      (mantra-parse parser fixture-single-key)
      (should-not result))))
