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
    (mantra-parser-set-state parser fixture-single-key)
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
          (progn (mantra-subscribe parser my-parser)
                 (pubsub-subscribe (mantra-parser-name my-parser)
                                   "my-subscriber"
                                   my-subscriber)
                 (funcall body-4))
        (mantra-unsubscribe parser my-parser)
        (pubsub-unsubscribe (mantra-parser-name my-parser)
                            "my-subscriber")))))

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

(ert-deftest mantra-register-test ()
  ;; registering a parser
  (let ((parser mantra-key-sequences-parser))
    (mantra-register parser)
    (should (member parser mantra-parsers)))

  ;; parser isn't registered more than once
  (let ((parser mantra-key-sequences-parser))
    (mantra-register parser)
    (mantra-register parser)
    (setq mantra-parsers
          (cl-remove parser
                     mantra-parsers
                     :count 1))
    (should-not (member parser mantra-parsers))))

(ert-deftest mantra-key-sequences-parser-test ()
  (let ((parser mantra-key-sequences-parser))
    (should (funcall (mantra-parser-start parser) "a"))
    (should (funcall (mantra-parser-stop parser) "abc" [97 98 99]))
    (should-not (funcall (mantra-parser-abort parser) "abc" [97 98 99]))))

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
  ;; null state is determined using map on the empty vector
  (with-fixture fixture-parser-basic
    (should (equal [] (mantra-parser-null-state parser))))
  (with-fixture fixture-parser-nondefault
    (should (equal "" (mantra-parser-null-state parser)))))

(ert-deftest key-listening-test ()
  (with-fixture fixture-parser-accept-all
    (with-key-listening
     (should (member parser mantra-parsers))))
  (with-fixture fixture-parser-accept-all
    (with-key-listening
     (mantra-unregister parser)
     (should-not (member parser mantra-parsers))))
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
    (should (mantra-parsing-in-progress-p parser)))
  (with-fixture fixture-parser-nondefault
    (should-not (mantra-parsing-in-progress-p parser)))
  (with-fixture fixture-nondefault-parser-with-state
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
    (should (mantra-parsing-in-progress-p parser)))
  (with-fixture fixture-parser-nondefault
    ;; values are mapped and composed with state
    (mantra-parser-set-state parser "a b c ")
    (mantra-parse parser
                  [100 101 102])
    (should (equal "a b c d e f"
                   (mantra-parser-state parser))))
  (with-fixture fixture-abort-parser-with-state
                (mantra-parse parser
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
    (should-not (mantra-parsing-in-progress-p parser))))

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
      (should-not (mantra-parsing-in-progress-p parser)))))

(ert-deftest mantra-subscribe-test ()
  ;; subscriber receives parsed tokens
  (with-fixture fixture-multi-level-parsers
    (let ((result nil))
      (mantra-parse-finish parser fixture-single-key)
      (should (equal '((my-parser []) (my-parser [108]))
                     result))))
  ;; subscriber stops receiving parsed tokens upon unsubscribing
  (with-fixture fixture-multi-level-parsers
    (let ((result nil))
      (mantra-unsubscribe parser my-parser)
      (mantra-parse-finish parser fixture-single-key)
      (should-not result))))
