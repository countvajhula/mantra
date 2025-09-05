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

(require 'mantra-dsl)

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

;; `with-temp-buffer' would be preferable to `progn' here
;; but `execute-kbd-macro' resets the buffer back to the
;; "containing" one (even with `emacs -Q`), for reasons unknown,
;; and that causes the insertions to occur there rather than
;; in the temporary buffer.
(defun fixture-empty-buffer (body)
  (unwind-protect
      (progn
        (funcall body)
        (setq result (buffer-string)))
    (erase-buffer)))

(defun fixture-nonempty-buffer (body)
  (unwind-protect
      (progn
        (insert "hello")
        (funcall body)
        (setq result (buffer-string)))
    (erase-buffer)))

;;
;; Tests
;;

(ert-deftest key-vector-test ()
  (let ((result))
    (with-fixture fixture-empty-buffer
      (mantra-eval [97]))
    (should
     (equal "a"
            result)))
  (let ((result))
    (with-fixture fixture-empty-buffer
      (mantra-eval [97 98 99 134217826 134217827]))
    (should
     (equal "Abc"
            result))))

(ert-deftest key-test ()
  (let ((result))
    (with-fixture fixture-empty-buffer
      (mantra-eval "a"))
    (should
     (equal "a"
            result)))
  (let ((result))
    (with-fixture fixture-empty-buffer
      (mantra-eval "abc M-b M-c"))
    (should
     (equal "Abc"
            result))))

(ert-deftest insertion-test ()
  (let ((result))
    (with-fixture fixture-empty-buffer
      (mantra-eval '(insertion "a" 0 t)))
    (should
     (equal "a"
            result)))
  (let ((result))
    (with-fixture fixture-empty-buffer
      (mantra-eval '(insertion "hello C-c C-v M-f there" 0 t)))
    (should
     (equal "hello C-c C-v M-f there"
            result)))
  (let ((result))
    ;; nonzero offset
    (with-fixture fixture-nonempty-buffer
                  (mantra-eval '(insertion "a" -1 t)))
    (should
     (equal "hellao"
            result)))
  (let ((result))
    ;; move point
    (with-fixture fixture-empty-buffer
                  (mantra-eval '(seq ((insertion "a" 0 t)
                                      (insertion "b" 0 t)))))
    (should
     (equal "ab"
            result)))
  (let ((result))
    ;; preserve point
    (with-fixture fixture-empty-buffer
                  (mantra-eval '(seq ((insertion "a" 0 nil)
                                      (insertion "b" 0 nil)))))
    (should
     (equal "ba"
            result))))

(ert-deftest deletion-test ()
  (let ((result))
    (with-fixture fixture-nonempty-buffer
      (goto-char 4)
      (mantra-eval '(deletion -2 3)))
    (should
     (equal "ho"
            result))))

(ert-deftest move-test ()
  (let ((result))
    (with-fixture fixture-nonempty-buffer
                  (goto-char 0)
                  (mantra-eval '(move 2))
                  (kill-region (point) (point-max)))
    (should
     (equal "he"
            result))))

(ert-deftest fallback-test ()
  (let ((result))
    (with-fixture fixture-empty-buffer
      (mantra-eval (lambda (&rest args)
                     (insert "bc"))))
    (should
     (equal "bc"
            result))))

(ert-deftest repetition-test ()
  (let ((result))
    (with-fixture fixture-empty-buffer
      (mantra-eval '(repetition "a" 3)))
    (should
     (equal "aaa"
            result))))

(ert-deftest seq-test ()
  (let ((result))
    (with-fixture fixture-empty-buffer
      (mantra-eval '(seq ("a"
                          (lambda (&rest args)
                            (insert "bc"))))))
    (should
     (equal "abc"
            result)))
  ;; composition of sequences appends phases
  (let ((result (mantra-seq-compose '(seq ([]
                                           "a"))
                                    '(seq ([98]
                                           [99])))))
    (should
     (equal '(seq ([] "a" [98] [99]))
            result))))

(ert-deftest mantra-eval-test ()
  (let ((result))
    (with-fixture fixture-empty-buffer
      (mantra-eval '(seq ((repetition "a" 3)
                          "b"
                          (insertion "c" 0 t)
                          (lambda (&rest args)
                            (insert "def"))))))
    (should
     (equal "aaabcdef"
            result)))

  ;; using a nondefault computation doesn't change the behavior
  (let ((result))
    (with-fixture fixture-empty-buffer
      (mantra-eval '(seq ((repetition "a" 3)
                          "b"
                          (insertion "c" 0 t)
                          (lambda (&rest args)
                            (insert "def"))))
                   (mantra-make-computation :map #'list
                                            :compose #'append)))
    (should
     (equal "aaabcdef"
            result)))

  ;; computation computes expected result
  (with-fixture fixture-empty-buffer
    (let ((result
           (mantra-eval '(seq ((repetition "a" 3)
                               "b"
                               (insertion "c" 0 t)
                               (lambda (computation result)
                                 (insert "def")
                                 result)))
                        (mantra-make-computation :map #'list
                                                 :compose #'append))))
      (should
       ;; TODO: why the two leading empty vectors?
       (equal '([] [] [97] [97] [97] [98])
              result)))))
