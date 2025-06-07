;;; mantra-dsl.el --- Mantras, not macros! -*- lexical-binding: t -*-

;; This file is NOT a part of Gnu Emacs.

;; This work is "part of the world."  You are free to do whatever you
;; like with it and it isn't owned by anybody, not even the
;; creators.  Attribution would be appreciated and is a valuable
;; contribution in itself, but it is not strictly necessary nor
;; required.  If you'd like to learn more about this way of doing
;; things and how it could lead to a peaceful, efficient, and creative
;; world, and how you can help, visit https://drym.org.
;;
;; This paradigm transcends traditional legal and economic systems, but
;; for the purposes of any such systems within which you may need to
;; operate:
;;
;; This is free and unencumbered software released into the public domain.
;; The authors relinquish any copyright claims on this work.

;;; Commentary:

;; A DSL for composing mantras

;;; Code:

(require 'cl-lib)

(defun mantra-make-key (key)
  "A primitive KEY sequence."
  `(key ,key))

(defun mantra--key-key (key)
  "The underlying key sequence in KEY."
  (cadr key))

(defun mantra-key-p (obj)
  "Check if OBJ specifies a key sequence."
  (condition-case nil
      (eq 'key
          (nth 0 obj))
    (error nil)))

(defun mantra-make-seq (&rest mantras)
  "Construct a sequence of mantras."
  `(seq ,mantras))

(defun mantra-seq-p (obj)
  "Check if OBJ specifies a seq."
  (condition-case nil
      (eq 'seq
          (nth 0 obj))
    (error nil)))

(defun mantra--seq-phases (seq)
  "Get the phases of a SEQ.

Each phase could be any mantra."
  (nth 1 seq))

(defun mantra--seq-first (seq)
  "The first mantra in SEQ."
  (car (mantra--seq-phases seq)))

(defun mantra--seq-rest (seq)
  "The rest of the mantras in SEQ, except the first."
  (apply #'mantra-make-seq
         (cdr (mantra--seq-phases seq))))

(defun mantra--seq-null-p (seq)
  "Check if SEQ is empty or null."
  (null (mantra--seq-phases seq)))

(defun mantra-make-repetition (mantra &optional times)
  "A specification to repeat a MANTRA TIMES times.

If TIMES is nil, repeat indefinitely until the mantra fails."
  (list 'repetition
        mantra
        times))

(defun mantra-repetition-p (obj)
  "Check if OBJ specifies a repetition."
  (condition-case nil
      (eq 'repetition
          (nth 0 obj))
    (error nil)))

(defun mantra--repetition-mantra (repetition)
  "Get the mantra component of the REPETITION.

This is the mantra that is intended to be looped."
  (nth 1 repetition))

(defun mantra--repetition-times (repetition)
  "Get the times component of the REPETITION.

This is the number of times the mantra should be repeated."
  (nth 2 repetition))

(defun mantra--repetition-null-p (repetition)
  "Check if REPETITION is empty or null."
  (let ((times (mantra--repetition-times repetition)))
    (and times (zerop times))))

(defun mantra--repetition-rest (repetition)
  "A repetition defined from the remaining repetitions of the mantra.

This includes the remaining repetitions in REPETITION, not counting the
first.  This is useful for structural recursion during repetition
execution."
  (let ((mantra (mantra--repetition-mantra repetition))
        (times (mantra--repetition-times repetition)))
    (mantra-make-repetition mantra (when times (1- times)))))

(defun mantra-make-insertion (text)
  "A primitive operation to insert text into a buffer."
  `(insertion ,text))

(defun mantra--insertion-text (insertion)
  "The text to insert in INSERTION."
  (cadr insertion))

(defun mantra-insertion-p (obj)
  "Check if OBJ specifies an insertion."
  (condition-case nil
      (eq 'insertion
          (nth 0 obj))
    (error nil)))

(defun mantra-p (obj)
  "Check if OBJ specifies a mantra."
  (or (vectorp obj)
      (mantra-key-p obj)
      (mantra-seq-p obj)
      (mantra-repetition-p obj)
      (mantra-insertion-p obj)))

(defconst mantra--null (vector)
  "The null mantra.")

(cl-defun mantra-make-computation (&key
                                   (map #'identity)
                                   (compose #'identity))
  "A computation to be performed as part of mantra recitation.

MAP - the function to be applied to the result of each key sequence,
which transforms it to the parsed type.
COMPOSE - a binary function to be applied in combining results from nested
computations (each of the parsed type) to yield the provisional result
\(also of the parsed type)."
  (list 'computation
        map
        compose))

(defun mantra--computation-map (computation)
  "The map procedure of the COMPUTATION."
  (nth 1 computation))

(defun mantra--computation-compose (computation)
  "The compose procedure of the COMPUTATION."
  (nth 2 computation))

(defun mantra-compose-computation (a b computation)
  "Compose results of mantra evaluation according to COMPUTATION.

Combine the result of a mantra computation A with the accumulated
computation B into an aggregate result."
  (when (and a b)
    (funcall (mantra--computation-compose computation)
             a
             b)))

(defconst mantra--computation-default
  (mantra-make-computation :map #'list
                           :compose #'append)
  "The default computation done on mantras.

Each result is wrapped in a list.  These are concatenated using list
concatenation.")

(defun mantra-eval-key-vector (key-vector &optional computation result)
  "Evaluate KEY-VECTOR.

A key vector is a primitive Emacs key sequence and the base case of
mantra evaluation. It is evaluated using `execute-kbd-macro', and the
overall computation is stitched together in terms of the executed key
and the accumulated result.

To do this, the executed key is first parsed via the `map' predicate,
and then combined with the RESULT so far using the `compose'
predicate, as specified in the COMPUTATION associated with the mantra.

When COMPUTATION is left unspecified, `mantra--computation-default' is
used, guided by which, this function returns a list of keys recited.
Note that, in particular, if RESULT happens to be empty and if a key
is recited, then this doesn't return the recited key itself but
rather, a singleton list containing the key."
  (let* ((computation (or computation mantra--computation-default))
         (result (or result
                     (funcall (mantra--computation-map computation)
                              mantra--null)))
         (recited-mantra (condition-case nil
                             (progn (execute-kbd-macro key-vector)
                                    t)
                           (error nil))))
    (when recited-mantra
      (mantra-compose-computation result
                                  (funcall (mantra--computation-map computation)
                                           key-vector)
                                  computation))))

(defun mantra-eval-key (key &optional computation result)
  "Evaluate KEY.

A KEY is a string representation of a key sequence. It is translated
in a straightforward way to the primitive key vector representation to
be evaluated."
  (let ((key-string (mantra--key-key key)))
    (mantra-eval-key-vector (string-to-vector
                             (kbd key-string))
                            computation
                            result)))

(defun mantra-eval-seq (seq computation result)
  "Execute a SEQ.

Attempts the seq in the order of its phases.  The seq
succeeds only if all of the phases succeed, and otherwise fails.

See `mantra-eval-move' for more on COMPUTATION and RESULT."
  (if (mantra--seq-null-p seq)
      result
    (let ((current-phase (mantra--seq-first seq))
          (remaining-seq (mantra--seq-rest seq)))
      (let ((executed-phase (mantra-eval current-phase
                                         computation
                                         result)))
        (when executed-phase
          (mantra-eval-seq remaining-seq
                           computation
                           executed-phase))))))

(defun mantra-eval-repetition (repetition computation result)
  "Execute a REPETITION.

This repeats some mantra as specified.

See `mantra-eval-key' for more on COMPUTATION and RESULT."
  (if (mantra--repetition-null-p repetition)
      result
    (let ((mantra (mantra--repetition-mantra repetition))
          (times (mantra--repetition-times repetition))
          (remaining-repetition (mantra--repetition-rest repetition)))
      (let ((executed-phase (mantra-eval mantra
                                         computation
                                         result)))
        (if executed-phase
            (mantra-eval-repetition remaining-repetition
                                    computation
                                    executed-phase)
          (when (not times)
            ;; if looping indefinitely, then count 0
            ;; times executed as success
            result))))))

(defun mantra-eval-insertion (insertion &optional computation result)
  "Evaluate INSERTION.

An insertion when evaluated inserts text into the buffer.

Like key vectors, this is a primitive operation of the Mantra DSL."
  (let ((text (mantra--insertion-text insertion))
        (result (or result
                    (funcall (mantra--computation-map computation)
                             mantra--null))))
    (insert text)
    result))

(defun mantra--eval (mantra computation result)
  "Helper to evaluate MANTRA.

See `mantra-eval-move' for more on COMPUTATION and RESULT."
  (cond ((mantra-repetition-p mantra)
         (mantra-eval-repetition mantra
                                 computation
                                 result))
        ((mantra-seq-p mantra)
         (mantra-eval-seq mantra
                          computation
                          result))
        ((mantra-key-p mantra)
         (mantra-eval-key mantra
                          computation
                          result))
        ((vectorp mantra)
         (mantra-eval-key-vector mantra
                                 computation
                                 result))
        ((mantra-insertion-p mantra)
         (mantra-eval-insertion mantra
                                computation
                                result))
        ;; fall back to a lambda. It must still accept
        ;; the same arguments as any mantra, so that
        ;; it could in principle produce a valid result
        ;; but in practice the fallback lambda may be
        ;; used for predicates where we only care whether
        ;; the output is truthy or not.
        (t (funcall mantra
                    computation
                    result))))

(defun mantra-eval (mantra
                    &optional
                    computation
                    result)
  "Recite (evaluate) a MANTRA.

MANTRA could be a primitive key sequence, a sequence of mantras, or
any other mantra. If it is not a specific mantra form, then it is
assumed to be an ELisp function, and the rule for interpretation is to
apply the function.

The evaluation is done in a \"tail-recursive\" way, by passing the
in-progress RESULT forward through subsequent stages of mantra
evaluation.

This function, along with any of the more specific mantra evaluators
such as `mantra-eval-seq', evaluates to a COMPUTATION on the mantra
actually recited.

See `mantra-eval-key' for more on COMPUTATION and RESULT."
  (let* ((computation (if computation
                          computation
                        mantra--computation-default))
         (result (or result
                     (mantra-eval-key-vector mantra--null
                                             computation))))
    (mantra--eval mantra
                  computation
                  result)))

(provide 'mantra-dsl)
;;; mantra-dsl.el ends here
