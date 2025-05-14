;;; mantra.el --- Mantras, not macros! -*- lexical-binding: t -*-

;; Author: Sid Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/mantra
;; Version: 0.0
;; Package-Requires: ((emacs "25.1") (pubsub "0.0"))

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

;; Mantras, not macros!

;;; Code:

;; (key-description [3 108])
;; kbd

(require 'pubsub)

;; TODO: instead of having a dynamic global,
;; we could define languages as sets of parsers,
;; and infer the appropriate language based on
;; conditions at start.
(defvar mantra-parsers
  nil
  "Current set of parsers actively parsing keyboard input.")

(defconst mantra--index-name 0
  "The name of the parser.")

(defconst mantra--index-start 1
  "The index of the condition to start parsing.")

(defconst mantra--index-stop 2
  "The index of the condition to successfully end parsing.")

(defconst mantra--index-abort 3
  "The index of the condition to abort parsing.")

(defconst mantra--index-state 4
  "The index of the state variable in a parser.")

(defun mantra-parser-name (parser)
  "The name of the PARSER."
  (seq-elt parser mantra--index-name))

(defun mantra-parser-start (parser)
  "Condition to start parsing for PARSER."
  (seq-elt parser mantra--index-start))

(defun mantra-parser-stop (parser)
  "Condition to end parsing for PARSER."
  (seq-elt parser mantra--index-stop))

(defun mantra-parser-abort (parser)
  "Condition to abort parsing for PARSER."
  (seq-elt parser mantra--index-abort))

(defun mantra-parser-state (parser)
  "Get accumulated state in PARSER."
  (seq-elt parser mantra--index-state))

(defun mantra-parser-set-state (parser new-state)
  "Set state on PARSER to NEW-STATE."
  (aset parser mantra--index-state new-state))

(defun mantra-parser-append-state (parser new-state)
  "Append NEW-STATE to PARSER's existing state."
  (mantra-parser-set-state parser
                           (vconcat (mantra-parser-state parser)
                                    new-state)))

(defun mantra-parser-clear-state (parser)
  "Clear the PARSER's state."
  (mantra-parser-set-state parser (vector)))

(defun mantra-make-parser (name start end abort)
  "Make a PARSER named NAME with START, END, and ABORT conditions.

A parser is a set of criteria for storing key sequences in it in the
form of a START condition to determine the start of a key sequence of
interest, a STOP condition to determine the end, and an ABORT
condition to abort parsing.

The START condition is checked in `pre-command-hook' and STOP and
ABORT conditions are checked in `post-command-hook'.  Once START is
satisfied, the key sequences are accumulated in the parser state as a
composed key sequence (vector).  When STOP is satisfied (which might
happen during invocation of the same command, or it might not), a
match event is generated containing the entire sequence in STATE as a
single key sequence vector.  If ABORT is satisfied during parsing, the
state is cleared."
  (vector name
          start
          end
          abort
          (vector)))

(defvar mantra-basic-parser
  (mantra-make-parser "basic"
                      (lambda (_key-seq) t)
                      (lambda (_key-seq) t)
                      (lambda (_key-seq) nil))
  "A parser to recognize all key sequences.")

(defun mantra-pre-command-listener ()
  "Listen for the key sequences on the Emacs command loop."
  (mantra-listen-start (this-command-keys-vector)))

(defun mantra-post-command-listener ()
  "Listen for the key sequences on the Emacs command loop."
  (mantra-listen-end (this-command-keys-vector)))

(defun mantra-listen-start (key-seq)
  "Notify all parsers of a newly entered key sequence KEY-SEQ.

This does some basic \"lexing\" of the key sequence, discarding rather
than forwarding empty sequences."
  (when (and key-seq (not (seq-empty-p key-seq)))
    (dolist (parser mantra-parsers)
      (mantra-parse parser key-seq))))

(defun mantra-listen-end (key-seq)
  "Notify all parsers after the conclusion of a command for a key sequence KEY-SEQ."
  (when (and key-seq (not (seq-empty-p key-seq)))
    (dolist (parser mantra-parsers)
      (mantra-parse-finish parser key-seq))))

(defun mantra-register (parser)
  "Register PARSER to receive key sequence events."
  (push parser mantra-parsers))

(defun mantra-initialize ()
  "Register an initial basic parser that accepts any key sequence."
  (add-hook 'pre-command-hook #'mantra-pre-command-listener)
  (add-hook 'post-command-hook #'mantra-post-command-listener)
  (mantra-register mantra-basic-parser))

(defun mantra-parsing-in-progress-p (parser)
  "Whether PARSER is already parsing, i.e., accumulating state."
  (not
   (seq-empty-p
    (mantra-parser-state parser))))

(defun mantra-parse (parser key-seq)
  "Parse KEY-SEQ using PARSER.

If PARSER is already parsing (i.e., it already has state), then append
KEY-SEQ to PARSER's current state.  Otherwise, begin parsing by
initializing PARSER's state to KEY-SEQ if the start condition is met.
Otherwise, do nothing."
  (when (or (mantra-parsing-in-progress-p parser)
            (funcall (mantra-parser-start parser)
                     key-seq))
    (mantra-parser-append-state parser key-seq)))

(defun mantra-accept (parser)
  "Accept the current state in PARSER.

This publishes the parsed key sequence under the topic with PARSER's
name.  Note that as the pub/sub system is not persistent, it does not
store any parsed key sequences after notifying subscribers of them.

After publishing the match, this clears the parser state."
  (let ((name (mantra-parser-name parser))
        (state (mantra-parser-state parser)))
    (when (seq-empty-p state)
      (error "Can't accept empty key sequence!"))
    ;; TODO: does the published notice need to have
    ;; any metadata?
    (pubsub-publish name state)
    ;; clear state
    (mantra-parser-clear-state parser)))

(defun mantra-parse-finish (parser key-seq)
  "Accept or abort parsing by PARSER.

KEY-SEQ is the current key sequence.  Aborting or accepting is based on
the entire parsed state in PARSER, not just the current key sequence."
  ;; state already includes key-seq by this point,
  ;; as this is being called in post-command
  ;; TODO: should also pass state to stop and abort predicates?
  (when (mantra-parsing-in-progress-p parser)
    (cond ((funcall (mantra-parser-abort parser) key-seq)
           (mantra-parser-clear-state parser))
          ((funcall (mantra-parser-stop parser) key-seq)
           (mantra-accept parser)))))


(provide 'mantra)
;;; mantra.el ends here
