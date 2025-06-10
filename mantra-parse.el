;;; mantra-parse.el --- Mantras, not macros! -*- lexical-binding: t -*-

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

;; Parse keyboard activity

;;; Code:

;; (key-description [3 108])
;; kbd

(require 'pubsub)

(defconst mantra--index-name 0
  "The name of the parser.")

(defconst mantra--index-start 1
  "The index of the condition to start parsing.")

(defconst mantra--index-stop 2
  "The index of the condition to successfully stop parsing.")

(defconst mantra--index-abort 3
  "The index of the condition to abort parsing.")

(defconst mantra--index-map 4
  "The index of the map function in a parser.")

(defconst mantra--index-compose 5
  "The index of the compose function in a parser.")

(defconst mantra--index-state 6
  "The index of the state variable in a parser.")

(defun mantra-parser-name (parser)
  "The name of the PARSER."
  (seq-elt parser mantra--index-name))

(defun mantra-parser-start (parser)
  "Condition to start parsing for PARSER."
  (seq-elt parser mantra--index-start))

(defun mantra-parser-stop (parser)
  "Condition to stop parsing for PARSER."
  (seq-elt parser mantra--index-stop))

(defun mantra-parser-abort (parser)
  "Condition to abort parsing for PARSER."
  (seq-elt parser mantra--index-abort))

(defun mantra-parser-map (parser)
  "Function to apply to each parsed key sequence in PARSER."
  (seq-elt parser mantra--index-map))

(defun mantra-parser-compose (parser)
  "Function to compute the fresh state in PARSER.

This value is a binary function, i.e., a function taking two
arguments, where the first is the accumulated parser state and the
second is the result of applying the \"map\" function to the fresh key
sequence."
  (seq-elt parser mantra--index-compose))

(defun mantra-parser-null-state (parser)
  "The null parsing state inferred for the current PARSER.

This is the state the parser is always initialized to, and if it is
the current state of the parser, then that means that it isn't
currently in the middle of parsing something.

The null state is derived from applying the parser's `map' function to
the empty vector."
  (funcall (mantra-parser-map parser)
           (vector)))

(defun mantra-parser-state (parser)
  "Get accumulated state in PARSER."
  (seq-elt parser mantra--index-state))

(defun mantra-parser-set-state (parser new-state)
  "Set state on PARSER to NEW-STATE."
  (aset parser mantra--index-state new-state))

(defun mantra-parser-clear-state (parser)
  "Clear the PARSER's state."
  (mantra-parser-set-state parser
                           (mantra-parser-null-state parser)))

(defun mantra-make-parser (name
                           start
                           stop
                           &optional
                           abort
                           map
                           compose)
  "Make a parser for keyboard activity.

A parser is a set of criteria for recognizing key sequences, in the
form of a START condition to determine the start of a key sequence of
interest, a STOP condition to determine the end, and an ABORT
condition to abort parsing. Matches are published on a pub/sub system
(using the `pubsub' package), under the topic NAME.

NAME is the name of the parser and could be any string, but as it will
be used as the topic for publishing output in a global pub/sub system,
it should follow Emacs's naming conventions for global identifiers.
Specifically, the name should be prefixed with the package name.

The START and STOP conditions are checked in `post-command-hook'. Once
START is satisfied, the key sequences (themselves, by default) are
accumulated in the parser state as a composed key sequence (vector).
When STOP is satisfied (which might happen during invocation of the
same command, or it might not), a match event is published containing
the entire sequence in STATE as a single key sequence vector (once
again, by default. The specific nature of the parsed data can be
defined using the MAP and COMPOSE predicates, as explained below). If
ABORT is satisfied during parsing, the state is cleared. If no ABORT
condition is specified, a default one is used that never aborts.

MAP and COMPOSE define how the result of parsing is constructed and
composed.  MAP is a function of one argument that is invoked with the
fresh key sequence at each step during parsing.  The result is then
combined with the accumulated parser state using COMPOSE, which is a
function that will be invoked with two arguments, the first being the
accumulated state and the second, the result of applying MAP to the
fresh key sequence at that step.

If no MAP function is provided, the identity function is used as the
default, so that the parsed value at each step is simply the input key
sequence.  If no COMPOSE function is provided, it defaults to `vconcat'
which is appropriate for composing key sequence vectors (i.e., the
default values if no MAP is specified), so that the resulting parse is
a single key sequence vector capturing the entire parsed stream.

The state at the time of acceptance is published as the result of
parsing."
  (let ((abort (or abort (lambda (_input _state) nil)))
        (map (or map #'identity))
        (compose (or compose #'vconcat)))
    (vector name
            start
            stop
            abort
            map
            compose
            ;; initialize to null state
            (funcall map (vector)))))

(defun mantra-post-command-listener ()
  "Listen for the key sequences on the Emacs command loop.

Notify all primitive parsers of a newly entered key sequence KEY-SEQ.

This does some basic \"lexing\" of the key sequence, discarding rather
than forwarding empty sequences."
  (let ((key-seq (this-command-keys-vector)))
    (when (and key-seq (not (seq-empty-p key-seq)))
      (pubsub-publish "mantra-key-sequences"
                      key-seq))))

(defun mantra-connect ()
  "Connect registered mantra parsers to the Emacs command loop.

The parsers will be notified of all keyboard activity, at the
granularity of when key sequences match a command.

This \"connection\" is only relevant for *primitive* parsers that
directly parse key sequences on the Emacs command loop."
  (add-hook 'post-command-hook #'mantra-post-command-listener))

(defun mantra-disconnect ()
  "Disconnect registered mantra parsers from the Emacs command loop."
  (remove-hook 'post-command-hook #'mantra-post-command-listener))

(defun mantra-subscribe (topic subscriber)
  "Subscribe SUBSCRIBER to TOPIC.

SUBSCRIBER is expected to be a parser, and TOPIC, a string.

This could be done directly by clients, as the \"data bus\" being used
to publish parsed tokens is the \"public\" pubsub library, dynamically
available in Emacs and not specific to Mantra. But it's useful to
encapsulate the details of feeding parsers input here, as it affords a
simple interface for clients in terms of just defining parsers and
specifying pairwise subscriptions amongst them, instead of worrying
about actually connecting them and feeding the tokens forward in the
proper way (as we do here)."
  (pubsub-subscribe topic
                    (mantra-parser-name subscriber)
                    (lambda (input)
                      (mantra-feed-parser subscriber
                                          input))))

(defun mantra-unsubscribe (topic subscriber)
  "Unsubscribe SUBSCRIBER from TOPIC.


SUBSCRIBER is expected to be a parser, and TOPIC, a string."
  (pubsub-unsubscribe topic
                      (mantra-parser-name subscriber)))

(defun mantra-parsing-in-progress-p (parser)
  "Whether PARSER is already parsing, i.e., accumulating state."
  (not (equal (mantra-parser-null-state parser)
              (mantra-parser-state parser))))

(defun mantra-parse (parser input)
  "Parse INPUT using PARSER.

If PARSER is already parsing (i.e., it already has state), then append
INPUT to PARSER's current state.  Otherwise, begin parsing by
initializing PARSER's state to INPUT if the start condition is met.
Otherwise, do nothing.

If parsing is in progress, either prior to or as a result of the
present invocation, then also check the abort condition for the parser
to see if parsing should be aborted. This is done at this stage before
the actual command is executed (i.e., not in `mantra-parse-finish'
like checking the accept predicate) to support avoiding an infinite
loop in some self-referential cases like repeating the last command.

INPUT could be anything, but may typically be the current key sequence
entered on the Emacs command loop. Aborting or accepting is based on
the entire parsed state in PARSER, not just the current key sequence."
  (when (or (mantra-parsing-in-progress-p parser)
            (funcall (mantra-parser-start parser)
                     input))
    (mantra-parser-set-state parser
                             (funcall (mantra-parser-compose parser)
                                      (mantra-parser-state parser)
                                      (funcall (mantra-parser-map parser)
                                               input)))
    ;; The accept predicate is checked in `mantra-parse-finish', at
    ;; which point state already includes input. For consistency in
    ;; the interface, we check the abort predicate here *after*
    ;; incorporating input into state.
    (when (funcall (mantra-parser-abort parser)
                   input
                   (mantra-parser-state parser))
      (mantra-parser-clear-state parser))))

(defun mantra-accept (parser)
  "Accept the current state in PARSER.

This publishes the parsed key sequence under the topic with PARSER's
name.  Note that as the pub/sub system is not persistent, it does not
store any parsed key sequences after notifying subscribers of them.

After publishing the match, this clears the parser state."
  (let ((name (mantra-parser-name parser))
        (state (mantra-parser-state parser)))
    (pubsub-publish name state)
    ;; clear state
    (mantra-parser-clear-state parser)))

(defun mantra-parse-finish (parser input)
  "Accept PARSER state or continue parsing.

INPUT is the current key sequence.  Aborting or accepting is based on
the entire parsed state in PARSER, not just the current key sequence.

If neither accept nor abort conditions are met, do nothing, i.e.,
continue parsing."
  ;; state already includes input by this point,
  ;; as this is being called in post-command
  (when (and (mantra-parsing-in-progress-p parser)
             (funcall (mantra-parser-stop parser)
                      input
                      (mantra-parser-state parser)))
    (mantra-accept parser)))

(defun mantra-feed-parser (parser input)
  "Feed INPUT to PARSER.

Typically, the `parse' and `parse-finish' stages are done together
rather than at separate times, so this function that does both may be
used, as a convenience.

But having them be separate interfaces allows the client to decide
when the parser accepts or aborts, lending additional flexibility in
cases where perhaps greedy matching may not be desirable.

For instance, if anything starting with \"h\" and ending with \"ello\"
happens to be a valid parse for some language, and if we provide the
input \"hellohairyello\" incrementally, `mantra-feed-parser' would
accept \"hello\" and \"hairyello\", but using the interfaces
separately allows us to parse either this result, or even the entire
input, as a valid expression.

Although I'm not aware of a current use for it, it doesn't hurt to
separately provide `parse' and `parse-finish', and it also makes
these stages easier to test."
  (mantra-parse parser input)
  (mantra-parse-finish parser input))


(provide 'mantra-parse)
;;; mantra-parse.el ends here
