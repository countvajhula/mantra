;;; mantra-debug.el --- Mantras, not macros! -*- lexical-binding: t -*-

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

;; Debugging tools for mantra parsers

;;; Code:

(defun mantra-debug-parser-start (orig-fn key-seq)
  "Print debugging information about the start phase of parsing.

This is intended to be used to advise the start phase of a parser.
ORIG-FN is the start function, and KEY-SEQ is the currently entered
key sequence."
  (let ((result (funcall orig-fn key-seq)))
    (message "DEBUG (start): key %s result %s"
             (key-description key-seq)
             ;; convert to boolean
             (not (not result)))
    result))

(defun mantra-debug-parser-stop (orig-fn key-seq state)
  "Print debugging information about the stop phase of parsing.

This is intended to be used to advise the stop phase of a parser.
ORIG-FN is the stop function, KEY-SEQ is the currently entered key
sequence, and STATE is the accumulated parser state."
  (let ((result (funcall orig-fn key-seq state)))
    (message "DEBUG (stop): key %s state %s result %s"
             (key-description key-seq)
             state
             ;; convert to boolean
             (not (not result)))
    result))

(defun mantra-debug-parser-abort (orig-fn key-seq state)
  "Print debugging information about the abort phase of parsing.

This is intended to be used to advise the abort phase of a parser.
ORIG-FN is the abort function, KEY-SEQ is the currently entered key
sequence, and STATE is the accumulated parser state."
  (let ((result (funcall orig-fn key-seq state)))
    (message "DEBUG (abort): key %s state %s result %s"
             (key-description key-seq)
             state
             ;; convert to boolean
             (not (not result)))
    result))

(provide 'mantra-debug)
;;; mantra-debug.el ends here
