;;; mantra-util.el --- Mantras, not macros! -*- lexical-binding: t -*-

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

;; Helpful utilities

;;; Code:

(require 'mantra-dsl)

(defun mantra--pretty-print (mantra)
  "A helper for pretty-printing MANTRA."
  (cond ((vectorp mantra) (key-description mantra))
        ((stringp mantra) mantra)
        ((mantra-seq-p mantra) (mapcar #'mantra--pretty-print
                                       (mantra--seq-phases mantra)))
        ((mantra-insertion-p mantra) (substring-no-properties (mantra--insertion-text mantra)))))

(defun mantra-pretty-print (mantra)
  "An identifiable string representation of MANTRA."
  (let ((result (mantra--pretty-print mantra)))
    (if (listp result)
        (string-join (flatten-list result))
      result)))


(provide 'mantra-util)
;;; mantra-util.el ends here
