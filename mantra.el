;;; mantra.el ---  A system for scripting and parsing activity beyond macros -*- lexical-binding: t -*-

;; Author: Sid Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/mantra
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (pubsub "0.1"))

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

;; Mantras, not macros! A system for defining, parsing, and acting on
;; user activity.
;;
;; This package provides two main facilities:
;;
;; 1. A DSL for composing "mantras" to represent complex user
;;    actions. Mantras are a more expressive form of keyboard macro,
;;    able to include conditions, loops, and programmatic logic. This
;;    can be used to script user behavior, similar to how Selenium is
;;    used to automate web browsers.
;;
;; 2. A real-time parser that can match live user activity against
;;    arbitrary patterns and conditions. This allows you to define
;;    patterns of "interesting" activity and trigger custom actions
;;    (including, commonly, mantras) when those patterns are detected.
;;
;; Together, these components enable the creation of powerful tools
;; like intelligent repeat mechanisms (e.g., repeating actions that
;; involve completion UIs), creating contextual location histories
;; like the jump and change lists in Evil mode, or advanced command
;; recorders.

;;; Code:

(require 'mantra-dsl)
(require 'mantra-parse)

(provide 'mantra)
;;; mantra.el ends here
