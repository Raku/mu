(in-package #:kp6-lisp)

; quit from cl-port:
;;; Basic extensions: conditions, compositions &c
;;;
;;; Copyright (C) 1999-2006 by Sam Steingold
;;; This is open-source software.
;;; GNU Lesser General Public License (LGPL) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.
;;; See <URL:http://www.gnu.org/copyleft/lesser.html>
;;; for details and the precise copyright document.
;;;
;;; $Id: ext.lisp,v 1.43 2006/04/07 21:59:23 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/port/ext.lisp,v $
(defun quit (&optional code)
  #+abcl (ext:quit code)
  #+allegro (excl:exit code)
  #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
  #+cmu (ext:quit code)
  #+cormanlisp (win32:exitprocess code)
  #+gcl (lisp:bye code)
  #+lispworks (lw:quit :status code)
  #+lucid (lcl:quit code)
  #+sbcl (sb-ext:quit :unix-status
                      (typecase code ((signed-byte 32) code) (null 0) (t 1)))
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'quit code)))