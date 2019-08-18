;;; navigel-test.el --- Tests for navigel.el         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for navigel.el.

;;; Code:

(require 'navigel)

(require 'ert)

(require 'cl-lib)

(defvar navigel-run-at-time-calls nil
  "Save calls to `run-at-time'.")

(defun navigel-call-run-at-time ()
  "Execute each element of `navigel-run-at-time-calls'."
  (pcase-dolist (`(,_time ,_repeat ,function ,args) (seq-copy navigel-run-at-time-calls))
    (apply function args)
    (setq navigel-run-at-time-calls (cdr navigel-run-at-time-calls))))

(defmacro navigel-make-synchronous (&rest body)
  "Make `run-at-time' synchronous while executing BODY."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'run-at-time)
              (lambda (time repeat function &rest args)
                (setq navigel-run-at-time-calls
                      (append navigel-run-at-time-calls `((,time ,repeat ,function ,args)))))))
     (unwind-protect
         (progn ,@body)
       (setq navigel-run-at-time-calls nil))))

(ert-deftest navigel-async-mapcar-sync-mapfn ()
  (let ((actual nil))
    (navigel-make-synchronous
        (navigel-async-mapcar
         (lambda (item callback)
           (funcall callback (1+ item)))
         '(1 2 3)
         (lambda (result)
           (setq actual result)))
      (should (equal (length navigel-run-at-time-calls) 1))
      (navigel-call-run-at-time)
      (should (equal actual '(2 3 4))))))

(ert-deftest navigel-async-mapcar-async-mapfn ()
  (let ((actual nil))
    (navigel-make-synchronous
        (navigel-async-mapcar
         (lambda (item callback)
           (run-at-time item nil callback (1+ item)))
         '(1 2 3)
         (lambda (result)
           (setq actual result)))
      (should (equal (length navigel-run-at-time-calls) 3))
      (navigel-call-run-at-time)
      (should (equal (length navigel-run-at-time-calls) 1))
      (navigel-call-run-at-time)
      (should (equal actual '(2 3 4))))))

(ert-deftest navigel-async-mapc ()
  (let ((result (make-vector 3 0))
        (callback-called nil))
    (navigel-make-synchronous
        (navigel-async-mapc
         (lambda (item callback)
           (setf (seq-elt result (car item)) (cdr item))
           (funcall callback))
         (list '(0 .  a) '(1 .  b) '(2 . c))
         (lambda ()
           (should (equal result [a b c]))
           (setq callback-called t)))
      (should (equal (length navigel-run-at-time-calls) 1))
      (navigel-call-run-at-time)
      (should callback-called))))

(provide 'navigel-test)
;;; navigel-test.el ends here
