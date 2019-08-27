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

(ert-deftest navigel-insert-context-in-args ()
  (progn
    ;; no arguments:
    (should (equal
             (navigel--insert-context-in-args 'foo '())
             '(&context (navigel-app foo))))
    ;; only mandatory arguments:
    (should (equal
             (navigel--insert-context-in-args 'foo '(a))
             '(a &context (navigel-app foo))))
    ;; special argument:
    (should (equal
             (navigel--insert-context-in-args 'foo '(a &optional b))
             '(a &context (navigel-app foo) &optional b)))
    ;; context argument:
    (should (equal
             (navigel--insert-context-in-args 'foo '(a &context (a b)))
             '(a &context (navigel-app foo) (a b))))
    ;; special + context argument:
    (should (equal
             (navigel--insert-context-in-args 'foo '(a &context (a b) &optional b))
             '(a &context (navigel-app foo) (a b) &optional b)))))

(provide 'navigel-test)
;;; navigel-test.el ends here
