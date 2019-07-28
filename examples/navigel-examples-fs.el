;;; navigel-examples-fs.el --- Example of navigel to navigate the filesystem  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'f)

(require 'navigel)

(cl-defmethod navigel-name (file &context (navigel-app navigel-examples-fs))
  (f-filename file))

(cl-defmethod navigel-parent (file &context (navigel-app navigel-examples-fs))
  (f-dirname file))

(cl-defmethod navigel-children (directory callback &context (navigel-app navigel-examples-fs))
  "Call CALLBACK with the files in DIRECTORY as parameter."
  (when (not (file-directory-p directory))
    (user-error "%s is not a directory" directory))
  (run-at-time nil nil callback (f-entries directory)))

(cl-defmethod navigel-equal (file1 file2 &context (navigel-app navigel-examples-fs))
  (f-equal-p file1 file2))

(defun navigel-examples-fs-list-files (&optional directory)
  "List files of DIRECTORY, home directory if nil."
  (interactive (list "~/"))
  (let ((navigel-app 'navigel-examples-fs))
    (navigel-open
     (f-expand directory)
     (f-expand "~/.emacs.d"))))

(cl-defmethod navigel-open (file _target &context (navigel-app navigel-examples-fs))
  (if (f-file-p file)
      (find-file file)
    (cl-call-next-method)))

(defun navigel-examples-fs-size (file)
  "Return FILE size as number of bytes."
  (nth 7 (file-attributes file)))

(cl-defmethod navigel-tablist-format (_entity &context (navigel-app navigel-examples-fs))
  (vector (list "Size (B)" 10
                (lambda (entry1 entry2)
                  (< (navigel-examples-fs-size (car entry1)) (navigel-examples-fs-size (car entry2))))
                :right-align t)
          (list "Name" 0 t)))

(cl-defmethod navigel-entity-to-columns (entity &context (navigel-app navigel-examples-fs))
  (vector (number-to-string (navigel-examples-fs-size entity))
          (navigel-name entity)))

(cl-defmethod navigel-delete (files &context (navigel-app navigel-examples-fs))
  (dolist (file files)
    (if (f-directory-p file)
        (delete-directory file t t)
      (delete-file file t))))

(provide 'navigel-examples-fs)
;;; navigel-examples-fs.el ends here
