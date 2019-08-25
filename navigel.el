;;; navigel.el --- Facilitate the creation of tabulated-list based UIs -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Url: https://gitlab.petton.fr/DamienCassou/navigel
;; Package-requires: ((emacs "25.1") (tablist "1.0"))
;; Version: 0.3.0

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

;; This library makes it simpler for Emacs Lisp developers to define
;; user-interfaces based on tablists (also known as tabulated-lists).
;; Overriding a few (CL) methods and calling `navigel-open' is all
;; that's required to get a nice UI to navigate your domain objects
;; (e.g., files, music library, database).
;;
;; Features include :
;;
;; - pressing RET to open the entity at point in another buffer;
;; - pressing ^ to open the current entity's parent;
;; - marking entities for bulk operations (e.g., delete);
;; - `imenu' support for quick navigation;

;;; Code:

(require 'tablist)
(require 'seq)


(defgroup navigel nil
  "Navigel."
  :group 'magit-extensions)

(defcustom navigel-changed-hook nil
  "Normal hook run after a navigel's tablist buffer has been refreshed or populated."
  :type 'hook)


;; Private variables

(defvar navigel-entity nil
  "Specify the entity that was used to generate the buffer.")

(defvar navigel-app nil
  "Specify the application that was used to generate the buffer.")


;; Private functions

(defun navigel--tablist-operation-function (operation &rest args)
  "Setup `tablist' operations in current buffer.

OPERATION and ARGS are defined by `tablist-operations-function'."
  (cl-case operation
    (supported-operations '(find-entry delete))
    (find-entry (navigel-open (car args) nil))
    (delete (navigel-delete (car args) #'navigel-revert-buffer))))

(defun navigel--imenu-extract-index-name ()
  "Return the name of entity at point for `imenu'.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line."
  (navigel-imenu-name (tabulated-list-get-id)))

(defun navigel--imenu-prev-index-position ()
  "Move point to previous line in current buffer.
This function is used as a value for
`imenu-prev-index-position-function'."
  (unless (bobp)
    (forward-line -1)))

(defun navigel-go-to-entity (entity)
  "Move point to ENTITY.
Return non-nil if ENTITY is found, nil otherwise."
  (goto-char (point-min))
  (while (and (not (= (point) (point-max)))
              (not (navigel-equal (navigel-entity-at-point) entity)))
    (forward-line 1))
  (not (= (point) (point-max))))

;; CL Context rewriter: this lets users write "&context (navigel-app
;; something)" instead of "&context (navigel-app (eql something))"
(cl-generic-define-context-rewriter navigel-app (app)
  `(navigel-app (eql ,app)))


;; Generic methods: Those methods are the one you may override.

(cl-defgeneric navigel-name (entity)
  "Return a short string describing ENTITY.

The returned value is the default for `navigel-buffer-name',
`navigel-tablist-name' and `navigel-imenu-name'.  Those can be
overridden separately if necessary."
  (format "%s" entity))

(cl-defgeneric navigel-buffer-name (entity)
  "Return a string representing ENTITY in the buffer's name."
  (navigel-name entity))

(cl-defgeneric navigel-tablist-name (entity)
  "Return a string representing ENTITY in tablist columns."
  (navigel-name entity))

(cl-defgeneric navigel-imenu-name (entity)
  "Return a string representing ENTITY for `imenu'."
  (navigel-name entity))

(cl-defgeneric navigel-children (entity callback)
  "Execute CALLBACK with the list of ENTITY's children as argument.
This method must be overridden for any tablist view to work.")

(cl-defmethod navigel-children ((entities list) callback)
  "Execute CALLBACK with the children of ENTITIES as argument."
  (navigel-async-mapcar  #'navigel-children entities callback))

(cl-defgeneric navigel-parent (_entity)
  "Return the parent of ENTITY if possible, nil if not."
  nil)

(cl-defgeneric navigel-equal (entity1 entity2)
  "Return non-nil if ENTITY1 and ENTITY2 represent the same entity."
  (equal entity1 entity2))

(cl-defgeneric navigel-entity-at-point ()
  "Return the entity at point or nil if none.")

(cl-defmethod navigel-entity-at-point (&context (major-mode (derived-mode navigel-tablist-mode)))
  (tabulated-list-get-id))

(cl-defgeneric navigel-marked-entities (&optional _at-point-if-empty)
  "Return a list of entities that are selected.
If no entity is selected and AT-POINT-IF-EMPTY is non-nil, return
a list with just the entity at point."
  nil)

(cl-defmethod navigel-marked-entities (&context (major-mode (derived-mode navigel-tablist-mode))
                                                &optional at-point-if-empty)
  ;; `tablist-get-marked-items' automatically includes the entity at
  ;; point if no entity is marked. We have to remove it unless
  ;; `at-point-if-empty' is non-nil.
  (let ((entities (mapcar #'car (tablist-get-marked-items))))
    (if (or (> (length entities) 1)
            (save-excursion ;; check if the entity is really marked
              (navigel-go-to-entity (car entities))
              (tablist-get-mark-state))
            at-point-if-empty)
        entities
      (list))))

(cl-defgeneric navigel-entity-buffer (entity)
  "Return a buffer name for ENTITY.
The default name is based on `navigel-app' and `navigel-buffer-name'."
  (format "*%s-%s*" navigel-app (navigel-buffer-name entity)))

(cl-defgeneric navigel-entity-tablist-mode (_entity)
  "Enable the `major-mode' most suited to display children of ENTITY."
  (navigel-tablist-mode))

(cl-defgeneric navigel-tablist-format (_entity)
  "Return a vector specifying columns to display ENTITY's children.
The return value is set as `tabulated-list-format'."
  (vector (list "Name" 0 t)))

(cl-defgeneric navigel-entity-to-columns (entity)
  "Return the column descriptors to display ENTITY in a tabulated list.
The return value is a vector for `tabulated-list-entries'.

The vector should be compatible to the one defined with
`navigel-tablist-format'."
  (vector (navigel-tablist-name entity)))

(cl-defgeneric navigel-open (entity target)
  "Open a buffer displaying ENTITY.
If TARGET is non-nil and is in buffer, move point to it.

By default, list ENTITY's children in a tabulated list.
"
  (navigel-list-children entity target))

(cl-defgeneric navigel-delete (_entity &optional _callback)
  "Remove ENTITY from its parent.
If non-nil, call CALLBACK with no parameter when done."
  (user-error "This operation is not supported in this context"))

(cl-defmethod navigel-delete ((entities list) &optional callback)
  "Remove each item of ENTITIES from its parent.
If non-nil, call CALLBACK with no parameter when done."
  (navigel-async-mapc #'navigel-delete entities callback))

(defun navigel-async-mapcar (mapfn list callback)
  "Apply MAPFN to each element of LIST and pass result to CALLBACK.

MAPFN is a function taking 2 arguments: the element to map and a
callback to call when the mapping is done."
  (if (not list)
      (funcall callback nil)
    (let ((result (make-vector (length list) nil))
          (count 0))
      (cl-loop for index below (length list)
               for item in list
               do (let ((index index) (item item))
                    (funcall
                     mapfn
                     item
                     (lambda (item-result)
                       (setf (seq-elt result index) item-result)
                       (cl-incf count)
                       (when (eq count (length list))
                         ;; use `run-at-time' to ensure that CALLBACK is
                         ;; consistently called asynchronously even if MAPFN is
                         ;; synchronous:
                         (run-at-time
                          0 nil
                          callback
                          (seq-concatenate 'list result))))))))))

(defun navigel-async-mapc (mapfn list callback)
  "Same as `navigel-async-mapcar' but for side-effects only.

MAPFN is a function taking 2 arguments: an element of LIST and a
callback.  MAPFN should call the callback with no argument when
done computing.

CALLBACK is a function of no argument that is called when done
computing for the all elements of LIST."
  (navigel-async-mapcar
   (lambda (item callback) (funcall mapfn item (lambda () (funcall callback nil))))
   list
   (lambda (_result) (funcall callback))))

(defun navigel-list-children (entity &optional target)
  "Open a new buffer showing ENTITY's children.

If TARGET is non-nil and is in buffer, move point to it.

Interactively, ENTITY is either the element at point or the user
is asked for a top level ENTITY."
  ;; save navigel-app because (navigel-tablist-mode) will reset it
  (let ((app navigel-app)
        (buffer (get-buffer-create (navigel-entity-buffer entity))))
    (with-current-buffer buffer
      (navigel-entity-tablist-mode entity)
      (setq-local tabulated-list-padding 2) ; for `tablist'
      (setq-local navigel-entity entity)
      (setq-local navigel-app app)
      (setq-local tablist-operations-function #'navigel--tablist-operation-function)
      (setq-local revert-buffer-function #'navigel-revert-buffer)
      (setq-local imenu-prev-index-position-function
                  #'navigel--imenu-prev-index-position)
      (setq-local imenu-extract-index-name-function
                  #'navigel--imenu-extract-index-name)
      (setq-local tabulated-list-format (navigel-tablist-format entity))
      (tabulated-list-init-header)
      (navigel-refresh target)
      (switch-to-buffer buffer))))

(defun navigel--save-state ()
  "Return an object representing the state of the current buffer.
This should be restored with `navigel--restore-state'.

The state contains the entity at point, the column of point, and the marked entities."
  `(
    (entity-at-point . ,(navigel-entity-at-point))
    (column . ,(current-column))
    (marked-entities . ,(navigel-marked-entities))))

(defun navigel--restore-state (state)
  "Restore STATE.  This was saved with `navigel--save-state'."
  (let-alist state
    (if .entity-at-point
        (navigel-go-to-entity .entity-at-point)
      (setf (point) (point-min)))
    (when .column
      (setf (point) (line-beginning-position))
      (forward-char .column))
    (when .marked-entities
      (save-excursion
        (dolist (entity .marked-entities)
          (when (navigel-go-to-entity entity)
            (tablist-put-mark)))))))

(defun navigel-refresh (&optional target)
  "Compute `navigel-entity' children and list those in the current buffer.

If TARGET is non-nil and is in buffer, move point to it."
  (let ((entity navigel-entity)
        ;; save navigel-app so we can rebind below
        (app navigel-app))
    (message (if (equal (point-min) (point-max))
                 "Populating…"
               "Refreshing…"))
    (navigel-children
     entity
     (lambda (children)
       ;; restore navigel-app
       (let ((navigel-app app) state)
         (with-current-buffer (get-buffer-create (navigel-entity-buffer entity))
           (setq state (navigel--save-state))
           (setq-local tabulated-list-entries
                       (mapcar
                        (lambda (child) (list child (navigel-entity-to-columns child)))
                        children))
           (tabulated-list-print)
           (navigel--restore-state state)
           (when target
             (navigel-go-to-entity target))
           (run-hooks 'navigel-changed-hook)
           (message "Ready!")))))))

(defun navigel-revert-buffer (&rest _args)
  "Compute `navigel-entity' children and list those in the current buffer."
  (navigel-refresh))

(defun navigel-tablist-open-entity-parent-at-point ()
  "Open a buffer showing the parent of entity at point."
  (interactive)
  (let* ((entity (navigel-entity-at-point))
         (parent (navigel-parent entity))
         (ancestor (and parent (navigel-parent parent))))
    (cond ((and ancestor (navigel-equal parent navigel-entity))
           (navigel-open ancestor parent))
          ((and parent (not (navigel-equal parent navigel-entity)))
           (navigel-open parent entity))
          (t
           (message "No parent to go to.")))))

(defvar navigel-tablist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "^") #'navigel-tablist-open-entity-parent-at-point)
    map)
  "Keymap for `navigel-tablist-mode'.")

(define-derived-mode navigel-tablist-mode tablist-mode "navigel-tablist"
  "Major mode for all elcouch listing modes.")

(provide 'navigel)
;;; navigel.el ends here

;;; LocalWords:  navigel tablist tablists keymap
