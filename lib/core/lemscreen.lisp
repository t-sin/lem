(defpackage :lem.lemscreen
  (:use :cl :lem :lem.button)
  (:export :lemscreen-active-hud-attribute
           :lemscreen-attribute
           :lemscreen-background-attribute
           :lemscreen)
  #+sbcl
  (:lock t))
(in-package :lem.lemscreen)

(define-attribute lemscreen-active-hud-attribute
  (t :foreground "black" :background "dark gray"))

(define-attribute lemscreen-attribute
  (t :foreground "white" :background "gray"))

(define-attribute lemscreen-background-attribute
  (t :underline-p t))

(defstruct window-backup
  x y width height buffer-name)

(defclass lemscreen-window (header-window)
  ((buffer
    :initarg :buffer
    :accessor lemscreen-status-line-buffer)
   (window-tree-list
    :initform '()
    :accessor lemscreen-window-tree-list
    :type window-backup)
   (window-tree-index
    :initform -1
    :accessor lemscreen-window-tree-index)))

(defvar *lemscreen* nil)

(defun lemscreen-init ()
  (let ((buffer (make-buffer "*lemscreen*" :enable-undo-p nil :temporary t)))
    (setf (variable-value 'truncate-lines :buffer buffer) nil)
    (setf *lemscreen* (make-instance 'lemscreen-window
                                     :buffer buffer))))
;; よくわかってない
(defun lemscreen-require-update ()
  (block exit
    (unless (eq (current-buffer) (lemscreen-prev-current-buffer *lemscreen*))
      (return-from exit t))
    (unless (eq (window-tree)
                (elt (lemscreen-window-tree-list *lemscreen*)
                     (lemscreen-window-tree-index *lemscreen*)))
      (return-from exit t))
    (unless (= (display-width) (lemscreen-display-width *lemscreen*))
      (return-from exit t))
    nil))

;; よくわかってない
(defmethod window-redraw ((window lemscreen-window) force)
  (when (or force (lemscreen-require-update))
    (let* ((buffer (lemscreen-status-lien-buffer *lemscreen*))
           (p (buffer-point buffer))
           (charpos (point-charpos p)))
      (erase-buffer buffer)
      (dolist (buffer (buffer-list))
        (let ((focusp (eq buffer (current-buffer))))
          (let ((start-pos (point-charpos p)))
            (insert-button p
                           (let ((name (buffer-name buffer)))
                             (if (< 20 (length name))
                                 (format nil " ~A... " (subseq name 0 17))
                                 (format nil " ~A " name)))
                           (let ((buffer buffer))
                             (lambda () (switch-to-buffer buffer nil)))
                           :attribute (if focusp
                                          'lemscreen-active-tab-attribute
                                          'lemscreen-attribute))
            (when focusp
              (let ((end-pos (point-charpos p)))
                (unless (<= start-pos charpos (1- end-pos))
                  (setf charpos start-pos)))))))
      (let ((n (- (display-width) (point-column p))))
        (when (> n 0)
          (insert-string p (make-string n :initial-element #\space)
                         :attribute 'lemscreen-background-attribute)))
      (line-offset p 0 charpos))
    (call-next-method)))

;; よくわかってない
(defun lemscreen-clear-cache ()
  (setf (lemscreen-buffer *lemscreen*) nil)
  (setf (lemscreen-window-tree-list *lemscreen*) '())
  (setf (lemscreen-window-tree-index *lemscreen*) -1))

(defun lemscreen-off ()
  (when (and (variable-value 'lemscreen :global)
             *lemscreen*)
    (lemscreen-clear-cache)
    (delete-window *lemscreen*)
    (setf *lemscreen* nil)))

(defun lemscreen-on ()
  (unless (variable-value 'lemscreen :global)
    (lemscreen-init)))

(define-editor-variable lemscreen nil ""
  (lambda (value)
    (if value
        (lemscreen-on)
        (lemscreen-off))))

(define-command lemscreen () ()
  (setf (variable-value 'lemscreen :global)
        (not (variable-value 'lemscreen :global))))

;;; key bindings
;; C-z c => create new lemscreen
;; C-z d => delete current lemscreen
;; C-z n => switch to next lemscreen
;; C-z p => switch to previous lemscreen

(define-command lemscreen-create (n) ("p")
  )

(define-command lemscreen-delete (n) ("p")
  )

(define-command lemscreen-next (n) ("p")
  (let ((p (buffer-point (lemscreen-buffer *lemscreen*))))
    (dotimes (_ n)
      (forward-button p))
    (let ((button (button-at p)))
      (when button
        (move-point p (button-end button))
        (character-offset p -1)
        (button-action button)))))

(define-command lemscreen-prev (n) ("p")
  (let ((p (buffer-point (lemscreen-buffer *lemscreen*))))
    (dotimes (_ n)
      (backward-button p))
    (let ((button (button-at p)))
      (when button
        (button-action button)))))

(add-hook *after-init-hook*
          (lambda ()
            (when (variable-value 'lemscreen :global)
              (lemscreen-init))))
