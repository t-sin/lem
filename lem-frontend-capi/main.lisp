(in-package :lem-capi)

(defclass capi (lem::implementation)
  ()
  (:default-initargs
   :native-scroll-support nil
   :redraw-after-modifying-floating-window nil))

(setf lem:*implementation* (make-instance 'capi))

(defvar *editor-pane*)
(defvar *editor-thread*)

(defmethod lem::interface-invoke ((implementation capi) function)
  (setf *editor-pane* (make-instance 'lem-editor-pane))
  (capi:display
   (make-instance 'capi:interface
                  :auto-menus nil
                  :best-width 800
                  :best-height 600
                  :layout (make-instance 'capi:column-layout :description (list *editor-pane*))))
  (setf *editor-thread*
        (funcall function
                 nil
                 (lambda (report)
                   (declare (ignore report))
                   (capi:quit-interface *editor-pane*)))))

(defmethod lem::interface-display-background-mode ((implementation capi))
  (background-mode *editor-pane*))

(defmethod lem::interface-update-foreground ((implementation capi) color-name)
  (set-foreground *editor-pane* color-name))

(defmethod lem::interface-update-background ((implementation capi) color-name)
  (set-background *editor-pane* color-name))

(defmethod lem::interface-display-width ((implementation capi))
  (lem-editor-pane-char-width *editor-pane*))

(defmethod lem::interface-display-height ((implementation capi))
  (lem-editor-pane-char-height *editor-pane*))

(defmethod lem::interface-make-view ((implementation capi) window x y width height use-modeline)
  (add-view *editor-pane* x y width height use-modeline))

(defmethod lem::interface-delete-view ((implementation capi) view)
  (delete-view *editor-pane* view))

(defmethod lem::interface-clear ((implementation capi) view)
  (clear view))

(defmethod lem::interface-set-view-size ((implementation capi) view width height)
  (set-view-size view width height))

(defmethod lem::interface-set-view-pos ((implementation capi) view x y)
  (set-view-position view x y))

(defmethod lem::interface-print ((implementation capi) view x y string attribute)
  (draw-string view x y string attribute))

(defmethod lem::interface-clear-eol ((implementation capi) view x y)
  (clear-eol view x y))

(defmethod lem::interface-clear-eob ((implementation capi) view x y)
  (clear-eob view x y))

(defmethod lem::interface-print-modeline ((implementation capi) view x y string attribute)
  (draw-string-in-modeline view x y string attribute))

(defmethod lem::interface-move-cursor ((implementation capi) view x y)
  nil)

(defmethod lem::interface-redraw-view-after ((implementation capi) view focus-window-p)
  nil)

(defmethod lem::interface-update-display ((implementation capi))
  nil)

(defmethod lem::interface-scroll ((implementation capi) view n)
  nil)

(pushnew :lem-capi *features*)
