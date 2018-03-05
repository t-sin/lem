(in-package :lem-capi)

(defmacro progn-in-pane-process ((pane &key wait) &body body)
  (if wait
      `(capi:apply-in-pane-process-wait-single ,pane nil (lambda () ,@body))
      `(capi:apply-in-pane-process ,pane (lambda () ,@body))))

(defvar *default-font-family* "DejaVu Sans Mono")
(defvar *default-font-size* 10)

(defvar *font*
  (gp:make-font-description :size *default-font-size*
                            :family *default-font-family*))

(defvar *bold-font*
  (gp:make-font-description :size *default-font-size*
                            :family *default-font-family*
                            :weight :bold))

(defun font-char-size (self &optional font)
  (multiple-value-bind (left top right bottom)
      (gp:get-string-extent self "a" font)
    (values (- right left)
            (+ (abs top) bottom))))

(defun font-char-width (self &optional font)
  (values (font-char-size self font)))

(defun font-char-height (self  &optional font)
  (nth-value 1 (font-char-size self font)))

(defun convert-color (color default-color)
  (if-let (rgb (lem:parse-color color))
      (let ((n (/ 1.0 255)))
        (destructuring-bind (r g b) rgb
          (color:make-rgb (* r n) (* g n) (* b n))))
    default-color))

(defun gesture-spec-to-key (gesture-spec)
  (labels ((shift-bit-p (modifiers)
             (/= 0 (logand modifiers sys:gesture-spec-shift-bit)))
           (control-bit-p (modifiers)
             (/= 0 (logand modifiers sys:gesture-spec-control-bit)))
           (meta-bit-p (modifiers)
             (/= 0 (logand modifiers sys:gesture-spec-meta-bit))))
    (when (sys:gesture-spec-p gesture-spec)
      (let* ((data (sys:gesture-spec-data gesture-spec))
             (modifiers (sys:gesture-spec-modifiers gesture-spec))
             (shiftp (shift-bit-p modifiers))
             (ctrlp (control-bit-p modifiers))
             (metap (meta-bit-p modifiers))
             (sym (typecase data
                    (string data)
                    (keyword (string-capitalize data))
                    (integer
                     (let ((char (code-char data)))
                       (cond ((char= char #\Return)
                              "Return")
                             ((char= char #\Tab)
                              "Tab")
                             ((char= char #\Escape)
                              "Escape")
                             ((char= char #\Backspace)
                              "Backspace")
                             (t
                              (string char))))))))
        (when sym
          (cond ((and (not metap) ctrlp (not shiftp) (string= sym "i"))
                 (lem:make-key :sym "Tab"))
                (t
                 (lem:make-key :meta metap
                               :ctrl ctrlp
                               :shift shiftp
                               :sym sym))))))))

(defclass lem-editor-pane (capi:pinboard-layout)
  ((background-mode :initform :light :accessor lem-editor-pane-background-mode))
  (:default-initargs
   :font *font*
   :input-model '((:gesture-spec key-press))
   :resize-callback 'resize-callback))

(defclass lem-view (capi:output-pane)
  ((char-width :initarg :char-width :accessor lem-view-char-width)
   (char-height :initarg :char-height :accessor lem-view-char-height)
   (use-modeline :initarg :use-modeline :accessor lem-view-use-modeline))
  (:default-initargs
   :font *font*
   :foreground :black
   :background :white))

(defun add-view (lem-editor-pane char-x char-y char-width char-height use-modeline)
  (progn-in-pane-process (lem-editor-pane :wait t)
    (when use-modeline (incf char-height))
    (multiple-value-bind (font-width font-height)
        (font-char-size lem-editor-pane)
      (let* ((x (* char-x font-width))
             (y (* char-y font-height))
             (width (* char-width font-width))
             (height (* char-height font-height))
             (view (make-instance 'lem-view
                                  :x x :y y
                                  :width width :height height
                                  :char-width char-width :char-height char-height
                                  :use-modeline use-modeline)))
        (push view (capi:layout-description lem-editor-pane))
        view))))

(defun delete-view (lem-editor-pane view)
  (progn-in-pane-process (view)
    (setf (capi:layout-description lem-editor-pane)
          (delete view (capi:layout-description lem-editor-pane)))))

(defun resize-callback (&rest args)
  (declare (ignore args))
  (lem:send-event :resize))

(defun key-press (self x y gesture-spec)
  (declare (ignore self x y))
  (when-let (key (gesture-spec-to-key gesture-spec))
    (lem:send-event key)))

(defun background-mode (lem-editor-pane)
  (progn-in-pane-process (lem-editor-pane :wait t)
    (lem-editor-pane-background-mode lem-editor-pane)))

(defun set-foreground (lem-editor-pane color)
  (when-let (color (convert-color color nil))
    (dolist (view (capi:layout-description lem-editor-pane))
      (setf (capi:simple-pane-foreground view) color))))

(defun set-background (lem-editor-pane color)
  (when-let (color (convert-color color nil))
    (dolist (view (capi:layout-description lem-editor-pane))
      (setf (capi:simple-pane-background view) color))))

(defun set-font (lem-editor-pane font)
  (declare (ignore lem-editor-pane font))
  )

(defun lem-editor-pane-char-width (lem-editor-pane)
  (values (floor (capi:simple-pane-visible-width lem-editor-pane)
                 (font-char-width lem-editor-pane))))

(defun lem-editor-pane-char-height (lem-editor-pane)
  (values (floor (capi:simple-pane-visible-height lem-editor-pane)
                 (font-char-height lem-editor-pane))))

(defun set-view-position (view char-x char-y)
  (progn-in-pane-process (view)
    (multiple-value-bind (font-width font-height)
        (font-char-size view)
      (let ((x (* char-x font-width))
            (y (* char-y font-height)))
        (setf (capi:static-layout-child-position view) (values x y))))))

(defun set-view-size (view char-width char-height)
  (progn-in-pane-process (view)
    (when (lem-view-use-modeline view) (incf char-height))
    (multiple-value-bind (font-width font-height)
        (font-char-size view)
      (let ((width (* char-width font-width))
            (height (* char-height font-height)))
        (setf (lem-view-char-width view) char-width)
        (setf (lem-view-char-height view) char-height)
        (setf (capi:static-layout-child-size view) (values width height))))))

(defun %draw-string (view char-x char-y string &key foreground background underline bold reverse)
  (let ((font (if bold
                  (gp:find-best-font view *bold-font*)
                  (gp:find-best-font view *font*))))
    (when reverse (rotatef foreground background))
    (multiple-value-bind (left top right bottom)
        (gp:get-string-extent view "a" font)
      (let* ((font-width (- right left))
             (font-height (+ (abs top) bottom))
             (x0 (* char-x font-width))
             (x x0)
             (y (+ (* char-y font-height) (abs top))))
        (loop :for c :across string :do
              (gp:draw-character view c x y :font font :foreground foreground :background background :block t)
              (if (lem-base:wide-char-p c)
                  (incf x (* 2 font-width))
                  (incf x font-width)))
        (when underline
          (gp:draw-line view x0 y x y :foreground foreground))))))

(defun draw-string (view char-x char-y string attribute)
  (progn-in-pane-process (view)
    (setf attribute (lem:ensure-attribute attribute nil))
    (if attribute
        (let ((foreground (or (convert-color (lem:attribute-foreground attribute) nil)
                              (capi:simple-pane-foreground view)))
              (background (or (convert-color (lem:attribute-background attribute) nil)
                              (capi:simple-pane-background view)))
              (underline-p (lem:attribute-underline-p attribute))
              (bold-p (lem:attribute-bold-p attribute))
              (reverse-p (lem:attribute-reverse-p attribute)))
          (%draw-string view char-x char-y string
                        :foreground foreground :background background
                        :underline underline-p :bold bold-p :reverse reverse-p))
        (%draw-string view char-x char-y string
                      :foreground (capi:simple-pane-foreground view)
                      :background (capi:simple-pane-background view)))))

(defun draw-string-in-modeline (view char-x char-y string attribute)
  (draw-string view char-x (+ char-y (lem-view-char-height view) -1) string attribute))

(defun %clear-eol (view char-x char-y)
  (multiple-value-bind (font-width font-height)
      (font-char-size view)
    (let ((x (* char-x font-width))
          (y (* char-y font-height)))
      (gp:draw-rectangle view x y
                         (- (capi:simple-pane-visible-width view) x)
                         font-height
                         :foreground (capi:simple-pane-background view)
                         :filled t))))

(defun clear-eol (view char-x char-y)
  (progn-in-pane-process (view)
    (%clear-eol view char-x char-y)))

(defun clear-eob (view char-x char-y)
  (progn-in-pane-process (view)
    (%clear-eol view char-x char-y)
    (multiple-value-bind (font-width font-height)
        (font-char-size view)
      (let* ((x (* char-x font-width))
             (y (* char-y font-height)))
        (gp:draw-rectangle view x y
                           (- (capi:simple-pane-visible-width view) x)
                           (- (capi:simple-pane-visible-height view) y font-height)
                           :foreground (capi:simple-pane-background view)
                           :filled t)))))

(defun clear (view)
  (progn-in-pane-process (view)
    (gp:draw-rectangle view 0 0
                       (capi:simple-pane-visible-width view)
                       (capi:simple-pane-visible-height view)
                       :foreground (capi:simple-pane-background view)
                       :filled t)))
