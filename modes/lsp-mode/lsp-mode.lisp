(defpackage :lem-lsp-mode/lsp-mode
  (:use :cl :lem :alexandria)
  (:import-from :lem-lsp-mode/json)
  (:import-from :lem-lsp-mode/json-lsp-utils)
  (:import-from :lem-lsp-mode/utils)
  (:import-from :lem-lsp-mode/protocol)
  (:import-from :lem-lsp-mode/request)
  (:import-from :lem-lsp-mode/client))
(in-package :lem-lsp-mode/lsp-mode)

(cl-package-locks:lock-package :lem-lsp-mode/lsp-mode)
(lem-lsp-mode/project:local-nickname :protocol :lem-lsp-mode/protocol)
(lem-lsp-mode/project:local-nickname :utils :lem-lsp-mode/utils)
(lem-lsp-mode/project:local-nickname :request :lem-lsp-mode/request)
(lem-lsp-mode/project:local-nickname :json :lem-lsp-mode/json)
(lem-lsp-mode/project:local-nickname :client :lem-lsp-mode/client)
(lem-lsp-mode/project:local-nickname :completion :lem.completion-mode)

;;;
(defmacro with-editor-thread (() &body body)
  `(send-event
    (lambda ()
      ,@body
      (redraw-display))))

(defmacro lambda-with-editor-thread ((value) &body body)
  `(lambda (,value)
     (with-editor-thread ()
       ,@body)))

;;;
(defparameter *client-capabilities-text*
  (load-time-value
   (uiop:read-file-string
    (asdf:system-relative-pathname :lem-lsp-mode
                                   "client-capabilities.json"))))

(defun client-capabilities ()
  (lem-lsp-mode/json-lsp-utils:coerce-json
   (yason:parse *client-capabilities-text*)
   'protocol:client-capabilities))

;;;
(defvar *language-id-server-info-map* (make-hash-table :test 'equal))

(defstruct server-info
  port
  process)

(defun server-process-buffer-name (spec)
  (format nil "*Lsp <~A>*" (spec-langauge-id spec)))

(defun make-server-process-buffer (spec)
  (make-buffer (server-process-buffer-name spec)))

(defun run-server (spec)
  (flet ((output-callback (string)
           (let* ((buffer (make-server-process-buffer spec))
                  (point (buffer-point buffer)))
             (buffer-end point)
             (insert-string point string))))
    (let* ((port (lem-utils/socket:random-available-port))
           (process (lem-process:run-process (funcall (spec-command spec) port)
                                             :output-callback #'output-callback)))
      (make-server-info :process process :port port))))

(defun get-running-server-info (spec)
  (gethash (spec-langauge-id spec) *language-id-server-info-map*))

(defun ensure-running-server-process (spec)
  (unless (get-running-server-info spec)
    (setf (gethash (spec-langauge-id spec) *language-id-server-info-map*)
          (run-server spec))
    (values)))

(defun quit-all-server-process ()
  (maphash (lambda (language-id server-info)
             (declare (ignore language-id))
             (lem-process:delete-process (server-info-process server-info)))
           *language-id-server-info-map*))

;;;
(defmacro with-jsonrpc-error (() &body body)
  (with-unique-names (c)
    `(handler-case (progn ,@body)
       (jsonrpc/errors:jsonrpc-callback-error (,c)
         (editor-error "~A" ,c)))))

;;;
(defvar *workspaces* '())

(defstruct workspace
  root-uri
  client
  language-id
  server-capabilities
  server-info
  (trigger-characters (make-hash-table)))

(defun find-workspace (root-uri language-id)
  (dolist (workspace *workspaces*)
    (when (and (equal (workspace-root-uri workspace)
                      root-uri)
               (equal (workspace-language-id workspace)
                      language-id))
      (return workspace))))

(defun buffer-workspace (buffer)
  (buffer-value buffer 'workspace))

(defun (setf buffer-workspace) (workspace buffer)
  (setf (buffer-value buffer 'workspace) workspace))

(defun buffer-language-spec (buffer)
  (get-language-spec (buffer-major-mode buffer)))

(defun buffer-language-id (buffer)
  (let ((spec (buffer-language-spec buffer)))
    (when spec
      (spec-langauge-id spec))))

(defun buffer-version (buffer)
  (buffer-modified-tick buffer))

(defun buffer-uri (buffer)
  (utils:pathname-to-uri (buffer-filename buffer)))

(defun get-workspace-from-point (point)
  (buffer-workspace (point-buffer point)))

(define-minor-mode lsp-mode
    (:name "Language Client"
     :enable-hook 'enable-hook))

(defun enable-hook ()
  (add-hook *exit-editor-hook* 'quit-all-server-process)
  (ensure-lsp-buffer (current-buffer))
  (text-document/did-open (current-buffer))
  (enable-document-highlight-idle-timer))

(defun find-root-pathname (directory uri-patterns)
  (or (utils:find-root-pathname directory
                                (lambda (file)
                                  (let ((file-name (file-namestring file)))
                                    (dolist (uri-pattern uri-patterns)
                                      (when (search uri-pattern file-name)
                                        (return t))))))
      (pathname directory)))

(defgeneric make-client (client-params))

(defstruct client-params)
(defstruct (tcp-client-params (:include client-params)) port)

(defun get-connected-port (spec)
  (let ((server-info (get-running-server-info spec)))
    (assert server-info)
    (server-info-port server-info)))

(defun make-client-params-from-spec (spec)
  (ecase (spec-mode spec)
    (:tcp (make-tcp-client-params :port (get-connected-port spec)))))

(defmethod make-client ((client-params tcp-client-params))
  (make-instance 'client:tcp-client :port (tcp-client-params-port client-params)))

(defun make-client-and-connect (spec)
  (let ((client (make-client (make-client-params-from-spec spec))))
    (client:jsonrpc-connect client)
    client))

(defun convert-to-characters (string-characters)
  (map 'list
       (lambda (string) (char string 0))
       string-characters))

(defun get-completion-trigger-characters (workspace)
  (convert-to-characters
   (handler-case
       (protocol:completion-options-trigger-characters
        (protocol:server-capabilities-completion-provider
         (workspace-server-capabilities workspace)))
     (unbound-slot ()
       nil))))

(defun get-signature-help-trigger-characters (workspace)
  (convert-to-characters
   (handler-case
       (protocol:signature-help-options-trigger-characters
        (protocol:server-capabilities-signature-help-provider
         (workspace-server-capabilities workspace)))
     (unbound-slot ()
       nil))))

(defun self-insert-hook (c)
  (when-let* ((workspace (buffer-workspace (current-buffer)))
              (command (gethash c (workspace-trigger-characters workspace))))
    (funcall command c)))

(defun buffer-change-event-to-content-change-event (point arg)
  (labels ((inserting-content-change-event (string)
             (let ((position (point-to-lsp-position point)))
               (json:make-json :range (make-instance 'protocol:range
                                                     :start position
                                                     :end position)
                               :range-length 0
                               :text string)))
           (deleting-content-change-event (count)
             (with-point ((end point))
               (character-offset end count)
               (json:make-json :range (make-instance 'protocol:range
                                                     :start (point-to-lsp-position point)
                                                     :end (point-to-lsp-position end))
                               :range-length (count-characters point end)
                               :text ""))))
    (etypecase arg
      (character
       (inserting-content-change-event (string arg)))
      (string
       (inserting-content-change-event arg))
      (integer
       (deleting-content-change-event arg)))))

(defun handle-change-buffer (point arg)
  (let ((buffer (point-buffer point))
        (change-event (buffer-change-event-to-content-change-event point arg)))
    (text-document/did-change buffer (json:json-array change-event))))

(defun assign-workspace-to-buffer (buffer workspace)
  (setf (buffer-workspace buffer) workspace)
  (add-hook (variable-value 'kill-buffer-hook :buffer buffer) 'text-document/did-close)
  (add-hook (variable-value 'after-save-hook :buffer buffer) 'text-document/did-save)
  (add-hook (variable-value 'before-change-functions :buffer buffer) 'handle-change-buffer)
  (add-hook (variable-value 'self-insert-after-hook :buffer buffer) 'self-insert-hook)
  (setf (variable-value 'lem.language-mode:completion-spec)
        (completion:make-completion-spec #'text-document/completion
                                         :prefix-search t))
  (setf (variable-value 'lem.language-mode:find-definitions-function)
        #'find-definitions)
  (setf (variable-value 'lem.language-mode:find-references-function)
        #'find-references)
  (dolist (character (get-completion-trigger-characters workspace))
    (setf (gethash character (workspace-trigger-characters workspace))
          (lambda (c)
            (declare (ignore c))
            (lem.language-mode::complete-symbol))))
  (dolist (character (get-signature-help-trigger-characters workspace))
    (setf (gethash character (workspace-trigger-characters workspace))
          #'lsp-signature-help-with-trigger-character)))

(defun initialize-workspace (workspace)
  (push workspace *workspaces*)
  (jsonrpc:expose (client:client-connection (workspace-client workspace))
                  "textDocument/publishDiagnostics"
                  'text-document/publish-diagnostics)
  ;; initialize, initializedが失敗したときに、無効なworkspaceが残ってしまう問題があるかもしれない
  (initialize workspace)
  (initialized workspace)
  workspace)

(defun ensure-lsp-buffer (buffer)
  (let ((spec (buffer-language-spec buffer)))
    (ensure-running-server-process spec)
    (let* ((language-id (spec-langauge-id spec))
           (root-uri (utils:pathname-to-uri
                      (find-root-pathname (buffer-directory buffer)
                                          (spec-root-uri-patterns spec))))
           (workspace (or (find-workspace root-uri language-id)
                          (initialize-workspace
                           (make-workspace :client (make-client-and-connect spec)
                                           :root-uri root-uri
                                           :language-id language-id)))))
      (assign-workspace-to-buffer buffer workspace))))

(defun point-to-lsp-position (point)
  (make-instance 'protocol:position
                 :line (1- (line-number-at-point point))
                 :character (point-charpos point)))

(defun move-to-lsp-position (point position)
  (buffer-start point)
  (line-offset point
               (protocol:position-line position)
               (protocol:position-character position)))

(defun make-lsp-range (start end)
  (make-instance 'protocol:range
                 :start (point-to-lsp-position start)
                 :end (point-to-lsp-position end)))

(defun buffer-to-text-document-item (buffer)
  (make-instance 'protocol:text-document-item
                 :uri (buffer-uri buffer)
                 :language-id (buffer-language-id buffer)
                 :version (buffer-version buffer)
                 :text (buffer-text buffer)))

(defun make-text-document-identifier (buffer)
  (make-instance
   'protocol:text-document-identifier
   :uri (buffer-uri buffer)))

(defun make-text-document-position-arguments (point)
  (list :text-document (make-text-document-identifier (point-buffer point))
        :position (point-to-lsp-position point)))

(defun find-buffer-from-uri (uri)
  (let ((pathname (utils:uri-to-pathname uri)))
    (dolist (buffer (buffer-list))
      (when (uiop:pathname-equal pathname (buffer-filename buffer))
        (return buffer)))))

(defun get-buffer-from-text-document-identifier (text-document-identifier)
  (let ((uri (protocol:text-document-identifier-uri text-document-identifier)))
    (find-buffer-from-uri uri)))

(defun apply-text-edits (buffer text-edits)
  (with-point ((start (buffer-point buffer) :left-inserting)
               (end (buffer-point buffer) :left-inserting))
    (utils:do-sequence (text-edit text-edits)
      (let ((range (protocol:text-edit-range text-edit))
            (new-text (protocol:text-edit-new-text text-edit)))
        (move-to-lsp-position start (protocol:range-start range))
        (move-to-lsp-position end (protocol:range-end range))
        (delete-between-points start end)
        (insert-string start new-text)))))

(defgeneric apply-document-change (document-change))

(defmethod apply-document-change ((document-change protocol:text-document-edit))
  (let* ((buffer
           (get-buffer-from-text-document-identifier
            (protocol:text-document-edit-text-document document-change))))
    (apply-text-edits buffer (protocol:text-document-edit-edits document-change))))

(defmethod apply-document-change ((document-change protocol:create-file))
  (error "createFile is not yet supported"))

(defmethod apply-document-change ((document-change protocol:rename-file))
  (error "renameFile is not yet supported"))

(defmethod apply-document-change ((document-change protocol:delete-file))
  (error "deleteFile is not yet supported"))

(defun apply-workspace-edit (workspace)
  (labels ((apply-document-changes (document-changes)
             (utils:do-sequence (document-change document-changes)
               (apply-document-change document-change)))
           (apply-changes (changes)
             (declare (ignore changes))
             (error "Not yet implemented")))
    (if-let ((document-changes (handler-case (protocol:workspace-edit-document-changes workspace)
                                 (unbound-slot () nil))))
      (apply-document-changes document-changes)
      (when-let ((changes (handler-case (protocol:workspace-edit-changes workspace)
                            (unbound-slot () nil))))
        (apply-changes changes)))))

;;; General Messages

(defun initialize (workspace)
  (let ((initialize-result
          (request:request
           (workspace-client workspace)
           (make-instance
            'request:initialize-request
            :params (make-instance
                     'protocol:initialize-params
                     :process-id (utils:get-pid)
                     :client-info (json:make-json :name "lem" #|:version "0.0.0"|#)
                     :root-uri (workspace-root-uri workspace)
                     :capabilities (client-capabilities)
                     :trace "off"
                     :workspace-folders (json:json-null))))))
    (setf (workspace-server-capabilities workspace)
          (protocol:initialize-result-capabilities initialize-result))
    (setf (workspace-server-info workspace)
          (protocol:initialize-result-server-info initialize-result)))
  (values))

(defun initialized (workspace)
  (request:request (workspace-client workspace)
                   (make-instance 'request:initialized-request)))

;;; Text Synchronization

(defun text-document/did-open (buffer)
  (request:request
   (workspace-client (buffer-workspace buffer))
   (make-instance 'request:text-document-did-open
                  :params (make-instance 'protocol:did-open-text-document-params
                                         :text-document (buffer-to-text-document-item buffer)))))

(defun text-document/did-change (buffer content-changes)
  (request:request
   (workspace-client (buffer-workspace buffer))
   (make-instance 'request:text-document-did-change
                  :params (make-instance 'protocol:did-change-text-document-params
                                         :text-document (make-instance 'protocol:versioned-text-document-identifier
                                                                       :version (buffer-version buffer)
                                                                       :uri (buffer-uri buffer))
                                         :content-changes content-changes))))

(defun text-document/did-save (buffer)
  (request:request
   (workspace-client (buffer-workspace buffer))
   (make-instance 'request:text-document-did-save
                  :params (make-instance 'protocol:did-save-text-document-params
                                         :text-document (make-text-document-identifier buffer)
                                         :text (buffer-text buffer)))))

(defun text-document/did-close (buffer)
  (request:request
   (workspace-client (buffer-workspace buffer))
   (make-instance 'request:text-document-did-close
                  :params (make-instance 'protocol:did-close-text-document-params
                                         :text-document (make-text-document-identifier buffer)))))

;;; publishDiagnostics

;; TODO
;; - tagSupport
;; - versionSupport

(define-attribute diagnostic-error-attribute
  (t :foreground "red" :underline-p t))

(define-attribute diagnostic-warning-attribute
  (t :foreground "orange" :underline-p t))

(define-attribute diagnostic-information-attribute
  (t :foreground "gray" :underline-p t))

(define-attribute diagnostic-hint-attribute
  (t :foreground "yellow" :underline-p t))

(defun diagnostic-severity-attribute (diagnostic-severity)
  (switch (diagnostic-severity :test #'=)
    (protocol:diagnostic-severity.error
     'diagnostic-error-attribute)
    (protocol:diagnostic-severity.warning
     'diagnostic-warning-attribute)
    (protocol:diagnostic-severity.information
     'diagnostic-information-attribute)
    (protocol:diagnostic-severity.hint
     'diagnostic-hint-attribute)))

(defstruct diagnostic
  buffer
  position
  message)

(defun buffer-diagnostic-overlays (buffer)
  (buffer-value buffer 'diagnostic-overlays))

(defun (setf buffer-diagnostic-overlays) (overlays buffer)
  (setf (buffer-value buffer 'diagnostic-overlays) overlays))

(defun clear-diagnostic-overlays (buffer)
  (mapc #'delete-overlay (buffer-diagnostic-overlays buffer))
  (setf (buffer-diagnostic-overlays buffer) '()))

(defun buffer-diagnostics (buffer)
  (mapcar (lambda (overlay)
            (overlay-get overlay 'diagnostic))
          (buffer-diagnostic-overlays buffer)))

(defun point-to-xref-position (point)
  (lem.language-mode::make-xref-position :line-number (line-number-at-point point)
                                         :charpos (point-charpos point)))

(defun highlight-diagnostic (buffer diagnostic)
  (with-point ((start (buffer-point buffer))
               (end (buffer-point buffer)))
    (let ((range (protocol:diagnostic-range diagnostic)))
      (move-to-lsp-position start (protocol:range-start range))
      (move-to-lsp-position end (protocol:range-end range))
      (let ((overlay (make-overlay start end
                                   (handler-case (protocol:diagnostic-severity diagnostic)
                                     (unbound-slot ()
                                       'diagnostic-error-attribute)
                                     (:no-error (severity)
                                       (diagnostic-severity-attribute severity))))))
        (overlay-put overlay
                     'diagnostic
                     (make-diagnostic :buffer buffer
                                      :position (point-to-xref-position start)
                                      :message (protocol:diagnostic-message diagnostic)))
        (push overlay (buffer-diagnostic-overlays buffer))))))

(defun highlight-diagnostics (params)
  (when-let ((buffer (find-buffer-from-uri (protocol:publish-diagnostics-params-uri params))))
    (clear-diagnostic-overlays buffer)
    (utils:do-sequence (diagnostic (protocol:publish-diagnostics-params-diagnostics params))
      (highlight-diagnostic buffer diagnostic))))

(defun text-document/publish-diagnostics (params)
  (request::do-request-log "textDocument/publishDiagnostics" params :from :server)
  (let ((params (lem-lsp-mode/json-lsp-utils:coerce-json params 'protocol:publish-diagnostics-params)))
    (send-event (lambda () (highlight-diagnostics params)))))

(define-command lsp-document-diagnostics () ()
  (when-let ((diagnostics (buffer-diagnostics (current-buffer))))
    (lem.sourcelist:with-sourcelist (sourcelist "*Diagnostics*")
      (dolist (diagnostic diagnostics)
        (lem.sourcelist:append-sourcelist
         sourcelist
         (lambda (point)
           (insert-string point (buffer-filename (diagnostic-buffer diagnostic))
                          :attribute 'lem.sourcelist:title-attribute)
           (insert-string point ":")
           (insert-string point
                          (princ-to-string (lem.language-mode::xref-position-line-number
                                            (diagnostic-position diagnostic)))
                          :attribute 'lem.sourcelist:position-attribute)
           (insert-string point ":")
           (insert-string point
                          (princ-to-string (lem.language-mode::xref-position-charpos
                                            (diagnostic-position diagnostic)))
                          :attribute 'lem.sourcelist:position-attribute)
           (insert-string point ":")
           (insert-string point (diagnostic-message diagnostic)))
         (let ((diagnostic diagnostic))
           (lambda (set-buffer-fn)
             (funcall set-buffer-fn (diagnostic-buffer diagnostic))
             (lem.language-mode:move-to-xref-location-position
              (buffer-point (diagnostic-buffer diagnostic))
              (diagnostic-position diagnostic)))))))))

;;; hover

;; TODO
;; - workDoneProgress
;; - partialResult
;; - hoverClientCapabilitiesのcontentFormatを設定する
;; - hoverのrangeを使って範囲に背景色をつける
;; - markdownの中のコード表示時に対象の言語のシンタックスハイライトをする
;; - serverでサポートしているかのチェックをする

(defun hover-to-string (hover)
  (flet ((marked-string-to-string (marked-string)
           (if (stringp marked-string)
               marked-string
               (or (json:json-get marked-string "value")
                   ""))))
    (let ((contents (protocol:hover-contents hover)))
      (cond
        ;; MarkedString
        ((json:json-object-p contents)
         (marked-string-to-string contents))
        ;; MarkedString[]
        ((json:json-array-p contents)
         (with-output-to-string (out)
           (dolist (content contents)
             (write-string (marked-string-to-string content)
                           out))))
        ;; MarkupContent
        ((typep contents 'protocol:markup-content)
         (protocol:markup-content-value contents))
        (t
         "")))))

(defun provide-hover-p (workspace)
  (handler-case (protocol:server-capabilities-hover-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun text-document/hover (point)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-hover-p workspace)
      (let ((result
              (request:request
               (workspace-client workspace)
               (make-instance 'request:hover-request
                              :params (apply #'make-instance
                                             'protocol:hover-params
                                             (make-text-document-position-arguments point))))))
        (when result
          (hover-to-string result))))))

(define-command lsp-hover () ()
  (message "~A" (text-document/hover (current-point))))

;;; completion

;; TODO
;; - serverでサポートしているかのチェックをする
;; - workDoneProgress
;; - partialResult
;; - completionParams.context, どのように補完が起動されたかの情報を含める
;; - completionItemの使っていない要素が多分にある
;; - completionResolve

(defclass completion-item (completion:completion-item)
  ((sort-text
    :initarg :sort-text
    :reader completion-item-sort-text)))

(defun convert-completion-items (items)
  (sort (map 'list
             (lambda (item)
               (make-instance 'completion-item
                              :label (protocol:completion-item-label item)
                              :detail (handler-case (protocol:completion-item-detail item)
                                        (unbound-slot () ""))
                              :sort-text (handler-case (protocol:completion-item-sort-text item)
                                           (unbound-slot ()
                                             (protocol:completion-item-label item)))))
             items)
        #'string<
        :key #'completion-item-sort-text))

(defun convert-completion-list (completion-list)
  (convert-completion-items (protocol:completion-list-items completion-list)))

(defun convert-completion-response (value)
  (cond ((typep value 'protocol:completion-list)
         (convert-completion-list value))
        ((json:json-array-p value)
         (convert-completion-items value))
        (t
         nil)))

(defun provide-completion-p (workspace)
  (handler-case (protocol:server-capabilities-completion-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun text-document/completion (point)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-completion-p workspace)
      (convert-completion-response
       (request:request
        (workspace-client workspace)
        (make-instance 'request:completion-request
                       :params (apply #'make-instance
                                      'protocol:completion-params
                                      (make-text-document-position-arguments point))))))))

;;; signatureHelp

(define-attribute signature-help-active-parameter-attribute
  (t :underline-p t))

(defun provide-signature-help-p (workspace)
  (handler-case (protocol:server-capabilities-signature-help-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun display-signature-help (signature-help)
  (let* ((buffer (make-buffer nil :temporary t))
         (point (buffer-point buffer)))
    (setf (lem:variable-value 'lem::truncate-character :buffer buffer) #\space)
    (let ((active-parameter
            (handler-case (protocol:signature-help-active-parameter signature-help)
              (unbound-slot () nil)))
          (active-signature
            (handler-case (protocol:signature-help-active-signature signature-help)
              (unbound-slot () nil))))
      (utils:do-sequence ((signature index) (protocol:signature-help-signatures signature-help))
        (when (plusp index) (insert-character point #\newline))
        (let ((active-signature-p (eql index active-signature)))
          (if active-signature-p
              (insert-string point "* ")
              (insert-string point "- "))
          (insert-string point
                         (protocol:signature-information-label signature))
          (when active-signature-p
            (let ((parameters
                    (handler-case
                        (protocol:signature-information-parameters signature)
                      (unbound-slot () nil))))
              (when (< active-parameter (length parameters))
                ;; TODO: labelが[number, number]の場合に対応する
                (let ((label (protocol:parameter-information-label (elt parameters active-parameter))))
                  (when (stringp label)
                    (with-point ((p point))
                      (line-start p)
                      (when (search-forward p label)
                        (with-point ((start p))
                          (character-offset start (- (length label)))
                          (put-text-property start p :attribute 'signature-help-active-parameter-attribute)))))))))
          (insert-character point #\space)
          (insert-character point #\newline)
          (handler-case (protocol:signature-information-documentation signature)
            (unbound-slot () nil)
            (:no-error (documentation)
              (insert-string point documentation)))))
      (buffer-start (buffer-point buffer))
      (message-buffer buffer))))

(defun text-document/signature-help (point &optional signature-help-context)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-signature-help-p workspace)
      (let ((result (request:request
                     (workspace-client workspace)
                     (make-instance 'request:signature-help
                                    :params (apply #'make-instance
                                                   'protocol:signature-help-params
                                                   (append (when signature-help-context
                                                             `(:context ,signature-help-context))
                                                           (make-text-document-position-arguments point)))))))
        (when result
          (display-signature-help result))))))

(defun lsp-signature-help-with-trigger-character (character)
  (text-document/signature-help (current-point)
                                (make-instance 'protocol:signature-help-context
                                               :trigger-kind protocol:signature-help-trigger-kind.trigger-character
                                               :trigger-character (string character)
                                               :is-retrigger (json:json-false)
                                               #|:active-signature-help|#)))

(define-command lsp-signature-help () ()
  (text-document/signature-help (current-point)
                                (make-instance 'protocol:signature-help-context
                                               :trigger-kind protocol:signature-help-trigger-kind.invoked
                                               :is-retrigger (json:json-false))))

;;; declaration

(defun provide-declaration-p (workspace)
  (handler-case (protocol:server-capabilities-declaration-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun text-document/declaration (point)
  (declare (ignore point))
  ;; TODO: goplsが対応していなかったので後回し
  nil)

;;; definition

(defun provide-definition-p (workspace)
  (handler-case (protocol:server-capabilities-definition-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defgeneric convert-location (location)
  (:method ((location protocol:location))
    ;; TODO: end-positionも使い、定義位置への移動後のハイライトをstart/endの範囲にする
    (let* ((start-position (protocol:range-start (protocol:location-range location)))
           (end-position (protocol:range-end (protocol:location-range location)))
           (uri (protocol:location-uri location))
           (file (utils:uri-to-pathname uri)))
      (declare (ignore end-position))
      (lem.language-mode:make-xref-location
       :filespec file
       :position (lem.language-mode::make-position
                  (1+ (protocol:position-line start-position))
                  (protocol:position-character start-position)))))
  (:method ((location protocol:location-link))
    (error "locationLink is unsupported")))

(defun convert-definition-response (value)
  (cond ((typep value 'protocol:location)
         (list (convert-location value)))
        ((json:json-array-p value)
         ;; TODO: location-link
         (map 'list #'convert-location value))
        (t
         nil)))

(defun text-document/definition (point)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-definition-p workspace)
      (convert-definition-response
       (request:request
        (workspace-client workspace)
        (make-instance 'request:definition
                       :params (apply #'make-instance
                                      'protocol:definition-params
                                      (make-text-document-position-arguments point))))))))

(defun find-definitions (point)
  (with-jsonrpc-error ()
    (text-document/definition point)))

;;; type definition

(defun provide-type-definition-p (workspace)
  (handler-case (protocol:server-capabilities-type-definition-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun convert-type-definition-response (value)
  (convert-definition-response value))

(defun text-document/type-definition (point)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-type-definition-p workspace)
      (convert-type-definition-response
       (request:request
        (workspace-client workspace)
        (make-instance 'request:type-definition
                       :params (apply #'make-instance
                                      'protocol:type-definition-params
                                      (make-text-document-position-arguments point))))))))

(define-command lsp-type-definition () ()
  (let ((xref-locations
          (with-jsonrpc-error ()
            (text-document/type-definition (current-point)))))
    (lem.language-mode::show-locations xref-locations)))

;;; implementation

(defun provide-implementation-p (workspace)
  (handler-case (protocol:server-capabilities-implementation-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun convert-implementation-response (value)
  (convert-definition-response value))

(defun text-document/implementation (point)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-implementation-p workspace)
      (convert-implementation-response
       (request:request
        (workspace-client workspace)
        (make-instance 'request:implementation
                       :params (apply #'make-instance
                                      'protocol:type-definition-params
                                      (make-text-document-position-arguments point))))))))

(define-command lsp-implementation () ()
  (let ((xref-locations
          (with-jsonrpc-error ()
            (text-document/implementation (current-point)))))
    (lem.language-mode::show-locations xref-locations)))

;;; references

(defun provide-references-p (workspace)
  (handler-case (protocol:server-capabilities-references-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun xref-location-to-content (location)
  (let* ((buffer (find-file-buffer (lem.language-mode:xref-location-filespec location) :temporary t))
         (point (buffer-point buffer)))
    (lem.language-mode::move-to-location-position point (lem.language-mode:xref-location-position location))
    (string-trim '(#\space #\tab) (line-string point))))

(defun convert-references-response (value)
  (lem.language-mode:make-xref-references
   :type nil
   :locations (mapcar (lambda (location)
                        (lem.language-mode:make-xref-location
                         :filespec (lem.language-mode:xref-location-filespec location)
                         :position (lem.language-mode:xref-location-position location)
                         :content (xref-location-to-content location)))
                      (convert-definition-response value))))

(defun text-document/references (point &optional include-declaration)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-references-p workspace)
      (convert-references-response
       (request:request
        (workspace-client workspace)
        (make-instance 'request:references
                       :params (apply #'make-instance
                                      'protocol:reference-params
                                      :context (make-instance 'protocol:reference-context
                                                              :include-declaration (json:to-json-boolean
                                                                                    include-declaration))
                                      (make-text-document-position-arguments point))))))))

(defun find-references (point)
  (text-document/references point))

;;; document highlights

(define-attribute document-highlight-text-attribute
  (t :background "yellow4"))

(defun provide-document-highlight-p (workspace)
  (handler-case (protocol:server-capabilities-document-highlight-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defvar *document-highlight-overlays* '())

(defun clear-document-highlight-overlays ()
  (mapc #'delete-overlay *document-highlight-overlays*))

(defun display-document-highlights (buffer document-highlights)
  (clear-document-highlight-overlays)
  (with-point ((start (buffer-point buffer))
               (end (buffer-point buffer)))
    (utils:do-sequence (document-highlight document-highlights)
      (let* ((range (protocol:document-highlight-range document-highlight)))
        (move-to-lsp-position start (protocol:range-start range))
        (move-to-lsp-position end (protocol:range-end range))
        (push (make-overlay start end 'document-highlight-text-attribute)
              *document-highlight-overlays*)))))

(defun text-document/document-highlight (point)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-document-highlight-p workspace)
      (request:request-async
       (workspace-client workspace)
       (make-instance 'request:document-highlight
                      :params (apply #'make-instance
                                     'protocol:document-highlight-params
                                     (make-text-document-position-arguments point)))
       (lambda-with-editor-thread (value)
         (display-document-highlights (point-buffer point)
                                      value))))))

(define-command lsp-document-highlight () ()
  (when (mode-active-p (current-buffer) 'lsp-mode)
    (text-document/document-highlight (current-point))))

(defvar *document-highlight-idle-timer* nil)

(defun enable-document-highlight-idle-timer ()
  (unless *document-highlight-idle-timer*
    (setf *document-highlight-idle-timer*
          (start-idle-timer 500 t #'lsp-document-highlight))
    (add-hook *post-command-hook* 'clear-document-highlight-overlays)))

;;; document symbols

;; TODO
;; - position順でソートする

(define-attribute symbol-kind-file-attribute
  (t :foreground "snow1"))

(define-attribute symbol-kind-module-attribute
  (t :foreground "firebrick"))

(define-attribute symbol-kind-namespace-attribute
  (t :foreground "dark orchid"))

(define-attribute symbol-kind-package-attribute
  (t :foreground "green"))

(define-attribute symbol-kind-class-attribute
  (t :foreground "bisque2"))

(define-attribute symbol-kind-method-attribute
  (t :foreground "MediumPurple2"))

(define-attribute symbol-kind-property-attribute
  (t :foreground "MistyRose4"))

(define-attribute symbol-kind-field-attribute
  (t :foreground "azure3"))

(define-attribute symbol-kind-constructor-attribute
  (t :foreground "LightSkyBlue3"))

(define-attribute symbol-kind-enum-attribute
  (t :foreground "LightCyan4"))

(define-attribute symbol-kind-interface-attribute
  (t :foreground "gray78"))

(define-attribute symbol-kind-function-attribute
  (t :foreground "LightSkyBlue"))

(define-attribute symbol-kind-variable-attribute
  (t :foreground "LightGoldenrod"))

(define-attribute symbol-kind-constant-attribute
  (t :foreground "yellow2"))

(define-attribute symbol-kind-string-attribute
  (t :foreground "green"))

(define-attribute symbol-kind-number-attribute
  (t :foreground "yellow"))

(define-attribute symbol-kind-boolean-attribute
  (t :foreground "honeydew3"))

(define-attribute symbol-kind-array-attribute
  (t :foreground "red"))

(define-attribute symbol-kind-object-attribute
  (t :foreground "PeachPuff4"))

(define-attribute symbol-kind-key-attribute
  (t :foreground "lime green"))

(define-attribute symbol-kind-null-attribute
  (t :foreground "gray"))

(define-attribute symbol-kind-enum-membe-attribute
  (t :foreground "PaleTurquoise4"))

(define-attribute symbol-kind-struct-attribute
  (t :foreground "turquoise4"))

(define-attribute symbol-kind-event-attribute
  (t :foreground "aquamarine1"))

(define-attribute symbol-kind-operator-attribute
  (t :foreground "SeaGreen3"))

(define-attribute symbol-kind-type-attribute
  (t :foreground "moccasin"))

(defun preview-symbol-kind-colors ()
  (let* ((buffer (make-buffer "symbol-kind-colors"))
         (point (buffer-point buffer)))
    (dolist (attribute
             (list 'symbol-kind-file-attribute
                   'symbol-kind-module-attribute
                   'symbol-kind-namespace-attribute
                   'symbol-kind-package-attribute
                   'symbol-kind-class-attribute
                   'symbol-kind-method-attribute
                   'symbol-kind-property-attribute
                   'symbol-kind-field-attribute
                   'symbol-kind-constructor-attribute
                   'symbol-kind-enum-attribute
                   'symbol-kind-interface-attribute
                   'symbol-kind-function-attribute
                   'symbol-kind-variable-attribute
                   'symbol-kind-constant-attribute
                   'symbol-kind-string-attribute
                   'symbol-kind-number-attribute
                   'symbol-kind-boolean-attribute
                   'symbol-kind-array-attribute
                   'symbol-kind-object-attribute
                   'symbol-kind-key-attribute
                   'symbol-kind-null-attribute
                   'symbol-kind-enum-membe-attribute
                   'symbol-kind-struct-attribute
                   'symbol-kind-event-attribute
                   'symbol-kind-operator-attribute
                   'symbol-kind-type-attribute))
      (insert-string point (string-downcase attribute) :attribute attribute)
      (insert-character point #\newline))))

(defun provide-document-symbol-p (workspace)
  (handler-case (protocol:server-capabilities-document-symbol-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun symbol-kind-to-string-and-attribute (symbol-kind)
  (switch (symbol-kind :test #'=)
    (protocol:symbol-kind.file
     (values "File" 'symbol-kind-file-attribute))
    (protocol:symbol-kind.module
     (values "Module" 'symbol-kind-module-attribute))
    (protocol:symbol-kind.namespace
     (values "Namespace" 'symbol-kind-namespace-attribute))
    (protocol:symbol-kind.package
     (values "Package" 'symbol-kind-package-attribute))
    (protocol:symbol-kind.class
     (values "Class" 'symbol-kind-class-attribute))
    (protocol:symbol-kind.method
     (values "Method" 'symbol-kind-method-attribute))
    (protocol:symbol-kind.property
     (values "Property" 'symbol-kind-property-attribute))
    (protocol:symbol-kind.field
     (values "Field" 'symbol-kind-field-attribute))
    (protocol:symbol-kind.constructor
     (values "Constructor" 'symbol-kind-constructor-attribute))
    (protocol:symbol-kind.enum
     (values "Enum" 'symbol-kind-enum-attribute))
    (protocol:symbol-kind.interface
     (values "Interface" 'symbol-kind-interface-attribute))
    (protocol:symbol-kind.function
     (values "Function" 'symbol-kind-function-attribute))
    (protocol:symbol-kind.variable
     (values "Variable" 'symbol-kind-variable-attribute))
    (protocol:symbol-kind.constant
     (values "Constant" 'symbol-kind-constant-attribute))
    (protocol:symbol-kind.string
     (values "String" 'symbol-kind-string-attribute))
    (protocol:symbol-kind.number
     (values "Number" 'symbol-kind-number-attribute))
    (protocol:symbol-kind.boolean
     (values "Boolean" 'symbol-kind-boolean-attribute))
    (protocol:symbol-kind.array
     (values "Array" 'symbol-kind-array-attribute))
    (protocol:symbol-kind.object
     (values "Object" 'symbol-kind-object-attribute))
    (protocol:symbol-kind.key
     (values "Key" 'symbol-kind-key-attribute))
    (protocol:symbol-kind.null
     (values "Null" 'symbol-kind-null-attribute))
    (protocol:symbol-kind.enum-member
     (values "EnumMember" 'symbol-kind-enum-member-attribute))
    (protocol:symbol-kind.struct
     (values "Struct" 'symbol-kind-struct-attribute))
    (protocol:symbol-kind.event
     (values "Event" 'symbol-kind-event-attribute))
    (protocol:symbol-kind.operator
     (values "Operator" 'symbol-kind-operator-attribute))
    (protocol:symbol-kind.type-parameter
     (values "TypeParameter" 'symbol-kind-type-attribute))))

(define-attribute document-symbol-detail-attribute
  (t :foreground "gray"))

(defun append-document-symbol-item (sourcelist buffer document-symbol nest-level)
  (let ((selection-range (protocol:document-symbol-selection-range document-symbol))
        (range (protocol:document-symbol-range document-symbol)))
    (lem.sourcelist:append-sourcelist
     sourcelist
     (lambda (point)
       (multiple-value-bind (kind-name attribute)
           (symbol-kind-to-string-and-attribute (protocol:document-symbol-kind document-symbol))
         (insert-string point (make-string (* 2 nest-level) :initial-element #\space))
         (insert-string point (format nil "[~A]" kind-name) :attribute attribute)
         (insert-character point #\space)
         (insert-string point (protocol:document-symbol-name document-symbol))
         (insert-string point " ")
         (when-let (detail (handler-case (protocol:document-symbol-detail document-symbol)
                             (unbound-slot () nil)))
           (insert-string point detail :attribute 'document-symbol-detail-attribute))))
     (lambda (set-buffer-fn)
       (funcall set-buffer-fn buffer)
       (let ((point (buffer-point buffer)))
         (move-to-lsp-position point (protocol:range-start selection-range))))
     :highlight-overlay-function (lambda (point)
                                   (with-point ((start point)
                                                (end point))
                                     (make-overlay
                                      (move-to-lsp-position start (protocol:range-start range))
                                      (move-to-lsp-position end (protocol:range-end range))
                                      'lem.sourcelist::jump-highlight)))))
  (utils:do-sequence
      (document-symbol
       (handler-case (protocol:document-symbol-children document-symbol)
         (unbound-slot () nil)))
    (append-document-symbol-item sourcelist buffer document-symbol (1+ nest-level))))

(defun display-document-symbol-response (buffer value)
  (lem.sourcelist:with-sourcelist (sourcelist "*Document Symbol*")
    (utils:do-sequence (item value)
      (append-document-symbol-item sourcelist buffer item 0))))

(defun text-document/document-symbol (buffer)
  (when-let ((workspace (buffer-workspace buffer)))
    (when (provide-document-symbol-p workspace)
      (request:request
       (workspace-client workspace)
       (make-instance 'request:document-symbol
                      :params (make-instance
                               'protocol:document-symbol-params
                               :text-document (make-text-document-identifier buffer)))))))

(define-command lsp-document-symbol () ()
  (display-document-symbol-response
   (current-buffer)
   (text-document/document-symbol (current-buffer))))

;;; formatting

(defun provide-formatting-p (workspace)
  (handler-case (protocol:server-capabilities-document-formatting-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun make-formatting-options (buffer)
  (make-instance
   'protocol:formatting-options
   :tab-size (variable-value 'tab-width :buffer buffer)
   :insert-spaces (not (variable-value 'indent-tabs-mode :buffer buffer))
   :trim-trailing-whitespace t
   :insert-final-newline t
   :trim-final-newlines t))

(defun text-document/formatting (buffer)
  (when-let ((workspace (buffer-workspace buffer)))
    (when (provide-formatting-p workspace)
      (apply-text-edits
       buffer
       (request:request
        (workspace-client workspace)
        (make-instance 'request:document-formatting
                       :params (make-instance
                                'protocol:document-formatting-params
                                :text-document (make-text-document-identifier buffer)
                                :options (make-formatting-options buffer))))))))

(define-command lsp-document-format () ()
  (text-document/formatting (current-buffer)))

;;; range formatting

;; WARNING: goplsでサポートされていないので動作未確認

(defun provide-range-formatting-p (workspace)
  (handler-case (protocol:server-capabilities-document-range-formatting-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun text-document/range-formatting (start end)
  (when (point< end start) (rotatef start end))
  (let ((buffer (point-buffer start)))
    (when-let ((workspace (buffer-workspace buffer)))
      (when (provide-range-formatting-p workspace)
        (apply-text-edits
         buffer
         (request:request
          (workspace-client workspace)
          (make-instance 'request:document-range-formatting
                         :params (make-instance
                                  'protocol:document-range-formatting-params
                                  :text-document (make-text-document-identifier buffer)
                                  :range (make-lsp-range start end)
                                  :options (make-formatting-options buffer)))))))))

(define-command lsp-document-range-format (start end) ("r")
  (text-document/range-formatting start end))

;;; rename

;; TODO
;; - prepareSupport

(defun provide-rename-p (workspace)
  (handler-case (protocol:server-capabilities-rename-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun text-document/rename (point new-name)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-rename-p workspace)
      (let ((response
              (with-jsonrpc-error ()
                (request:request
                 (workspace-client workspace)
                 (make-instance 'request:rename
                                :params (apply #'make-instance
                                               'protocol:rename-params
                                               :new-name new-name
                                               (make-text-document-position-arguments point)))))))
        (when (typep response 'protocol:workspace-edit)
          (apply-workspace-edit response))))))

(define-command lsp-rename (new-name) ("sNew name: ")
  (text-document/rename (current-point) new-name))

;;;
(defvar *language-spec-table* (make-hash-table))

(defun get-language-spec (major-mode)
  (gethash major-mode *language-spec-table*))

(defun spec-langauge-id (spec)
  (getf spec :language-id))

(defun spec-root-uri-patterns (spec)
  (getf spec :root-uri-patterns))

(defun spec-mode (spec)
  (getf spec :mode))

(defun spec-port (spec)
  (getf spec :port))

(defun spec-command (spec)
  (getf spec :command))

(defmacro def-language-spec (major-mode &rest plist)
  `(progn
     (setf (gethash ',major-mode *language-spec-table*)
           (list ,@plist))
     ,(when (mode-hook major-mode)
        `(add-hook ,(mode-hook major-mode) 'lsp-mode))))

(def-language-spec lem-go-mode:go-mode
  :language-id "go"
  :root-uri-patterns '("go.mod")
  :command (lambda (port) `("gopls" "serve" "-port" ,(princ-to-string port)))
  :mode :tcp
  :port 12345)


#|
Language Features
- [X] completion
- [ ] completion resolve
- [X] hover
- [X] signatureHelp
- [ ] declaration
- [X] definition
- [X] typeDefinition
- [X] implementation
- [X] references
- [X] documentHighlight
- [X] documentSymbol
- [ ] codeAction
- [ ] codeLens
- [ ] codeLens resolve
- [ ] documentLink
- [ ] documentLink resolve
- [ ] documentColor
- [ ] colorPresentation
- [X] formatting
- [X] rangeFormatting
- [ ] onTypeFormatting
- [X] rename
- [ ] prepareRename
- [ ] foldingRange
- [ ] selectionRange

TODO
- partialResult
- workDoneProgress
|#