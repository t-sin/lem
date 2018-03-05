(defpackage :lem-interface
  (:nicknames :lem-if)
  (:export :invoke
           :display-background-mode
           :update-foreground
           :update-background
           :display-width
           :display-height
           :make-view
           :delete-view
           :clear
           :set-view-size
           :set-view-pos
           :print
           :print-modeline
           :clear-eol
           :clear-eob
           :move-cursor
           :redraw-window
           :redraw-view-after
           :update-display
           :scroll))

(defpackage :lem
  (:use :cl :lem-base)
  (:export . #.(loop :for sym :being :the :external-symbols :of (find-package :lem-base)
                     :collect (make-symbol (string sym)))))

(defpackage :lem-user
  (:use :cl :lem))
