;;;; package.lisp

(defpackage webdriver
  (:use
   :cl
   :series
   :lol
   ;;:trivia
   )
  (:shadowing-import-from
   :series
   :split
   :choose
   )

  (:import-from
   :alexandria

   :with-gensyms
   :assoc-value
   :if-let
   :when-let
   :when-let*
   )

  (:export
   ;;; ------------------------------ base-utility
   :protocol-error

   :wd-obj
   :wd-ref

   :wd-make-cookie
   :wd-make-rect
   :wd-make-capabilities-for-chrome
   :wd-make-capabilities-for-firefox

   :wd-element-to-id

   :wd-key
   :wd-keys
   :wd-mouse-button
   :wd-find-type

   ;;:wd-compile-actions

   ;;; ------------------------------ configuration

   :*wd-base*
   :*wd-default-find-element-method*

   ;;; ------------------------------ primitive-api. http (get post delete)
   :wd-get
   :wd-post
   :wd-delete

   ;;; ------------------------------ 8. session
   :*wd-session*
   :*wd-session-w3c-p*

   :wd-make-session
   :wd-delete-session
   :wd-init-session

   :with-wd

   :wd-status

   :wd-timeouts
   :wd-set-timeouts

   ;; ----
   :wd-session-status%
   :wd-all-sessions%
   :wd-delete-all-sessions%

   ;;; ------------------------------ wrapper-for-primitive-api.

   ;; ---------- 9. navigation
   :wd-url ;; setf-able
   :wd-back
   :wd-forward
   :wd-refresh
   :wd-title

   ;; ---------- 10. command-context (window frame)
   :wd-window ;; setf-able
   :wd-all-windows

   :wd-switch-to-frame
   :wd-switch-to-parent-frame

   :wd-window-rect      ;; setf-able.

   :wd-window-maximize
   :wd-window-minimize
   :wd-window-fullscreen

   ;; ----
   :wd-window-size%     ;; setf-able
   :wd-window-position% ;; setf-able

   ;; ---------- 12. element-retrieval
   :wd-find
   :wd-find-all
   :wd-active


   ;; ---------- 13. element-state
   :wd-element-selectedp
   :wd-element-attribute
   :wd-element-property
   :wd-element-css
   :wd-element-text
   :wd-element-name
   :wd-element-rect
   :wd-element-enabledp

   ;; ----
   :wd-element-value%
   :wd-element-displayedp%
   :wd-element-location%
   :wd-element-location-in-view%
   :wd-element-size%

   ;; ---------- 14. element-interaction
   :wd-element-click
   :wd-element-clear
   :wd-element-append-values
   :wd-element-set-values

   ;; ----
   ;;:wd-element-append-values%
   ;;:wd-element-set-values%

   ;; ---------- 15. document-handling (js source)
   :wd-execute
   :wd-source

   ;; ---------- 16. cookie
   :wd-all-cookies ;; setfable
   :wd-cookie      ;; setfable

   ;; ---------- 17. actrion
   :wd-actions
   :wd-delete-actions

   ;; ---- oss
   :wd-mouse%
   :wd-moveto%
   :wd-append-keys%

   ;; ---------- 18. user-prompt
   :wd-alert-accept
   :wd-alert-dismiss
   :wd-alert-text

   ;; ---------- 19. screen-capture
   :wd-screenshot
   :wd-screenshot-save-to

   ;; ---------- not in w3c
   :wd-log-types%

   ;;; ------------------------------ util
   :wd-wait-until
   :wd-google
   ))

