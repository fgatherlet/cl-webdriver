;;(in-package :webdriver)
(defpackage :webdriver.w3c
  (:use
   :cl
   :series)
  (:import-from
   :webdriver
   :wd-get
   :wd-post
   :wd-delete
   :wd-obj
   :wd-ref
   :wd-element-to-id
   :wd-keys
   :wd-key
   :wd-mouse-button
   :wd-find-type
   :*wd-default-find-element-method*
   :*wd-session-w3c-p*
   :wd-compile-actions)
  (:import-from
   :alexandria
   :when-let
   ))

(in-package :webdriver.w3c)

;;;; https://www.w3.org/TR/webdriver1/#element-interactability

;;; ------------------------------ 9. navigation
(defun wd-url ()
  (wd-get "url"))

(defun (setf wd-url) (url)
  (wd-post (wd-obj "url" url) "url"))

(defun wd-back () (wd-post nil "back"))
(defun wd-forward () (wd-post nil "forward"))
(defun wd-refresh () (wd-post "refresh"))
(defun wd-title () (wd-get "title"))

;;; ------------------------------ 10. command contexts (window frame)

(defun wd-window ()
  (wd-get "window"))

(defun (setf wd-window) (window)
  (if window
      (wd-post
       (wd-obj (if *wd-session-w3c-p* "handle" "name") window)
       "window")
    (wd-delete "window")))

(defun wd-all-windows ()
  (wd-get "window" "handles"))

(defun wd-switch-to-frame (id)
  "id is element or integer or nil."
  (check-type id (or null integer wd-obj))
  (wd-post (wd-obj "id" id) "frame"))

(defun wd-switch-to-parent-frame ()
  (wd-post nil "frame" "parent"))

(defun wd-window-rect ()
  (wd-get "window" "rect"))

(defun (setf wd-window-rect) (xy)
  (wd-post xy "window" "rect"))

(defun wd-window-maximize ()
  (wd-post nil "window" "maximize"))

(defun wd-window-minimize ()
  (wd-post nil "window" "minimize"))

(defun wd-window-fullscreen ()
  (wd-post nil "window" "fullscreen"))

;;; ------------------------------ 12. element retrieval

(defun wd-find (value
                 &key
                   (from nil) ;; element
                   (by *wd-default-find-element-method*)
                   )
  (let ((request (wd-obj "value" value "using" (wd-find-type by))))
    (if from
        (wd-post request "element" (wd-element-to-id from) "element")
      (wd-post request "element"))))

(defun wd-find-all (value
                     &key
                       (from nil) ;; element
                       (by *wd-default-find-element-method*)
                       )
  (let ((request (wd-obj "value" value "using" (wd-find-type by))))
    (if from
        (wd-post request "element" (wd-element-to-id from) "elements")
      (wd-post request "elements"))))

(defun wd-active ()
  (wd-post nil "element" "active"))

;;; ------------------------------ 13. element state

(defun wd-element-selectedp (element)
  (wd-get "element" (wd-element-to-id element) "selected"))

(defun wd-element-attribute (element name)
  "get dom attribute. stratic value?"
  (wd-get "element" (wd-element-to-id element) "attribute" name))

(defun wd-element-property (element name)
  "get dom property. dynamic value?"
  (wd-get "element" (wd-element-to-id element) "property" name))

(defun wd-element-css (element name)
  "css value"
  (wd-get "element" (wd-element-to-id element) "css" name))

(defun wd-element-text (element)
  (wd-get "element" (wd-element-to-id element) "text"))

(defun wd-element-name (element)
  (wd-get "element" (wd-element-to-id element) "name"))

(defun wd-element-rect (element)
  (wd-get "element" (wd-element-to-id element) "rect"))

(defun wd-element-enabledp (element)
  (wd-get "element" (wd-element-to-id element) "enabled"))

;;; ------------------------------ 14. element interaction

(defun wd-element-click (element)
  (check-type element wd-obj)
  (wd-post nil "element" (wd-element-to-id element) "click"))

(defun wd-element-clear (element)
  (check-type element wd-obj)
  (wd-post nil "element" (wd-element-to-id element) "clear"))

(defun wd-element-append-values (element &rest values)
  "caution. this function append element value. not clear automatically."
  (let* (;; w3c
         (text (apply #'wd-keys values))
         ;; oss
         (value (collect (map-fn t #'princ-to-string (scan text)))))
    (wd-post
     (wd-obj
      :text text
      :value value)
     "element" (wd-element-to-id element) "value")))

(defun wd-element-set-values (element &rest values)
  (wd-element-clear element)
  (apply #'wd-element-append-values element values))

;;; ------------------------------ 15. document handling (js, source)

(defun wd-source () (wd-get "source"))

(defun wd-execute (script
                    &key
                      (args '())
                      (syncp t)
                      )
  (check-type script string)
  (check-type args list)
  (wd-post (wd-obj "script" script
                     "args" args)
            (if syncp
                (if *wd-session-w3c-p* "execute/sync" "execute")
              "execute/async")))

;;; ------------------------------ 16. cookie

(defun wd-all-cookies ()
  (wd-get "cookie"))

(defun (setf wd-all-cookies) (cookie)
  "add cookie (caution. not clear and add, but just add) if (identity cookie).
clear all cookies if (null cookie.)"
  (check-type cookie (or wd-obj null))
  (if cookie
      (wd-post
       (wd-obj :cookie cookie)
       "cookie")
    (wd-delete "cookie")))

(defun wd-cookie (name)
  (wd-get "cookie" name))

(defun (setf wd-cookie) (cookie name)
  (check-type cookie (or wd-obj null))
  (if cookie
      (setf (wd-all-cookies) cookie)
    (wd-delete "cookie" name)))

;;; ------------------------------ 17. actions

(defun wd-actions-aux (actions)
  (wd-post
   (apply #'wd-compile-actions actions)
   "actions"))

(defmacro wd-actions (&rest actions)
  `(wd-actions-aux ',actions))

(defun wd-delete-actions ()
  (wd-delete "actions"))

;;; ------------------------------ 18. user prompts

(defun wd-alert-accept ()
  (wd-post nil "alert/accept"))

(defun wd-alert-dismiss ()
  (wd-post nil "alert/dismiss"))

(defun wd-alert-text ()
  (wd-get "alert/text"))

(defun (setf wd-alert-text) (text)
  (wd-post
   (wd-obj "text" text)
   "alert/text"))

;; pst dismiss_alertg
;; pst accept_alert
;; get alert_text
;; pst alert_text
;; pst alert/credentials

;;; ------------------------------ 19. screen capture

(defun wd-screenshot (&key element)
  (if element
      (wd-get "element" (wd-element-to-id element) "screenshot")
    (wd-get "screenshot")))

(defun wd-screenshot-save-to (xpathname
                               &key
                                 (element nil)
                                 (if-exists :error))
  (when-let ((val (wd-screenshot :element element)))
    (with-open-file (stream xpathname :direction :output :element-type '(unsigned-byte 8) :if-exists if-exists)
      (collect-stream
       stream
       (scan (base64:base64-string-to-usb8-array val))
       #'write-byte))))

