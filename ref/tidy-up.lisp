;;; ------------------------------ session

(defun yas-make-session ()
  )
(defun yas-delete-session ()
  )

;;; ------------------------------ session not in w3c
(defun yas-status ()
  )
(defun yas-session-status ()
  )


(defun yas-all-sessions ()
  )

(defun yas-delete-session ()
  ;; delete *yas-session*
  )

(defun yas-delete-all-sessions ()
  ;; delete *yas-session*
  )

;;; ------------------------------ basic
(defun yas-url ()
  )
(defun (setf yas-url) (x)
  )

(defun yas-forward ()
  )
(defun yas-back ()
  )
(defun yas-refresh ()
  )
(defun yas-source ()
  )
(defun yas-title ()
  )

;;; ------------------------------ js
(defun yas-execute (&key (syncp t))
  )


;;; ------------------------------ screenshot
(defun yas-screenshot (&key (element nil))
  )

;;; ------------------------------ frame
(defun yas-switch-to-frame ()
  )

(defun yas-switch-to-parent-frame ()
  )

;;; ------------------------------ window
(defun yas-window ()
  )

(defun (setf yas-window) (window)
  ;; if nil, close
  )

(defun yas-all-windows ()
  )

(defun yas-window-fullscreen ()
  )
(defun yas-window-minimize ()
  )
(defun yas-window-maximize ()
  )

(defun yas-window-size ()
  )
(defun (setf yas-window-size) (xy)
  )
(defun yas-window-position ()
  )
(defun (setf yas-window-position) (xy)
  )
(defun yas-window-rect ()
  )
(defun (setf yas-window-rect) (xy)
  )

;;; ------------------------------ window oss compatible
(defun yas-oss-window ()
  )
(defun yas-oss-all-windows ()
  )
(defun yas-oss-window-size (window)
  )
(defun (setf yas-oss-window-size) (window xy)
  )
(defun yas-oss-window-position (window)
  )
(defun (setf yas-oss-window-position) (window xy)
  )
(defun yas-oss-window-maximize (window)
  )

;;; ------------------------------ element
(defun yas-find (xpath)
  )
(defun yas-find-all (xpath)
  )
(defun yas-active ()
  )
(defun yas-element-selectedp (element)
  )
(defun yas-element-attribute (element name)
  )
(defun yas-element-property (element name)
  )
(defun yas-element-css (element name)
  )
(defun yas-element-text (element)
  )
(defun yas-element-name (element)
  )
(defun yas-element-rect (element)
  )
(defun yas-element-enabledp (element)
  )

;;; ------------------------------ element not in w3c
(defun yas-element-value (element)
  )
(defun yas-element-displayedp (element)
  )
(defun yas-element-location (element)
  )
(defun yas-element-location-in-view (element)
  )
(defun yas-element-size (element)
  )

;;; ------------------------------ element operation
(defun yas-element-click (element)
  )
(defun yas-element-tap (element)
  )
(defun yas-element-clear (element)
  )
(defun (setf yas-element-value) (element value)
  "caution. this function append element value. not clear automatically."
  )

;;; ------------------------------ cookie
(defun yas-all-cookies ()
  )
(defun (setf yas-all-cookies) (obj)
  ;; if obj is nil, delete all cookies.
  )

(defun yas-cookie (name)
  )
(defun (setf yas-cookie) (name obj)
  ;; obj is nil, delete cookie.
  )


;;; ------------------------------ action
(defun (setf yas-actions) (actions)
  ;; if nil; delete
  )

;;; ------------------------------ action not in w3c
;; pst click
;; pst doubleclick
;; pst buttondown
;; pst buttonup
;; pst moveto
;; pst modifier
;; pst keys

;;; ------------------------------ alert
(defun yas-alert-dismiss ()
  )
(defun yas-alert-accept ()
  )
(defun yas-alert-text ()
  )
(defun (setf yas-alert-text) (text)
  )

;;; ------------------------------ alert not in w3c

;; pst dismiss_alertg
;; pst accept_alert
;; get alert_text
;; pst alert_text
;; pst alert/credentials


;;; ------------------------------ timeout

(defun yas-timeout (timeout)
  )

;;; ------------------------------ timeout not in w3c
;;  implicitly_wait:    [:pst, '/timeouts/implicit_wait'],
;;  set_script_timeout: [:pst, '/timeouts/async_script'],

;;; ------------------------------ not in w3c
;; # logs
;; get_available_log_types: [:get, '/log/types'],
;; get_log: [:pst, '/log']
;;
;; # html 5
;; execute_sql: [:pst, '/execute_sql'],
;; get_location: [:get, '/location'],
;; set_location: [:pst, '/location'],
;; get_app_cache: [:get, '/application_cache'],
;; get_app_cache_status: [:get, '/application_cache/status'],
;; clear_app_cache: [:del, '/application_cache/clear'],
;; get_network_connection: [:get, '/network_connection'],
;; set_network_connection: [:pst, '/network_connection'],
;; get_local_storage_item: [:get, '/local_storage/key/:key'],
;; remove_local_storage_item: [:del, '/local_storage/key/:key'],
;; get_local_storage_keys: [:get, '/local_storage'],
;; set_local_storage_item: [:pst, '/local_storage'],
;; clear_local_storage: [:del, '/local_storage'],
;; get_local_storage_size: [:get, '/local_storage/size'],
;; get_session_storage_item: [:get, '/session_storage/key/:key'],
;; remove_session_storage_item: [:del, '/session_storage/key/:key'],
;; get_session_storage_keys: [:get, '/session_storage'],
;; set_session_storage_item: [:pst, '/session_storage'],
;; clear_session_storage: [:del, '/session_storage'],
;; get_session_storage_size: [:get, '/session_storage/size'],
;;
;; # rotatable
;; get_screen_orientation: [:get, '/orientation'],
;; set_screen_orientation: [:pst, '/orientation'],
;; # ime
;; ime_get_available_engines: [:get, '/ime/available_engines'],
;; ime_get_active_engine: [:get, '/ime/active_engine'],
;; ime_is_activated: [:get, '/ime/activated'],
;; ime_deactivate: [:pst, '/ime/deactivate'],
;; ime_activate_engine: [:pst, '/ime/activate'],
;; # touch
;; touch_single_tap: [:pst, '/touch/click'],
;; touch_double_tap: [:pst, '/touch/doubleclick'],
;; touch_long_press: [:pst, '/touch/longclick'],
;; touch_down: [:pst, '/touch/down'],
;; touch_up: [:pst, '/touch/up'],
;; touch_move: [:pst, '/touch/move'],
;; touch_scroll: [:pst, '/touch/scroll'],
;; touch_flick: [:pst, '/touch/flick'],




