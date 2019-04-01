(defpackage :webdriver.oss
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
   :wd-keys
   :wd-mouse-button
   :wd-element-to-id
   :wd-preprocess-action)
  (:import-from
   :webdriver.w3c
   :wd-url
   :wd-window
   )
  (:import-from
   :alexandria
   :when-let
   ))

(in-package :webdriver.oss)

;;;; https://www.w3.org/TR/webdriver1/#element-interactability

;;; ------------------------------ 9. navigation

;;; ------------------------------ 10. command contexts (window frame)

;;(defun (setf wd-window%) (window)
;;  (if window
;;      (wd-post (wd-obj "name" window) "window")
;;    (wd-delete "window")))


(defun wd-window-rect ()
  (let ((size (wd-window-size%))
        (position (wd-window-position%))
        (rect (wd-obj)))
    (setf (wd-ref rect :x) (wd-ref position :x)
          (wd-ref rect :y) (wd-ref position :y)
          (wd-ref rect :width) (wd-ref size :width)
          (wd-ref rect :height) (wd-ref size :height))
    rect))

(defun (setf wd-window-rect) (rect)
  (setf (wd-window-size%) rect)
  (setf (wd-window-position%) rect))

(defun wd-window-size% ()
  (when-let ((window (wd-window)))
    (wd-get "window" window "size")))

(defun (setf wd-window-size%) (xy)
  "xy is like (:obj (\"height\" . xx) (\"width\" . xx))."
  (when-let ((window (wd-window)))
    (wd-post
     xy
     "window" window "size")))

(defun wd-window-position% ()
  (when-let ((window (wd-window)))
    (wd-get "window" window "position")))

(defun (setf wd-window-position%) (xy)
  "xy is like (:obj (\"x\" . xx) (\"y\" . xx))."
  (when-let ((window (wd-window)))
    (wd-post
     xy
     "window" window "position")))

;;; ------------------------------ 12. element retrieval

;;; ------------------------------ 13. element state

(defun wd-element-value% (element)
  "what is this?"
  (wd-get "element" (wd-element-to-id element) "value"))
(defun wd-element-displayedp% (element)
  (wd-get "element" (wd-element-to-id element) "displayed"))
(defun wd-element-location% (element)
  "what is this?"
  (wd-get "element" (wd-element-to-id element) "location"))
(defun wd-element-location-in-view% (element)
  (wd-get "element" (wd-element-to-id element) "location_in_view"))
(defun wd-element-size% (element)
  (wd-get "element" (wd-element-to-id element) "size"))

;;; ------------------------------ 14. element interaction

;;; ------------------------------ 15. document handling (js, source)

;;; ------------------------------ 16. cookie

;;; ------------------------------ 17. actions

(defmacro wd-actions (&rest actions)
  ;; TODO: this is too naive.
  `(apply #'wd-eval-actions% ',actions))

(defun wd-delete-actions ()
  ;; TODO: implement
  `(wd-append-keys% :null))


(defun wd-eval-actions% (&rest actions)
  (dolist (action actions)
    (let ((action (wd-preprocess-action action))) ;; lame hack...
      (ecase (car action)
        (:mouse-move
         (destructuring-bind (element &key (x 0) (y 0)) (cdr action)
           (wd-moveto% element :x x :y y)))
        (:mouse-down
         (destructuring-bind (&optional (button :left)) (cdr action)
           (wd-mouse% :action :down :button button)))
        (:mouse-up
         (destructuring-bind (&optional (button :left)) (cdr action)
           (wd-mouse% :action :up :button button)))

        ;; ----------
        (:key-down
         (destructuring-bind (value) (cdr action)
           (wd-append-keys% (list value))))
        (:key-up
         (destructuring-bind (value) (cdr action)
           (wd-append-keys% (list value))))

        ;; ----------
        (:mouse-click
         (destructuring-bind (&optional (button :left)) (cdr action)
           (wd-mouse% :action :click :button button)))

        (:keys
         (destructuring-bind (&rest values) (cdr action)
           (apply #'wd-append-keys% values)))))))

(defun wd-mouse% (&key (action :click) (button :left))
  ;;(check-type action (or :click :doubleclick :down :up))
  (wd-post (wd-obj "button" (wd-mouse-button button))
            (ecase action
              (:click "click")
              (:doubleclick "doubleclick")
              (:down "buttondown")
              (:up "buttonup"))))

(defun wd-moveto% (element &key (x 0) (y 0))
  (wd-post (wd-obj
             :element (wd-element-to-id element)
             ;;:value element
             :xoffset x
             :yoffset y
             )
            "moveto"))

(defun wd-append-keys% (&rest keys)
  "caution. this function append element value. not clear automatically."
  (wd-post
   (wd-obj
    "value"
    (list (apply #'wd-keys keys)))
   "keys"))

;;;;; ??
;;(defun wd-append-meta-keys% (&rest keys)
;;  "caution. this function append element value. not clear automatically."
;;  (wd-post
;;   (wd-obj
;;    "value"
;;    (list (apply #'wd-keys keys)))
;;   "modifier"))

;;; ------------------------------ 18. user prompts

;;; ------------------------------ 19. screen capture

;;; ------------------------------ not in w3c

(defun wd-log-types% ()
  (wd-get "log" "types"))

(defun (setf wd-log-types%) (type)
  (wd-post
   (wd-obj "type" type)
   "log"))

