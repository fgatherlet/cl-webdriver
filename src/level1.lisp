(in-package :webdriver)

(defmacro use-w3c-fn (symbol)
  `(setf (fdefinition ',symbol)
         (fdefinition ',(intern (symbol-name symbol) :webdriver.w3c))))

(defmacro use-w3c-setf (symbol)
  `(setf (fdefinition '(setf ,symbol))
         (fdefinition '(setf ,(intern (symbol-name symbol) :webdriver.w3c)))))

;;; ------------------------------ 9. navigation
(use-w3c-fn wd-url)
(use-w3c-setf wd-url)

(use-w3c-fn wd-back)
(use-w3c-fn wd-forward)
(use-w3c-fn wd-title)

;;; ------------------------------ 10. command contexts (window frame)

(use-w3c-fn wd-window)
(use-w3c-setf wd-window)

(use-w3c-fn wd-all-windows)

(use-w3c-fn wd-switch-to-frame)
(use-w3c-fn wd-switch-to-parent-frame)

;; window-rect
(defun wd-window-rect ()
  (if *wd-session-w3c-p*
      (webdriver.w3c::wd-window-rect)
    (webdriver.oss::wd-window-rect)))

(defun (setf wd-window-rect) (rect)
  (if *wd-session-w3c-p*
      (setf (webdriver.w3c::wd-window-rect) rect)
    (setf (webdriver.oss::wd-window-rect) rect)))

(use-w3c-fn wd-window-maximize)
(use-w3c-fn wd-window-minimize)
(use-w3c-fn wd-window-fullscreen)

;;; ------------------------------ 12. element retrieval

(use-w3c-fn wd-find)
(use-w3c-fn wd-find-all)
(use-w3c-fn wd-active)

;;; ------------------------------ 13. element state

(use-w3c-fn wd-element-selectedp)
(use-w3c-fn wd-element-attribute)
(use-w3c-fn wd-element-property)
(use-w3c-fn wd-element-css)
(use-w3c-fn wd-element-text)
(use-w3c-fn wd-element-name)
(use-w3c-fn wd-element-rect)
(use-w3c-fn wd-element-enabledp)

;;; ------------------------------ 14. element interaction

(use-w3c-fn wd-element-click)
(use-w3c-fn wd-element-clear)
(use-w3c-fn wd-element-append-values)
(use-w3c-fn wd-element-set-values)

;;; ------------------------------ 15. document handling (js, source)

(use-w3c-fn wd-source)
(use-w3c-fn wd-execute)

;;; ------------------------------ 16. cookie

(use-w3c-fn wd-all-cookies)
(use-w3c-setf wd-all-cookies)
(use-w3c-fn wd-cookie)
(use-w3c-setf wd-cookie)

;;; ------------------------------ 17. actions

;; wd-actions
(defmacro wd-actions (&rest actions)
  "w3c note. chrome does not support actions yet. safari actions has bugs."
  `(if *wd-session-w3c-p*
       (webdriver.w3c::wd-actions ,@actions)
     (webdriver.oss::wd-actions ,@actions)))

(defun wd-delete-actions ()
  (if *wd-session-w3c-p*
      (webdriver.w3c::wd-delete-actions)
    (webdriver.oss::wd-delete-actions)))

;;; ------------------------------ 18. user prompts

(use-w3c-fn wd-alert-accept)
(use-w3c-fn wd-alert-dismiss)
(use-w3c-fn wd-alert-text)
(use-w3c-setf wd-alert-text)

;;; ------------------------------ 19. screen capture

(use-w3c-fn wd-screenshot)
(use-w3c-fn wd-screenshot-save-to)


