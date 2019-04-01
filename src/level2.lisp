(in-package :webdriver)

;; utilities

(defun wd-wait-until% (test-fn &key (time 10) (sleep-sec 0.5))
  (collect-first (choose
                  (mapping ((i (scan-range :length time)))
                    (if-let ((ret (funcall test-fn)))
                      ret
                      (sleep sleep-sec))))))

(defmacro wd-wait-until ((&key (time 80) (sleep-sec 0.1)) &body body)
  `(wd-wait-until%
    (lambda () ,@body)
    ,@(when time `(:time ,time))
    ,@(when sleep-sec `(:sleep-sec ,sleep-sec))))

(defun wd-google (q)
  (setf (wd-url) (format nil "https://www.google.com/search?q=~a" q)))

(defun wd-make-window ()
  (let ((windows (wd-all-windows))
        diff)
    (wd-execute "window.open()")
    (setq diff (set-difference (wd-all-windows) windows :test #'string=))
    (if (and diff (= 1 (length diff)))
        (setf (wd-window) (car diff))
      (error "can not create window"))))

