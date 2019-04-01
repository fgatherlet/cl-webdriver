(in-package :webdriver)

;;; ------------------------------ condition

(define-condition protocol-error (error)
  ((http-status :initarg :http-status :initform nil)
   (oss-status :initarg :oss-status :initform nil)
   (response :initarg :response :initform nil)
   (response-source :initarg :response-source :initform nil)
   ))

(defmethod print-object ((error protocol-error) stream)
  (with-slots (http-status oss-status response response-source) error
    (format stream "~a" (type-of error))
    (if *wd-session-w3c-p*
        (when http-status
          (format stream "~%(http-status:~s)" http-status))
      (when oss-status
        (format stream "~%(oss-status:~s)" oss-status)))
    (when response
      (format stream "~%(response:~s)" response))
    (when response-source
      (if (<= (length response-source) 512)
          (format stream "~%(response-source:~s)" response-source)
          (format stream
                  "~%(response-source:~s...)"
                  (subseq response-source 0 512))))))

;;; ------------------------------ wd-obj

(defun camelize (x)
  "simple camelize function"
  (if (keywordp x)
      (with-output-to-string (*standard-output*)
        (let (prev-is-dash-p)
          (iterate ((char (scan (symbol-name x))))
            (cond
              ((char= #\- char) (setq prev-is-dash-p t))
              (prev-is-dash-p
               (write-char (char-upcase char))
               (setq prev-is-dash-p nil))
              (t
               (write-char (char-downcase char)))))))
    x))

(defun wd-obj-p (x)
  (and (consp x)
       (eql :obj (car x))))
(deftype wd-obj ()
  `(satisfies wd-obj-p))

(defun wd-obj (&rest rest)
  "Short hand to make jsown object"
  (cons :obj
        (collect (mapping (((k v) (chunk 2 2 (scan rest))))
                   (cons (camelize k)
                         (camelize v)
                         )))))

(defun wd-ref (obj key)
  ;; from jsown::val-safe
  (handler-case
      (values (jsown:val obj (camelize key)) t)
    (error () nil nil)))

(defun (setf wd-ref) (val obj key)
  (setf (jsown:val obj (camelize key))
        (camelize val)))

;;; ------------------------------

(defun wd-make-cookie (name value &key path domain secure expiry)
  (wd-obj
   :name name
   :value value
   :path path
   :domain domain
   :secure secure
   :expiry expiry))

(defun wd-make-rect (&key (x 32) (y 32) (width 1024) (height 1024))
  (wd-obj :x x :y y :width width :height height))

(defun wd-element-to-id (obj)
  "extract element-string from the yes-element like (:obj (<element-key> . <element-string>),,,).
wd-ref obj \"ELEMENT\" is better. but geckodriver's key would be \"element-xxx\"..."
  (cdr (car (cdr obj))))

;;; ------------------------------ short hand to handle capabilities.

(defun wd-make-capabilities-for-chrome (&key
                                          (headlessp nil)
                                          (user-data-dir nil)
                                          (user-agent nil)
                                          (w3c-p nil)
                                           )
  (let* ((sub-obj-args '())
         (sub-obj (wd-obj))
         (obj (wd-obj
               :browser-name "chrome")))
    (when w3c-p
      (setf (wd-ref sub-obj :w3c) t))
    (when headlessp
      (push "--headless" sub-obj-args))
    (when user-agent
      (push (format nil "--user-agent='~a'" (compile-user-agent user-agent)) sub-obj-args))
    (when user-data-dir
      (push (format nil "--user-data-dir=~a" user-data-dir) sub-obj-args))

    (setf (wd-ref sub-obj "args") sub-obj-args
          (wd-ref obj "goog:chromeOptions") sub-obj)
    obj))

(defun wd-make-capabilities-for-firefox (&key
                                            (headlessp nil)
                                            (user-data-dir nil)
                                            (user-agent nil)
                                       )
  (let* ((sub-obj-args '())
         (sub-obj-prefs (wd-obj))
         (sub-obj (wd-obj))
         (obj (wd-obj
               "browserName" "firefox")))
    (when headlessp
      (push "-headless" sub-obj-args))
    (when user-agent
      ;;(setf (wd-ref sub-obj-prefs "general.useragent.extra.firefox") (compile-user-agent user-agent)))
      (setf (wd-ref sub-obj-prefs "general.useragent.override") (compile-user-agent user-agent)
            (wd-ref sub-obj-prefs "general.useragent.updates.enabled") t))


    (setf (wd-ref sub-obj "args") sub-obj-args
          (wd-ref sub-obj "prefs") sub-obj-prefs
          (wd-ref obj "moz:firefoxOptions") sub-obj)
    obj))

