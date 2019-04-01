(in-package :webdriver)

(defvar *wd-default-find-element-method* :xpath)

(defvar *wd-base* "http://127.0.0.1:4444/wd/hub")

(defvar *wd-session* nil)
(defvar *wd-session-w3c-p* nil)

(defmacro! with-wd-handler (&body body)
  `(let (,g!response-source
         ,g!response
         ,g!response-value
         ,g!response-error
         ,g!http-status
         ,g!oss-status)
     (handler-case
         (multiple-value-bind (,g!body ,g!status)
             (progn ,@body)
           (setq ,g!response-source ,g!body
                 ,g!response (jsown:parse ,g!body)
                 ,g!http-status ,g!status))
       (dex:http-request-failed (,g!err)
         (setq ,g!response-source (dex:response-body ,g!err)
               ,g!response (ignore-errors (jsown:parse ,g!response-source))
               ,g!http-status (dex:response-status ,g!err))))
     (if (or (eql t ,g!response) ;; jsown parse bugp?
             (null ,g!response))
       (error (make-instance
               'protocol-error
               :http-status ,g!http-status
               :oss-status ,g!oss-status
               :source ,g!response-source)))
     (if *wd-session-w3c-p*
         (unless (= 200 ,g!http-status)
           (setq ,g!response-error ,g!http-status))
       (progn
         (unless (= 0 (wd-ref ,g!response :status))
           (setq ,g!response-error (wd-ref ,g!response :status)))))
     (setq ,g!response-value (wd-ref ,g!response :value))

     (values (if ,g!response-error nil ,g!response-value)
             ,g!response-error
             ,g!response)))

;;; ------------------------------ webdriver primitive api.

(defun wd-get (&rest paths)
  "PATHS are the path of webdriver api splited by \"/\".
And after the \"*wd-base*/session/<session-id>\".

This function returns 3 values.
0: Selenium value field at the selenium response.
1: selenium error code (nil if error code is 0).
2: selenium response. "
  (with-wd-handler
    (dex:get (apply #'make-url paths))))

(defun wd-post (jobj &rest paths)
  "Like wd-get. But execute http post.
 JOBJ is jsown object to send to selenium webdriver."
  (with-wd-handler
    (dex:post (apply #'make-url paths)
              :content (or (and jobj (jsown:to-json jobj)) "{}")
              :headers '(("Content-Type" . "application/json;charset=UTF-8"))
              )))

(defun wd-delete (&rest paths)
  "Like yes-get. But execute http delete."
  (with-wd-handler
    (dex:delete (apply #'make-url paths))))

;;; ------------------------------ 8. sessions

(defun wd-make-session (desired-capabilities)
  (let (response-source
        response
        response-error
        session-id
        http-status
        oss-status
        w3c-p
        )
    (handler-case
        (multiple-value-bind (body status)
            (dex:post (format nil "~a/session" *wd-base*)
                      :content
                      (jsown:to-json
                       (wd-obj
                        :capabilities (wd-obj)
                        :desired-capabilities desired-capabilities)))
          (setq http-status status
                response-source body))
      (dex:http-request-failed (err)
        (setq response-source (dex:response-body err)
              http-status (dex:response-status err))))
    (setq response (ignore-errors (jsown:parse response-source)))

    ;;(error (format nil "response[~s]" response))

    (when (or (eql t response) ;; jsown parse bugp? parse html and emmit t.
              (null response))
      (error (make-instance
              'protocol-error
              :http-status http-status
              :oss-status oss-status
              :response-source response-source
              )))

    (setq oss-status (wd-ref response :status))

    (setq w3c-p (if oss-status nil t))

    ;;(error "~a:~a:~a" w3c-p http-status oss-status)

    (if w3c-p
        (setq response-error (if (= 200 http-status) nil http-status))
      (setq response-error (if (= 0 oss-status) nil oss-status)))

    (setq session-id (or (wd-ref response :session-id)
                         (wd-ref (wd-ref response :value) :session-id)))

    (values session-id w3c-p response-error response)))

(defun wd-delete-session ()
  (prog1 *wd-session*
    (wd-delete)
    (setq *wd-session* nil)))

(defun wd-init-session (&key
                           (capabilities (wd-make-capabilities-for-chrome))
                           (base "http://127.0.0.1:4444/wd/hub")
                         &aux
                           body)
  (setq *wd-base* base)
  (multiple-value-setq (*wd-session*
                        *wd-session-w3c-p*
                        response-status
                        body)
    (wd-make-session capabilities))
  (values *wd-session* *wd-session-w3c-p* response-status body))


(defmacro! with-wd ((&key
                      (capabilities (wd-make-capabilities-for-chrome))
                      ;;(session (wd-make-session (wd-make-capabilities-for-chrome)))
                      (base "http://127.0.0.1:4444/wd/hub"))
                   &body body)
  "Execute body with temporary session. The session would be cleaned up after execution."
  `(let (*wd-session*
         *wd-session-w3c-p*
         ,g!response-error
         *wd-base*)
     (wd-init-session
      :capabilities ,capabilities
      :base ,base)
     ;;(multiple-value-setq (*wd-session* *wd-session-w3c-p*)
     ;;(wd-make-session capabilities))
     (unwind-protect
          (progn
            ,@body)
       (wd-delete-session)
       )))

;; ----------

(defun wd-status ()
  (with-wd-handler
    (dex:get (format nil "~a/status" *wd-base*))))

;; ----------

(defun wd-timeouts ()
  "unit is milli seconds?"
  (wd-get "timeouts"))

(defun wd-set-timeouts (&key
                           (implicit 0)
                           (page-load 300000)
                           (script 30000))
  "unit is milli seconds?"
  (wd-post
   (wd-obj
    "implicit" implicit
    "pageLoad" page-load
    "script" script)
   "timeouts"
   ))

;; ---------- not in w3c. for grid.

(defun wd-session-status% ()
  (wd-get))

(defun wd-all-sessions% ()
  "for webdriver grid."
  (with-wd-handler
    (dex:get (format nil "~a/sessions" *wd-base*))))


(defun wd-delete-all-sessions% ()
  "for webdriver grid."
  (mapc (lambda (x)
          (let ((*wd-session* (wd-ref x "id")))
            (wd-delete-session)))
        (wd-all-sessions%)))

