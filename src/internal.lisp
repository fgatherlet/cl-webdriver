(in-package :webdriver)

;;; collection of intenal utilities.

(defun wd-find-type (type)
  (ecase type
    (:xpath "xpath")
    (:css "css selector")
    (:link-text "link text")
    (:partial-link-text "partial link text")
    (:tag-name "tag name")

    ;; below is not w3c
    (:id "id")
    (:name "name")
    (:class-name "class name")
    ))

(defun make-url(&rest path)
  (with-output-to-string (*standard-output*)
    (princ *wd-base*)
    (format t "/session/~a" *wd-session*)
    (iterate ((stop (scan path)))
      (princ "/")
      (princ stop))))

;;(defpattern wd-obj (key val)
;;  (with-gensyms (it)
;;    `(guard1 (,it :type list) (listp ,it)
;;             (wd-ref ,it ,key) ,val)))

;;(defun clos-obj-to-wd-obj (obj)
;;  "Simple converter from clos object(and structure) to wd-obj."
;;  (cons :obj
;;        (mapcar (lambda (sd)
;;                  (let ((ss (closer-mop:slot-definition-name sd)))
;;                    (cons
;;                     (string-downcase (symbol-name ss))
;;                     (slot-value obj ss))))
;;                (closer-mop:class-slots (class-of obj)))))

;;; ------------------------------ key

;; ref: selenium/rb/lib/selenium/webdriver/common/keys.rb
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *keymap*
    '(:null #\ue000
      :cancel #\ue001
      :help #\ue002
      :backspace #\ue003
      :tab #\ue004
      :clear #\ue005
      :return #\ue006
      :enter #\ue007
      :shift #\ue008
      :left-shift #\ue008
      :control #\ue009
      :left-control #\ue009
      :alt #\ue00A
      :left-alt #\ue00A
      :pause #\ue00B
      :escape #\ue00C
      :space #\ue00D
      :page-up #\ue00E
      :page-down #\ue00F
      :end #\ue010
      :home #\ue011
      :left #\ue012
      :arrow-left #\ue012
      :up #\ue013
      :arrow-up #\ue013
      :right #\ue014
      :arrow-right #\ue014
      :down #\ue015
      :arrow-down #\ue015
      :insert #\ue016
      :delete #\ue017
      :semicolon #\ue018
      :equals #\ue019
      :numpad0 #\ue01A
      :numpad1 #\ue01B
      :numpad2 #\ue01C
      :numpad3 #\ue01D
      :numpad4 #\ue01E
      :numpad5 #\ue01F
      :numpad6 #\ue020
      :numpad7 #\ue021
      :numpad8 #\ue022
      :numpad9 #\ue023
      :multiply #\ue024
      :add #\ue025
      :separator #\ue026
      :subtract #\ue027
      :decimal #\ue028
      :divide #\ue029
      :f1 #\ue031
      :f2 #\ue032
      :f3 #\ue033
      :f4 #\ue034
      :f5 #\ue035
      :f6 #\ue036
      :f7 #\ue037
      :f8 #\ue038
      :f9 #\ue039
      :f10 #\ue03A
      :f11 #\ue03B
      :f12 #\ue03C
      :meta #\ue03D
      :command #\ue03D ;; alias
      :left-meta #\ue03D ;; alias
      :right-shift #\ue050
      :right-control #\ue051
      :right-alt #\ue052
      :right-meta #\ue053
      :numpad-page-up #\ue054
      :numpad-page-down #\ue055
      :numpad-end #\ue056
      :numpad-home #\ue057
      :numpad-left #\ue058
      :numpad-up #\ue059
      :numpad-right #\ue05A
      :numpad-down #\ue05B
      :numpad-insert #\ue05C
      :numpad-delete #\ue05D
      ))
  )

(defun wd-key (key)
  (make-string 1 :initial-element (getf *keymap* key)))

(defun wd-keys (&rest keys)
  (with-output-to-string (*standard-output*)
    (dolist (key keys)
      (etypecase key
        (string (princ key))
        (symbol
         (if-let ((key-val (getf *keymap* key)))
           (princ key-val)
           (error "key is invalid [~s]" key)))))))

(defun wd-mouse-button (&optional (button :left))
  (ecase button
    (:left 0)
    (:middle 1)
    (:right 2)))

(defun wd-preprocess-action (action)
  (mapcar
   (lambda (x)
     (cond
       ((or (keywordp x)
            (stringp x)
            (numberp x)
            (typep x 'wd-obj))
        x)
       ;;((symbolp x) (eval x)) ;; ugly
       (t (eval x)) ;; ugly hack
       ))
   action
  ))

(defun wd-compile-actions (&rest actions &aux key-actions mouse-actions)
  (macrolet ((pause-mouse ()
               `(push (wd-obj
                       :type :pause
                       :duration 0)
                      mouse-actions))
             (pause-key ()
               `(push (wd-obj
                       :type :pause
                       ;;:key (wd-key :null)
                       :duration 0)
                      key-actions)))
    (dolist (action actions)
      (let ((action (wd-preprocess-action action))) ;; lame hack...
        (ecase (car action)
          (:mouse-move
           (destructuring-bind (element &key (x 0) (y 0)) (cdr action)
             (pause-key)
             (push (wd-obj :type :pointer-move
                           :origin element
                           :x x
                           :y y)
                   mouse-actions)
             ))
          (:mouse-down
           (destructuring-bind (&optional (button :left)) (cdr action)
             (pause-key)
             (push (wd-obj :type :pointer-down
                           :button (wd-mouse-button button))
                   mouse-actions)))
          (:mouse-up
           (destructuring-bind (&optional (button :left)) (cdr action)
             (pause-key)
             (push (wd-obj :type :pointer-up
                           :button (wd-mouse-button button))
                   mouse-actions)))
          
          ;; ----------
          (:key-down
           (destructuring-bind (value) (cdr action)
             (pause-mouse)
             (push (wd-obj :type :key-down
                           :value (wd-keys value))
                   key-actions)))
          (:key-up
           (destructuring-bind (value) (cdr action)
             (pause-mouse)
             (push (wd-obj :type :key-up
                           :value (wd-keys value))
                   key-actions)))
          ;; ----------
          (:mouse-click
           (destructuring-bind (&optional (button :left)) (cdr action)
             (pause-key)
             (pause-key)
             (push (wd-obj :type :pointer-down :button (wd-mouse-button button)) mouse-actions)
             (push (wd-obj :type :pointer-up :button (wd-mouse-button button)) mouse-actions)))

          (:keys
           (destructuring-bind (&rest values) (cdr action)
             (dolist (value values)
               (typecase value
                 (symbol
                  (pause-mouse)
                  (pause-mouse)
                  (push (wd-obj :type :key-down :value (wd-key value)) key-actions)
                  (push (wd-obj :type :key-up :value (wd-key value)) key-actions))
                 (string
                  (iterate ((char (scan value)))
                    (pause-mouse)
                    (pause-mouse)
                    (push (wd-obj :type :key-down :value (princ-to-string char)) key-actions)
                    (push (wd-obj :type :key-up :value (princ-to-string char)) key-actions)))))))))))
  (wd-obj
   :type "key"
   :actions
   (list
    (wd-obj
     :type :pointer
     :pointer-type :mouse
     :id :mouse
     :actions (nreverse mouse-actions))
    (wd-obj
     :type :key
     :id :key
     :actions (nreverse key-actions)
     ))))

;;(wd-mouse-button :left)





;; from ruby selenium driver.. I can not find any documantation...
;;   https://github.com/SeleniumHQ/selenium/blob/master/rb/lib/selenium/webdriver/common/error.rb
;;
;;  1  => IndexOutOfBoundsError,
;;  2  => NoCollectionError,
;;  3  => NoStringError,
;;  4  => NoStringLengthError,
;;  5  => NoStringWrapperError,
;;  6  => NoSuchDriverError,
;;  7  => NoSuchElementError,
;;  8  => NoSuchFrameError,
;;  9  => UnknownCommandError,
;;  10 => StaleElementReferenceError,
;;  11 => ElementNotVisibleError,
;;  12 => InvalidElementStateError,
;;  13 => UnknownError,
;;  14 => ExpectedError,
;;  15 => ElementNotSelectableError,
;;  16 => NoSuchDocumentError,
;;  17 => JavascriptError,
;;  18 => NoScriptResultError,
;;  19 => XPathLookupError,
;;  20 => NoSuchCollectionError,
;;  21 => TimeOutError,
;;  22 => NullPointerError,
;;  23 => NoSuchWindowError,
;;  24 => InvalidCookieDomainError,
;;  25 => UnableToSetCookieError,
;;  26 => UnhandledAlertError,
;;  27 => NoAlertPresentError,
;;  28 => ScriptTimeOutError,
;;  29 => InvalidElementCoordinatesError,
;;  30 => IMENotAvailableError,
;;  31 => IMEEngineActivationFailedError,
;;  32 => InvalidSelectorError,
;;  33 => SessionNotCreatedError,
;;  34 => MoveTargetOutOfBoundsError,
;;  # The following are W3C-specific errors,
;;  # they don't really need error codes, we just make them up!
;;  51 => InvalidXpathSelectorError,
;;  52 => InvalidXpathSelectorReturnTyperError,
;;  60 => ElementNotInteractableError,
;;  61 => InvalidArgumentError,
;;  62 => NoSuchCookieError,
;;  63 => UnableToCaptureScreenError

(defun compile-user-agent (x)
  (etypecase x
    (string x)
    (symbol
     (ecase x
       (:iphone "Mozilla /5.0 (iPhone; CPU iPhone OS 9_1 like Mac OS X) AppleWebKit/601.1.46 (KHTML, like Gecko) Version/9.0 Mobile/13B5110e Safari/601.1 ")))))
