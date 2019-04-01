(defsystem cl-webdriver-test
  :author "fgatherlet <fgatherlet@gmail.com>"
  :license "MIT"
  :depends-on (:ya-selenium
               :prove)
  :defsystem-depends-on (:prove-asdf)
  :components
  ((module "t"
           :components ((:test-file "webdriver")
                        )))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
