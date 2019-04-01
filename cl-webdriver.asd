(asdf:defsystem #:cl-webdriver
  :description "cl-webdriver is a tiny library to talk with Selenium 3. forked from cl-selenium-webdriver."
  :author "fgatherlet <fgatherlet@gmail.com>"
  :license "MIT"
  :depends-on (:dexador
               :jsown
               :alexandria
               :series
               :let-over-lambda
               ;; :trivia
               ;; :closer-mop
               )
  :serial t
  :components ((:module "src"
                :components
                ((:file "level2" :depends-on ("level1"))

                 (:file "level1" :depends-on ("level1-oss"))
                 (:file "level1-oss" :depends-on ("level1-w3c"))
                 (:file "level1-w3c" :depends-on ("level0"))

                 (:file "level0" :depends-on ("base" "internal"))
                 (:file "base" :depends-on ("package"))
                 (:file "internal" :depends-on ("package"))
                 (:file "package")
                 )))
  :in-order-to ((test-op (test-op cl-webdriver-test)))
  )
