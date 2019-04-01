# cl-webdriver (selenium binding)
`cl-webdriver` is a library to talk with webdriver (selenium 3).
It is very thin wrapper of [raw http json api](https://www.w3.org/TR/webdriver1/).

This is a forked project of [cl-selenium-webdriver](https://github.com/TatriX/cl-selenium-webdriver),
to talk with selenium 3 standalone server (or chromedriver, geckodriver, safaridriver).

Additionally added and removed and renamed almost all APIs.




## Warning
This software is in development. The APIs will be likely to change.

## Usage

### Simple example.

```
(in-package :cl-user)
(use-package :webdriver)

;; launch geckodriver or something.

(wd-init-session
 :base "http://127.0.0.1:4444"
 :capabilities (wd-make-capabilities-for-firefox))

(setf (wd-url) "https://www.google.com")
(wd-element-set-values (wd-find "//input[@name='q']") "common lisp" :enter)

(let ((url
    (wd-element-attribute (wd-find "//a[ contains(@href, 'wikipedia')] ") "href")))
  (setf (wd-url) url))
  
(print (wd-title))

(print (wd-source))

(print (wd-execute "return 1+1;"))

(wd-execute "alert('hello')")
(print (wd-alert-text))
(wd-alert-accept)
```

### Original example.

Based on `cl-selenium-webdriver` example.

```lisp
(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :webdriver))

(defpackage go-test
  (:use :cl :webdriver))
(in-package :go-test)

(defparameter *code* "
package main
import \"fmt\"

func main() {
    fmt.Print(\"Hello WebDriver!\")
}")

(wd-init-session
 :base "http://127.0.0.1:4444"
 :capabilities (wd-make-capabilities-for-chrome))

(setf (wd-url) "http://play.golang.org/?simple=1")

(let ((elem (wd-find "#code" :by :css)))
  (wd-element-clear elem)
  (wd-element-append-values elem *code*)
  (wd-element-click (wd-find "#run" :by :css))

  (wd-wait-until ()
    (let* ((output (wd-find "#output" :by :css))
           (output-text (and output (wd-element-text output))))
      (not (and output
                (equal "Waiting for remote server..."
                       output-text)))))

  (print (wd-element-text (wd-find "#output" :by :css))))
```

## Installation

```text
git clone https://github.com/fgatherlet/cl-webdriver ~/quicklisp/local-projects/
(ql:quickload :cl-webdriver)
```

You need a running instance of webdriver.
(chromedriver, geckodriver, safaridriver, selenium-server-standalone etc.)

### some webdriver links.

- selenium standalone server

[Download](http://www.seleniumhq.org/download/) it and run:
```
curl -L0 <some version 3 standalone jar> -o selenium-server-standalone.jar
java -jar selenium-server-standalone.jar
```

### specify webdriver endpoint

You can specify webdriver endpoint with `*wd-base*`.

The default would be `http://127.0.0.1:9515` for `chromedriver`.
`http://127.0.0.1:4444/wd/hub` for `selenium-server-standalone`.

## Bugs

### w3c's actions api is not stable.

Each webdriver's implementation seems to be not mature.
You shoud use `wd-element-(click|clear|append-values|set-values)` API.

### utility to handle capabilities is too simple and lame.

### Testcase is not yet.

### I don't have any microsoft product.

## Copyright

Licensed under the MIT License.
