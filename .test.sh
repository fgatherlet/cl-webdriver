#/bin/sh

sbcl --non-interactive \
     --eval '(ql:quickload :ya-selenium)' \
     --eval '(asdf:test-system :ya-selenium)'
