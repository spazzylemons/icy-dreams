#!/bin/bash
set -e

buildapp --output icy-dreams \
    --load ~/quicklisp/setup.lisp \
    --load $(pwd)/icy-dreams.asd \
    --load-system icy-dreams \
    --eval '(defun main (argv) (declare (ignore argv)) (icy-dreams:buildapp-main))' \
    --entry main \
    --compress-core
