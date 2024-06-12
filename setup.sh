#!/bin/bash
check_ubuntu() {
  if [ -f /etc/os-release ]; then
    . /etc/os-release
    if [ "$ID" != "ubuntu" ]; then
      echo "This script is intended to be run on Ubuntu."
      exit 1
    fi
  else
    echo "Unable to detect the operating system."
    exit 1
  fi
}

check_ubuntu

sudo apt install -y libev4 libsqlite3-dev

curl https://beta.quicklisp.org/quicklisp.lisp -o ~/quicklisp.lisp

sbcl --noinform --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(ql-util:without-prompting (ql:add-to-init-file))" --non-interactive

sbcl --noinform --eval "(ql:quickload '(:hunchentoot :drakma :cl-yaml :cl-json :jonathan :cl-ppcre :spinneret :dexador :rove :vecto :woo :cl-dbi :clsql-sqlite3 :mito :bknr.datastore :cl-project))" --non-interactive
