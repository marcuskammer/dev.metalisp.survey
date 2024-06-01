ENV QUICKLISP_HOME=~/quicklisp
curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --noinform \
         --non-interactive \
         --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install :path "$QUICKLISP_HOME")' \
         --eval '(ql-util:without-prompting (ql:add-to-init-file))'
