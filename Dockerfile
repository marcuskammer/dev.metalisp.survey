# Use an official Debian image as a parent image
FROM debian:buster

# Set environment variables
ENV SBCL_HOME=/opt/sbcl
ENV QUICKLISP_HOME=/home/lisp/quicklisp

# Install system dependencies
RUN apt-get update && apt-get install -y \
    sbcl \
    curl \
    --no-install-recommends && \
    rm -rf /var/lib/apt/lists/*

# Add a non-root user
RUN useradd -ms /bin/bash lisp

# Switch to the new user
USER lisp
WORKDIR /home/lisp

# Download and install Quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --noinform \
         --non-interactive \
         --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install :path "$QUICKLISP_HOME")' \
         --eval '(ql-util:without-prompting (ql:add-to-init-file))'

# Set the default command to start SBCL configured with Quicklisp
CMD ["sbcl"]
