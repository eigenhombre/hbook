FROM ubuntu

RUN apt-get -qq -y update
RUN apt-get -qq -y upgrade

RUN apt-get install -qq -y sbcl make curl git ecl

# Pull down Quicklisp and install it
RUN curl -s -o quicklisp.lisp http://beta.quicklisp.org/quicklisp.lisp

RUN ecl --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install :path "/home/janice/quicklisp")' \
         --eval '(ql:quickload :1am)'

RUN echo | ecl --load /home/janice/quicklisp/setup.lisp --eval '(ql:add-to-init-file)'
RUN echo | sbcl --load /home/janice/quicklisp/setup.lisp --eval '(ql:add-to-init-file)' --quit

RUN sbcl --eval '(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)'

ENV LISP_HOME=/home/janice/quicklisp/local-projects
WORKDIR /home/janice/hbook

# Run the unit tests:
COPY . /home/janice/hbook
RUN make test-ecl
RUN make test
