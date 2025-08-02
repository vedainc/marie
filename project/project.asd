;;;; ${project}.asd --- main ASDF file of ${project}

(defsystem #:${project}
    :name "${project}"
    :long-name "${project}"
    :description ""
    :long-description ""
    :version #.(uiop:read-file-form (make-pathname :directory '(:relative "src") :name "version" :type "lisp"))
    :author "${author} <${email}>"
    :maintainer "${author} <${email}>"
    :license ""
    :homepage ""
    :bug-tracker ""
    :source-control ""
    :class :package-inferred-system
    :depends-on (#:marie
                 #:clingon
                 #:${project}/src/specials
                 #:${project}/src/core
                 #:${project}/src/main
                 #:${project}/src/driver
                 #:${project}/src/user)
    :in-order-to ((test-op (test-op "${project}-tests")))
    :build-operation "program-op"
    :build-pathname "${project}"
    :entry-point "${project}/src/main:main"
    :perform (load-op :after (o c) (uiop:symbol-call :${project}/src/main :initialize)))
