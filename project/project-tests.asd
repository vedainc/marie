;;;; ${project}-tests.asd --- test ASDF file of ${project}

(defsystem #:${project}-tests
    :name "${project}-tests"
    :long-name "${project}"
    :description ""
    :long-description ""
    :version (:read-file-form #P"version-tests.lisp")
    :author "${author} <${email}>"
    :maintainer "${author} <${email}>"
    :license ""
    :homepage ""
    :bug-tracker ""
    :source-control ""
    :class :package-inferred-system
    :depends-on (#:fiveam
                 #:marie
                 #:${project}
                 #:${project}/t/main-tests
                 #:${project}/t/driver-tests
                 #:${project}/t/user-tests)
    :perform (test-op (o c) (uiop:symbol-call :${project}/t/main-tests :run-tests)))
