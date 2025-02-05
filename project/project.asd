;;;; ${project}.asd --- top-level ASDF file for ${project}

(defsystem #:${project}
    :name "${project}"
    :long-name "${project}"
    :description ""
    :long-description ""
    :version (:read-file-form #P"version.sexp")
    :author "${author} <${email}>"
    :maintainer "${author} <${email}>"
    :license ""
    :homepage ""
    :bug-tracker ""
    :source-control ""
    :class :package-inferred-system
    :depends-on (#:marie
                 #:clingon
                 #:${project}/src/core
                 #:${project}/src/driver
                 #:${project}/src/user
                 #:${project}/src/cli)
    :in-order-to ((test-op (test-op "${project}-tests")))
    :build-operation "program-op"
    :build-pathname "${project}"
    :entry-point "${project}/src/cli:main")
