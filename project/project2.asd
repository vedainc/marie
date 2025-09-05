;;;; ${project}.asd --- main ASDF file of ${project}

(defsystem #:${project}
    :name "${project}"
    :version "0.0.0"
    :author "${author} <${email}>"
    :maintainer "${author} <${email}>"
    :class :package-inferred-system
    :depends-on (#:${project}/src/main
                 #:${project}/src/driver))
