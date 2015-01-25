(defsystem :sb-directory
    :description "A suite of functions to walk and get information
    about directories for SBCL."
    :depends-on ("sb-posix")
    :components ((:file "sb-directory")))
