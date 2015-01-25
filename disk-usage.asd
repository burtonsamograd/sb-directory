(defsystem :disk-usage
    :description "A simple GTK application for showing the size of a
    directory tree."
    :depends-on ("sb-directory")
    :components ((:file "disk-usage")))
