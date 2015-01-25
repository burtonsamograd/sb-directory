(eval-when (:compile-toplevel :load-toplevel)
  (require :cl-cffi-gtk)
  (require :sb-directory))

(defpackage :disk-usage
  (:use :common-lisp :gobject :gtk)
  (:export "MAIN"))
(in-package :disk-usage)

(defun create-list-store (path)
  (let ((data (sb-directory:du path :sort-predicate #'>))
	(list-store (make-instance 'gtk-list-store
				   :column-types
				   '("gint" "gchararray" "glong")))
	(i 1))
    (mapc (lambda (path-size)
	    (let ((path (car path-size))
		  (size (cdr path-size)))
	      (gtk-list-store-set list-store
				  (gtk-list-store-append list-store)
				  i
				  path
				  size))
	    (incf i))
	  data)
    list-store))

(defun main (&optional (path "/tmp"))
  (within-main-loop
    (let* ((window (gtk-window-new :toplevel))
	   (scrolled-window (make-instance 'gtk-scrolled-window))
	   (list-store (create-list-store path))
	   (tree-view (make-instance 'gtk-tree-view :model list-store)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      (setf (gtk-window-title window) "Disk Usage")
      (setf (gtk-window-default-size window) '(800 600))

      (setf (gtk-tree-view-enable-grid-lines tree-view) 3)
      (dolist (col '(("#" . 0) ("Directory" . 1) ("Size" . 2)))
	(let* ((renderer (gtk-cell-renderer-text-new))
	       (column (gtk-tree-view-column-new-with-attributes (car col)
								 renderer
								 "text" (cdr col))))
	  (gtk-tree-view-column-set-resizable column t)
	  (gtk-tree-view-append-column tree-view column)))

      (gtk-container-add scrolled-window tree-view)
      (gtk-container-add window scrolled-window)
      (gtk-widget-show-all window))))
