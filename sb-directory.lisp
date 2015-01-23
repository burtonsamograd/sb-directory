;; sb-directory
;;
;; A suite of directory walking and sizing functions using the
;; sb-posix routines of SBCL:
;;
;;     directory-p - is this string a dir
;;     mapdir - call fn on all entries in dir
;;     dirsize - amount of bytes in dir
;;     dirusage - amount of bytes in and under dir
;;     dirwalk - call fn on all directories under dir
;;     du - return bytes in all directories under dir
;;
;; All paths are strings.
;;
;; Burton Samograd
;; Licence: BSD
;; 2015

(defpackage sb-directory
  (:use cl)
  (:export "MAPDIR" "DIRSIZE" "DIRUSAGE" "DIRWALK" "DU"))
(in-package sb-directory)

(defun directory-p (path)
  "If path is a directory, return T"
  (sb-posix:s-isdir
   (sb-posix:stat-mode
    (sb-posix:lstat path))))
  
(defun last-char (s)
  "Return the last character of the string s"
  (char s (1- (length s))))

(defun mapinto! (fn list)
  "Like mapcar, but reuses the list structure of list to hold the
results of calling fn on each element of the list."
  (when list
    (setf (car list) (funcall fn (car list)))
    (mapinto! fn (cdr list)))
  list)

(defun mapdir (path &optional (fn #'identity) (filter-dot-and-dotdot t))
  "Call fn on all files and directories entries in path, returning a
list of the results. 

It would be nice to be able to do this functionally without building
the list of entries first, but a recursive call into mapdir can result
in too many open files, so this is the next best option by building
the list of paths first and then using mapinto! to at least save some
re-consing."
  (let ((terminator (if (char= (last-char path) #\/) "" "/"))
	paths)
    (unwind-protect
	 (let ((open-dir (sb-posix:opendir path)))
	   (do ((dirent (sb-posix:readdir open-dir) 
			(sb-posix:readdir open-dir)))
	       ((sb-alien:null-alien dirent))
	     (let ((name (sb-posix:dirent-name dirent)))
	       (unless (and filter-dot-and-dotdot
			    (or (string= name "..") (string= name ".")))
		 (push (concatenate 'string path terminator name)
		       paths))))
	   (sb-posix:closedir open-dir)))
    (mapinto! fn paths)))

(defun dirsize (path)
  "Return the number of bytes contained in path."
  (reduce #'+ (mapdir path
	       (lambda (d)
		     (sb-posix:stat-size
		      (sb-posix:lstat d)))) :initial-value 0))
  

(defun dirusage (path)
  "Return the sum bytes contained in and all directories under path."
  (reduce #'+ 
	  (mapcar (lambda (name-buf)
		    (let ((name (car name-buf))
			  (buf (cdr name-buf)))
		      (if (sb-posix:s-isdir (sb-posix:stat-mode buf))
			  (dirusage name)
			  (sb-posix:stat-size buf))))
		  (mapdir path (lambda (d) (cons d (sb-posix:lstat d)))))
	  :initial-value 0))

(defun dirwalk (path &optional (fn #'identity)) 
  "Call fn on all directories under path. Returns nil."
  (dolist (name-buf (mapdir path (lambda (d) (cons d (sb-posix:lstat d)))))
    (let ((name (car name-buf))
	  (buf (cdr name-buf)))
      (when (sb-posix:s-isdir (sb-posix:stat-mode buf))
	(funcall fn name)
	(dirwalk name fn)))))

(defun du (path &key (sort t) (sort-predicate #'<))
  "Return a list ((name . size-in-bytes) ..) of all directories under path"
  (let (results)
    (dirwalk path (lambda (d) (push (cons d (dirsize d)) results)))
    (if sort
      (sort results sort-predicate :key #'cdr)
      results)))
