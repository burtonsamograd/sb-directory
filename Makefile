all: disk-usage

disk-usage:  Makefile
	sbcl --non-interactive									\
	--eval "(declaim (sb-ext:muffle-conditions sb-kernel:undefined-alien-style-warning))"	\
	--eval "(require 'asdf)"								\
	--eval "(require 'disk-usage)"								\
	--eval "(sb-ext:save-lisp-and-die \"disk-usage\"					\
		:executable t									\
		:toplevel (lambda ()								\
				(let* ((argv sb-ext:*posix-argv*)				\
				       (dir (if (= 2 (length argv)) (second argv) \"/tmp\")))	\
				  (bordeaux-threads:join-thread (disk-usage:main dir))))	\
		:save-runtime-options t)"
