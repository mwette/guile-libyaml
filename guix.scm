(define-public guile-libyaml
  (let ((commit "f5d33a6880e96571d3cb079ed7755ffc156cac46")
        (revision "1"))
    (package
     (name "guile-libyaml")
     (version (git-version "0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/mwette/guile-libyaml")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32
	 "12x91983fh1j39zy7kbk19acc1rqdh8515ddx1mh7l26j04k9wgq"))))
     (build-system gnu-build-system)
     (arguments
      `(#:modules (((guix build guile-build-system)
		    #:prefix guile:)
		   ,@%gnu-build-system-modules)
        #:imported-modules ((guix build guile-build-system)
			    ,@%gnu-build-system-modules)
	#:tests? #false ; there are none
	#:phases
	(modify-phases %standard-phases
	  (delete 'configure)
	  (add-after 'unpack 'remove-unused-files
	    (lambda* (#:key inputs #:allow-other-keys)
	      (for-each
	       delete-file
	       '("guix.scm" "demo1.yml" "demo1.scm"
		 "yaml/libyaml.scm"
		 ;; This file is mismatched with the generated FFI code.
		 "yaml/ffi-help-rt.scm"))
	      (copy-file
	       (string-append
		(assoc-ref inputs "nyacc")
		"/share/guile/site/3.0/system/ffi-help-rt.scm")
	       "yaml/ffi-help-rt.scm")
               (substitute* "yaml/ffi-help-rt.scm"
                 (("system ffi-help-rt") "yaml ffi-help-rt"))
               #true))
           (add-before 'build 'build-ffi
             (lambda* (#:key inputs #:allow-other-keys)
               (invoke "guild" "compile-ffi"
                       "--no-exec" ; allow us to patch the generated file
                       "yaml/libyaml.ffi")
               (substitute* "yaml/libyaml.scm"
                 (("system ffi-help-rt") "yaml ffi-help-rt")
                 (("dynamic-link \"libyaml\"")
                  (format #false "dynamic-link \"~a/lib/libyaml\""
                          (assoc-ref inputs "libyaml"))))
               #true))
           (replace 'build
             (assoc-ref guile:%standard-phases 'build))
           (delete 'install))))
      (inputs
       `(("guile" ,guile-3.0)
         ("libyaml" ,libyaml)))
      (propagated-inputs
       `(("guile-bytestructures" ,guile-bytestructures)))
      (native-inputs
       `(("nyacc" ,nyacc)))
      (home-page "https://github.com/mwette/guile-libyaml")
      (synopsis "Guile wrapper for libyaml")
      (description
       "This package provides a simple yaml module for Guile using the
ffi-helper from nyacc.")
      (license license:lgpl3+))))
