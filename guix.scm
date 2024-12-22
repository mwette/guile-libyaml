(define-public guile-libyaml-next
  (let ((commit "TBD")
        (revision "2"))
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
	 "TBD"))))
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
	       '("guix.scm" "demo1.yml" "demo1.scm"))
              (mkdir-p "nyacc/foreign") ;; ???
              (copy-file
               (string-append
                (assoc-ref inputs "nyacc")
                "/share/guile/site/3.0/nyacc/foreign/arch-info.scm")
               "nyacc/foreign/arch-info.scm")
              (copy-file
               (string-append
                (assoc-ref inputs "nyacc")
                "/share/guile/site/3.0/nyacc/foreign/cdata.scm")
               "nyacc/foreign/cdata.scm")
              #true))
           (add-before 'build 'build-ffi
             (lambda* (#:key inputs #:allow-other-keys)
               (invoke "guild" "compile-ffi" "--no-exec" "yaml/libyaml.ffi")
               #true))
           (replace 'build
             (assoc-ref guile:%standard-phases 'build))
           (delete 'install))))
      (inputs
       `(("guile" ,guile-3.0)
         ("libyaml" ,libyaml)))
      (native-inputs
       `(("nyacc" ,nyacc-next)))
      (home-page "https://github.com/mwette/guile-libyaml")
      (synopsis "Guile wrapper for libyaml")
      (description
       "This package provides a simple yaml module for Guile using the
ffi-helper from nyacc.")
      (license license:lgpl3+))))
