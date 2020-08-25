(import (gnu packages web)
	(gnu packages guile)
	(guix packages)
	(guix gexp)
	(guix build-system trivial)
	(prefix (guix licenses) license:))

(define-public guile-yaml
  (package
    (name "guile-yaml")
    (version "0.1")
   (source (local-file "."))
   (build-system trivial-build-system)
   (propagated-inputs `(
                        ("libyaml" ,libyaml)
                        ("guile" ,guile-3.0)
			("guile-bytestructures" ,guile-bytestructures)
                        ))
   (home-page "https://github.com/mwette/guile-libyaml")
   (synopsis "Guile wrapper for libyaml")
   (description "simple yaml module using ffi-helper from www.nongnu.org/nyacc")

   (license license:lgpl3+)))

guile-yaml

