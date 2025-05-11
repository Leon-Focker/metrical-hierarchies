(asdf:defsystem #:metrical-hierarchies
  :version "1.0"
  :default-component-class "cl-source-file.lsp"
  :description "A library with several functions for determining the metrical weight of a beat."
  :author "Leon Focker"
  :license "GNU General Public License v3.0"
  :depends-on ()
  :serial t
  :components ((:file "package")
	       (:file "slippery-utils")
	       (:file "metrical-hierarchies")))

;; EOF metrical-hierarchies.asd
