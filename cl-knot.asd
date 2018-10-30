;;;; cl-knot.asd
;;;;
;;;; Author: Lauren Capelluto

(asdf:defsystem #:cl-knot
  :description "Help kitty untangle the knot!"
  :author "Lauren Capelluto"
  :depends-on (#:clim)
  :components ((:file "package")
               (:file "cl-knot")))
