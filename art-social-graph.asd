(require :asdf)

(in-package :asdf)

(asdf:defsystem :art-social-graph
  :serial t
  :depends-on (:drakma :cl-ppcre :cl-graph :closure-html :cxml-stp :cxml)
  :components
  ((:file "package")
   (:file "utilities")
   (:file "artfacts")))


;; You may need to install some libraries using asdf-install
;;(require 'asdf)
;;(require 'asdf-install)
;;(asdf-install:install 'drakma)
;;(asdf-install:install 'cl-ppcre)
;;(asdf-install:install 'cl-graph)
;;(asdf-install:install 'closure-html)
;;(asdf-install:install 'cxml)
;;(asdf-install:install 'cxml-stp)