(asdf:defsystem :opty
  :name "Opty"
  :author "Ties Stuij <ties@stuij.se>"
  :description "Opty, the cuddly toy optimizing compiler."
  :license "MIT"
  :serial t
  :components
  ((:file "package")
   (:file "src/common")
   (:file "src/graphs")
   (:file "src/domination")
   (:file "src/flow-graph")
   (:file "src/testing"))
  :depends-on (:parachute :alexandria))

