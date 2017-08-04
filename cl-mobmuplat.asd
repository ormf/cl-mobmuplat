;;;; mobmuplat.asd
;;;;
;;;; Copyright (c) 2017 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(asdf:defsystem #:cl-mobmuplat
  :description "Describe mobmuplat here"
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license "licensed under the GPL v2 or later"
  :serial t
  :depends-on (:cl-ppcre :cl-json)
  :components ((:file "package")
               (:file "widgets")))

