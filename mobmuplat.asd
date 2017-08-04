;;;; mobmuplat.asd
;;;;
;;;; Copyright (c) 2017 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(asdf:defsystem #:cl-mobmuplat
  :description "Simple api for creating MobMuPlat files in json format."
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license "licensed under the GPL v2 or later"
  :serial t
  :depends-on (:cl-json)
  :components ((:file "package")
               (:file "widgets")
               (:file "mobmuplat")))

