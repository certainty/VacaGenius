(in-package :cl-user)

(defpackage :vacaygenius
  (:use :cl)
  (:import-from :serapeum :->)
  (:local-nicknames (:a :alexandria)
                    (:s :serapeum)
                    (:cass :org.shirakumo.classowary)))
