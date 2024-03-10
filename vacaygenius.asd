(in-package :asdf-user)

(defsystem :vacaygenius
  :description "Tired of scheduling optimal vacation days? Let VacayGenius do it for you!"
  :long-description #.(uiop:read-file-string (uiop:subpathname *load-pathname* "README.md"))
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :maintainer "David Krentzlin <david.krentzlin@gmail.com>"
  :source-control (:git "https://github.com/certainty/VacayGenius.git")
  :depends-on
  (:alexandria
   :serapeum
   :screamer
   :local-time)
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "poc")))))
