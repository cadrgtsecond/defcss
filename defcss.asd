(in-package :asdf-user)

(defsystem "defcss"
  :description "Painless CSS with LASS"
  :author "Abhinav Krishna <abhinavkrishnacr2020@gmail.com>"
  :version "0.0.1"
  :depends-on ("lass" "alexandria")
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "main")))))
