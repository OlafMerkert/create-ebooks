(defsystem create-ebook
  :depends-on (ol-utils
               web-utils
               cl-ppcre)
  :serial t
  :components ((:file "ebook-structure")
               (:file "devworks-info")))
