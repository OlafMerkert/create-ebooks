(defsystem create-ebook
  :depends-on (ol-utils
               web-utils
               cl-ppcre
               zip
               flexi-streams
               uuid)
  :serial t
  :components ((:file "ebook-structure")
               (:file "devworks-info")))
