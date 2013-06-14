(defpackage :generate-epub
  (:nicknames :epub)
  (:use :cl :ol :cl-who)
  (:export))

(in-package :generate-epub)

(setf cl-who::*prologue* "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      cl-who::*html-empty-tag-aware-p* nil)

(defparameter media-type-table
  '(("opf"   . "application/oebps-package+xml")
    ("epub"  . "application/epub+zip")
    ("ncx"   . "application/x-dtbncx+xml")
    ("xhtml" . "application/xhtml+xml")))

(defun media-type (filename)
  "return correct media-type for the given filename, by extracting the file ending and guessing."
  (values (assoc1 (car (split-sequence:split-sequence #\. filename :from-end t :count 1))
                  media-type-table
                  nil :test #'string=)))

(defun generate-mimetype ()
  (media-type ".epub"))

(defun generate-container (files)
  (with-html-output-to-string (stream nil :prologue t :indent t)
    (:container :xmlns "urn:oasis:names:tc:opendocument:xmlns:container"
                :version "1.0"
                (:rootfiles
                 (dolist (file files)
                   (htm (:rootfile :full-path file
                                   :media-type (media-type file))))))))

(generate-container '("inhalt.opf"))

(defun desc->id (string)
  "generate an id from the given STRING by prepending `id `, then
  replacing spaces with underscores."
  (substitute #\Space #\_ (concatenate 'string "id " string)))


(defun generate-opf (&key book-id language title files-with-id)
  (with-html-output-to-string (stream nil :prologue t :indent t)
    (:package :xmlns\:xsi        "http\://www.w3.org/2001/XMLSchema-instance"
              :xmlns\:dc         "http\://purl.org/dc/elements/1.1/"
              :xmlns\:opf        "http\://www.idpf.org/2007/opf"
              :xmlns             "http://www.idpf.org/2007/opf"
              :version           "2.0"
              :unique-identifier book-id
              (:metadata
               (:dc\:language :xsi\:type "dcterms:RFC3066" (str language))
               (:dc\:title (str title))
               (:dc\:identifier :id book-id (str (desc->id title))))
              (:manifest
               (dolist (f-w-i files-with-id)
                 (htm (:item :id (first f-w-i) :href (second f-w-i) :media-type (media-type (second f-w-i))))))
              (:spine :toc (caar files-with-id)
                      (dolist (f-w-i (rest files-with-id))
                        (htm (:itemref :idref (first f-w-i))))))))


(generate-opf :book-id "BookId" :title "Hello World" :language "de-DE"
              :files-with-id '(("ncx"  "inhalt.ncx")
                               ("Datei_1" "inhalt.xhtml")))

(defun generate-ncx (&key language title files-with-id)
  (with-html-output-to-string (stream nil :prologue t :indent t)
    (princ "<!DOCTYPE ncx 
    PUBLIC \"-//NISO//DTD ncx 2005-1//EN\" \"http:\"//www.daisy.org/z3986/2005/ncx-2005-1.dtd\">" stream)
    (terpri stream)
    (:ncx :xmlns "http://www.daisy.org/z3986/2005/ncx/"
          :version "2005-1"
          :xml\:lang language
          (:head
           (:meta :name "dc:Title" :content title)
           (:meta :name "dtb:uid" :content (desc->id title)))
          (:|docTitle|
            (:text (str title)))
          (:|navMap|
            (let ((counter 0))
              (dolist )))))
  )
