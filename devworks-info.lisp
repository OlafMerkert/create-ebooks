(defpackage :epub-devworks
  (:use :cl :ol
        :ebook-structure
        :cl-who :web-utils)
  (:export))

(in-package :epub-devworks)

(defparameter archive-tree
  '(:root
    "mimetype"
    ("META-INF/"
     "container.xml")
    ("OEBPS/"
     "content.opf"
     "title.html"
     "content.html"
     "stylesheet.css"
     "toc.ncx"
     ("images/"
      "cover.png"))))

(defgeneric generate-file% (archive-node ebook &key)
  (:documentation "return a string with the content of the given node
  of the archive-tree, for the specific ebook."))

(defun generate-file (node-string &rest args)
  (apply #'generate-file%
         (aif (symbolp node-string)
              it
              (symb (string-upcase node-string)))
         args))

(defmacro define-generate ((archive-node ebook &rest args) &body body)
  `(defmethod generate-file%
       ((,(first archive-node) (eql ',(symb
                                       (string-upcase (second archive-node)))))
        ,ebook
        ,@(or args '(&key)))
     ,@body))

;; todo generation of directories
(defun node-directory-p (node-string)
  "check whether the given string  describes a directory in the archive
tree."
  (char= #\/ (alast node-string)))

(define-generate ((archive-node "mimetype") (ebook ebook))
  "application/epub+zip")

(define-generate ((archive-node "container.xml") (ebook ebook))
  (format nil
          "<?xml version=\"1.0\"?>
<container version=\"1.0\" xmlns=\"urn:oasis:names:tc:opendocument:xmlns:container\">
  <rootfiles>
    <rootfile full-path=\"~A\"
     media-type=\"application/oebps-package+xml\" />
  </rootfiles>
</container>"
          "OEBPS/content.opf"))

(define-generate ((archive-node "content.opf") (ebook ebook))
  (with-slots (title creator identifier language) ebook
    (format nil
            "<?xml version='1.0' encoding='utf-8'?>
<package xmlns=\"http://www.idpf.org/2007/opf\"
            xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
            unique-identifier=\"bookid\" version=\"2.0\">
  <metadata>
    <dc:title>~A</dc:title>
    <dc:creator>~A</dc:creator>
    <dc:identifier id=\"bookid\">~A</dc:identifier>
    <dc:language>~A</dc:language>
    <meta name=\"cover\" content=\"cover-image\" />
  </metadata>
  <manifest>
    <item id=\"ncx\" href=\"~A\" media-type=\"application/x-dtbncx+xml\"/>
    <item id=\"cover\" href=\"~A\" media-type=\"application/xhtml+xml\"/>
    <item id=\"content\" href=\"~A\"
media-type=\"application/xhtml+xml\"/>
    <item id=\"cover-image\" href=\"~A\" media-type=\"image/png\"/>
    <item id=\"css\" href=\"~A\" media-type=\"text/css\"/>
  </manifest>
  <spine toc=\"ncx\">
    <itemref idref=\"cover\" linear=\"no\"/>
    <itemref idref=\"content\"/>
  </spine>
  <guide>
    <reference href=\"~A\" type=\"cover\" title=\"Cover\"/>
  </guide>
</package>"
            title creator identifier language
            "toc.ncx" "title.html" "content.html" "images/cover.png" "stylesheet.css" "title.html")))

(define-generate ((archive-node "toc.ncx") (ebook ebook))
  (with-slots (title identifier) ebook
    ;; TODO navpoints
    (format nil
            "<?xml version='1.0' encoding='utf-8'?>
<!DOCTYPE ncx PUBLIC \"-//NISO//DTD ncx 2005-1//EN\"
                 \"http://www.daisy.org/z3986/2005/ncx-2005-1.dtd\">
<ncx xmlns=\"http://www.daisy.org/z3986/2005/ncx/\" version=\"2005-1\">
  <head>
    <meta name=\"dtb:uid\"
content=\"~A\"/>
    <meta name=\"dtb:depth\" content=\"1\"/>
    <meta name=\"dtb:totalPageCount\" content=\"0\"/>
    <meta name=\"dtb:maxPageNumber\" content=\"0\"/>
  </head>
  <docTitle>
    <text>~A</text>
  </docTitle>
  <navMap>
    <navPoint id=\"navpoint-1\" playOrder=\"1\">
      <navLabel>
        <text>~A</text>
      </navLabel>
      <content src=\"~A\"/>
    </navPoint>
~{~A~}
  </navMap>
</ncx>"
            identifier title
            "Book cover" "title.html"
            (mapcar (lambda (x)
                      (destructuring-bind (label . source) x
                        (format nil
                                "    <navPoint id=\"navpoint-2\" playOrder=\"2\">
      <navLabel>
        <text>~A</text>
      </navLabel>
      <content src=\"~A~A\"/>
    </navPoint>
"
                                label "content.html" source)))
                    (chapter-refs ebook)))))

(defun chapter-refs (ebook)
  "return a list of navpoint targets for the various chapters."
  (let ((chapter-nr 0))
    (mapcar (lambda (c)
              (cons (format nil "chapter-~A" (incf chapter-nr))
                    (title c)))
            (chapters ebook))))


(define-generate ((archive-node "title.html") (ebook ebook))
  (with-slots (title) ebook
    (html-doc (:title title :style "stylesheet.css")
      (:h1 (str title))
      (:div
       (:img :src "images/cover.png" :alt "Title page")))))

(define-generate ((archive-node "stylesheet.css") (ebook ebook))
  "/* no style atm */")

(define-generate ((archive-node "cover.png") (ebook ebook))
    (cover ebook))

(define-generate ((archive-node "content.html") (ebook ebook))
  (with-slots (title) ebook
    (let ((chapter-nr 0))
     (html-doc (:title title :style "stylesheet.css")
       (dolist (chapter (chapters ebook))
         (htm
          (:a :id (fmt "chapter-~A"(incf chapter-nr)))
          (:h1 (fmt "~A ~A" chapter-nr (title chapter)))
          (dolist (p (paragraphs chapter))
            (htm (:p (str (contents p)))))))))))
