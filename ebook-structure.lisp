(defpackage :ebook-structure
  (:nicknames :ebook)
  (:use :cl :ol)
  (:export
   :ebook
   :title
   :creator
   :identifier
   :chapters
   :chapter
   :paragraphs
   :paragraph
   :contents
   :edition
   :copyright
   :define))

(in-package :ebook-structure)

(defclass ebook ()
  ((title      :initarg :title
               :accessor title)
   (creator    :initarg :creator
               :accessor creator)
   (identifier :initarg :identifier
               :initform ""
               :accessor identifier)
   (chapters   :initarg :chapters
               :initform nil
               :accessor chapters)
   (edition    :initarg :edition
               :initform ""
               :accessor edition)
   (copyright  :initarg :copyright
               :initform ""
               :accessor copyright))
  (:documentation "representing a single book with title, creator and
  contents."))

(create-standard-print-object ebook title "by" creator)

(defclass chapter ()
  ((title      :initarg :title
               :initform nil
               :accessor title)
   (paragraphs :initarg :paragraphs
               :initform nil
               :accessor paragraphs))
  (:documentation "representing a single chapter in a book"))

(create-standard-print-object chapter title)

(defclass paragraph ()
  ((contents :initarg :contents
             :initform ""
             :accessor contents))
  (:documentation "representing a single paragraph in a chapter"))

(defmethod print-object ((object paragraph) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (contents)
        object
      (format stream "\"~a\"" contents))))

(defparameter empty-lines-re "\\n(\\s*\\n)+")

(defun split-paragraph (string)
  "Take a string and split it paragraphs at empty lines (only with whitespace)."
  (let ((paragraphs)
        (begin 0))
    (cl-ppcre:do-matches (start
                          end
                          empty-lines-re
                          string)
      (push (make-instance 'paragraph
                           :contents (subseq string begin start))
            paragraphs)
      (setf begin end))
    (nreverse paragraphs)))

(defmacro define
    ((parameter-name
      &rest ebook-properties)
     &rest chapters)
  `(defparameter ,parameter-name
     (make-instance
      'ebook ,@ebook-properties
      :chapters
      (list
       ,@(mapcar
          (lambda (c)
            (destructuring-bind (label title &rest paragraphs) c
              (declare (ignore label))
              `(make-instance 'chapter
                              :title ,title
                              :paragraphs
                              (mapcan #'split-paragraph
                                      (list ,@paragraphs)))))
          chapters)))))
