(defpackage :ebook-structure
  (:use :cl :ol )
  (:export
   :ebook
   :title
   :creator
   :identifier
   :chapters
   :chapter
   :paragraphs
   :paragraph
   :contents))

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
               :accessor chapters))
  (:documentation "representing a single book with title, creator and
  contents."))

(defclass chapter ()
  ((title      :initarg :title
               :initform nil
               :accessor title)
   (paragraphs :initarg :paragraphs
               :initform nil
               :accessor paragraphs))
  (:documentation "representing a single chapter in a book"))

(defclass paragraph ()
  ((contents :initarg :contents
             :initform ""
             :accessor contents))
  (:documentation "representing a single paragraph in a chapter"))

