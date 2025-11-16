;;;;
;;;; Copyright (c) 2017 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package :cl-mobmuplat)

(alexandria:define-constant +left+ 0 :test #'equalp)
(alexandria:define-constant +center+ 1 :test #'equalp)
(alexandria:define-constant +right+ 2 :test #'equalp)
(alexandria:define-constant +top+ 0 :test #'equalp)
(alexandria:define-constant +bottom+ 2 :test #'equalp)
(alexandria:define-constant +toggle+ 0 :test #'equalp)
(alexandria:define-constant +momentary+ 1 :test #'equalp)
(alexandria:define-constant +hybrid+ 2 :test #'equalp)
(alexandria:define-constant +slider-index-and-value+ 0 :test #'equalp)
(alexandria:define-constant +all-slider-values+ 1 :test #'equalp)
(alexandria:define-constant +line+ 0 :test #'equalp)
(alexandria:define-constant +fill+ 1 :test #'equalp)
(alexandria:define-constant +select+ 0 :test #'equalp)
(alexandria:define-constant +draw+ 1 :test #'equalp)
(alexandria:define-constant +transparent+ '(0 0 0 0) :test #'equalp)
(alexandria:define-constant +white+ '(1.0 1.0 1.0 1.0) :test #'equalp)
(alexandria:define-constant +black+ '(0.0 0.0 0.0 1.0) :test #'equalp)


#|

factory for widget definitions: usage: 

(make-widget-definition 'make-button
 (decode-json-from-string 
  "{\"highlightColor\":[1.0,0.0,0.0,1.0],\"address\":\"/myButton\",\"color\":[0.0,0.0,1.0,1.0],\"class\":\"MMPButton\",\"frame\":[0.0,0.0,100.0,100.0]}"))

Afterwards manually quote all lists in the resulting expression in &key definitions, 
backquote the body and precede all values in the body with a comma.

|#

(setf *print-case* :downcase)

(defun make-widget-definition (name defs)
  (let* ((keynames (mapcar #'first defs))
         (symnames (mapcar (lambda (keyname) (read-from-string
                                         (format nil "~a" keyname)))
                           keynames)))
    `(defun ,name (&key ,@(mapcar (lambda (symname def) (list
                                                    symname
                                                    (if (consp (cdr def))
                                                        `,(cdr def)
                                                        (cdr def))))
                                  symnames defs))
       ,(mapcar (lambda (keyname symname)
                  (cons keyname symname))
                keynames symnames))))

;;;; begin api

(defun make-mmp
  (&key (pd-file "mmp-test.pd")
     (background-color '(0.5 0.5 0.5 1.0))
     (page-count 1)
     (port 54321)
     (is-page-scroll-short-end nil)
     (start-page-index 0)
     (canvas-type "1600x900")
     (is-orientation-landscape t)
     (prefer-android-font-display-in-editor t)
     (version  2.0)
     (gui 'list))
  "creates the main mmp frame. the :gui contains a flat list with
all gui items drawn on a single canvas. Multiple Pages are referenced
with coordinate offsets of multiples of the page-width. Using
#'encode-json on the result will create the json string/file."
  `((:pd-file . ,pd-file)
    (:background-color . ,background-color)
    (:page-count . ,page-count)
    (:port . ,port)
    (:is-page-scroll-short-end . ,(if is-page-scroll-short-end 'true 'false))
    (:start-page-index . ,start-page-index)
    (:canvas-type . ,canvas-type)
    (:gui . ,gui)
    (:is-orientation-landscape . ,(or is-orientation-landscape 'Null))
    (:prefer-android-font-display-in-editor . ,(if prefer-android-font-display-in-editor 'true 'false))
    (:version . ,version)))

(defun make-slider
    (&key (highlight-color '(1.0 0.0 0.0 1.0))
       (address "\/mySlider")
       (color '(0.0 0.0 1.0 1.0))
       (range 24)
       (class "MMPSlider")
       (is-horizontal t)
       (frame '(0 0 240.0 40.0)))
  `((:highlight-color . ,highlight-color) (:address . ,address) (:color . ,color)
    (:range . ,range) (:class . ,class) (:is-horizontal . ,(if is-horizontal 'true 'false)) (:frame . ,frame)))

(defun make-knob
       (&key (highlight-color '(1.0 0.0 0.0 1.0)) (address "/myKnob")
        (color '(0.0 0.0 1.0 1.0)) (indicator-color '(1.0 1.0 1.0 1.0)) (range 1)
        (class "MMPKnob") (frame '(0.0 0.0 100.0 100.0)))
  `((:highlight-color . ,highlight-color) (:address . ,address) (:color . ,color)
   (:indicator-color . ,indicator-color) (:range . ,range) (:class . ,class)
   (:frame . ,frame)))

(defun make-xyslider
       (&key (highlight-color '(1.0 0.0 0.0 1.0)) (address "/myXYSlider")
        (color '(0.0 0.0 1.0 1.0)) (class "MMPXYSlider")
        (frame '(0.0 0.0 100.0 100.0)))
  `((:highlight-color . ,highlight-color) (:address . ,address) (:color . ,color)
    (:class . ,class) (:frame . ,frame)))

(defun make-label
       (&key (highlight-color '(1.0 0.0 0.0 1.0)) (address "/myLabel")
        (text-size 16) (h-align +left+) (color '(0.0 0.0 1.0 1.0))
        (android-font "Roboto-Regular") (text-font-family "Default")
        (text "Label") (text-font "") (class "MMPLabel")
        (v-align +top+) (frame '(0.0 0.0 100.0 30.0)))
  `((:highlight-color . ,highlight-color) (:address . ,address)
    (:text-size . ,text-size) (:h-align . ,h-align) (:color . ,color)
    (:android-font . ,android-font) (:text-font-family . ,text-font-family)
    (:text . ,text) (:text-font . ,text-font) (:class . ,class)
    (:v-align . ,v-align) (:frame . ,frame)))

(defun make-button
       (&key (highlight-color '(1.0 0.0 0.0 1.0)) (address "/myButton")
        (color '(0.0 0.0 1.0 1.0)) (class "MMPButton")
        (frame '(0.0 0.0 100.0 100.0)))
  `((:highlight-color . ,highlight-color) (:address . ,address) (:color . ,color)
    (:class . ,class) (:frame . ,frame)))

(defun make-toggle
       (&key (highlight-color '(1.0 0.0 0.0 1.0))
          (border-thickness 4)
          (address "\/myToggle")
          (color '(0.0 0.0 1.0 1.0))
          (class "MMPToggle")
          (frame '(0.0 0.0 40.0 40.0)))
  `((:highlight-color . ,highlight-color) (:border-thickness . ,border-thickness)
    (:address . ,address) (:color . ,color) (:class . ,class) (:frame . ,frame)))

(defun make-grid
    (&key (mode 0) (highlight-color '(1.0 0.0 0.0 1.0)) (border-thickness 4)
       (address "\/myGrid") (color '(0.0 0.0 1.0 1.0)) (dim '(4 4))
       (class "MMPGrid") (cell-padding 0) (frame '(0 0 160 160)))
  `((:mode . ,mode) (:highlight-color . ,highlight-color)
    (:border-thickness . ,border-thickness) (:address . ,address) (:color . ,color)
    (:dim . ,dim) (:class . ,class) (:cell-padding . ,cell-padding)
    (:frame . ,frame)))

(defun make-panel
       (&key (highlight-color '(1.0 0.0 0.0 1.0)) (address "\/myPanel")
        (color '(0.0 0.0 1.0 1.0)) (pass-touches nil) (class "MMPPanel")
        (frame '(0.0 0.0 100.0 100.0)))
  `((:highlight-color . ,highlight-color) (:address . ,address) (:color . ,color)
    (:pass-touches . ,(if pass-touches 'true 'false)) (:class . ,class) (:frame . ,frame)))

(defun make-multislider
    (&key (highlight-color '(1.0 0.0 0.0 1.0))
       (address "\/myMultiSlider01")
       (color '(0.0 0.0 1.0 1.0))
       (range 24)
       (output-mode +all-slider-values+)
       (class "MMPMultiSlider")
       (frame '(64.0 1.0 960.0 180.0)))
  `((:highlight-color . ,highlight-color) (:address . ,address) (:color . ,color)
    (:range . ,range) (:output-mode . ,output-mode) (:class . ,class) (:frame . ,frame)))

(defun make-lcd
       (&key (highlight-color '(1.0 0.0 0.0 1.0)) (address "/myLCD")
        (color '(0.0 0.0 1.0 1.0)) (class "MMPLCD")
        (frame '(0.0 0.0 100.0 100.0)))
  `((:highlight-color . ,highlight-color) (:address . ,address) (:color . ,color)
   (:class . ,class) (:frame . ,frame)))

(defun make-multitouch
       (&key (highlight-color '(1.0 0.0 0.0 1.0)) (address "/myMultiTouch")
        (color '(0.0 0.0 1.0 1.0)) (class "MMPMultiTouch")
        (frame '(0.0 0.0 100.0 100.0)))
  `((:highlight-color . ,highlight-color) (:address . ,address) (:color . ,color)
   (:class . ,class) (:frame . ,frame)))

(defun make-menu
       (&key (highlight-color '(1.0 0.0 0.0 1.0)) (address "/myMenu")
        (color '(0.0 0.0 1.0 1.0)) (title "Menu") (class "MMPMenu")
        (frame '(0.0 0.0 200.0 40.0)))
  `((:highlight-color . ,highlight-color) (:address . ,address) (:color . ,color)
   (:title . ,title) (:class . ,class) (:frame . ,frame)))

(defun make-table
       (&key (mode +select+) (highlight-color '(1.0 0.0 0.0 1.0)) (address "/myTable")
        (color '(0.0 0.0 1.0 1.0)) (selection-color '(1.0 1.0 1.0 0.5))
        (display-range-lo -1.0) (display-range-hi 1.0) (class "MMPTable")
        (display-mode +line+) (frame '(0.0 0.0 100.0 100.0)))
  `((:mode . ,mode) (:highlight-color . ,highlight-color) (:address . ,address)
    (:color . ,color) (:selection-color . ,selection-color)
    (:display-range-lo . ,display-range-lo)
    (:display-range-hi . ,display-range-hi) (:class . ,class)
    (:display-mode . ,display-mode) (:frame . ,frame)))

(defun zoom-frame (frame zoom)
  (mapcar (lambda (x) (* zoom x))
          frame))
