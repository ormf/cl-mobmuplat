(in-package #:cl-mobmuplat)

;;; create the lisp structure of a gui with one slider and one button:

(make-mmp
 :pd-file "mmp-test.pd"
 :canvas-type "tallTablet"
 :gui (list
       (make-slider :frame '(0 0 240 30))
       (make-button :frame '(0 40 30 30))))

;;; convert to json-string:

(encode-json-to-string
 (make-mmp
  :pd-file "mmp-test.pd"
  :canvas-type "widePhone"
  :gui (list
        (make-slider :frame '(0 0 240 30))
        (make-button :frame '(0 40 30 30)))))

;;; Write a gui with one slider and 8 buttons to file: Note the
;;; boolean predicate for the :is-orientation-landscape

(with-open-file (out "/tmp/mmp-test.mmp" :direction :output :if-exists :supersede)
  (encode-json
   (make-mmp
    :pd-file "mmp-test.pd"
    :canvas-type "tallPhone"
    :is-orientation-landscape nil
    :gui (append
          (list
           (make-slider :frame '(80 0 240 50)))
          (loop for count below 8 collect
               (make-button
                :frame `(80 ,(+ 60 (* 60 count)) 50 50)
                :address (format nil "myButton~2,'0d" count)))))
   out))

;; Check the widgets.lisp file for all possible keywords for the make-mmp
;; and gui widget functions.

