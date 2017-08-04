cl-mobmuplat is a simple api to simplify the creation of json files
for the [MobMuPlat](http://danieliglesia.com/mobmuplat/) platform
using common lisp.

Dependencies:

- [cl-json](https://github.com/hankhero/cl-json) (using github) or via [quicklisp](https://www.quicklisp.org/beta/)

Installation:

If quicklisp is installed, put the folder into
~/quicklisp/local-projects or alternatively put it in a location where
asdf looks for .asd files.

Then use (ql:quickload "cl-mobmuplat") or (require 'cl-mobmuplat)

example:

```cl
(require 'cl-mobmuplat)
(in-package :cl-mobmuplat)

;;; create the lisp structure of a gui with one slider and one button:

(make-mmp
 :pd-file "mmp-test.pd"
 :canvas-type "tallTablet"
 :gui (list
       (make-slider :frame '(80 0 240 50))
       (make-button :frame '(80 60 50 50))))

;;; convert to a json-string:

(encode-json-to-string
 (make-mmp
  :pd-file "mmp-test.pd"
  :canvas-type "widePhone"
  :gui (list
        (make-slider :frame '(80 0 240 50))      
        (make-button :frame '(80 60 50 50)))))

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
```

Check the widgets.lisp file for all possible keywords for the make-mmp
and gui widget functions.

A multipage object is the same as a single page object with a
larger screen size. Use the appropriate x-offsets to draw gui
objects on the relevant pages.

The code is (c) Orm Finnendahl, released under the GPL, version 2 or
later, without any warranties. Use at your own risk.