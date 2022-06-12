(defpackage #:clestica
  (:use :cl))

(in-package :clestica)

;; apologies for this travesty
(defun date-of-easter (year)
  "Gets the date of Easter for a given year"
  (let* ((a (mod year 19))
         (b (truncate (/ year 100)))
         (c (mod year 100))
         (d (truncate (/ b 4)))
         (e (mod b 4))
         (f (truncate (/ (+ b 8)
                         25)))
         (g (truncate (/ (+ (- b f)
                            1)
                         3)))
         (h (mod (+ (- (- (+ (* 19 a)
                             b)
                          d)
                       g)
                    15)
                 30))
         (i (truncate (/ c 4)))
         (k (mod c 4))
         (l (mod (- (- (+ (+ 32
                             (* 2 e))
                          (* 2 i))
                       h)
                    k)
                 7))
         (m (truncate (/ (+ (+ a
                               (* 11 h))
                            (* 22 l))
                         451)))
         (fn-10 (+ (- (+ h l)
                      (* 7 m))
                   114))
         (n (truncate (/ fn-10 31)))
         (p (mod fn-10 31))
         (day (+ 1 p)))
    (format nil "~A ~A" day
            (if (equal n 3)
                "March"
                "April"))))

(defun leap-year? (year)
  (if (and (= 0 (mod year 4))
           (= 0 (mod year 100))
           (= 0 (mod year 400)))
      t
      nil))

(defun date-to-daynum (day month year)
  (let ((leap? (if (leap-year? year)
                   62
                   63)))
    (if (> day 2)
        (+ day (- (truncate (* (+ 1 month)
                                 30.6))
                    leap?))
        (+ day (truncate (/ (* (- month 1)
                               leap?)
                            2)))
        )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; coordinates

(defclass angle ()
  ((degrees :initarg :degrees :accessor degrees)
   (minutes :initarg :minutes :accessor minutes)
   (seconds :initarg :seconds :accessor seconds)))

(defun make-angle (degrees minutes seconds)
  (make-instance 'angle :degrees degrees :minutes minutes :seconds seconds))

(defgeneric degree-conv (input)
  (:documentation "Convert between degrees and decimals"))

(defmethod degree-conv ((input angle))
  (with-slots (degrees minutes seconds) input
      (float (+ degrees (/ (+ minutes (/ seconds 60)) 60)))))

(defmethod degree-conv ((dec float))
  (let* ((degrees (nth-value 0 (truncate dec)))
         (min (* 60 (nth-value 1 (truncate dec))))
         (sec (* 60 (nth-value 1 (truncate min)))))
    (make-angle degrees min sec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; sun

(defvar *epoch* (local-time:encode-timestamp 0 0 0 0 1 1 2010))
(defvar *ecliptic-longitude* 279.557208)
(defvar *perigee-longitude* 283.112438)
(defvar *sun-eccentricity* 0.016705)

(defun epoch-difference (date)
  (if (stringp date)
      (epoch-difference
       (local-time:parse-timestring date))
      (truncate (/ (local-time:timestamp-difference
                    date *epoch*)
                   86400))))

(defun sun-mean-anomaly (date)
  (let ((d (epoch-difference date)))
    (- (+ (* (/ 360 365.242191) d)
          *ecliptic-longitude*)
       *perigee-longitude*)))

(defun sun-true-anomaly (date)
  (let ((d (epoch-difference date)))
    (+ (* (/ 360 365.242191) d)
       (* (* (/ 360 pi) *sun-eccentricity*)
          (sin (sun-mean-anomaly date))))))
