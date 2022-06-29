(defpackage #:clestica
  (:use :cl)
  (:import-from :local-time
                :timestamp
                :astronomical-julian-date
                :today
                :now))

(in-package :clestica)

(local-time:reread-timezone-repository)

;; apologies for this travesty
(defgeneric date-of-easter (date)
  (:documentation "Get the date of Easter for a year")
  (:method (date)
      (let* ((year (local-time:timestamp-year date))
             (a (mod year 19))
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
             (day (+ 1 p))
             (month (if (equal n 3)
                        3 4)))
        (local-time:encode-timestamp
         0 0 0 0 day month year))))

(defgeneric leap-year? (date)
  (:documentation "Check if a year is a leap year")
  (:method ((date timestamp))
    (let ((year (local-time:timestamp-year date)))
      (if (and (= 0 (mod year 4))
               (= 0 (mod year 100))
               (= 0 (mod year 400)))
          t
          nil))))

(defgeneric date->daynum (date)
  (:documentation "Convert a day to the day number for the year.")
  (:method (date)
    (let* ((day (local-time:timestamp-day date))
           (month (local-time:timestamp-month date))
           (leap? (leap-year? date)))
      (labels ((fn-8 (x) (truncate (* 30.6 (+ 1 x))))
               (fn-4 (x) (if leap? (* 62 x) (* 63 x)))
               (fn-11 (x) (if leap? (- x 62) (- x 63))))
        (if (> month 2)
            (+ day (fn-11 (fn-8 month)))
            (+ day (truncate (/ (fn-4 (- month 1)) 2))))))))


(defclass julian-date () ((days :initarg :days :accessor days)))

(defvar *gregorian-epoch* (local-time:encode-timestamp
                           0 0 0 0 15 10 1582))

(defmethod print-object ((object julian-date) stream)
  (when *print-escape*
    (write-char #\@ stream))
  (write-string (write-to-string (days object)) stream))

(defgeneric julian-conv (date)
  (:documentation "Convert between Julian and Gregorian dates"))

(defmethod julian-conv ((date timestamp))
  (let* ((year (local-time:timestamp-year date))
         (month (local-time:timestamp-month date))
         (day (+ (local-time:timestamp-day date)
                 (float (/ (local-time:timestamp-hour date) 24))))
         (year' year)
         (month' month)
         (a nil)
         (b nil)
         (c (if (< year' 0)
                (truncate (- (* year' 365.25) 0.75))
                (truncate (* year' 365.25)))))
    (if (or (= month 1) (= month 2))
        (progn
          (setf year' (- year 1))
          (setf month' (+ month 12))))
    (if (local-time:timestamp> date *gregorian-epoch*)
        (progn
          (setf a (truncate (/ y' 100)))
          (setf b (+ (- 2 a) (truncate (/ a 4)))))
        (setf b 0))

  ))

(defmethod julian-conv ((date julian-date))
  (if (> date 2299160)
      (let* ((a (/ (- date 1867216.25) 36524.25))
             (b (+ 1 (- (+ date a) (truncate (/ a 4)))))))
      ))

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
  (:documentation "Convert between angles and decimals"))

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
