(in-package :vacaygenius)

(defun ->date (date)
  (local-time:parse-timestring date))

(defparameter *start-date* (->date "2024-01-01"))
(defparameter *end-date*   (->date "2024-06-01"))

(defparameter *holidays* (list (->date "2024-01-01")
                               (->date "2024-01-06")
                               (->date "2024-04-19")
                               (->date "2024-04-21")
                               (->date "2024-05-01")
                               (->date "2024-05-29")))

(defparameter *work-days-per-week* 5)
(defparameter *max-vacation-days* 8)
(defparameter *min-vacation-days* 4)

(defun weekendp (date)
  (let ((day (local-time:timestamp-day-of-week date)))
    (or (= day 6) (= day 7))))

(defun holidayp (date)
  "Returns true if the given date is a holiday"
  (member date *holidays* :test #'local-time:timestamp=))

(defun blockedp (date)
  "Returns true if the given date is a weekend or a holiday"
  (or (weekendp date) (holidayp date)))

(defclass scheduling-range ()
  ((start-date
    :reader scheduling-range-start
    :initarg :start-date)
   (end-date
    :reader scheduling-range-end
    :initarg :end-date)
   (days
    :reader scheduling-range-days
    :initarg :days)))

(defun make-scheduling-range (start-date-string end-date-string)
  (let* ((start-date (->date start-date-string))
         (end-date (->date end-date-string))
         (date (local-time:timestamp+ start-date 0 :day))
         (days (loop
                 :for day = date then (local-time:timestamp+ day 1 :day)
                 :while (local-time:timestamp<= day end-date)
                 :collect day :into days
                 :finally (return (coerce days 'vector)))))
    (make-instance 'scheduling-range
                   :start-date start-date
                   :end-date end-date
                   :days days)))

(defun count-days-in (scheduling-range)
  "Returns the number of days in the given range"
  (length (scheduling-range-days scheduling-range)))

(defmethod print-object ((range scheduling-range) stream)
  (print-unreadable-object (range stream :type t)
    (with-slots (start-date end-date) range
      (format stream "from ~A to ~A" start-date end-date))))

(s:defconst +day-free+ 0)
(s:defconst +day-vacation+ 1)
(s:defconst +day-blocked+ 2)

(screamer::defun fact! (bool)
  (unless bool
    (csp:fail)))


(defun blocked-day-indices (schedule-range)
  "Returns the indices of the blocked days in the given range."
  (loop :for day :across (scheduling-range-days schedule-range)
        :for i :from 0
        :when (blockedp day)
          :collect i))

(screamer::defun compute-schedules* (schedule-range)
  "Compute all possible schedules for the given range."
  (let* ((day-vars (loop :for day :across (scheduling-range-days schedule-range)
                         :for var = (csp:an-integer-betweenv +day-free+ +day-blocked+)
                         :collect var :into vars
                         :finally (return (coerce vars 'vector)))))

    ;; constrain the blocked days
    (loop :for idx :in (blocked-day-indices schedule-range)
          :do (csp:assert! (csp:=v  (aref day-vars idx) +day-blocked+)))

    ;; TODO: refactor this niceley
    (let* ((day-vars-ls (coerce day-vars 'list))
           (blocked-days  (csp:applyv #'csp:count-truesv (loop :for day :in day-vars-ls :collect (csp:=v day +day-blocked+))))
           (vacation-days (csp:applyv #'csp:count-truesv (loop :for day :in day-vars-ls :collect (csp:=v day +day-vacation+))))
           (lonely-vacation-days
             (csp:applyv #'csp:count-truesv
                         (loop :for i :from 1 :below (1- (length day-vars))
                               :for day = (aref day-vars i)
                               :for next-day = (aref day-vars (1+ i))
                               :for prev-day = (aref day-vars (1- i))
                               :collect (csp:andv (csp:=v day +day-vacation+)
                                                  (csp:andv (csp:=v next-day +day-free+)
                                                            (csp:=v prev-day +day-free+))))))
           (leisure-days (csp:+v blocked-days vacation-days)))

      ;; constrain days patterns
      (csp:assert! (csp:>=v vacation-days *min-vacation-days*))
      (csp:assert! (csp:<=v vacation-days *max-vacation-days*))

      (csp:assert! (csp:<=v lonely-vacation-days 4))

      (csp:solution
       (list
        :days day-vars-ls
        :leisure-days leisure-days
        :blocked-days blocked-days
        :vacation-days vacation-days
        :lonely-vacation-days lonely-vacation-days)
       (csp:static-ordering #'csp:linear-force)))))

(defun compute-schedules (schedule-range)
  "Compute all possible schedules for the given range."
  (let ((all-schedules  (csp:all-values (compute-schedules* schedule-range))))
    ;; sort by leisure days
    (sort all-schedules #'> :key (lambda (schedule) (csp:value-of (getf schedule :leisure-days))))))

;; Ideas for constraints:
;; Maximum number of days
;; Days blocked and not available for vacation
;; Minimum consequective days per vacation
;; Maximum consecutive days per vacation
;; Spread of vacations
;; Overlap with specific dates / ranges (e.g. holidays and school breaks)
;;
