(in-package :vacaygenius)

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
(defparameter *min-vacation-days* 2)


(defun ->date (date)
  (local-time:parse-timestring date))

(defun weekendp (date)
  (let ((day (local-time:timestamp-day-of-week date)))
    (or (= day 6) (= day 7))))

(defclass scheduling-range ()
  ((start-date
    :reader scheduling-range-start
    :initarg :start-date)
   (end-date
    :reader scheduling-range-end
    :initarg :end-date)))

(defun make-scheduling-range (start-date-string end-date-string)
  (make-instance 'scheduling-range
                 :start-date (->date start-date-string)
                 :end-date (->date end-date-string)))

(defun list-of-days-in (scheduling-range)
  "Returns a list of all days in the given range"
  (with-slots (start-date end-date) scheduling-range
    (let ((date (local-time:timestamp+ start-date 0 :day)))
      (loop :for day = date then (local-time:timestamp+ day 1 :day)
            :for i :from 0
            :while (local-time:timestamp<= day end-date)
            :collect (cons day i)))))

(defun filter-days (scheduling-range predicate)
  "Finds all days in the given range that satisfy the given predicate"
  (loop :for (date . index) :in (list-of-days-in scheduling-range)
        :when (funcall predicate date)
          :collect date))

(defun filter-weekend-days (scheduling-range)
  "Finds all weekends in the given range"
  (filter-days scheduling-range #'weekendp))

(defun filter-holidays (scheduling-range)
  "Finds all holidays in the given range"
  (filter-days scheduling-range #'holidayp))

(defun holidayp (date)
  "Returns true if the given date is a holiday"
  nil)

(defun filter-blocked-days (scheduling-range)
  "Finds all days that are blocked (weekends, explicitly blocked, holidays, etc.) in the given range"
  (append (filter-weekend-days scheduling-range)
          (filter-holidays scheduling-range)))

(defmethod print-object ((range scheduling-range) stream)
  (print-unreadable-object (range stream :type t)
    (with-slots (start-date end-date) range
      (format stream "from ~A to ~A" start-date end-date))))

(defun day-ref (scheduling-range index)
  "Return the date at the given index in the scheduling range."
  (assert (or (zerop index) (plusp index)))
  (with-slots (start-date end-date) scheduling-range
    (let ((date (local-time:timestamp+ start-date index :day)))
      (if (local-time:timestamp< date end-date)
          date
          nil))))

;; the schedule state
(defclass schedule ()
  ((days :initarg :days
         :reader schedule-days)
   (vacation-days
    :initarg :vacation-days)
   (lonely-vacation-days
    :initarg :lonely-vacation-days)
   (min-streak
    :initarg :min-streak)
   (max-streak
    :initarg :max-streak)))

(defun date-range-to-list (start-date end-date)
  "Generate a list of dates from the start-date to the end-date, inclusive."
  (loop for date = start-date then (1+ date)
        while (<= date end-date)
        collect date))

;; (defun generate-vacation-schedules (start-date end-date max-vacation-days)
;;   "Generate all possible vacation schedules within the given date range and max days."
;;   (let* ((date-list (date-range-to-list start-date end-date))
;;          (schedule-length (length date-list))
;;          (vacation-days-left (make-variable "Max Vacation Days")))

;;     ;; Define the initial constraint on the maximum vacation days
;;     (assert! (integerpv max-vacation-days))
;;     (assert! (<=v vacation-days-left max-vacation-days))

;;     ;; This accumulates the choices for each day and number of vacation days taken
;;     (let ((schedule (make-array schedule-length :initial-element 'work-day))
;;           (days-taken 0))
;;       ;; For each day, nondeterministically choose if it's a vacation day or workday
;;       (loop for date in date-list
;;             for i from 0
;;             do (let ((decision (an-integer-betweenv 0 1))) ; 0 for workday, 1 for vacation day
;;                  (assert! (<=v (+v days-taken decision) vacation-days-left)) ; Do not exceed max
;;                  (setf (aref schedule i)
;;                        (if (zerop (value-of decision)) 'work-day 'vacation-day))
;;                  (setf days-taken (+ days-taken (value-of decision))))
;;       ;; Return the schedule
;;       (solution (list schedule days-taken) (static-ordering #'divide-and-conquer-force))))))

;; (defun visualize-schedule (schedule start-date)
;;   "Prints the schedule in a simple format where 'V' is a vacation day and 'W' is a workday."
;;   (format t "Schedule starting from day ~D:~%" start-date)
;;   (loop for day in schedule do (format t "~A " (if (eql day 'work-day) "W" "V")))
;;   (format t "~%"))

;; (defun example-run ()
;;   (let ((schedules (all-values (generate-vacation-schedules 1 14 5)))) ; 14-day range with 5 max days
;;     (dolist (schedule schedules)
;;       (visualize-schedule (first schedule) 1))))

;; ;; https://nikodemus.github.io/screamer/

;; (defun try-it (&optional (max-day 10))
;;   (let ((schedule (loop for i from 1 to max-day collect (an-integer-betweenv 0 1))))
;;     ;; todo compute leasure days correctly
;;     (let ((leasure-days (apply #'+v schedule)))
;;       (assert! (<v leasure-days *max-vacation-days*))
;;       (assert! (>v leasure-days *min-vacation-days*))
;;       (solution
;;        (list sum days)
;;        (static-ordering #'divide-and-conquer-force)))))

;; use with (all-values (try-it))

;; Ideas for constraints:
;; Maximum number of days
;; Days blocked and not available for vacation
;; Minimum consequective days per vacation
;; Maximum consecutive days per vacation
;; Spread of vacations
;; Overlap with specific dates / ranges (e.g. holidays and school breaks)
;;
