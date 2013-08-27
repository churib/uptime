;;;; uptime.lisp

;;; parses 'syslog' for init and shutdown times of computer

(in-package #:uptime)

;; (defvar *logfile* #p"/var/log/syslog")
(defvar *logfile* #p"/home/timo/devel/lisp/uptime/syslog")

(defun read-timestamps ()
  (let ((timestamps (mapcar (lambda (line)
                              (parse-date (extract-date-part-from-line line)))
                            (slurp-file *logfile*))))
    timestamps))

(defun parse-date (string)
  (let ((month (position (subseq string 0 3)
                         local-time:+short-month-names+
                         :test #'equal))
        (day   (parse-integer (subseq string  4  6)))
        (hour  (parse-integer (subseq string  7  9)))
        (min   (parse-integer (subseq string 10 12)))
        (sec   (parse-integer (subseq string 13 15)))
        (year  (local-time:with-decoded-timestamp (:year year) (local-time:today) year)))
    (local-time:encode-timestamp 0 sec min hour day month year)))

(defun extract-date-part-from-line (line)
  (subseq line 0 15))

(defun slurp-file (filename)
  "Returns list of lines of file with FILENAME."
  (let ((lines))
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil)
         while line do (push line lines))
      (nreverse lines))))

(defun days-equal? (timestamp1 timestamp2)
  "Returns true iff timestamps equals at least by days."
  (local-time:with-decoded-timestamp (:year year1 :month month1 :day day1) timestamp1
    (local-time:with-decoded-timestamp (:year year2 :month month2 :day day2) timestamp2
      (and (= year1  year2)
           (= month1 month2)
           (= day1   day2)))))

(defun days-not-equal? (timestamp1 timestamp2)
  "Returns true iff timestamps differ by at least days."
  (not (days-equal? timestamp1 timestamp2)))

(defun days (timestamps)
  "TODO add doc"
  (labels ((days-rec (timestamps start current next days)
;;             (break)
             (cond ((null timestamps)               (nreverse days)) ;;; we are done
                   ((null start)                    (days-rec (cdr timestamps) (car timestamps) current next days))
                   ((null current)                  (days-rec (cdr timestamps) start (car timestamps) next days))
                   ((null next)                     (days-rec (cdr timestamps) start current (car timestamps) days))
                   ((days-not-equal? start current) (days-rec (cdr timestamps) current nil nil (cons (list start) days)))
                   ((days-not-equal? start next)    (days-rec (cdr timestamps) next nil nil (cons (list start current) days)))
                   (t                               (days-rec (cdr timestamps) start next (car timestamps) days))
             )))
    (days-rec timestamps nil nil nil nil)))
