;;;; uptime.lisp

;;; parses 'syslog' for init and shutdown times of computer

(in-package #:uptime)

;; (defvar *logfile* #p"/var/log/syslog")
(defvar *logfile* #p"syslog")

(defun run ()
  (let ((dates (mapcar (lambda (line)
                         (parse-date (extract-date-string line)))
                       (slurp-file *logfile*))))
    dates))

(defun day-change? (date1 date2)
  "Returns true iff dates differ by at least one day."
  (or (/= (year-of  date1) (year-of  date2))
      (/= (month-of date1) (month-of date2))
      (/= (day-of   date1) (day-of   date2))))
            
(defun parse-date (string)
  (chronicity:parse string :context :past))

(defun extract-date-string (line)
  (subseq line 0 15))

(defun slurp-file (filename)
  "Returns lines of file with FILENAME."
  (let ((lines))
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil)
         while line do (push line lines))
      (nreverse lines))))
