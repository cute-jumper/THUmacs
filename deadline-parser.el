;;; deadline-parser.el --- A bruteforce parser for http://learn.tsinghua.edu.cn

;; Copyright (C) 2012  qjp

;; Author: qjp <qjp@qjp-ideapad>
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Custom userid an userpass to your own.
;; Functions in this file should be used by your functions.

;;; Code:

(eval-when-compile (require 'cl))

;; Variables used to customization
(defvar dp-userid "userid" "user name to login")
(defvar dp-userpass "userpass" "user password")

;; Constants
(defconst dp-baseurl "http://learn.tsinghua.edu.cn" "Base url")

;; ------------------------------------------------------------
;; Functions used for web communication, including asyn and 
;; sync versions, but only sync version used actually.
;; ------------------------------------------------------------
(defun dp-url-http-post-asyn (url args callback)
  "Send ARGS to URL as a POST request."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (mapconcat (lambda (arg)
                      (concat (url-hexify-string (car arg))
                              "="
                              (url-hexify-string (cdr arg))))
                    args
                    "&")))
    (url-retrieve url callback)))

(defun dp-url-http-post-sync (url args)
  "Send ARGS to URL as a POST request."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (mapconcat (lambda (arg)
                      (concat (url-hexify-string (car arg))
                              "="
                              (url-hexify-string (cdr arg))))
                    args
                    "&")))
    (url-retrieve-synchronously url)))

;; ------------------------------------------------------------
;; Util functions
;; ------------------------------------------------------------
(defun dp-string-trim (str)
  "trim leading and tailing whitespace from STR."
  (while (string-match "\\`^\n+\\|^\\s-+\\|\\s-+$\\|\n+\\|\r+\\|^\r\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun dp-delete-response-header ()
  "Delete response's header in order to feed into libxml parser"
  (let (header-end)
    (goto-char (point-min))
    (delete-region (point-min)
                   (1- (search-forward "<")))))

(defun dp-todo-deadline-filter (deadlines)  
  "Filter used to get only todo-homework's deadline items
TODO: maybe I can implement this as an universal
filter function which it isn't now"
  (let (done todo)
    (mapc (lambda (x)
              (if (string= "已经提交" (car x))
                  (setq done (cons (cdr x) done))
                (setq todo (cons (cdr x) todo)))
              nil)
            deadlines)
    (reverse todo)))

;; ------------------------------------------------------------
;; Parsers to retrieve the information needed from the libxml
;; parser's result.  I admit that these two parsers are very 
;; ugly for using too much nth, car, cdr, and tooooooooooooo
;; many magic numbers. An elegant way to do so is to construct
;; a universal parser for the HTML tree.
;; Attention: These functions should *only* work when current
;; buffer is the buffer returned by `url-retrieve'
;; ------------------------------------------------------------

(defun dp-course-parser ()
  "In-buffer parser for courses' information."
  (let ((local-strip-course-name (lambda (x)
                                (let ((index (string-match "(.*)" x)))
                                  (substring x 0 index))))
        tree
        lst)    
    (dp-delete-response-header)
    (setq tree (libxml-parse-html-region (point-min) (point-max)))
    (mapcar
     (lambda (x)
       (setq lst (cdr (car (last (nth 3 x) 1)))) ;get useful part
       (cons (cdr (car (car lst)))      ;url
             (funcall local-strip-course-name
              (dp-string-trim (car (cdr lst)))))) ;name
     (remove-if-not                          ;get rid of junk
      (lambda (x) (and (listp x) (eq (car x) 'tr) (not (eq (nth 1 x) nil))))
      (nth 5 (nth 3 tree))))))

(defun dp-deadline-parser ()
  "In-buffer parser for deadlines' information.
Steps are essentially indentical to the above parser"
  (dp-delete-response-header)
  (let ((tree (libxml-parse-html-region (point-min) (point-max))))
    (mapcar (lambda (x)
              (let (status names dates)
                (setq status (dp-string-trim (nth 2 (nth 8 x))))
                (setq names (nth 2 (nth 2 (nth 2 x))))
                (setq dates (nth 2 (nth 6 x)))
                (list status names dates)))
            (remove-if-not
             (lambda (x)
               (and (listp x)
                    (eq (car x) 'tr)
                    (not (eq (nth 1 x) nil))))
             (nth 4 (nth 2 (nth 4 (nth 3 (nth 3 tree)))))))))

;; ------------------------------------------------------------
;; Wrapper functions to return parser's results
;; ------------------------------------------------------------

(defun dp-get-course-homework-alist ()
  "Return homework's url and name associative list"
  (with-current-buffer (url-retrieve-synchronously
                        "http://learn.tsinghua.edu.cn/MultiLanguage/lesson/student/MyCourse.jsp?language=cn")
    (mapcar
     (lambda (x)
       (cons
        (replace-regexp-in-string  "course_locate" "hom_wk_brw"
                                   (concat dp-baseurl (car x)))
        (cdr x)))
     (dp-course-parser))))

(defun dp-get-deadline-alist (homework-alist filter-func)
  "Return the deadline associative list filtered by `filter-func'"
  (mapcar
   (lambda (x)
     (with-current-buffer (url-retrieve-synchronously (car x))
       (cons 
        (cdr x) 
        (funcall filter-func (ignore-errors (dp-deadline-parser))))))
   homework-alist))

;; ------------------------------------------------------------
;; Top level implementation
;; ------------------------------------------------------------

(defun dp-do-learn-login ()
  "Do web login using `dp-url-http-post-sync'"
  (dp-url-http-post-sync
   "https://learn.tsinghua.edu.cn/MultiLanguage/lesson/teacher/loginteacher.jsp"
   (list
    `("userid" . ,dp-userid) `("userpass" . ,dp-userpass) `("submit1" .,(encode-coding-string "登录" 'gbk)))))

(defun dp-retrieve-deadlines ()    
  "Return todo-homework deadlines' assosiative list"
  (dp-get-deadline-alist 
   (dp-get-course-homework-alist)
   'dp-todo-deadline-filter))

(defun dp-process-all-deadlines ()
  "Do all things and return an asso"
  (interactive)
  (dp-do-learn-login)
  (dp-retrieve-deadlines))

(provide 'deadline-parser)
;;; deadline-parser.el ends here
