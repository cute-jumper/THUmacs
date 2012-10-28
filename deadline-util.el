;;; deadline-util.el --- A series of tools of Emacs Lisp making use of `deadline-parser.el'

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

;; Custom the variable dp-homework-file to your own file.
;; Call `dp-update-deadline-and-notify' to enjoy!!

;;; Code:

(require 'deadline-parser)

;; Variable for customization
(defvar dp-homework-file ""
  "Where these deadlines should be stored on disk.")

;; ------------------------------------------------------------
;; Util functions. First two are copied from 
;; https://github.com/dengste/org-caldav
;; ------------------------------------------------------------

(defun dp-buffer-narrowed-p ()
  "Return non-nil if current buffer is narrowed.
Copy from https://github.com/dengste/org-caldav/blob/master/org-caldav.el.
Actually, this function may have a bug, or not, when a buffer's narrowed
region is as large as whole."
  (> (buffer-size) (- (point-max)
                      (point-min))))

(defun dp-narrow-to-get-next-heading (mark)
  "Narrow next event in the current buffer.
If buffer is currently not narrowed, narrow to the first one.
Returns nil if there are no more events.
This is a modified version of org-caldav-narrow-next-event from 
https://github.com/dengste/org-caldav/blob/master/org-caldav.el"  
  ;; (interactive "sInput a mark:")
  (if (not (dp-buffer-narrowed-p))
      (goto-char (point-min))
    (goto-char (point-max))
    (widen))
  (if (null (re-search-forward mark nil t))
      (progn
        ;; No more section
        (widen)	
        nil)
    (narrow-to-region (line-beginning-position) ;I modify this form
                      (save-excursion
                        (cond 
                         ((re-search-forward mark nil t)
                          (forward-line -1)
                          (end-of-line))
                         (t (goto-char (point-max))))
                        (point)))
    t))

(defun dp-parse-deadline (lst)
  "take apart the string list generate by `split-string'"
  (mapcar 'dp-string-trim
          (list             
           (replace-regexp-in-string "^\\*\\* " "" (car lst))
           (replace-regexp-in-string "^[ ]*deadline:[ ]*<\\|>" "" (nth 1 lst)))))

(defun dp-buffer-string-no-properties ()
  (buffer-substring-no-properties (point-min) (point-max)))

;; ------------------------------------------------------------
;; Functions supporting notifications of deadlines' update
;; ------------------------------------------------------------
(defun dp-diff-two-alist (prev cur depth)  
  "Return an associative list consisting of the (cur-prev)"
  (let (item
        lst)
    (loop for i in cur do
          (setq item (assoc (car i) prev))
          (if item
              (when (= depth 0) ;when in top level
                (let ((child (dp-diff-two-alist
                              (cdr item) (cdr i) 1)))
                  (when child
                    (setq lst (cons (cons (car i) child) lst)))))
            (setq lst (cons i lst))))
    (reverse lst)))

(defun dp-send-notify (diff)
  "Send a system notification according to the difference
Input is the return value of `dp-diff-two-alist'"
  (eval-when-compile (require 'notifications))
  (let ((body "")
        (count 0))
    (loop for item in diff do
          (when (cdr item)
            (setq count (+ count (length (cdr item))))
            (setq body (concat body 
                               (car item)
                               ":<br/>"
                               (mapconcat 
                                (lambda (x)
                                  (mapconcat 'identity
                                             x "\t"))
                                (cdr item) "<br/>")
                               "<br/>"))))
    (if (= count 0)
        (notifications-notify :title "没有新的未交作业！")
      (notifications-notify 
       :title (format "新的未交作业(%d)" count)
       :body body))))


;; ------------------------------------------------------------
;; Interactive functions
;; ------------------------------------------------------------

(defun dp-write-deadlines-to-file (file &optional dl)
  "Write the deadlines to a file"
  (interactive "FFile to write to: ")
  (let ((deadlines (if dl
                       dl
                     (dp-process-all-deadlines))))
    (with-temp-file file
      (loop for x in deadlines do
            (insert "* " (car x) "\n")
            (loop for y in (cdr x) do
                  (insert "** " (car y) "\n")
                  (insert "   DEADLINE:<" (cadr y) ">\n"))))
    (message "Write to %s" file)))

(defun dp-get-all-sub-headings (str)  
  "Return an associative list consisting of deadlines of certain course
Called by `dp-read-deadlines-from-file'"
  (with-temp-buffer
    (insert str)
    (let ((course-name (save-excursion
                         (goto-char (point-min))
                         (buffer-substring-no-properties
                          (search-forward "* ")
                          (prog2 (end-of-line) (point))))) ;get top level heading
          current-deadlines)
      (while (dp-narrow-to-get-next-heading "^\\*\\* ")
        (setq
         current-deadlines
         (cons
          (dp-parse-deadline (split-string (dp-buffer-string-no-properties) "\n"))
          current-deadlines)))
      (cons course-name (reverse current-deadlines)))))

(defun dp-read-deadlines-from-file (file)
  "Get deadlines from the contents previous stored in the file"
  (interactive "FRead from file: ")  
  (let (prev-deadeline-alist)
    (with-current-buffer (find-file-noselect file t)
      (while (dp-narrow-to-get-next-heading "^\\* ")
        (save-excursion
          (unless
              (re-search-forward "^\\*" nil t)
            (goto-char (point-max)))
          (setq 
           prev-deadeline-alist
           (cons (dp-get-all-sub-headings 
                  (dp-buffer-string-no-properties))
                 prev-deadeline-alist)))))
    (reverse prev-deadeline-alist)))

(defun dp-update-deadline-and-notify ()
  (interactive)
  (let ((current (dp-process-all-deadlines))
        (previous (dp-read-deadlines-from-file dp-homework-file))
        diff)
    (setq diff (dp-diff-two-alist previous current 0))
    (if (null diff)
        (message "No update~~~")
      (dp-write-deadlines-to-file dp-homework-file current)
      (message "WTF! New homework comes out!!!"))
    (dp-send-notify diff)))

(provide 'deadline-util)
;;; deadline-util.el ends here
