;;; -*- lexical-binding: t -*-
;;; ohai-utils.el --- Utility functions for use elsewhere.

;; Copyright (C) 2015 Shafayet Khan

;; Author: Shafayet Khan

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

;;; Code:

;; Utility function that runs code after feature has been loaded
(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(defun xah-make-backup ()
  "Make a backup copy of current file or dired marked files.
If in dired, backup current file or marked files.
The backup file name is
 ‹name›~‹timestamp›~
example:
 file.html~20150721T014457~
in the same dir. If such a file already exist, it's overwritten.
If the current buffer is not associated with a file, nothing's done.
URL `http://ergoemacs.org/emacs/elisp_make-backup.html'
Version 2015-10-14"
  (interactive)
  (let ((ξfname (buffer-file-name)))
    (if ξfname
        (let ((ξbackup-name
               (concat ξfname "~" (format-time-string "%Y%m%dT%H%M%S") "~")))
          (copy-file ξfname ξbackup-name t)
          (message (concat "Backup saved at: " ξbackup-name)))
      (if (string-equal major-mode "dired-mode")
          (progn
            (mapc (lambda (ξx)
                    (let ((ξbackup-name
                           (concat ξx "~" (format-time-string "%Y%m%dT%H%M%S") "~")))
                      (copy-file ξx ξbackup-name t)))
                  (dired-get-marked-files))
            (message "marked files backed up"))
        (user-error "buffer not file nor dired")))))

(defun xah-make-backup-and-save ()
  "backup of current file and save, or backup dired marked files.
For detail, see `xah-make-backup'.
If the current buffer is not associated with a file, nothing's done.
URL `http://ergoemacs.org/emacs/elisp_make-backup.html'
Version 2015-10-14"
  (interactive)
  (if (buffer-file-name)
      (progn
        (xah-make-backup)
        (when (buffer-modified-p)
          (save-buffer)))
    (progn
      (xah-make-backup))))

(provide 'ohai-utils)
;;; ohai-utils.el ends here
