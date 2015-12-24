;;; -*- lexical-binding: t -*-
;;; ohai-fonts-custom.el --- On-the-fly font size adjustment.

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

;; Set default font in initial window and for any new window
;; Curated from: http://ergoemacs.org/emacs/emacs_list_and_set_font.html
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (when (member "Input Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Input Mono-12"))
    (add-to-list 'default-frame-alist '(font . "Input Mono-12"))))
 ((string-equal system-type "darwin") ; Mac OS X
  (when (member "Input Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Input Mono-12"))
    (add-to-list 'default-frame-alist '(font . "Input Mono-12"))))
 ((string-equal system-type "gnu/linux") ; linux
  (when (member "Input Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Input Mono-12"))
    (add-to-list 'default-frame-alist '(font . "Input Mono-12")))))

(provide 'ohai-fonts-custom)
