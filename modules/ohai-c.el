;;; -*- lexical-binding: t -*-
;;; ohai-c.el

;; Copyright (C) 2016 Shafayet Khan

;; Author: Shafayet Khan <shafayetkhan@gmail.com>

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

(setq
 c-default-style "gnu" ;; set style to "gnu"
 ;;c-indentation-style "gnu"
 )

(use-package cc-mode)
(use-package semantic
  :config
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (global-semantic-stickyfunc-mode 1)
  (semantic-mode 1))

(defun alexott/cedet-hook ()
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'c-mode-hook 'alexott/cedet-hook)
(add-hook 'c++-mode-hook 'alexott/cedet-hook)

;; Enable EDE only in C/C++
(use-package ede
  :config
  (global-ede-mode))

;; Keybinding for C/C++ mode
;; (define-key c-mode-map  [(tab)] 'company-complete)
;; (define-key c++-mode-map  [(tab)] 'company-complete)
(define-key c-mode-map (kbd "C-c |") 'ff-find-other-file)
(define-key c++-mode-map (kbd "C-c |") 'ff-find-other-file)

;; Place warning font around TODO and others
(defvar shafi/c-mode-keywords
  '(("\\<\\(FIX\\|TODO\\|XXX\\|FIXME\\|HACK\\|REFACTOR\\)"
     1 font-lock-warning-face t)))

(font-lock-add-keywords 'c-mode shafi/c-mode-keywords)
(font-lock-add-keywords 'c++-mode shafi/c-mode-keywords)

(provide 'ohai-c)
;;; ohai-c.el ends here
