;;; -*- lexical-binding: t -*-
;;; ohai-navigation.el --- Moving around.

;; Copyright (C) 2015 Bodil Stokke

;; Author: Bodil Stokke <bodil@bodil.org>

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

;; Make PgUp/Dn move the point.
(setq scroll-error-top-bottom t)

;; Avy is a quick way to jump around your buffers.
;; https://github.com/abo-abo/avy
(use-package avy
  :demand t
  :bind (("C-;" . avy-goto-word-1)
         ("C-:" . avy-goto-char))
  :config
  (with-eval-after-load "isearch"
    (define-key isearch-mode-map (kbd "C-;") 'avy-isearch)))

;; Smart home key.
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key (kbd "<home>") 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

;; Consider CamelCase chunks as words when navigating.
(global-subword-mode 1)
(diminish 'subword-mode)

;; Enhance C-x o when more than two windows are open.
(use-package ace-window
  :bind (("C-x o" . ace-window)
         ("C-x C-o" . ace-swap-window)))


;; Use C-x M-p to kill the buffer in the other window, revealing
;; the next buffer in the stack.
(global-set-key
 (kbd "C-x M-p")
 (lambda () (interactive)
   (save-excursion
     (other-window 1)
     (quit-window))))

;; Display incremental search stats in the modeline.
(use-package anzu
  :demand t
  :config
  (global-anzu-mode 1)
  ;; Anzu provides a version of `query-replace' and friends which give visual
  ;; feedback when composing regexps. Let's replace the regular versions.
  :bind(("C-%" . anzu-query-replace-at-cursor)
        ("M-%" . anzu-query-replace)
        ("C-M-%" . anzu-query-replace-regexp))
  :diminish anzu-mode)

;; Enable the awesome winner mode to navigate window layouts
(winner-mode 1)

;; Neotree - Nerdtree like navigation bar
(use-package neotree
  :init
  (setq neo-smart-open t)
  :bind ("<f8>" . neotree-toggle))

;; Popwin - pretty awesome
;; https://github.com/m2ym/popwin-el
(use-package popwin
  :config
  (popwin-mode 1)
  ;;(setq popwin:special-display-config nil)
  (setq display-buffer-function 'popwin:display-buffer)
  (add-to-list 'popwin:special-display-config '("*Occur*"))
  (push '(ag-mode :width 0.5 :position right) popwin:special-display-config)
  (push '(Man-mode :width 0.5 :position right) popwin:special-display-config)
  )


;; Move around blocks easily
(defun ergoemacs-forward-block ()
  "Move cursor forward to the beginning of next text block.
A text block is separated by 2 empty lines (or line with just whitespace).
In most major modes, this is similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table."
  (interactive)
  (if (search-forward-regexp "\n[[:blank:]\n]*\n+" nil "NOERROR")
      (progn (backward-char))
    (progn (goto-char (point-max)))))

(defun ergoemacs-backward-block ()
  "Move cursor backward to previous text block.
See: `ergoemacs-forward-block'"
  (interactive)
  (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR")
      (progn
        (skip-chars-backward "\n\t ")
        (forward-char 1))
    (progn (goto-char (point-min)))))

;; map M-p to `ergoemacs-forward-block'
(global-set-key (kbd "s-n") 'ergoemacs-forward-block)

;; map M-n to `ergoemacs-backward-block'
(global-set-key (kbd "s-p") 'ergoemacs-backward-block)

;; Require custom navigation configurations
(require 'ohai-navigation-custom)

(provide 'ohai-navigation)
