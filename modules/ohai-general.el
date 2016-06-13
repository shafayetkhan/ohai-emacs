;;; -*- lexical-binding: t -*-
;;; ohai-general.el --- Setting up the basics.

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

;; Make sure we always use UTF-8.
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(load-library "iso-transl")

;; Use volatile-highlights
(use-package volatile-highlights
  :config
  (volatile-highlights-mode 1))

;; Use clean-aindent-mode
(use-package clean-aindent-mode
  :config
  (add-hook 'prog-mode-hook 'clean-aindent-mode))

;; dtrt-indent
(use-package dtrt-indent
  :config
  (dtrt-indent-mode 1)
  (setq dtrt-indent-verbosity 0))

;; Use ws-butler
(use-package ws-butler
  :config
  (add-hook 'c-mode-common-hook 'ws-butler-mode)
  (add-hook 'text-mode 'ws-butler-mode)
  (add-hook 'fundamental-mode 'ws-butler-mode)
  :diminish ws-butler-mode)

;; Use smartparens
(use-package smartparens
  :config
  (require 'smartparens-config)
  (setq sp-highlight-pair-overlay nil)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  (smartparens-global-mode 1))

;; Use comment-dwim-2
(use-package comment-dwim-2
  :bind (("M-;" . comment-dwim-2)))

;; Use Duplicate thing
(use-package duplicate-thing
  :config
  :bind (("M-c" . duplicate-thing)))

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))



;; Automatically save buffers before launching M-x compile and friends,
;; instead of asking you if you want to save.
(setq compilation-ask-about-save nil)

;; Make the selection work like most people expect.
(delete-selection-mode t)
(transient-mark-mode t)

;; Automatically update unmodified buffers whose files have changed.
(global-auto-revert-mode 1)

;; If available, use `xdg-open' to open URLs.
(when (ohai/is-exec "xdg-open")
  (setq-default
   browse-url-browser-function (quote browse-url-generic)
   browse-url-generic-program "xdg-open"))

;; Use M-x scratch C-u to create a scratch buffer based on mode
(use-package scratch)


(provide 'ohai-general)
