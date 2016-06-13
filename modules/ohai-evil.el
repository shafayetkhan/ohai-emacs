;;; -*- lexical-binding: t -*-
;;; ohai-evil.el

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

;; Evil mode
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-default-state 'emacs) ;; ah! this is awesome.
  :config
  (evil-mode 1)
  (setq evil-normal-state-cursor '(box "orange"))
  (setq evil-insert-state-cursor '(box "chartreuse3"))
  (setq evil-emacs-state-cursor '(box "#7ec0ee"))
  (use-package evil-leader
    :config
    (global-evil-leader-mode))
  (use-package evil-surround
    :config
    (global-evil-surround-mode))
  (use-package evil-tutor)
  (use-package evil-indent-textobject)
  (dolist (mode '(ag-mode
                  flycheck-error-list-mode
                  git-rebase-mode
                  neotree-mode
                  ;;emacs-lisp-mode
                  ))
    (add-to-list 'evil-emacs-state-modes mode))
  )





(provide 'ohai-evil)
;; ohai-evil.el ends here
