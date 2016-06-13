;;; -*- lexical-binding: t -*-
;;; ohai-git.el --- Things for working with Git.

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

(require 'ohai-package)

;; Invoke Magit by typing C-x g, and you can thank me later.
;; See http://magit.github.io/ for instructions.
(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status))

;; Use M-x gist-buffer or M-x gist-region to create a gist
;; directly from the current buffer or selection.
(use-package gist)

;; Mark uncommitted changes in the fringe.
(use-package git-gutter-fringe
  :if window-system
  :config
  (global-git-gutter-mode t)
  :diminish git-gutter-mode)

;; Show commit message at current line to know why this line was changed
(use-package git-messenger
  :bind ("C-x v p" . git-messenger:popup-message)
  :config
  ;; Copy commit message
  (bind-keys :map git-messenger-map
             ("m" . git-messenger:copy-message))
  ;; Enable magit-commit-mode after typing 's', 'S', 'd'
  (add-hook 'git-messenger:popup-buffer-hook 'magit-commit-mode))

;; Convenient binding for vc-git-grep
(global-set-key (kbd "C-x v f") 'vc-git-grep)

(provide 'ohai-git)
