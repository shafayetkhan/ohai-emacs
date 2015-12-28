;;; -*- lexical-binding: t -*-
;;; ohai-orgmode.el --- Your personal everything manager.

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

;; Stop org-mode from highjacking shift-cursor keys.
(setq org-replace-disputed-keys t)

;; Always use visual-line-mode in org-mode, and wrap it at column 80.
(add-hook
 'org-mode-hook
 (lambda ()
   (visual-line-mode 1)
   (set-visual-wrap-column 80)))

;; Fancy bullet rendering.
(package-require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Insert links from clipboard.
(package-require 'org-cliplink)
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c M-l") 'org-cliplink))

;; Require custom orgmode configurations
(require 'ohai-orgmode-custom)

(provide 'ohai-orgmode)
