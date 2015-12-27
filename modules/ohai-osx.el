;;; -*- lexical-binding: t -*-
;;; ohai-osx.el --- On-the-fly font size adjustment.

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

(when *is-a-mac*
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)
  (setq default-input-method "MacOSX")
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "M-˙") 'ns-do-hide-others)
  (after-load 'nxml-mode
    (define-key nxml-mode-map (kbd "M-h") nil))
  (global-set-key (kbd "M-ˍ") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h
  ;; Make some super keychords behave the same as meta
  (define-key key-translation-map (kbd "s-x") (kbd "M-x"))
  (define-key key-translation-map (kbd "s-w") (kbd "M-w"))
  (define-key key-translation-map (kbd "s-b") (kbd "M-b"))
  (define-key key-translation-map (kbd "s-f") (kbd "M-f"))
  )


(provide 'ohai-osx)
;;; ohai-osx.el ends here
