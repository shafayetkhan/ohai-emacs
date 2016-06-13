;;; -*- lexical-binding: t -*-
;;; ohai-helm.el --- The Grand Emacs Incremental Narrowing Thingy.

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



(use-package helm
  :demand t
  :init
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (global-unset-key (kbd "C-z"))
  :config
  (require 'helm-config)
  (require 'helm)
  (require 'helm-grep)
  ;; Activate Helm.
  (helm-mode 1)
  ;; Tell Helm to resize the selector as needed.
  (helm-autoresize-mode 1)
  ;; Make Helm look nice.
  (setq-default helm-display-header-line nil
                helm-autoresize-min-height 10
                helm-autoresize-max-height 35
                helm-candidate-number-limit 500
                helm-buffer-max-length nil
                helm-split-window-in-side-p t
                helm-move-to-line-cycle-in-source t
                helm-ff-search-library-in-sexp t
                helm-scroll-amount 8
                helm-ff-file-name-history-use-recentf t

                helm-M-x-fuzzy-match t
                helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-match t
                helm-apropos-fuzzy-match t
                helm-semantic-fuzzy-match t
                helm-imenu-fuzzy-match t)
  (set-face-attribute 'helm-source-header nil :height 0.75)
  ;; Rebind tab to run persistent action
  (bind-keys :map helm-map
             ("<tab>" . helm-execute-persistent-action))
  ;; Make tab work in terminal as well
  (bind-keys :map helm-map
             ("C-i" . helm-execute-persistent-action))
  ;; List actions using C-z
  (bind-keys :map helm-map
             ("C-z" . helm-select-action))
  ;; Helm-grep-mode-map
  (bind-keys :map helm-grep-mode-map
             ("<return>" . helm-grep-mode-jump-other-window))
  (bind-keys :map helm-grep-mode-map
             ("n" . helm-grep-mode-jump-other-window-forward))
  (bind-keys :map helm-grep-mode-map
             ("p" . helm-grep-mode-jump-other-window-backward))

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  ;; use helm to list eshell history
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

  ;;; Save current position to mark ring
  (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

  ;; show minibuffer history with Helm
  (bind-keys :map minibuffer-local-map
             ("M-p" . helm-minibuffer-history))
  (bind-keys :map minibuffer-local-map
             ("M-n" . helm-minibuffer-history))

  (define-key global-map [remap find-tag] 'helm-etags-select)
  ;; Helm projectile
  (use-package helm-projectile
  :demand t
  :config
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)
  (setq projectile-indexing-method 'alien)
  ;; A binding for using Helm to pick files using Projectile,
  ;; and override the normal grep with a Projectile based grep.
  :bind (("C-c C-f" . helm-projectile-find-file-dwim)
         ("C-x C-g" . helm-projectile-grep)))
  ;; Helm-ag
  (use-package helm-ag
    :demand t
    :config
    (custom-set-variables
     '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
     '(helm-ag-command-option "--all-text")))
  ;; Enrich isearch with Helm using the `C-S-s' binding.
  ;; swiper-helm behaves subtly different from isearch, so let's not
  ;; override the default binding.
  (use-package swiper-helm
    :bind (("C-S-s" . swiper-helm)))
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  ;; Helm swoop
  (use-package helm-swoop
    :config
    ;; When doing isearch, hand the word over to helm-swoop
    (bind-keys :map isearch-mode-map
               ("M-i" . helm-swoop-from-isearch))
    (bind-keys :map helm-swoop-map
               ("M-i" . helm-multi-swoop-all-from-helm-swoop))
    (setq-default helm-multi-swoop-edit-save t
                  ;; helm-swoop-split-with-multiple-windows t
                  helm-swoop-split-direction 'split-window-vertically
                  helm-swoop-speed-or-color t)
    :bind (("C-c h o" . helm-swoop)
           ("C-c s" . helm-multi-swoop-all))
    )
  ;; Enable helm-gtags-mode
  (use-package helm-gtags
    :init
    (setq-default  helm-gtags-ignore-case t
                   helm-gtags-auto-update t
                   helm-gtags-use-input-at-cursor t
                   helm-gtags-pulse-at-cursor t
                   helm-gtags-prefix-key "\C-cg"
                   helm-gtags-suggested-key-mapping t)
    :config
    (add-hook 'dired-mode-hook 'helm-gtags-mode)
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)

    (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
    (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
    (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
    (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
    (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
    :diminish helm-gtags-mode)
  (global-set-key (kbd "C-c h g") 'helm-google-suggest)
  (global-set-key (kbd "C-c h C-c w") 'helm-wikipedia-suggest)
  (global-set-key (kbd "C-c h x") 'helm-register)

  ;; Replace common selectors with Helm versions.
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-g" . helm-do-grep)
         ("C-x b" . helm-buffers-list)
         ;; emacs key sequence starts with non-prefix key use-package bind error??
         ;;("C-c h g" . helm-google-suggest)
         ("C-t" . helm-imenu)
         ("M-y" . helm-show-kill-ring)
         ("C-h SPC" . helm-all-mark-rings)
         ;;("C-c h o" . helm-occur)
         ;;("C-c h C-c w" . helm-wikipedia-suggest)
         ;;("C-c h x" . helm-register)
         )
  )

;; Bind C-c C-e to open a Helm selection of the files in your .emacs.d.
;; We get the whole list of files and filter it through `git check-ignore'
;; to get rid of transient files.
(defun ohai-helm/gitignore (root files success error)
  (let ((default-directory root))
    (let ((proc (start-process "gitignore" (generate-new-buffer-name "*gitignore*")
                               "git" "check-ignore" "--stdin"))
          (s (lambda (proc event)
               (if (equal "finished\n" event)
                   (funcall success
                            (with-current-buffer (process-buffer proc)
                              (s-split "\n" (s-trim (buffer-string)))))
                 (funcall error event))
               (kill-buffer (process-buffer proc))
               (delete-process proc))))
      (set-process-sentinel proc s)
      (process-send-string proc (concat (s-join "\n" files) "\n"))
      (process-send-eof proc))))

(defun ohai-helm/files-in-repo (path success error)
  (let ((files (f-files path nil t)))
    (ohai-helm/gitignore path files
                         (lambda (ignored)
                           (funcall success (-difference files ignored)))
                         error)))

(defun ohai-helm/find-files-in-emacs-d ()
  (interactive)
  (ohai-helm/files-in-repo
   dotfiles-dir
   (lambda (files)
     (let ((relfiles (-filter
                      (lambda (f) (not (f-descendant-of? f ".git")))
                      (-map (lambda (f) (f-relative f dotfiles-dir)) files))))
       (find-file
        (concat dotfiles-dir
                (helm :sources (helm-build-sync-source ".emacs.d" :candidates relfiles)
                      :ff-transformer-show-only-basename helm-ff-transformer-show-only-basename
                      :buffer "*helm emacs.d*")))))
   (lambda (err) (warn "ohai-helm/find-files-in-emacs-d: %s" err))))

(global-set-key (kbd "C-c C-e") 'ohai-helm/find-files-in-emacs-d)

;; (when (executable-find "ag")
;;   (setq helm-grep-default-command "ag --nocolor --nogroup --ignore-case"
;;         helm-grep-default-recurse-command "ag --nocolor --nogroup --ignore-case"))

(provide 'ohai-helm)
;;; ohai-helm.el ends here
