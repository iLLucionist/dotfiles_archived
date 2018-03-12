
;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; -----------------------------------------------------------------

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    projectile
    helm
    helm-projectile
    evil
    evil-collection
    evil-escape
    evil-org
    evil-leader
    espresso-theme
    org
    material-theme))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)


;; BASIC CUSTOMIZATION
;; -----------------------------------------------------------------


(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'espresso t) ;; load material theme
(enable-theme 'espresso)
(global-linum-mode t) ;; enable line numbers globally

(set-frame-font "Anonymous Pro")
(set-face-attribute 'default nil :height 110)


;; EVIL MODE
;; -----------------------------------------------------------------

(setq evil-want-integration nil)
(require 'evil)
(when (require 'evil-collection nil t)
  (evil-collection-init))

(evil-mode 1)
(evil-escape-mode)
(setq-default evil-escape-key-sequence "jk")
(global-evil-leader-mode)

(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(setf evil-org-key-theme '(navigation insert textobjects additional))
(evil-org-set-key-theme)


(evil-leader/set-key "e" 'helm-find-files)


;; HELM COMPLETION
;; -----------------------------------------------------------------
(require 'helm)
(helm-mode 1)


;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (material-theme better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
