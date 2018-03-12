;; init.el --- Emacs configuration

;; ----------------------------------------------------------------------
;; PACKAGES
;; ======================================================================
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(;; DEFAULTS ;;
    better-defaults

    ;; VIM ;;
    evil
    evil-collection
    evil-escape
    evil-leader

    ;; PYTHON ;;
    elpy

    ;; IDE FEATURES ;;
    flycheck
    projectile
    helm
    helm-projectile

    ;; ORG ;;
    org
    evil-org

    ;; UI IMPORVEMENTS ;;
    which-key

    ;; THEMES ;;
    espresso-theme
    material-theme))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)




;; ----------------------------------------------------------------------
;; BASIC CONFIGURATION
;; ======================================================================

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'espresso t) ;; load material theme
(enable-theme 'espresso)
(global-linum-mode t) ;; enable line numbers globally
(tool-bar-mode -1) ;; hide tool bar
(menu-bar-mode -1 ) ;; hide menu bar

(set-frame-font "Anonymous Pro")
(set-face-attribute 'default nil :height 110)

(require 'which-key) ;; interactively discover keybindings
(which-key-mode) ;; enable which-key




;; ----------------------------------------------------------------------
;; HELM
;; ======================================================================

(require 'helm) ;; completion and narrowing framework
(require 'helm-config) ;; enable default helm config

(setq helm-split-window-inside-p t ; open helm inside current window
      ; move to end or beginning of source when reaching top or bottom
      helm-move-to-line-cycle-in-source t 
      ; show/use recently opened files as suggestions
      helm-ff-file-name-history-use-recentf t
      ; enable fuzzy matching
      helm-mode-fuzzy-match t
      helm-completion-in-region-fuzzy-match t)

(helm-mode 1)




;; -----------------------------------------------------------------
;; EVIL MODE
;; ======================================================================

(setq evil-want-integration nil) ;; must be disabled to use evil collection

(require 'evil)

;; evil leader key
(global-evil-leader-mode) ;; must be called before evil mode

(evil-mode 1) ;; enable evil mode

(evil-escape-mode)
(setq-default evil-escape-key-sequence "jk") ;; easy escape mode and plugins

;; evil collection keybindings
(when (require 'evil-collection nil t)
  (evil-collection-init))

;; evil integration with org
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
(require 'evil-org-agenda)
(setf evil-org-key-theme '(navigation insert textobjects additional))
(evil-org-set-key-theme)
(evil-org-agenda-set-keys)

;; auto fill in org mode
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; evil leader configuration for helm
(evil-leader/set-key "e" 'helm-find-files)

;; add one line between headings / sections
(setq org-cycle-separator-lines 1)
;; add new line when creating a heading
(setf org-blank-before-new-entry '((heading . 1) (plain-list-item . nil)))

(setq org-log-done t)

;; enter to open/close items in org mode
(evil-define-key 'normal evil-org-mode-map
  (kbd "RET") 'org-cycle)


;; ----------------------------------------------------------------------
;; SYNTAX CHECKING
;; ======================================================================

;;(when (require 'flycheck nil t)
;;  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;  (add-hook 'elpy-mode-hook 'flycheck-mode))



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
