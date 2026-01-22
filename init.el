;; learning to build an emacs
;  lxbtlr


(setq package-enable-at-startup nil)


;; package-management
;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default 'true)

(setq use-package-always-ensure 't
      use-package-always-defer 't) ; globally (default )set ensure and defer for all packages

;; allows upgrading builtin packages
(when (and (>= emacs-major-version 29)
           (>= emacs-major-version 1))
  (setq-default package-install-upgrade-built-in 't))


(add-to-list 'load-path (expand-file-name "./configs/" user-emacs-directory))

( setq inhibit-startup-message t 
       visible-bell t)

(global-display-line-numbers-mode 1)

;; ###############

(icomplete-mode 1)

(setq modus-themes-completions '((t . (extrabold))))

(load-theme 'modus-vivendi
	    nil)

;; ###############

(hl-line-mode t) ; highlight line mode (testing)

(setq hl-line-sticky-flag nil)

(blink-cursor-mode 1) ; make the cursor do a little dance

(recentf-mode 1) ; enable recent file mode

(setq history-length 25)

(savehist-mode 1)

(save-place-mode 1) ; save the last position of the cursor when you reopen the file

(global-auto-revert-mode 1)

(setq gloval-auto-revert-non-file-buffers t)

(use-package evil
  :ensure t
  :straight t
  :demand t
  :init
 (setq evil-want-keybinding nil) ;; recommended integration
 :config (evil-mode 1))

;; make all the other modes respect my nvim author-it-tie
(use-package evil-collection 
  :ensure t
  :straight t
  :demand t
  :after evil
  :config (evil-collection-init))

;; better git
(use-package magit
  :ensure t
  :straight t
  :defer t) 

;; testing -- dashboard
;; this does make loading much slower

(use-package dashboard
  :ensure t
  :straight t
  :demand t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner "dash/hydra.txt"))

(use-package pulsar
  :ensure t
  :bind
  ( :map global-map
    ("C-x l" . pulsar-pulse-line) ; overrides `count-lines-page'
    ("C-x L" . pulsar-highlight-permanently-dwim)) ; or use `pulsar-highlight-temporarily-dwim'
  :init
  (pulsar-global-mode 1)
  :custom
  (pulsar-generic 'pulsar-red)
  (pulsar-face 'pulsar-magenta)
  (pulsar-delay 0.05))
