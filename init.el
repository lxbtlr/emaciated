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
(setq straight-use-package-by-default t)

(setq use-package-always-ensure t
      use-package-always-defer t) ; globally (default )set ensure and defer for all packages

;; allows upgrading builtin packages
(when (and (>= emacs-major-version 29)
           (>= emacs-major-version 1))
  (setq-default package-install-upgrade-built-in 't))

(use-package no-littering :ensure t)

;; Setup the package repos
; (require 'package)
;  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;  (add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
;  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
; 
; 
; (package-initialize)



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
;; make searching menus in emacs work prettier / better / stronger

(use-package vertico
  ;:custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring




(use-package corfu
  :ensure t
  :defer nil
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Automatically create completion drop-down
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

;; corfu uses child frames (smaller frames inside the main frame view-port),
;; which does not work well in the terminal. corfu-terminal replaces the child
;; frames with popup/popon, which does work.


(straight-use-package
 '(popon :type git :repo "https://codeberg.org/akib/emacs-popon.git"))

 
;; (use-package popon
;;   :ensure (:host codeberg :repo "akib/emacs-popon")
;;   :defer t)



(straight-use-package
 '(corfu-terminal
   :type git
   :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
 :ensure t
 )

;; Add completion extensions that corfu can use.
(use-package cape
  :ensure t
  :defer nil
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  ;; :bind (("C-c p p" . completion-at-point) ;; capf
  ;;        ("C-c p t" . complete-tag)        ;; etags
  ;;        ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ("C-c p k" . cape-keyword)
  ;;        ("C-c p s" . cape-elisp-symbol)
  ;;        ("C-c p e" . cape-elisp-block)
  ;;        ("C-c p a" . cape-abbrev)
  ;;        ("C-c p l" . cape-line)
  ;;        ("C-c p w" . cape-dict)
  ;;        ("C-c p :" . cape-emoji)
  ;;        ("C-c p \\" . cape-tex)
  ;;        ("C-c p _" . cape-tex)
  ;;        ("C-c p ^" . cape-tex)
  ;;        ("C-c p &" . cape-sgml)
  ;;        ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; cape-tex is a capf for turning TeX/LaTeX commands into unicode characters.
  ;; For example, \section is turned into §.
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))

(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-theme 'doom-oxocarbon
        doom-themes-enable-italic t))

(load-theme 'doom-oxocarbon t)
