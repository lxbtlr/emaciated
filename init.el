;; learning to build an emacs
;  lxbtlr


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


