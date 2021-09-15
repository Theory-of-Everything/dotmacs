;; Direcotry with other Lisp files
(add-to-list 'load-path' "~/.emacs.d/lisp/")

;; extra file requires
(require 'pack)
(require 'orgconf)
(require 'evilconf)
(require 'scheme)

;; prompt yes/no to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; ui declutter
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen nil
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)
(setq ring-bell-function 'ignore)

;; ----------------------------------------------------------- functional packages

;; statusline
(use-package doom-modeline
             :ensure t
             :init
             (doom-modeline-mode 1)
             (setq doom-modeline-height 10
                   doom-modeline-bar-width 10
                   doom-modeline-buffet-encoding 'nondefault
                   doom-modeline-major-mode-icon t
                   doom-modeline-icon nil))
(doom-modeline-def-modeline 'main
    '(bar " " matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes checker input-method buffer-encoding major-mode process vcs " " bar)) ;; <-- added padding here)

;; dasboard config
(use-package dashboard
             :ensure t
             :config
             (dashboard-setup-startup-hook))
(setq dashboard-banner-logo-title "Welcome to eii-emacs")
(setq dashboard-startup-banner "~/.emacs.d/banner.png")
(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-navigator t)
;; custom navigator buttons
(setq dashboard-navigator-buttons
      `(
        ((,(all-the-icons-material "assignment" :height 1.1 :v-adjust 0.0)
          "Agenda"
          "Open Org Agenda"
          (lambda (&rest _) (org-agenda)))
         (,(all-the-icons-fileicon "emacs" :hieght 1.1 :v-adjust 0.0)
          "Edit Config"
          "Edit Emacs Config"
          (lambda (&rest _) (find-file "~/.emacs.d/init.el"))))))

;; Project management via projectile
(use-package projectile
  ;; keybind controlled with evil-leader
  :init
  (projectile-mode +1)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; page break lines
;;(use-package page-break-lines
;;  :init
;;  (turn-on-page-break-lines-mode))

;; file tree
(use-package neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(add-hook 'neotree-mode-hook
         (lambda ()
           (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
           (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
           (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
           (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
           (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
           (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
           (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)))
(setq neo-window-fixed-size nil)

;; magit for git stuff
(use-package magit
  :defer t)

;; which-key for keybind
(use-package which-key
  :config (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.1))

;; smooth scrolling
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))


;; ----------------------------------------------------------- "funny" packages
;; matrix client
(use-package matrix-client
  :quelpa (matrix-client :fetcher github :repo "alphapapa/matrix-client.el"
                         :files (:defaults "logo.png" "matrix-client-standalone.el.sh")))

;; ----------------------------------------------------------- modes
(use-package lua-mode
  :config
  (autoload 'lua-mode "lua-mode" "Lua Editing Mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d9a28a009cda74d1d53b1fbd050f31af7a1a105aa2d53738e9aa2515908cac4c" default))
 '(org-agenda-files '("~/org/agenda.org")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
