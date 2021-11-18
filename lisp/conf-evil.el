(provide 'conf-evil)

;; emacs vi bindings
(use-package evil
             :init
             (setq evil-want-integration t)
             (setq evil-want-keybinding nil)
             :config
             (evil-mode 1))

;; extra bindings for evil
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; orgmode evil binds
;; (use-package evil-org-mode
;;   :after org
;;   :config
;;   (require 'evil-org-agenda)
;;   (evil-org-agenda-set-keys))

;; evil leader key
(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    ;; General
    ".f" 'consult-isearch
    ".q" 'delete-frame
    ".e" 'eval-region
    ".s" 'straight-use-package
    ;; Undo
    "uv" 'undo-tree-visualize
    "uu" 'undo-tree-undo
    "ur" 'undo-tree-redo
    "uc" 'consult-yank-pop
    ;; Files
    "fr" 'consult-recent-file
    "fb" 'consult-bookmark
    "ff" 'find-file
    ;; Bufffers
    "bv" 'split-window-right
    "bh" 'split-window-below
    "bd" 'kill-current-buffer
    "bb" 'consult-buffer
    "bx" 'switch-to-scratch
    ;; Projectile
    "pa" 'projectile-add-known-project
    "pf" 'projectile-find-file
    "pp" 'projectile-switch-project
    "pg" 'projectile-grep
    "pm" 'projectile-commander
    "pc" 'projectile-compile-project
    ;; Org mode
    "oc" 'org-edit-special
    "ol" 'org-latex-preview
    "ot" 'org-ctrl-c-ctrl-c
    "oi" 'org-toggle-inline-images
    "oa" 'org-agenda
    "os" 'org-schedule
    ; Export
    "oep" 'org-latex-export-to-pdf
    "oeh" 'org-html-export-to-html
    "oem" 'org-man-export-to-man
    ; Roam
    "orf" 'org-roam-node-find
    "ori" 'org-roam-node-insert
    "oru" 'org-roam-db-sync
    "oro" 'orui-open
    ; Babel
    "obs" 'org-babel-execute-src-block
    "obb" 'org-babel-execute-buffer
    "obl" 'org-babel-load-file
    ;; Help
    "hh" 'help
    "hk" 'describe-key
    "hv" 'describe-variable
    "hf" 'describe-function
    "hs" 'describe-symbol
    "hm" 'describe-mode
    ;; Magit
    "gi" 'magit-init
    "gc" 'magit-commit
    "gp" 'magit-push
    "gC" 'magit-clone
    "gs" 'magit-status
    ;; neotree
    "tt" 'neotree-toggle
    ;; dashboard
    "~" 'dashboard-refresh-buffer))
