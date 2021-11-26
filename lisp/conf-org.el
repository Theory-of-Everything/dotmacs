(provide 'conf-org)
(use-package org)

;; ----------------------------------------- Orgmode config
(setq org-directory "~/org/"
      org-default-notes-file "~/org/notes.org")
(setq org-agenda-files '("~/org/agenda.org" "~/org/roam/20211110185637-ytvideos.org"))
(setq org-startup-with-inline-images t)
(setq org-clock-sound "~/.emacs.d/ding.wav")

;; orgmode bullets
(use-package org-bullets
  :after org
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

;; task completion log
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

;; extra ui additions
(setq org-hide-emphasis-markers t)
(setq org-image-actual-width '(300))
;; (set-face-attribute 'org-headline-done nil :strike-through t)
(setq org-agenda-start-on-weekday 0)
(setq org-src-tab-acts-natively t)

;; orgmode font config
;; make sure org-indent is avalible
(require 'org-indent)

;; preserve fixed-width fonts where needed
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; heading font settings
(set-face-attribute 'org-document-title nil :font "Work Sans" :weight 'bold :height 1.3)
(dolist (face '((org-level-1 . 1.0)
                (org-level-2 . 1.0)
                (org-level-3 . 1.0)
                (org-level-4 . 1.0)
                (org-level-5 . 1.0)
                (org-level-6 . 1.0)
                (org-level-7 . 1.0)
                (org-level-8 . 1.0)))
  (set-face-attribute (car face) nil :font "Work Sans" :weight 'medium :height (cdr face)))


;; orgmode keywords
(setq org-todo-keywords
     '((sequence "TODO" "NEXT" "IMPORTANT" "BACKLOG" "OPTIONAL" "DONE")))
(setq org-todo-keyword-faces
  '(("TODO"      . (:foreground "#0F64B7" :weight bold))
    ("BACKLOG"   . (:foreground "#CE3C65" :weight bold))
    ("WORKING"   . (:foreground "#AAF2AA" :weight bold))
    ("NEXT"      . (:foreground "#D5D5D5" :weight bold))
;;  ("ALMOST"    . (:foreground "#80D1FF" :weight bold))
    ("OPTIONAL"  . (:foreground "#21A08B" :weight bold))
    ("IMPORTANT" . (:foreground "#C25BAD" :weight bold))
    ("DONE"      . (:foreground "#68C768" :weight bold))))

;; agenda views
(setq org-agenda-custom-commands
   '(("d" "Dashboard"
      ((agenda "" ((org-deadline-warning-days 14)))
       (todo "IMPORTANT"
        ((org-agenda-overriding-header "Important")))
       (todo "BACKLOG"
        ((org-agenda-overriding-header "Backlogged")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ;; Low-effort next actions
    ("e" "Low-Effort" tags-todo "+TODO=\"TODO\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("y" "Youtube Videos" tags-todo "+yt"
     ((org-agenda-overriding-header "Planned Videos")))

    ("t" "School Tasks" tags-todo "+school"
     ((org-agenda-overriding-header "School Assignments")))

    ("W" "Work Tasks" tags-todo "+work-email")

    ("w" "Workflow Status"
     ((todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Backlogged")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "OPTIONAL"
            ((org-agenda-overriding-header "Optional Tasks")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "DONE"
            ((org-agenda-overriding-header "Completed Tasks")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "IMPORTANT"
            ((org-agenda-overriding-header "Important Tasks")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))))))

;; org-roam config
(setq org-roam-v2-ack t)

(use-package org-roam
  :custom
  (org-roam-directory "~/org/roam/")
  :config
  (org-roam-setup))

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  :bind(("C-c n l" . org-roam-buffer-toggle)
        ("C-c n f" . org-roam-node-find)
        ("C-c n i" . org-roam-node-insert))
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start 'nil)
  (org-roam-setup))

;; org-capture templates
(setq org-capture-templates
    '(("t" "Todo" entry (file "~/org/refile.org")
       "* TODO %?\n%U" :empty-lines 1)
      ("n" "Note" entry (file "~/org/refile.org")
       "* NOTE %?\n%U" :empty-lines 1)))

;; kill opened org files
(add-hook 'org-agenda-mode-hook
            (lambda ()
              (local-set-key (kbd "q") 'org-agenda-exit)))
