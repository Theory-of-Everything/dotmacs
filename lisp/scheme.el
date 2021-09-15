(provide 'scheme)

;; install theme packages
(use-package kaolin-themes)
(use-package atom-one-dark-theme)

;; load theme
(load-theme 'kaolin-dark t)

;; font 
(add-to-list 'default-frame-alist '(font . "agave Nerd Font Mono-12"))
(set-face-attribute 'default t :font "agave Nerd Font Mono-12")

;;symbol font
(set-fontset-font t 'symbol (font-spec :family "Twemoji") nil 'prepend)

;; fancy symbols
  (defun org/prettify-set ()
    (interactive)
    (setq prettify-symbols-alist
        '(("#+begin_src" . "")
          ("#+BEGIN_SRC" . "")
          ("#+end_src" . "")
          ("#+END_SRC" . "")
          ("#+begin_example" . "")
          ("#+BEGIN_EXAMPLE" . "")
          ("#+end_example" . "")
          ("#+END_EXAMPLE" . "")
          ("#+results:" . "")
          ("#+RESULTS:" . "")
          ("#+begin_quote" . "❝")
          ("#+BEGIN_QUOTE" . "❝")
          ("#+end_quote" . "❞")
          ("#+END_QUOTE" . "❞")
          ("[ ]" . "☐")
          ("[-]" . "◯")
          ("[X]" . "☑"))))
  (add-hook 'org-mode-hook 'org/prettify-set)

  (defun prog/prettify-set ()
    (interactive)
    (setq prettify-symbols-alist
        '(("lambda" . "λ")
          ("->" . "→")
          ("<-" . "←")
          ("<=" . "≤")
          (">=" . "≥")
          ("!=" . "≠")
          ("~=" . "≃")
          ("=~" . "≃"))))
  (add-hook 'prog-mode-hook 'prog/prettify-set)

(global-prettify-symbols-mode)

;; line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(dolist (mode '(org-mode-hook
  term-mode-hook
  eshell-mode-hook
  neotree-mode-hook
  elfeed-show-mode-hook
  circe-channel-mode-hook
  circe-chat-mode-hook
  doc-view-mode-hook
  xwidget-webkit-mode-hook
  woman-mode-hook))
(add-hook mode (lambda () (display-line-numbers-mode 0))))

;; tabs/spaces conf
(setq-default indent-tabs-mode nil
    tab-width 4)
(setq indent-line-function 'insert-tab)
