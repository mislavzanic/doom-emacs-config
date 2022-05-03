(server-start)

(setq user-full-name "Mislav Zanic"
      user-mail-address "mislavzanic3@gmail.com")

(map! :leader
      (:prefix ("b". "buffer")
       :desc "List bookmarks" "L" #'list-bookmarks
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))

(global-set-key (kbd "C-x b") 'exwm-workspace-switch-to-buffer)

(defadvice! prompt-for-buffer (&rest _)
  :after 'window-split (switch-to-buffer))

(setq fancy-splash-image "~/.config/.dotfiles/config/emacs/doom/logo.png")
(setq +doom-dashboard-banner-file (expand-file-name "logo.png" doom-private-dir)
      +doom-dashboard-banner-dir  "~/.config/doom-emacs/modules/ui/doom-dashboard/")


(set-frame-parameter (selected-frame) 'alpha '(85 . 85))
(add-to-list 'default-frame-alist '(alpha . (85 . 85)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq modus-themes-vivendi-color-overrides
      '((bg-alt . "#000000")))
(setq doom-theme 'modus-vivendi)

(use-package! dashboard
  :config
  (dashboard-setup-startup-hook)
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
          dashboard-startup-banner "~/.config/.dotfiles/config/emacs/doom/logo.png"
          dashboard-center-content t))

(after! doom-modeline
    (setq doom-modeline-major-mode-icon t
          doom-modeline-buffer-state-icon t
          doom-modeline-buffer-encoding nil
          doom-modeline-bar-width 6
          doom-modeline-lsp t
          doom-modeline-github nil)
    (custom-set-faces '(mode-line ((t (:height 1.0))))
                      '(mode-line-inactive ((t (:height 1.0)))))
    (remove-hook 'doom-modeline-mode-hook #'size-indication-mode))

(setq display-line-numbers-type 'relative
      tab-always-indent 'complete)

(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "Peep-dired image previews" "d p" #'peep-dired
        :desc "Dired view file" "d v" #'dired-view-file)))

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-find-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-chmod
  (kbd "O") 'dired-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-up-directory
  (kbd "% l") 'dired-downcase
  (kbd "% u") 'dired-upcase
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)

;; If peep-dired is enabled, you will get image previews as you go up/down with 'j' and 'k'
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(use-package! all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package! dired-open
  :config
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv")
                                ("pdf" . "zathura"))))

(global-set-key (kbd "C-x w") 'elfeed)

(setq elfeed-feeds
      '(("https://www.reddit.com/r/linux.rss" reddit linux)
        ("https://based.cooking/rss.xml" cooking)
        ("https://lukesmith.xyz/rss.xml" lukesmith linux)
        ("https://protesilaos.com/codelog.xml" prot coding)
        ("https://protesilaos.com/books.xml" prot books)
        ("https://www.hashicorp.com/blog/feed.xml" hashicorp devops infra)
        ("https://www.reddit.com/r/sysadmin/" reddit sysadmin)))

(map! :map evil-window-map
      "SPC" #'rotate-layout)
(define-key input-decode-map [(control ?i)] [control-i])
(define-key input-decode-map [(control ?I)] [(shift control-i)])
(map! :map 'evil-motion-state-map "C-i" nil)
(define-key evil-motion-state-map [control-i] 'evil-jump-forward)

(defun efs/configure-shell ()
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredumps t
        eshell-scroll-to-bottom-on-input t))
(use-package! eshell
  :hook (eshell-first-time-mode . efs/configure-shell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim"))))

(when IS-LINUX
    (load! "~/.config/.dotfiles/config/emacs/exwm/+exwm"))

(setq doom-font (font-spec :family "JetBrains Mono" :size 12)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 12)
      doom-big-font (font-spec :family "JetBrains Mono" :size 26))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq ivy-posframe-display-functions-alist
      '((swiper                     . ivy-posframe-display-at-point)
        (complete-symbol            . ivy-posframe-display-at-point)
        (counsel-M-x                . ivy-display-function-fallback)
        (counsel-esh-history        . ivy-posframe-display-at-window-center)
        (counsel-describe-function  . ivy-display-function-fallback)
        (counsel-describe-variable  . ivy-display-function-fallback)
        (counsel-find-file          . ivy-display-function-fallback)
        (counsel-recentf            . ivy-display-function-fallback)
        (counsel-register           . ivy-posframe-display-at-frame-bottom-window-center)
        (dmenu                      . ivy-posframe-display-at-frame-top-center)
        (nil                        . ivy-posframe-display))
      ivy-posframe-height-alist
      '((swiper . 20)
        (dmenu . 20)
        (t . 10)))
;; (ivy-posframe-mode 1) ; 1 enables posframe-mode, 0 disables it.

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(map! :leader
      (:prefix ("v" . "Ivy")
       :desc "Ivy push view" "v p" #'ivy-push-view
       :desc "Ivy switch view" "v s" #'ivy-switch-view))

(when (string-equal (getenv "USER") "mzanic")
    (load "~/.config/.dotfiles/config/emacs/doom/+k8s"))

(use-package! nyan-mode
  :config
  (nyan-mode t))

(setq org-directory "~/.local/org/"
      org-agenda-files '("~/.local/org/agenda.org")
      org-default-notes-file (expand-file-name "notes.org" org-directory)
      org-agenda-start-with-log-mode t
      org-hide-emphasis-markers t
      org-log-done 'time
      org-log-into-drawer t
      org-ellipsis " ▼ ")

(use-package! org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "●" "○" "◆" "●" "○" "◆")))

(set-face-attribute 'variable-pitch nil :font "Cantarell")

(setq org-entities-user
      '(("vdots" "\\vdots{}" t "&x2999" "..." "..." "⁞")
        ("mathcalR" "\\mathcal{R}" t "&x211B" "R" "R" "ℛ")
        ("mathbbR" "\\mathbb{R}" t "&x211D" "R" "R" "ℝ")
        ("mathbbN" "\\mathbb{N}" t "&x2115" "N" "N" "ℕ")))

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.config/.dotfiles/config/emacs/doom/config.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((prolog . t)))

(map! :leader
      :desc "Org babel tangle" "m B" #'org-babel-tangle)

(global-set-key (kbd "C-x j") 'org-journal-new-entry)
(setq org-journal-dir "~/.local/org/journal/"
      org-journal-date-prefix "* "
      org-journal-time-prefix "** "
      org-journal-date-format "%B %d, %Y (%A) "
      org-journal-file-format "%Y-%m-%d.org")

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun efs/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                    :templates '(("i" "inbox" plain "* %?"
                                 :if-new (file+head "inbox.org" "#+title: Inbox\n")))))
(global-set-key (kbd "C-c n b") #'efs/org-roam-capture-inbox)

;;
;; ORG-ROAM
;;


(use-package! org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/.local/org_roam")
  (org-roam-complete-everywhere t)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
         :map org-roam-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup)

  (defun efs/org-roam-filter-by-tag (tag-name)
    (lambda (node)
      (member tag-name (org-roam-node-tags node))))

  (setq org-roam-capture-templates
        '(("m" "main" plain
           "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("n" "notes" plain "%?"
           :if-new
           (file+head "notes/${title}.org" "#+title: ${title}\n#+filetags: :notes:\n#+startup: entitiespretty\n")
           :immediate-finish t
           :unnarrowed t)
          ("w" "work notes" plain "%?"
           :if-new
           (file+head "work_notes/${title}.org" "#+title: ${title}\n#+filetags: :work:\n")
           :immediate-finish t
           :unnarrowed t)))

  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))

  (setq org-roam-node-display-template
        (concat "(${type}) ${title:*} " (propertize "${tags:10}" 'face 'org-tag))))

(map! :leader
      (:prefix ("r" . "Org Roam")
       :desc "Create a node" "l" #'org-roam-buffer-toggle
       :desc "Find a node" "f" #'org-roam-node-find
       :desc "Insert a node" "i" #'org-roam-node-insert))

(defun efs/presentation-setup ()
  (setq text-scale-mode-amount 3)
  (org-latex-preview)
  (text-scale-mode 1))

(defun efs/presentation-end ()
  (text-scale-mode 0))

(use-package! org-tree-slide
  :hook ((org-tree-slide-play . efs/presentation-setup)
         (org-tree-slide-stop . efs/presentation-end)))

(global-set-key (kbd "C-x p") 'projectile-switch-project)

(use-package! lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(setq which-key-idle-delay 0.5)

(use-package! pulsar)
(setq pulsar-pulse-functions
      ;; NOTE 2022-04-09: The commented out functions are from before
      ;; the introduction of `pulsar-pulse-on-window-change'.  Try that
      ;; instead.
      '(recenter-top-bottom
        move-to-window-line-top-bottom
        reposition-window
        ;; bookmark-jump
        ;; other-window
        ;; delete-window
        ;; delete-other-windows
        forward-page
        backward-page
        scroll-up-command
        scroll-down-command
        ;; windmove-right
        ;; windmove-left
        ;; windmove-up
        ;; windmove-down
        ;; windmove-swap-states-right
        ;; windmove-swap-states-left
        ;; windmove-swap-states-up
        ;; windmove-swap-states-down
        ;; tab-new
        ;; tab-close
        ;; tab-next

        evil-window-middle
        evil-window-bottom
        evil-window-up

        org-next-visible-heading
        org-previous-visible-heading
        org-forward-heading-same-level
        org-backward-heading-same-level
        outline-backward-same-level
        outline-forward-same-level
        outline-next-visible-heading
        outline-previous-visible-heading
        outline-up-heading))

(setq pulsar-pulse-on-window-change t)
(setq pulsar-pulse t)
(setq pulsar-delay 0.055)
(setq pulsar-iterations 10)
(setq pulsar-face 'pulsar-magenta)
(setq pulsar-highlight-face 'pulsar-yellow)

(pulsar-global-mode 1)

(global-set-key (kbd "C-x x") '+vterm/toggle)
