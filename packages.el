;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! kaolin-themes)
(package! modus-themes)

(package! peep-dired)
(package! dired-icon)
(package! all-the-icons-dired)
(package! dired-open)

(package! page-break-lines)
(package! dashboard)

(package! smart-tabs-mode)
(package! nix-mode)
(package! haskell-mode)

(package! lsp-haskell)
(package! lsp-pyright)

(package! exwm)
(package! dmenu)
(package! helm-exwm)
(package! windower)

(package! org-bullets)
(package! org-journal)
(package! org-roam)
(package! org-tree-slide)

(package! nyan-mode)
(package! pulsar)

(package! ob-prolog)



(when (string-equal (getenv "USER") "mzanic")
    (package! kubernetes)
    (package! kubernetes-evil))

(unpin! (:lang go) (:tools lsp))
