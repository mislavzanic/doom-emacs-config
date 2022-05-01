(add-hook! 'kubernetes-mode-hook (lambda ()
                                   (local-set-key (kbd "C-c k n") #'kubernetes-set-namespace)))
(map! :leader
      (:prefix ("k" . "Kubernetes")
       :desc "Open kubernetes-el" "o" #'kubernetes-overview))
