(evil-define-key 'normal kubernetes-mode-map
  (kbd "C-c d") 'kubernetes-describe-pod
  (kbd "C-c s c") 'kubernetes-context
  (kbd "C-c y d") 'kubernetes-display-deployment
  (kbd "C-c y p") 'kubernetes-display-deployment
  (kbd "C-c s n") 'kubernetes-set-namespace)

(map! :leader
      (:prefix ("k" . "Kubernetes")
       :desc "Open kubernetes-el" "o" #'kubernetes-overview))
