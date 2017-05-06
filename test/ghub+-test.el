(require 'package)
(package-initialize)
(package-install-file "ghub+.el")

(require 'ghub)
(require 'ghub+)

;;; No apparent way to provide testing for authenticated requests
(setq ghub-authenticate nil)

(ert-deftest basic ()
  (should
   (let* ((repo '((owner (login . "vermiculus"))
                  (name . "ghub-plus")))
          (repo  (ghubp-get-repos-owner-repo repo)))
     (= 82884749 (alist-get 'id repo)))))
