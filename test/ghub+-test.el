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

(ert-deftest lint-unused-args ()
  (should (lint "ghub+.el" #'lint-unused-args 'per-form)))

(ert-deftest lint-undeclared-args ()
  (should (lint "ghub+.el" #'lint-undeclared-standard-args)))

(ert-deftest linter-selftest ()
  (message ">>> Start linter self-tests")
  (should (lint-unused-args '(defapiget-ghubp "/rate_limit" "" "" (repo issue) "/:repo.thing")))
  (should-not (lint-unused-args '(defapiget-ghubp "/some_call_with_no_args" "some-desc" "some-url"
                                   :post-process (lambda (o) (ghubp--post-process o '(subject))))))
  (message "<<< End linter self-tests"))
