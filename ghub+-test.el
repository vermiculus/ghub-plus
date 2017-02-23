;;; We can learn from the Hub project and use Cucumber (with ecukes)
;;; to mock API calls.

(require 'ert)

(should
 (equal (let ((obj '((name . "Hello-World")
                     (owner (login . "octocat")))))
          (list (ghub-resolve-api-params obj "/repos/:owner.login/:name/issues")
                (ghub-resolve-api-params obj "/repos/:owner.login/:name")
                (ghub-resolve-api-params obj "/:owner.login/:name/issues")
                (ghub-resolve-api-params obj "/:owner.login/:name")
                (ghub-resolve-api-params obj ":owner.login")
                (ghub-resolve-api-params obj "/:owner.login")
                (ghub-resolve-api-params obj "/:owner.login/")))
        (list "/repos/octocat/Hello-World/issues"
              "/repos/octocat/Hello-World"
              "/octocat/Hello-World/issues"
              "/octocat/Hello-World"
              "octocat"
              "/octocat"
              "/octocat/")))

(should
 (equal (let ((obj '((name . "hello^world")
                     (owner (login . "octo^cat")))))
          (ghub-resolve-api-params obj "/:owner.login/:name"))
        "/octo%5Ecat/hello%5Eworld"))


(should
 (equal (ghub--plist->alist '(:one two :three four))
        '((one . "two") (three . "four"))))

(ignore                                 ; until cucumber
 '(ghub-unpaginate
   (ghub-issues `((owner (login . "tarsius")) (name . "ghub"))
                :state 'open)))
