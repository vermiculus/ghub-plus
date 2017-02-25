GHub+
=====

A thick GitHub API client built on [`ghub`][ghub], the minuscule
GitHub API client.

[ghub]: //github.com/tarsius/ghub

## Tour by Example
```elisp
;;; GET /issues
(ghubp-get-issues)

;;; GET /issues?state=closed
(ghubp-get-issues :state 'closed)

(let ((repo (ghub-get "/repos/magit/magit")))
  (list
   ;; Magit's issues
   ;; GET /repos/magit/magit/issues
   (ghubp-get-repos-owner-repo-issues repo)

   ;; Magit's closed issues labeled 'easy'
   ;; GET /repos/magit/magit/issues?state=closed&labels=easy
   (ghubp-get-repos-owner-repo-issues repo
     :state 'closed :labels "easy")))
```

See [`ghub+.org`][manual] for the full manual.

[manual]: //github.com/vermiculus/ghub-plus/blob/master/ghub%2B.org
