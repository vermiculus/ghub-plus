;;; ghub+.el --- a thick GitHub API client built ghub  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: extensions, multimedia, tools
;; Homepage: https://github.com/vermiculus/ghub-plus
;; Package-Requires: ((emacs "25") (apiwrap "0.1") (ghub "20160808.538"))
;; Package-Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides some sugar for ghub.  See `ghub+.org' (which should have
;; been distributed with this file) for usage instructions.

;;; Code:

(require 'ghub)
(require 'apiwrap)

(eval-when-compile
  (apiwrap-new-backend "GitHub" "ghubp"
    ((repo . "REPO is a repository alist of the form returned by `/user/repos'.")
     (org  . "ORG is an organization alist of the form returned by `/user/orgs'."))
    (lambda (version link) (format "https://developer.github.com/v%d/%s" version link))
    ghub-get ghub-put ghub-head ghub-post ghub-patch ghub-delete))

;;; Utilities

(defmacro ghubp-unpaginate (&rest body)
  "Unpaginate API responses and execute BODY.
See `ghub-unpaginate'."
  `(let ((ghub-unpaginate t)) ,@body))

;;; Repositories
(ghubp-defget "/repos/:owner/:repo/collaborators"
  "List collaborators."
  3 "repos/collaborators/#list-collaborators"
  repo "/repos/:owner.login/:name/comments")

(ghubp-defget "/repos/:owner/:repo/comments"
  "List commit comments for a repository."
  3 "repos/comments/#list-commit-comments-for-a-repository"
  repo "/repos/:owner.login/:name/comments")

;;; Issues
(ghubp-defget "/issues"
  "List all issues assigned to the authenticated user across all
visible repositories including owned repositories, member
repositories, and organization repositories."
  3 "issues/#list-issues")

(ghubp-defget "/user/issues"
  "List all issues across owned and member repositories assigned
to the authenticated user."
  3 "issues/#list-issues")

(ghubp-defget "/orgs/:org/issues"
  "List all issues for a given organization assigned to the
authenticated user."
  3 "issues/#list-issues"
  org "/org/:login/issues")

(ghubp-defget "/repos/:owner/:repo/issues"
  "List issues for a repository."
  3 "issues/#list-issues-for-a-repository"
  repo "/repos/:owner.login/:name/issues")

(provide 'ghub+)
;;; ghub+.el ends here
