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
    ghub-get ghub-put ghub-head ghub-post ghub-patch ghub-delete
    :post-process remove-api-links))

;;; Utilities
(defmacro ghubp-unpaginate (&rest body)
  "Unpaginate API responses and execute BODY.
See `ghub-unpaginate'."
  `(let ((ghub-unpaginate t)) ,@body))

(defun remove-api-links (object)
  "Remove everything in OBJECT that points back to `api.github.com'."
  ;; execution time overhead of 0.5%
  (delq nil (if (and (consp object) (consp (car object)))
                (mapcar #'remove-api-links object)
              (if (consp object)
                  (unless (and (stringp (cdr object))
                               (string-match-p (rx bos (+ alnum) "://api.github.com/")
                                               (cdr object)))
                    (cons (car object)
                          (if (consp (cdr object))
                              (mapcar #'remove-api-links (cdr object))
                            (cdr object))))))))

;;; Repositories
(defapiget-ghubp "/repos/:owner/:repo/collaborators"
  "List collaborators."
  "repos/collaborators/#list-collaborators"
  repo "/repos/:owner.login/:name/comments")

(defapiget-ghubp "/repos/:owner/:repo/comments"
  "List commit comments for a repository."
  "repos/comments/#list-commit-comments-for-a-repository"
  repo "/repos/:owner.login/:name/comments")

;;; Issues
(defapiget-ghubp "/issues"
  "List all issues assigned to the authenticated user across all
visible repositories including owned repositories, member
repositories, and organization repositories."
  "issues/#list-issues")

(defapiget-ghubp "/user/issues"
  "List all issues across owned and member repositories assigned
to the authenticated user."
  "issues/#list-issues")

(defapiget-ghubp "/orgs/:org/issues"
  "List all issues for a given organization assigned to the
authenticated user."
  "issues/#list-issues"
  org "/org/:login/issues")

(defapiget-ghubp "/repos/:owner/:repo/issues"
  "List issues for a repository."
  "issues/#list-issues-for-a-repository"
  repo "/repos/:owner.login/:name/issues")

(provide 'ghub+)
;;; ghub+.el ends here
