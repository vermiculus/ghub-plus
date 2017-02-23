;;; ghub+.el --- a thick GitHub API client built ghub  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: extensions, multimedia, tools

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

(defmacro ghub-resolve-api-params (object url &optional noencode)
  "Resolve parameters in URL to values in OBJECT.

Unless NOENCODE is non-nil, OBJECT values will be passed through
`url-encode-url'.

Example:

\(ghub-resolve-api-params
    '((name . \"Hello-World\")
      \(owner (login . \"octocat\")))
  \"/repos/:owner.login/:name/issues\")
=> \"/repos/octocat/Hello-World/issues\"

"
  (declare (indent 1))
  (unless noencode
    (require 'url))
  (unless (stringp url)
    (error "URL must be literal"))
  ;; I realize the hackiness of this, but it works and it's evaluated
  ;; compile-time
  `(let-alist ,object
     ,(let (in-string)
        (with-temp-buffer
          (insert url)
          (goto-char 0)
          (insert "(concat \"")
          (while (search-forward ":" nil t)
            (goto-char (1- (point)))
            (insert "\" ")
            (unless noencode (insert "(url-encode-url "))
            (insert ".")
            (setq in-string nil)
            (delete-char 1)
            (when (search-forward "/" nil t)
              (goto-char (1- (point)))
              (unless noencode (insert ")"))
              (insert " \"")
              (setq in-string t)))
          (goto-char (point-max))
          (if in-string (insert "\"")
            (unless noencode (insert ")")))
          (insert ")")
          (delete "" (read (buffer-string)))))))

(defmacro ghub-unpaginate (&rest body)
  "Unpaginate API responses and execute BODY.
See `ghub-unpaginate'."
  `(let ((ghub-unpaginate t)) ,@body))

(defun ghub-plist->alist (plist)
  "Convert PLIST to an alist.
Alist keys will be symbols and its values will be coerced into
strings."
  (when (oddp (length plist))
    (error "bad plist"))
  (ghub--plist->alist-internal plist nil))

(defun ghub--plist->alist-internal (plist alist-build)
  (if plist (cons (let ((key (car plist))
                        (val (cadr plist)))
                    (cons (intern (substring (symbol-name key) 1))
                          (cond
                           ((stringp val) val)
                           ((symbolp val) (symbol-name val))
                           (t (error "unhandled case")))))
                  (ghub--plist->alist-internal (cddr plist) alist-build))))

(defun ghub-issues (repo &rest params)
  "Get a list of issues for REPO."
  (ghub-get (ghub-resolve-api-params repo
              "/repos/:owner.login/:name/issues")
            (ghub-plist->alist params)))

(provide 'ghub+)
;;; ghub+.el ends here
