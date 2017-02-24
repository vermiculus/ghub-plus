;;; ghub+dev.el --- development tools for ghub+      -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: tools

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

;; 

;;; Code:

(defun ghubp-resolve-api-params (object url &optional noencode)
  "Resolve parameters in URL to values in OBJECT.

Unless NOENCODE is non-nil, OBJECT values will be passed through
`url-encode-url'.

Example:

\(ghubp-resolve-api-params
    '((name . \"Hello-World\")
      \(owner (login . \"octocat\")))
  \"/repos/:owner.login/:name/issues\")
=> \"/repos/octocat/Hello-World/issues\"

"
  (declare (indent 1))
  (unless noencode
    (require 'url))
  (macroexp--expand-all
   `(let-alist ,object
      ,(let ((in-string t))
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
           (delete "" (read (buffer-string)))))))))

(defun ghubp-plist->alist (plist)
  "Convert PLIST to an alist.
Alist keys will be symbols and its values will be coerced into
strings."
  (when (= 1 (mod (length plist) 2))
    (error "bad plist"))
  (ghubp--plist->alist-internal plist nil))

(defun ghubp--plist->alist-internal (plist alist-build)
  (if plist (cons (let ((key (car plist))
                        (val (cadr plist)))
                    (cons (intern (substring (symbol-name key) 1))
                          (cond
                           ((stringp val) val)
                           ((symbolp val) (symbol-name val))
                           (t (error "unhandled case")))))
                  (ghubp--plist->alist-internal (cddr plist) alist-build))))

(defconst ghubp--standard-parameters
  '((repo . "REPO is a repository alist of the form returned by `/user/repos'.")))

(defun ghubp--defresource (method external-resource doc version link object internal-resource)
  "Define a new resource."
  (declare (indent defun) (doc-string 3))
  (setq internal-resource (or internal-resource external-resource))
  (let* ((symbol external-resource)
         (symbol (replace-regexp-in-string "/" "-" symbol t t))
         (symbol (replace-regexp-in-string ":" ""  symbol t t))
         (symbol (intern (concat "ghubp-" (symbol-name method) symbol)))
         (args (append (if object (list object)) '(&optional data &rest params)))
         (ghub-func (plist-get (list 'get #'ghub-get 'put #'ghub-put
                                     'head #'ghub-head 'post #'ghub-post
                                     'patch #'ghub-patch 'delete #'ghub-delete)
                               method))
         (object-param-doc (alist-get object ghubp--standard-parameters ""))
         (fmt-str "%s

%sPARAMS is a plist of parameters appended to the method call.

DATA is a data structure (understandable by `json-encode-list')
to be sent with this request.  If it's not required, it can
simply be omitted.

%s

This generated function wraps

    %s %s

which is documented at

    URL `https://developer.github.com/v%d/%s'"))
    (unless ghub-func
      (error "No function for method %S" method))
    (when object
      (unless object-param-doc
        (error "Standard parameter %s not documented in `ghubp--standard-parameters'" object))
      (unless (string= object-param-doc "")
        (setq object-param-doc (concat object-param-doc "\n\n"))))
    (eval `(defun ,symbol ,args ,(format fmt-str doc object-param-doc (make-string 20 ?-)
                                         (upcase (symbol-name method))
                                         external-resource version link)
                  (declare (indent defun))
                  (apply ',ghub-func
                         ,(ghubp-resolve-api-params object internal-resource)
                         (if (= 0 (mod (length params) 2))
                             ;; if params has an even number of
                             ;; elements (i.e., it's balanced), then
                             ;; `data' is real.
                             (list (ghubp-plist->alist params) data)
                           ;; otherwise, it's part of params
                           (list (ghubp-plist->alist (cons data params)))))))
    (put symbol 'ghubp-api-version version)
    (put symbol 'ghubp-api-method method)
    (put symbol 'ghubp-api-endpoint external-resource)
    (put symbol 'ghubp-api-documentation link)
    symbol))

(dolist (method '(get put head post patch delete))
  (let ((symbol (intern (concat "ghubp-def" (symbol-name method)))))
    (eval `(defmacro ,symbol (resource doc version link &optional object internal-resource)
             ,(format "Define a new %s resource wrapper function.

EXTERNAL-RESOURCE is the API endpoint as written in the GitHub
API documentation.  This string will be used to create the symbol
for the new function.

DOC is a documentation string.  Usually, this can be copied from
the GitHub API documentation.

VERSION is the GitHub API version this resource wraps.

LINK is a link to the GitHub API documentation.

If non-nil, OBJECT is a symbol that will be used to resolve
parameters in the resource and will be a required argument of the
new function.  If nil, it is ignored.

If non-nil, INTERNAL-RESOURCE is the resource used to resolve
OBJECT to the ultimate call." (upcase (symbol-name method)))
             `',(ghubp--defresource ',method resource doc version link object internal-resource)))))

(provide 'ghub+dev)
;;; ghub+dev.el ends here
