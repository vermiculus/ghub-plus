(load-file "./ghub+.el")
(message "ghub+ loaded manually: %s"
         (if (featurep 'ghub+) "yes" "no"))

(require 'subr-x)
(require 'dash)
(require 's)

(defun lint-is-api-form-p (form)
  "Is FORM a defapi* macro call?"
  (and (s-prefix-p "defapi" (symbol-name (car form)))
       form))

(defun lint-get-forms (filename)
  "Read FILENAME and return a list of its Lisp forms."
  (let ((pos 0) forms)
    (with-temp-buffer
      (insert-file-contents filename)
      (condition-case _
          (while t
            (when-let ((cell (read-from-string (buffer-string) pos)))
              (push (car cell) forms)
              (goto-char (setq pos (cdr cell)))))
        (error forms)))
    forms))

(defun lint-api-forms (filename)
  "From FILENAME, return a list of API forms."
  (-filter #'lint-is-api-form-p (lint-get-forms filename)))

(defun lint-arg-appears-in-target-p (arg target-string)
  "Does symbol ARG appear in TARGET-STRING?
Such that `apiwrap-resolve-api-params' would see it?"
  (and (stringp target-string)
       (or (s-contains-p (format ":%S." arg) target-string)
           (s-contains-p (format ":%S/" arg) target-string)
           (s-suffix-p   (format ":%S"  arg) target-string))))

(defun lint-macro-to-method (msym)
  "Get the HTTP method corresponding to MSYM."
  (let ((s (symbol-name msym)))
    (upcase (substring s 6 (s-index-of "-" s)))))

(defun lint-unused-args (form)
  "Check for any unused arguments in FORM.
If there are unused arguments, print them out with `message' and
return them.  Return nil if there are no offenders."
  (when (listp (nth 4 form))
    (let* ((interesting (-slice form 4 6))
           (args (car interesting))
           (target-string (cadr interesting))
           (filt (lambda (sym)
                   (lint-arg-appears-in-target-p
                    sym target-string)))
           (offenders (-remove filt args)))
      (dolist (arg offenders)
        (message "Unused argument in '%s': %S"
                 (lint-format-defapi-form form)
                 arg))
      offenders)))

(defun lint-format-defapi-form (form)
  "=> GET /some/thing/here"
  (concat (lint-macro-to-method (car form)) " " (cadr form)))

(defun lint-standard-args-undeclared--get-backend (filename)
  "Get the backend declaration form from FILENAME."
  (with-temp-buffer
    (insert-file filename)
    (let ((needle "(apiwrap-new-backend"))
      (search-forward needle)
      (backward-char (length needle))
      (read (current-buffer)))))

(defun lint-standard-args-undeclared--get-std-vars (backend)
  "Get the list of standard vars from BACKEND."
  (mapcar #'car (cadr (nth 3 backend))))

(defun lint-standard-args-undeclared--internal (form std-args)
  "Non-nil if FORM uses args not in STD-ARGS"
  (let (fail)
    (when (listp (nth 4 form))
      (dolist (std-arg (nth 4 form))
        (unless (memq std-arg std-args)
          (setq fail t)
          (message "Undeclared standard argument in '%s': %S"
                   (lint-format-defapi-form form)
                   std-arg))))
    fail))

(defun lint-undeclared-standard-args (filename)
  "Wrapper for `lint-standard-args-undeclared--internal'."
  (let ((forms (lint-api-forms filename))
        (stdargs (lint-standard-args-undeclared--get-std-vars
                  (lint-standard-args-undeclared--get-backend filename)))
        fail)
    (dolist (form forms)
      (setq fail (or (lint-standard-args-undeclared--internal form stdargs) fail)))
    fail))

(defun lint (filename lint-function &optional per-form)
  "Run all linting checks on forms in FILENAME."
  (let (fail)
    (if per-form
        (dolist (form (lint-api-forms filename))
          (setq fail (or (funcall lint-function form) fail)))
      (setq fail (funcall lint-function filename)))
    (not fail)))
