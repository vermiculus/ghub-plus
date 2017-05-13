(load-file "./ghub+.el")
(message "%S" load-path)
(message "ghub+ loaded manually: %s" (if (featurep 'ghub+) "yes" "no"))
