(in-package :silver-brain)

(defun main (&optional args)
  "Entry point of the Silver Brain server."
  (opts:define-opts
    (:name :help
     :short #\h
     :long "help"
     :description "print this help text")
    (:name :profile
     :short #\p
     :long "profile"
     :meta-var "PROFILE"
     :description "set profile to use
PROFILE is either 'product' (default) or 'dev'"
     :arg-parser (lambda (x) (make-keyword (string-upcase x)))))
  (let* ((options (opts:get-opts args))
         (help (getf options :help))
         (profile (getf options :profile :product)))
    (when help (print-help-and-quit))
    (conf:set-profile profile)
    ;; When booted via command line, disable the thread usage. Otherwise the
    ;; program quits immediately.
    (setf (conf:server-use-thread-p) nil)
    (setup-db)
    (start-server)))

(defun print-help-and-quit ()
  "Print help message and quit the software."
  (opts:describe :prefix "Silver Brain - Your external memory device.

Starts the server of Silver Brain software."
                 :usage-of "silver-brain")
  (uiop:quit 0))
