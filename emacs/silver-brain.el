;;; Package --- silver-brain

;;; Commentary:

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'polymode)
(require 'silver-brain-api)
(require 'silver-brain-buffer)
(require 'silver-brain-search)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             Util                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar silver-brain-default-content-format 'org
  "The default format for concept contents.
Supported values are: plain, markdown, org")

(defvar silver-brain-buffer-base-name "Silver Brain"
  "Buffer name of silver-brain.")

(defvar-local silver-brain--concept nil
  "The current concept.")

(defvar-local silver-brain--head-end-point nil
  "The end point of header part, including the separator line.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             Modes                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-derived-mode jhnn-magit-section-mode special-mode "Magit-Sections"
  "Parent major mode from which major modes with Magit-like sections inherit.

Magit-Section is documented in info node `(magit-section)'."
  :group 'magit-section
  ;; (buffer-disable-undo)
  (setq truncate-lines t)
  ;; (setq buffer-read-only t)
  (setq-local line-move-visual t) ; see #1771
  ;; Turn off syntactic font locking, but not by setting
  ;; `font-lock-defaults' because that would enable font locking, and
  ;; not all magit plugins may be ready for that (see #3950).
  (setq-local font-lock-syntactic-face-function #'ignore)
  (setq show-trailing-whitespace nil)
  (setq-local symbol-overlay-inhibit-map t)
  (setq list-buffers-directory (abbreviate-file-name default-directory))
  ;; (hack-dir-local-variables-non-file-buffer)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (add-hook 'post-command-hook #'magit-section-update-highlight t t)
  (add-hook 'deactivate-mark-hook #'magit-section-update-highlight t t)
  (setq-local redisplay-highlight-region-function
              'magit-section--highlight-region)
  (setq-local redisplay-unhighlight-region-function
              'magit-section--unhighlight-region)
  (when magit-section-disable-line-numbers
    (when (bound-and-true-p global-linum-mode)
      (linum-mode -1))
    (when (and (fboundp 'nlinum-mode)
               (bound-and-true-p global-nlinum-mode))
      (nlinum-mode -1))
    (when (and (fboundp 'display-line-numbers-mode)
               (bound-and-true-p global-display-line-numbers-mode))
      (display-line-numbers-mode -1)))
  (when (fboundp 'magit-preserve-section-visibility-cache)
    (add-hook 'kill-buffer-hook #'magit-preserve-section-visibility-cache)))

(define-derived-mode silver-brain-mode text-mode "Brain"
  "Main mode for single concept Silver Brain."
  (magit-section))

(define-derived-mode jhnn-org org-mode "Mind"
  )


(define-key silver-brain-mode-map (kbd "<tab>") 'magit-section-cycle)
(define-key silver-brain-mode-map (kbd "<backtab>") 'silver-brain-jump-to-previous-link)
(define-key silver-brain-mode-map (kbd "<return>") 'silver-brain-follow-link)
(define-key silver-brain-mode-map (kbd "C-<return>") 'silver-brain-follow-link-new-window)
(define-key silver-brain-mode-map (kbd "C-x C-s") 'silver-brain-save)
(define-key silver-brain-mode-map (kbd "g") 'silver-brain-refresh)
(define-key silver-brain-mode-map (kbd "r") 'silver-brain-rename)
(define-key silver-brain-mode-map (kbd "C-x C-c") 'silver-brain-get-uuid-at-point)
(define-key silver-brain-mode-map (kbd "s") 'silver-brain-save)
(define-key silver-brain-mode-map (kbd "o") 'silver-brain)
(define-key silver-brain-mode-map (kbd "O") 'silver-brain-new-window)
(define-key silver-brain-mode-map (kbd "d") 'silver-brain-delete)
(define-key silver-brain-mode-map (kbd "p") 'silver-brain-add-parent)
(define-key silver-brain-mode-map (kbd "c") 'silver-brain-add-child)
(define-key silver-brain-mode-map (kbd "f") 'silver-brain-add-friend)
(define-key silver-brain-mode-map (kbd "R") 'silver-brain-remove-relation)
(define-key silver-brain-mode-map (kbd "n") 'silver-brain-new-concept)
(define-key silver-brain-mode-map (kbd "q") 'silver-brain-kill-concept)
(define-key silver-brain-mode-map (kbd "Q") 'silver-brain-kill-all-concept)


(add-hook 'silver-brain-mode-hook 'silver-brain--poly-mode)

(org-link-set-parameters
 "sb"
 :face '(:background "purple")
 :follow (lambda (path)
           (silver-brain--open-concept path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                              UI                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun silver-brain-get-uuid-at-point (point)
  (interactive "d")
  (copy-to-clipboard
   (format "[[sb:%s][%s]]"
           (get-text-property point 'uuid)
           (thing-at-point 'word 'no-properties))))

(defun silver-brain-org ()
  (interactive )
  (let* ((target-uuid (silver-brain-search))
         (concept-name
          (silver-brain-concept-name
           (silver-brain-api--get-concept target-uuid))))
   (copy-to-clipboard
    (format "[[sb:%s][%s]]"
            target-uuid
            concept-name))))

(defun silver-brain-org-insert (point)
  (interactive "p")
  (let* ((target-uuid (silver-brain-search))
         (concept-name
          (silver-brain-concept-name
           (silver-brain-api--get-concept target-uuid))))
   (insert
    (format "[[sb:%s][%s]]"
            target-uuid
            concept-name))))


(defun silver-brain-get-org-link-of-sb-buffer ()
  (interactive)
  (copy-to-clipboard
   (format "[[sb:%s][%s]]"
           (silver-brain-concept-uuid silver-brain--concept)
           (silver-brain-concept-name silver-brain--concept))))



;;;###autoload
(defun silver-brain-follow-link (&optional event)
  "Follow a link under current point, EVENT is not used."
  (interactive)
  (silver-brain--open-concept (get-text-property (point) 'uuid)))

;;;###autoload
(defun silver-brain-follow-link-new-window (&optional event)
  "Follow a link under current point in a new window, EVENT is not used."
  (interactive)
  (silver-brain--open-concept-new-window (get-text-property (point) 'uuid)))

(defun silver-brain-confirm-save ()
  "Confirm to save silver-brain concept."
  (and (buffer-modified-p)
       (y-or-n-p "Current concept is modified; save it? ")
       (silver-brain-save)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Commands                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun silver-brain ()
  "The entry point of Silver Brain functions."
  (interactive)
  (let* ((uuid (silver-brain-search)))
    (when uuid
      (silver-brain-kill-concept)
      (silver-brain--open-concept uuid))))

;;;###autoload
(defun silver-brain-new-window ()
  "The entry point of Silver Brain functions."
  (interactive)
  (let* ((uuid (silver-brain-search)))
    (when uuid
      (silver-brain--open-concept-new-window uuid))))

;;;###autoload
(defun silver-brain-refresh ()
  "Refresh concept buffer."
  (interactive)
  (silver-brain--open-concept (silver-brain-concept-uuid silver-brain--concept)))

;;;###autoload
(defun silver-brain-save-all ()
  "Save all silver-brain buffer.
Note that if silver-brain-save will not work when called in a
poly-inner-mode buffer."
  (cl-dolist (buffer (silver-brain--find-concept-buffers :no-editor-p t))
    (with-current-buffer buffer
      (silver-brain-save))))

;;;###autoload
(defun silver-brain-save ()
  "Save silver-brain buffer.
Should be called in a silver-brain-mode buffer."
  (interactive)
  (when (buffer-modified-p)
    (setf (silver-brain-concept-content silver-brain--concept)
          (buffer-substring (1+ silver-brain--head-end-point) (point-max)))
    (silver-brain-api--update-concept silver-brain--concept)
    (set-buffer-modified-p nil)
    (message "Concept updated.")))

;;;###autoload
(defun silver-brain-new-concept ()
  "Create a new concept interactively and open it."
  (interactive)
  (let* ((name (read-string "Concept name: "))
         (content-format silver-brain-default-content-format)
         (concept (silver-brain-api--create-concept name content-format)))
    (silver-brain--open-concept-new-window (silver-brain-concept-uuid concept))))

;;;###autoload
(defun silver-brain-kill-concept ()
  "Kill current concept buffer."
  (interactive)
  (when (silver-brain--concept-buffer-p)
    (kill-buffer)))

;;;###autoload
(defun silver-brain-kill-all-concept ()
  "Kill all buffers of current Silver Brain concept."
  (interactive)
  (mapcar 'kill-buffer (silver-brain--find-concept-buffers)))

;;;###autoload
(defun silver-brain-rename ()
  "Rename current concept.
Should be called in silver-brain-mode buffers."
  (interactive)
  (let* ((name (read-string "New concept name: "
                            (silver-brain-concept-name silver-brain--concept)))
         (uuid (silver-brain-concept-uuid silver-brain--concept)))
    (setf (silver-brain-concept-name silver-brain--concept) name)
    (silver-brain-api--update-concept silver-brain--concept)
    (silver-brain--open-concept uuid)))

;;;###autoload
(defun silver-brain-delete ()
  "Delete current concept and all its relations."
  (interactive)
  (when (y-or-n-p "This will delete this concept; confirm? ")
    (silver-brain-api--delete-concept silver-brain--concept)
    (silver-brain-kill-concept)))

;;;###autoload
(defun silver-brain-add-parent ()
  "Add a parent."
  (interactive)
  (silver-brain--add-relation 'parent))

(defun silver-brain-add-treatment ()
  "Add a parent."
  (interactive)
  (silver-brain--add-relation 'treatment))

(defun silver-brain-add-child ()
  "Add a child."
  (interactive)
  (silver-brain--add-relation 'child))

;;;###autoload
(defun silver-brain-add-friend ()
  "Add a friend."
  (interactive)
  (silver-brain--add-relation 'friend))

;;;###autoload
(defun silver-brain-remove-relation ()
  "Search and remove relation."
  (interactive)
  (let* ((uuid (silver-brain-concept-uuid silver-brain--concept))
         (parents (silver-brain-api--get-relation 'parent uuid))
         (children (silver-brain-api--get-relation 'child uuid))
         (friends (silver-brain-api--get-relation 'friend uuid))
         (candidates (mapcar #'silver-brain-search--concept-to-candidate
                             (append parents children friends)))
         (selection (completing-read "Select: " candidates))
         (target-uuid (if selection
                          (silver-brain-search--candidate-to-uuid selection)
                        nil)))
    (when target-uuid
      (silver-brain-api--remove-relation
       (cond
        ((member target-uuid (mapcar 'silver-brain-concept-uuid parents)) 'parent)
        ((member target-uuid (mapcar 'silver-brain-concept-uuid children)) 'child)
        ((member target-uuid (mapcar 'silver-brain-concept-uuid friends)) 'friend))
       uuid target-uuid)
      (silver-brain-refresh))))

(defun silver-brain--add-relation (relation)
  "Add relation given by RELATION.
RELATION should be a symbol one of: '(parent child friend)."
  (let* ((target-uuid (silver-brain-search))
         (uuid (silver-brain-concept-uuid silver-brain--concept)))
    (when target-uuid
      (silver-brain-api--add-relation relation uuid target-uuid)
      (silver-brain-refresh))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Multi Mode                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-hostmode silver-brain--hostmode
  :mode 'silver-brain-mode)

(define-innermode silver-brain--org-innermode
  :mode 'jhnn-org
  :head-matcher (silver-brain--make-separator 10)
  :tail-matcher (silver-brain--make-impossible-matcher)
  :head-mode 'body
  :tail-mode 'body)

(define-polymode silver-brain--poly-mode
  :hostmode 'silver-brain--hostmode
  :innermodes '(silver-brain--org-innermode))

(provide 'silver-brain)

;;; silver-brain.el ends here
