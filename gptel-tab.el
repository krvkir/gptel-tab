;; gptel-tab --- Tab-specific contexts for gptel -*- lexical-binding:t; -*-
;; Author: Kirill Krasnoshchekov <krvkir@gmail.com>
;; Version: 0.1
;; Package-Requires: (gptel tab-bar)
;; Keywords: convenience
;; URL: https://github.com/krvkir/gptel-tab

;;; Code:

(require 'tab-bar)
(require 'gptel)
(require 'gptel-context)

;;* Customizations

(defcustom gptel-tab-verbose nil
  "Be verbose and exhaustively write things to Messages buffer when switching tabs."
  :type 'boolean
  :group 'gptel)

;;* Variables definition

(defvar gptel-tab--tab-contexts nil
  "Mapping of tab names to their GPTel context data (alist format).")

(defvar gptel-tab--current-tab-name nil "Last active tab name for context tracking.")

;;* Context serialization and restoration

(defun gptel-tab--serialize-context-entry (entry)
  "Makes a serializable representation of context entry which could be later
restored or saved to `.emacs.desktop` file."
  (if (bufferp (car entry))
	  ;; Buffer context:
	  ;; store file name or buffer name + region positions
	  (let* ((buf (car entry))
			 (buf-id
			  (or (buffer-file-name buf) (buffer-name buf)))
			 (regions
			  (mapcar (lambda (ov) (cons (overlay-start ov) (overlay-end ov))) (cdr entry))))
		`(:buffer ,buf-id :regions ,regions))
	;; File context (key is file path string)
	`(:file ,(car entry))))

(defun gptel-tab--restore-context-entry (entry)
  "Adds context entry based on its serialized representation."
  (cond
   ;; Buffer region context (entry = (buf-id . regions))
   ((plist-member entry :buffer)
    ;; Get buffer by its id.
    (let* ((buf-id (plist-get entry :buffer))
	   (buf (or (get-file-buffer buf-id)
		    (get-buffer buf-id))))
      ;; Restore all its regions.
      (dolist (pos (plist-get entry :regions))
	(with-current-buffer buf
	  (gptel-context--make-overlay (car pos) (cdr pos))))))
   ;; File context: ("/path/to/file" . props) or ("/path/to/file" . ((start . end)...))
   ((plist-member entry :file)
    ;; ... for now, file properties are not supported
    (gptel-context-add-file (plist-get entry :file)))))

;;* Save current context to the store
;; TODO Replace hash table with alist/plist/whatever to make the structure serializable
;;      and enable saving context to desktop-file.

(defun gptel-tab--save-current-tab-context ()
  "Save the current tab's GPTel context into `gptel-tab--tab-contexts`."
  (interactive)
  (let ((context-copy (mapcar #'gptel-tab--serialize-context-entry gptel-context--alist)))
    (when gptel-tab-verbose
      (message
       "[gptel-tab/save] %d elements in context copy, %d elemeints in gptel-context--alist."
       (length context-copy)
       (length gptel-context--alist)))
    ;; Copy current context list for storage.
    ;; ... use saved tab-name to handle state when we're in the new tab
    ;; and want to save the context before restoring saved one.
    (when gptel-tab-verbose
      (message "[gptel-tab/save] Saving context into tab %s" gptel-tab--current-tab-name))
    ;; (puthash gptel-tab--current-tab-name context-copy gptel-tab--tab-contexts)
	(let ((entry (assoc gptel-tab--current-tab-name gptel-tab--tab-contexts)))
	  (if entry
		  (setcdr entry context-copy)
		(push (cons gptel-tab--current-tab-name context-copy) gptel-tab--tab-contexts)))))

(defvar gptel-context-changed-hook nil
  "Hook run after GPTel context is modified.")

(defun run-gptel-context-changed-hook (&rest _)
  (run-hooks 'gptel-context-changed-hook))

(advice-add 'gptel-context-add :after #'run-gptel-context-changed-hook)
(advice-add 'gptel-context-remove :after #'run-gptel-context-changed-hook)
(advice-add 'gptel-context-confirm :after #'run-gptel-context-changed-hook)

(add-hook 'gptel-context-changed-hook #'gptel-tab--save-current-tab-context)

;;* Restore context from the store when changing tab

(defun gptel-tab--restore-tab-context (tab-name)
  "Restore GPTel context for tab named TAB-NAME from `gptel-tab--tab-contexts`."
  (let* (;; (stored-context (gethash tab-name gptel-tab--tab-contexts))
		 (entry (assoc tab-name gptel-tab--tab-contexts))
		 (stored-context (cdr entry)))
    ;; Clear the old context.
    (when gptel-tab-verbose
      (message "[gptel-tab/restore] Current tab name is: %s" gptel-tab--current-tab-name)
      (message "[gptel-tab/restore] Tab name to restore is: %s" tab-name)
      (message "[gptel-tab/restore] %d elements in stored context." (length stored-context)))
    (gptel-context-remove-all)
    (when gptel-tab-verbose
      (message "[gptel-tab/restore] %d elements in stored context." (length stored-context)))
    ;; Add entries from stored context.
    (mapcar #'gptel-tab--restore-context-entry stored-context)
    ;; Update context buffer view
    (when (get-buffer "*gptel-context*")
      (with-current-buffer "*gptel-context*"
        (save-window-excursion (revert-buffer nil t))))))

(defun gptel-tab--tab-switch-hook ()
  "Hook run on window config change to handle tab context switching."
  (let ((current-name (alist-get 'name (tab-bar--current-tab)))
	(has-explicit-name (alist-get 'explicit-name (tab-bar--current-tab))))
    ;; Set stored tab name if it's empty.
    (when (and (not gptel-tab--current-tab-name) has-explicit-name)
      (setq gptel-tab--current-tab-name current-name))
    ;; If current tab has explicit name and it differs from the stored one,
    ;; save the current context and restore stored context for the current
    ;; tab. If there's no saved context, we just clear the old one.
    (when (and
	   has-explicit-name
	   (not (equal current-name gptel-tab--current-tab-name)))
      (when gptel-tab-verbose
	(message "[gptel-tab/switch] Switching tab context: %s to %s"
		 gptel-tab--current-tab-name current-name))
      ;; Tab changed (or first time initialization)
      (gptel-tab--save-current-tab-context)	      ; save old tab
      (setq gptel-tab--current-tab-name current-name) ; update current tab
      (gptel-tab--restore-tab-context current-name)
      (gptel-tab--save-current-tab-context) ; save old tab
      (when gptel-tab-verbose
	(message "[gptel-tab/switch] Finished switching tab context.")))))

(add-hook 'window-configuration-change-hook #'gptel-tab--tab-switch-hook)

;;* Auxilliary functions 

;; Advice for tab renaming – move context to new name key
(defun gptel-tab--handle-tab-rename (orig-fun &rest args)
  (let* ((old-name (alist-get 'name (tab-bar--current-tab)))
		 (old-entry (assoc old-name gptel-tab--tab-contexts)))
	(apply orig-fun args)				; perform rename
	(let ((new-name (alist-get 'name (tab-bar--current-tab))))
	  (when (and old-name
				 new-name
				 (not (equal old-name new-name))
				 old-entry)
		;; Transfer context mapping to new name
		(setcar old-entry new-name)
		(setq gptel-tab--current-tab-name new-name)))))

(advice-add #'tab-bar-rename-tab :around #'gptel-tab--handle-tab-rename)

;;* Saving and restoring sessions with desktop file.

(defun gptel-tab--restore-current-tab-context ()
  (gptel-tab--restore-tab-context (alist-get 'name (tab-bar--current-tab))))

(add-hook 'desktop-before-save-hook #'gptel-tab--save-current-tab-context)
(add-hook 'desktop-after-read-hook #'gptel-tab--restore-current-tab-context)

(provide 'gptel-tab)
