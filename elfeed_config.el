;; -----
;; use-package setup; recommended to use curl w/ elfeed, so set that here
(use-package elfeed
  :ensure t
  :config
  (elfeed-update)
  ;; the default filter elfeed uses 
  (setq-default elfeed-search-filter "@6-months-ago +unread")
  (setq elfeed-curl-program-name "curl")
  (setq elfeed-curl-timeout 200)
  ;; this is slow on windows, so give it a lot of time
  (setq url-queue-timeout 1000))

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.elfeed/mac-elfeed.org")))


;; -----
;; appearance customization
;; science articles have long titles, so set a wide width
(setq elfeed-search-title-max-width 140)


;; -----
;; make tagging entries as favorites easier
(defun mac-tag-favorite ()
  "Tag an entry as a favorite for elfeed while reading it."
  (elfeed-show-tag 'favorite))


;; -----
;; view/tag specific feeds
(defun mac-elfeed-science ()
  "Show unread science entries in elfeed."
  (interactive)
  (elfeed-search-set-filter "+science +unread @6-months-ago"))
  
(defun mac-elfeed-rxiv ()
  "Show unread biorxiv entries in elfeed."
  (interactive)
  (elfeed-search-set-filter "+rxiv +unread @6-months-ago"))

(defun mac-elfeed-emacs ()
  "Show unread emacs entries in elfeed."
  (interactive)
  (elfeed-search-set-filter "+emacs +unread @6-months-ago"))

(defun mac-elfeed-default ()
  "Show the default filter for elfeed."
  (interactive)
  (elfeed-search-set-filter "+unread @6-months-ago"))

(defun mac-elfeed-favorite-tag ()
  "Tag the selected entry as a favorite.  I'll come back to it later."
  (interactive)
  (let* ((entries (elfeed-search-selected))
         (tag 'favorite))
    (cl-loop for entry in entries do (elfeed-tag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun mac-elfeed-favorite-untag ()
  "Remove 'favorite' tag from all selected entries."
  (interactive )
  (let* ((entries (elfeed-search-selected))
         (tag 'favorite))
    (cl-loop for entry in entries do (elfeed-untag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defalias 'mac-elfeed-favorite-toggle
  (elfeed-expose #'elfeed-search-toggle-all 'favorite))
  
(defun mac-elfeed-favorites ()
  "Show entries marked as favorites in elfeed."
  (interactive)
  (elfeed-search-set-filter "+favorite"))

(defun mac-elfeed-old-tag ()
  "Tag the selected entry as old.  I'll come back to it later."
  (interactive)
  (let* ((entries (elfeed-search-selected))
         (tag 'old))
    (cl-loop for entry in entries do (elfeed-tag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun mac-elfeed-old-untag ()
  "Remove 'old' tag from all selected entries."
  (interactive )
  (let* ((entries (elfeed-search-selected))
         (tag 'old))
    (cl-loop for entry in entries do (elfeed-untag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defalias 'mac-elfeed-old-toggle
  (elfeed-expose #'elfeed-search-toggle-all 'old))

(defun mac-elfeed-not-old-new ()
  "Show the new entries filter for elfeed."
  (interactive)
  (elfeed-search-set-filter "+unread -old @6-months-ago"))




;; -----
;; multiple machine syncing functions
;; functions to support syncing .elfeed between machines
;; makes sure elfeed reads index from disk before launching
(defun bjm/elfeed-load-db-and-open ()
  "Wrapper to grab the remote index and load the elfeed db from disk before opening."
  (interactive)
  (start-process-shell-command
   (concat "elfeed_grab_remote at " (format-time-string "%Y.%m.%d %k:%M:%S %p")) "*elfeed-log*"
   "rsync -uvz mahlon@login.msi.umn.edu:/home/albertf/mahlon/index ~/.elfeed/index")
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force)
  (elfeed-update))


;; write to disk when quiting
(defun bjm/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (bury-buffer (get-buffer "*elfeed-search*"))
  (switch-to-previous-buffer)
  ;; sync the newly saved index to my remote storage site
  (start-process-shell-command
   (concat "elfeed_push_local at " (format-time-string "%Y.%m.%d %k:%M:%S %p")) "*elfeed-log*"
   "rsync -uvz --stats ~/.elfeed/index mahlon@login.msi.umn.edu:/home/albertf/mahlon/index")
  )


;; roll into a single function
(defun mac-elfeed ()
  "Function for using elfeed."
  (interactive)
  (let ((elfd (get-buffer "*elfeed-search*")))
    (if (and
         (bufferp elfd)
         (eq (current-buffer) elfd))
      (bjm/elfeed-save-db-and-bury)
    (bjm/elfeed-load-db-and-open))))


(defun my-fig-open ()
  (interactive)
  (org-next-link)
  (org-open-at-point))



;; -----
;; setup keys for my preference
(eval-after-load 'elfeed-search
  (progn
    ;; in an entry
    (define-key elfeed-show-mode-map (kbd "k") 'elfeed-kill-buffer)
    (define-key elfeed-show-mode-map (kbd "l") 'my-fig-open)
    (define-key elfeed-show-mode-map (kbd "f") (lambda () (interactive) (mac-tag-favorite)))
    
    ;; in the "*elfeed-search*" buffer
    (define-key elfeed-search-mode-map (kbd "a") 'mac-elfeed-science)
    (define-key elfeed-search-mode-map (kbd "e") 'mac-elfeed-emacs)
    (define-key elfeed-search-mode-map (kbd "v") 'mac-elfeed-rxiv)
    (define-key elfeed-search-mode-map (kbd "d") 'mac-elfeed-default)    
    (define-key elfeed-search-mode-map (kbd "f") 'mac-elfeed-favorite-toggle)
    (define-key elfeed-search-mode-map (kbd "F") 'mac-elfeed-favorites)
    (define-key elfeed-search-mode-map (kbd "k") 'mac-elfeed)
    (define-key elfeed-search-mode-map (kbd "o") 'mac-elfeed-old-toggle)
    (define-key elfeed-search-mode-map (kbd ";") 'mac-elfeed-not-old-new)
    )
  )
