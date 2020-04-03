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

(defun mac-elfeed-unread-not-favorite ()
  "Show entries marked as favorites in elfeed.  
Useful for catching things you might like to mark as read."
  (interactive)
  (elfeed-search-set-filter "+unread -favorite"))

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
  (let ((pr (concat
	     "elfeed rsync msi_index to local_index_backup at "
	     (format-time-string "%Y.%m.%d %k:%M:%S:%3N %p"))))
    (start-process-shell-command
     pr (get-buffer-create "*elfeed-log*")
     "rsync -ui mahlon@login.msi.umn.edu:/home/albertf/mahlon/msi_index ~/.elfeed/index_local_backup ;
     cp -v ~/.elfeed/index_local_backup ~/.elfeed/index")
    ;; wait_contingency: don't do anything until we've finished syncing
    ;; with the remote index
    (unless
	(not
	 (= (process-exit-status (get-process pr)) 0))
      (sit-for 0.5))
    ;; now load the db
    (with-current-buffer (get-buffer-create "*elfeed-log*")
      (read-only-mode -1)
      (insert
       (concat
	"Loaded elfeed db at "
	(format-time-string "%Y.%m.%d %k:%M:%S:%3N %p")
	"\n"))
      (read-only-mode 1))
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force)
    (elfeed-update)
    (message "elfeed-load-db-and-open"))
  )


;; write to disk when quiting
(defun bjm/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (switch-to-previous-buffer)
  (sit-for 4)
  ;; make a local copy of the index just in case
  (start-process-shell-command
   (concat
    "elfeed_local_backup (index_local_backup) at "
    (format-time-string "%Y.%m.%d %k:%M:%S:%3N %p"))
   (get-buffer-create "*elfeed-log*")
   "cp -v ~/.elfeed/index ~/.elfeed/index_local_backup")
  ;; sync the newly saved index to my remote storage site
  (start-process-shell-command
   (concat
    "elfeed rysnc local_index_backup to msi_index at "
    (format-time-string "%Y.%m.%d %k:%M:%S:%3N %p"))
   (get-buffer-create "*elfeed-log*")
   "rsync -ui ~/.elfeed/index_local_backup mahlon@login.msi.umn.edu:/home/albertf/mahlon/msi_index")
  (message "elfeed-save-db-and-bury")
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
;; open links with a custom function
(defun mac-elfeed-open-link ()
  "Open links in elfeed."
  (interactive)
  (beginning-of-buffer)
  (re-search-forward "^Link:.." nil nil)
  (shr-browse-url))

(defun mac-elfeed-open-link-in-next ()
  "Open links in elfeed using the NeXT browser."
  (interactive)  
  (let ((browse-url-generic-program (executable-find "next"))
      (browse-url-browser-function 'browse-url-generic))
    (beginning-of-buffer)
  (re-search-forward "^Link:.." nil nil)
  (shr-browse-url)))

(defun mac-generic-open-link-in-next ()
  "Open any link at point using the NeXT browser."
  (let ((browse-url-generic-program (executable-find "next"))
      (browse-url-browser-function 'browse-url-generic))
  (shr-browse-url)))




;; -----
;; setup keys for my preference
(eval-after-load 'elfeed-search
  (progn
    ;; in an entry
    (define-key elfeed-show-mode-map (kbd "k") 'elfeed-kill-buffer)
    (define-key elfeed-show-mode-map (kbd "l") 'mac-elfeed-open-link)
    (define-key elfeed-show-mode-map (kbd "m") 'mac-elfeed-open-link-in-next)    
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
    (define-key elfeed-search-mode-map (kbd "m") 'mac-elfeed-unread-not-favorite)    
    )
  )
