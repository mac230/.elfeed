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
(setq elfeed-search-title-max-width 140)


;; -----
;; make tagging entries as favorites easier
(defun mac-tag-favorite ()
  "Tag an entry as a favorite for elfeed."
  (elfeed-show-tag 'favorite))



;; -----
;; set up some feeds to follow
(setq elfeed-feeds
      '(
        ;; various biorxiv feeds
        "http://connect.biorxiv.org/biorxiv_xml.php?subject=cell_biology"
        "http://connect.biorxiv.org/biorxiv_xml.php?subject=genomics"
        "http://connect.biorxiv.org/biorxiv_xml.php?subject=genetics"
        "http://connect.biorxiv.org/biorxiv_xml.php?subject=synthetic_biology"

        ;; nature
        ;; nature journal
        "http://feeds.nature.com/nature/rss/current"
        ;; nature genetics
        "http://feeds.nature.com/ng/rss/current"
        ;; nature biotech
        "http://feeds.nature.com/nbt/rss/current"
        ;; nature systems biology
        "https://www.nature.com/npjsba/"

        ;; science
        ;; journal TOC
        "http://science.sciencemag.org/rss/current.xml"
        ;; this week in science
        "http://science.sciencemag.org/rss/twis.xml"

        ;; elife
        "https://elifesciences.org/rss/recent.xml"

        ;; cell
        "http://www.cell.com/cell/inpress.rss"
        "http://www.cell.com/cell/current.rss"

        ;; GSA
        ;; genetics
        "https://www.genetics.org/rss/current.xml"
        ;; G3
        "https://www.g3journal.org/rss/current.xml"

        ;; NAR
        "https://academic.oup.com/rss/site_5127/3091.xml"

        ;; PNAS
        "https://feeds.feedburner.com/Pnas-RssFeedOfEarlyEditionArticles"
        ;; pubmed cochrane database reviews
        "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/erss.cgi?rss_guid=1NSu_CQNBizymYejD9-Ot-IYbytteUrMny0SSFWm17hecDMkGM"

        ;; emacs
        ;; pragmatic emacs
        "http://pragmaticemacs.com/feed/"
        ;; planet emacs
        "https://planet.emacslife.com/atom.xml"
        )
      )



;; -----
;; bookmarks
(defun mac-bookmark-pubmed ()
  "Jump to pubmed entries."
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-pubmed"))

(defun mac-bookmark-biorxiv ()
  "Jump to pubmed entries."
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-biorxiv"))

(defun mac-bookmark-emacs ()
  "Jump to pubmed entries."
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-emacs"))


;; -----
;; multiple machine syncing functions
;; functions to support syncing .elfeed between machines
;; makes sure elfeed reads index from disk before launching
(defun bjm/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force)
  (elfeed-update))


;; write to disk when quiting
(defun bjm/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (kill-buffer "*elfeed-search*"))


;; roll into a single function
(defun mac-elfeed ()
  "Function for using elfeed."
  (interactive)
  (let ((ef (get-buffer "*elfeed-search*")))
    (if (and
         (bufferp ef)
         (eq (current-buffer) ef))
      (bjm/elfeed-save-db-and-bury)
    (bjm/elfeed-load-db-and-open))))


(defun my-fig-open ()
  (interactive)
  (org-next-link)
  (org-open-at-point))



;; -----
;; setup keys for my preference
;; in an entry
(define-key elfeed-show-mode-map (kbd "k") 'elfeed-kill-buffer)
(define-key elfeed-show-mode-map (kbd "l") 'my-fig-open)

;; in the search buffer
(define-key elfeed-search-mode-map (kbd "a") (lambda () (interactive) (mac-bookmark-pubmed)))
(define-key elfeed-search-mode-map (kbd "e") (lambda () (interactive) (mac-bookmark-emacs)))
(define-key elfeed-search-mode-map (kbd "v") (lambda () (interactive) (mac-bookmark-biorxiv)))
(define-key elfeed-search-mode-map (kbd "k") 'mac-elfeed)
