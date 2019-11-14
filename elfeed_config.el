;; -----
;; use-package setup; recommended to use curl w/ elfeed, so set that here
(use-package elfeed
  :ensure t
  :config
  (elfeed-update)
  (setq elfeed-curl-program-name "curl"))

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.elfeed/mac-elfeed.org")))


;; -----
;; appearance customization
(setq elfeed-search-title-max-width 140)



;; -----
;; set up some feeds to follow
(setq elfeed-feeds
      '(
        ;; various biorxiv feeds
        "http://connect.biorxiv.org/biorxiv_xml.php?subject=cell_biology"
        "http://connect.biorxiv.org/biorxiv_xml.php?subject=genomics"
        "http://connect.biorxiv.org/biorxiv_xml.php?subject=genetics"
        "http://connect.biorxiv.org/biorxiv_xml.php?subject=synthetic_biology"

        ;; pubmed 
        ;; pubmed nature
        "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/erss.cgi?rss_guid=1x9bY_ZPGMI-WNok46K5sa8KJdUXTERyDGcY8TAD38LR7hUgwB"
        ;; pubmed science
        "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/erss.cgi?rss_guid=1xu-3vENN2MKkGMHLZb2Uk0XFeWKZx2DWu3hOsQW5yS9REKYLw"
        ;; pubmed cell
        "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/erss.cgi?rss_guid=16uwQpOeqFYN8R3vULzEZgJuhluycOZ2QFpa-rdy6y4QvrBcSe"
        ;; pubmed nature biotechnology
        "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/erss.cgi?rss_guid=1NGmwZeh8JwYIxl-_piMmRRmNM2TRIjibYZkg7eYTLM5bw6sf-"
        ;; pubmed cochrane database reviews
        "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/erss.cgi?rss_guid=1NSu_CQNBizymYejD9-Ot-IYbytteUrMny0SSFWm17hecDMkGM"
        ;; pubmed nature genetics
        "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/erss.cgi?rss_guid=1LqKQ9r8nlqaZrQnKP1OLOGM3wrZIINX8VAHI1t_4hCUVa95FY"
        ;; pubmed proc natl acad sci
        "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/erss.cgi?rss_guid=1lkZ0SIS2BVLgpHkq-EtqYtgV9UCXuW3rUKYQAM8ule9KDakh4"
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

(defun mac-bookmark-rxiv ()
  "Jump to pubmed entries."
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-rxiv"))

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
  (elfeed-search-update--force))


;; write to disk when quiting
(defun bjm/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (elfeed-kill-buffer))


;; roll into a single function
(defun mac-elfeed ()
  "Function for using elfeed."
  (interactive)
  (if (bufferp (get-buffer "*elfeed-search*"))
      (bjm/elfeed-save-db-and-bury)
    (bjm/elfeed-load-db-and-open)))



;; -----
;; setup keys for my preference
;; in an entry
(define-key elfeed-show-mode-map (kbd "k") 'elfeed-kill-buffer)

;; in the search buffer
(define-key elfeed-search-mode-map (kbd "a") (lambda () (interactive) (mac-bookmark-pubmed)))
(define-key elfeed-search-mode-map (kbd "e") (lambda () (interactive) (mac-bookmark-rxiv)))
(define-key elfeed-search-mode-map (kbd "z") (lambda () (interactive) (mac-bookmark-emacs)))
(define-key elfeed-search-mode-map (kbd "k") 'mac-elfeed)
