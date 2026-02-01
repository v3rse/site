;; build-site.el
(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'htmlize) (package-install 'htmlize))
(unless (package-installed-p 'ox-rss) (package-install 'ox-rss))

(require 'ox-publish)
(require 'ox-rss)

(defun v3rse/get-content (x)
  (with-temp-buffer
    (insert-file-contents x)
    (buffer-string)))

;; Theme and Headers
(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-head "<link rel=\"stylesheet\" href=\"/links/style.css\"/>
                     <link rel=\"stylesheet\" href=\"https://fonts.googleapis.com/css2?family=Inter:wght@400;700&family=JetBrains+Mono:wght@400;700&family=Pixelify+Sans:wght@400;600;700&display=swap\"/>
                     <link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/Iosevka/6.0.0/iosevka-etoile/iosevka-etoile.min.css\"/>
                     <link rel=\"icon\" href=\"/favicon.svg\" type=\"image/svg+xml\"/>"
      org-html-preamble (v3rse/get-content "./header.html")
      org-html-postamble (v3rse/get-content "./footer.html"))

;; Simple sitemap formatter for blog index
(defun v3rse/blog-sitemap-format (entry style project)
  "Format sitemap ENTRY for the blog."
  (format "%s - [[file:%s][%s]]"
          (format-time-string "%Y-%m-%d"
                              (org-publish-find-date entry project))
          entry
          (org-publish-find-title entry project)))

;; Dynamic block for recent blogs on index page
(defun org-dblock-write:recent-blogs (params)
  "Generate a list of recent blog posts."
  (let* ((maxitems (or (plist-get params :maxitems) 5))
         (blog-dir "./content/blog/")
         (blog-files (directory-files blog-dir t "\\.org$"))
         (posts '()))
    
    ;; Collect blog posts with dates
    (dolist (file blog-files)
      (unless (string-match-p "\\(index\\|rss\\|feed\\)\\.org$" file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let* ((title "Untitled")
                 (date (current-time)))
            ;; Extract title
            (when (re-search-forward "^#\\+TITLE: \\(.+\\)$" nil t)
              (setq title (match-string 1)))
            ;; Extract date
            (goto-char (point-min))
            (when (re-search-forward "^#\\+DATE: <\\([^>]+\\)>" nil t)
              (setq date (org-parse-time-string (match-string 1))))
            (push (list :date (apply 'encode-time date)
                       :title title
                       :link (concat "blog/" (file-name-nondirectory file)))
                  posts)))))
    
    ;; Sort by date and take maxitems
    (setq posts (seq-take 
                 (sort posts (lambda (a b) 
                              (time-less-p (plist-get b :date) 
                                          (plist-get a :date))))
                 maxitems))
    
    ;; Insert the list
    (dolist (post posts)
      (insert (format "- %s - [[file:%s][%s]]\n"
                     (format-time-string "%Y-%m-%d" (plist-get post :date))
                     (plist-get post :link)
                     (plist-get post :title))))))



;; Project Configuration
(setq org-publish-project-alist
      (list
       ;; 1. Pages (Root)
       (list "pages"
             :base-directory "./content"
             :base-extension "org"
             :exclude "blog/"
             :publishing-directory "./public"
             :publishing-function 'org-html-publish-to-html
             :with-author nil
             :with-creator nil
             :with-toc nil
             :section-numbers nil
             :time-stamp-file nil)

       ;; 2. Blog Posts (HTML)
       (list "blog"
             :base-directory "./content/blog"
             :base-extension "org"
             :exclude "feed.org\\|index.org\\|rss.org"
             :publishing-directory "./public/blog"
             :publishing-function 'org-html-publish-to-html
             :with-author nil
             :with-creator nil
             :with-toc nil
             :section-numbers nil
             :time-stamp-file nil
             :auto-sitemap t
             :sitemap-filename "index.org"
             :sitemap-title "Blog"
             :sitemap-sort-files 'anti-chronologically
             :sitemap-format-entry 'v3rse/blog-sitemap-format)

       ;; 3. RSS Feed - Note: We generate this manually in generate-rss.el
       ;; No longer using ox-rss sitemap approach

       ;; 4. Assets
       (list "static"
             :base-directory "./content/links"
             :base-extension "css\\|js"
             :publishing-directory "./public/links"
             :publishing-function 'org-publish-attachment)
       
       (list "favicon"
             :base-directory "./content"
             :base-extension "svg\\|ico"
             :publishing-directory "./public"
             :publishing-function 'org-publish-attachment)
       
       (list "media"
             :base-directory "./content/media"
             :base-extension "png\\|jpg\\|gif"
             :publishing-directory "./public/media"
             :publishing-function 'org-publish-attachment)

       (list "site" :components '("pages" "blog" "static" "media" "favicon"))))

;; Generate recent blog list for index page
(let* ((blog-dir "./content/blog/")
       (blog-files (directory-files blog-dir t "\\.org$"))
       (posts '())
       (recent-list ""))
  
  ;; Collect blog posts
  (dolist (file blog-files)
    (unless (string-match-p "\\(index\\|rss\\|feed\\)\\.org$" file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((title "Untitled")
              (date (current-time)))
          (when (re-search-forward "^#\\+TITLE: \\(.+\\)$" nil t)
            (setq title (match-string 1)))
          (goto-char (point-min))
          (when (re-search-forward "^#\\+DATE: <\\([^>]+\\)>" nil t)
            (condition-case nil
                (setq date (apply 'encode-time (org-parse-time-string (match-string 1))))
              (error nil)))
          (push (list date title (file-name-nondirectory file)) posts)))))
  
  ;; Sort and take top 5
  (setq posts (seq-take (sort posts (lambda (a b) (time-less-p (nth 0 b) (nth 0 a)))) 5))
  
  ;; Build list string
  (dolist (post posts)
    (setq recent-list 
          (concat recent-list
                  (format "- %s - [[file:blog/%s][%s]]\n"
                         (format-time-string "%Y-%m-%d" (nth 0 post))
                         (nth 2 post)
                         (nth 1 post)))))
  
  ;; Update index.org
  (with-temp-file "./content/index.org"
    (insert-file-contents "./content/index.org")
    (goto-char (point-min))
    (when (re-search-forward "^#\\+BEGIN: recent-blogs.*$" nil t)
      (forward-line 1)
      (let ((start (point)))
        (re-search-forward "^#\\+END:" nil t)
        (beginning-of-line)
        (delete-region start (point)))
      (insert recent-list))))

(org-publish-all t)

;; Generate RSS feed manually
(load-file "./generate-rss.el")

(message "Build complete!")
