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
                     <link rel=\"stylesheet\" href=\"https://fonts.googleapis.com/css2?family=Inter:wght@400;700&family=JetBrains+Mono:wght@400;700&family=Press+Start+2P&display=swap\"/>
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

(org-publish-all t)

;; Generate RSS feed manually
(load-file "./generate-rss.el")

(message "Build complete!")
