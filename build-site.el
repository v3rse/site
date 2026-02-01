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
                     <link rel=\"stylesheet\" href=\"https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;700&family=ET+Book:wght@400;700&display=swap\"/>"
      org-html-preamble (v3rse/get-content "./header.html")
      org-html-postamble (v3rse/get-content "./footer.html"))

;; Sitemap Function (Chronological Blog Feed)
(defun v3rse/blog-sitemap-format-entry (entry style project)
  "Format a blog entry for the sitemap."
  (let ((title (org-publish-find-title entry project))
        (date (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))))
    (format "@@html:<div class=\"archive-item\"><span class=\"archive-date\">%s</span> <a href=\"%s\">%s</a></div>@@"
            date entry title)))

(defun v3rse/blog-sitemap-function (title list)
  "Generate the sitemap (Blog Index)."
  (concat "#+TITLE: Blog\n\n"
          (org-list-to-org list)))

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
             :sitemap-format-entry 'v3rse/blog-sitemap-format-entry
             :sitemap-function 'v3rse/blog-sitemap-function)

       ;; 3. RSS Feed
       (list "rss"
             :base-directory "./content/blog"
             :base-extension "org"
             :publishing-directory "./public"
             :publishing-function 'org-rss-publish-to-rss
             :rss-extension "xml"
             :html-link-home "https://www.nanaadane.com"
             :html-link-use-abs-url t
             :auto-sitemap t
             :sitemap-filename "feed.org"
             :sitemap-title "Nana Adane's Blog"
             :sitemap-style 'list
             :sitemap-sort-files 'anti-chronologically)

       ;; 4. Assets
       (list "static"
             :base-directory "./content/links"
             :base-extension "css\\|js"
             :publishing-directory "./public/links"
             :publishing-function 'org-publish-attachment)
       
       (list "media"
             :base-directory "./content/media"
             :base-extension "png\\|jpg\\|gif"
             :publishing-directory "./public/media"
             :publishing-function 'org-publish-attachment)

       (list "site" :components '("pages" "blog" "rss" "static" "media"))))

(org-publish-all t)
(message "Build complete!")
