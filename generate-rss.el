;; generate-rss.el - Standalone RSS generator
(require 'ox)
(require 'org)

(defun v3rse/generate-rss-feed ()
  "Generate RSS feed from blog posts."
  (let* ((blog-dir "./content/blog/")
         (output-file "./public/feed.xml")
         (blog-files (directory-files blog-dir t "\\.org$"))
         (posts '()))
    
    ;; Collect all blog posts
    (dolist (file blog-files)
      (unless (string-match-p "\\(index\\|rss\\|feed\\)\\.org$" file)
        (with-temp-buffer
          (insert-file-contents file)
          (org-mode)
          (let* ((title (or (car (plist-get (org-export-get-environment) :title))
                           (file-name-base file)))
                 (date (or (org-publish-find-date file nil)
                          (current-time)))
                 (link (concat "https://www.nanaadane.com/blog/"
                              (file-name-sans-extension (file-name-nondirectory file))
                              ".html"))
                 (content (buffer-substring-no-properties
                          (point-min)
                          (min (+ (point-min) 500) (point-max)))))
            (push (list :title (format "%s" title)
                       :date date
                       :link link
                       :content content)
                  posts)))))
    
    ;; Sort by date
    (setq posts (sort posts
                     (lambda (a b)
                       (time-less-p (plist-get b :date)
                                   (plist-get a :date)))))
    
    ;; Generate RSS XML
    (with-temp-file output-file
      (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n")
      (insert "<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">\n")
      (insert "<channel>\n")
      (insert "  <title>Nana Adane - Blog</title>\n")
      (insert "  <link>https://www.nanaadane.com/blog/</link>\n")
      (insert "  <description>Personal blog and projects</description>\n")
      (insert "  <language>en</language>\n")
      (insert (format "  <lastBuildDate>%s</lastBuildDate>\n"
                     (format-time-string "%a, %d %b %Y %H:%M:%S %z")))
      (insert "  <atom:link href=\"https://www.nanaadane.com/feed.xml\" rel=\"self\" type=\"application/rss+xml\" />\n")
      
      ;; Add items
      (dolist (post posts)
        (insert "  <item>\n")
        (insert (format "    <title>%s</title>\n" (plist-get post :title)))
        (insert (format "    <link>%s</link>\n" (plist-get post :link)))
        (insert (format "    <guid>%s</guid>\n" (plist-get post :link)))
        (insert (format "    <pubDate>%s</pubDate>\n"
                       (format-time-string "%a, %d %b %Y %H:%M:%S %z"
                                          (plist-get post :date))))
        (insert (format "    <description><![CDATA[%s...]]></description>\n"
                       (substring (plist-get post :content) 0
                                 (min 200 (length (plist-get post :content))))))
        (insert "  </item>\n"))
      
      (insert "</channel>\n")
      (insert "</rss>\n"))
    
    (message "RSS feed generated at %s" output-file)))

(v3rse/generate-rss-feed)
