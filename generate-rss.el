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
          (goto-char (point-min))
          
          ;; Skip all metadata (#+TITLE, #+DATE, #+CAPTION, etc.)
          (while (or (looking-at "^#\\+")
                    (looking-at "^[ \t]*$"))
            (forward-line 1))
          
          (let* ((title (or (car (plist-get (org-export-get-environment) :title))
                           (file-name-base file)))
                 (date (or (org-publish-find-date file nil)
                          (current-time)))
                 (link (concat "https://www.nanaadane.com/blog/"
                              (file-name-sans-extension (file-name-nondirectory file))
                              ".html"))
                 ;; Extract first meaningful text content
                 (raw-content
                  (save-excursion
                    (let ((content "")
                          (found nil)
                          (max-search 50))  ; Search through up to 50 lines
                      ;; Look through content until we find a paragraph
                      (while (and (not found) (not (eobp)) (> max-search 0))
                        (setq max-search (1- max-search))
                        (cond
                         ;; Skip headings but continue
                         ((looking-at "^\\*+ ")
                          (forward-line 1))
                         ;; Skip properties drawers
                         ((looking-at "^[ \t]*:PROPERTIES:")
                          (or (re-search-forward "^[ \t]*:END:" nil t)
                              (forward-line 1))
                          (forward-line 1))
                         ;; Skip blank lines
                         ((looking-at "^[ \t]*$")
                          (forward-line 1))
                         ;; Skip code blocks
                         ((looking-at "^[ \t]*#\\+BEGIN")
                          (or (re-search-forward "^[ \t]*#\\+END" nil t)
                              (forward-line 1))
                          (forward-line 1))
                         ;; Skip other org directives and links
                         ((or (looking-at "^[ \t]*#\\+")
                              (looking-at "^[ \t]*\\[\\["))
                          (forward-line 1))
                         ;; Found actual content!
                         (t
                          (let ((start (point))
                                (end (save-excursion
                                       (or (and (re-search-forward "^\\*\\|^[ \t]*#\\+\\|^[ \t]*:" nil t)
                                               (match-beginning 0))
                                           (point-max)))))
                            (setq content (buffer-substring-no-properties start end)
                                  found t)))))
                      content)))
                 ;; Clean up org syntax
                 (clean-content (replace-regexp-in-string
                                "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1"  ; Links with text
                                (replace-regexp-in-string
                                 "\\[\\[.*?\\]\\]" ""  ; Image/file links without text
                                 (replace-regexp-in-string
                                  "~\\([^~]+\\)~" "\\1"  ; Inline code
                                  (replace-regexp-in-string
                                   "=\\([^=]+\\)=" "\\1"  ; Verbatim
                                   (replace-regexp-in-string
                                    "\\*\\([^*\n]+\\)\\*" "\\1"  ; Bold
                                    (replace-regexp-in-string
                                     "/\\([^/\n]+\\)/" "\\1"  ; Italic
                                     raw-content))))))))
            (push (list :title (format "%s" title)
                       :date date
                       :link link
                       :content (string-trim clean-content))
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
        (let ((desc (plist-get post :content)))
          (when (> (length desc) 200)
            (setq desc (concat (substring desc 0 200) "...")))
          (insert (format "    <description><![CDATA[%s]]></description>\n" desc)))
        (insert "  </item>\n"))
      
      (insert "</channel>\n")
      (insert "</rss>\n"))
    
    (message "RSS feed generated at %s" output-file)))

(v3rse/generate-rss-feed)
