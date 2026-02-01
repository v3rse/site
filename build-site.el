;;; Only need for fixing fontification of code blocks since htmlize >= 1.34 required
(require 'package)

;; local package directory not global 
(setq package-user-dir (expand-file-name "./.packages"))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(package-install 'htmlize)

;;; Regular programming continues...
(require 'ox-publish)

(defun v3rse/get-content (x)
  "get contents of a file as a string"
  (with-temp-buffer
    (insert-file-contents x)
    (buffer-string)))

(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-head "<link rel=\"stylesheet\" href=\"links/main.css\"/>"
      org-html-preamble (v3rse/get-content "./header.html")
      org-html-postamble (v3rse/get-content "./footer.html"))

(setq org-publish-project-alist
      (list
       (list "pages"
	     :recursive t
	     :base-directory "./content"
	     :publishing-directory "./public"
	     :publishing-function 'org-html-publish-to-html
	     :creator "Nana Adane © 2025"
	     :with-author nil
	     :with-creator t
	     :with-toc nil
	     :section-numbers nil
	     :time-stamp-file nil
	     :auto-sitemap t
	     :sitemap-filename "meta.org"
	     :sitemap-title "Meta"
	     :sitemap-sort-files 'anti-chronologically)
       (list "media"
         :base-directory "./content/media"
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory "./public/media"
         :publishing-function 'org-publish-attachment)
       (list "links"
	  :base-directory "./content/links"
         :base-extension "css\\|js"
         :publishing-directory "./public/links"
         :publishing-function 'org-publish-attachment)
       (list "site"
	     :components (list "pages" "media" "links"))
       ))

(copy-file "./content/CNAME" "./public/" t)

;; regenerate all
(org-publish-all t)

(message "Build complete!")
