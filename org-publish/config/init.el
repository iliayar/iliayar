(message "Loading Emacs init file")

(load "/publish/packages.el")

(require 'org)
(require 'ox-rss)
(require 'python)

(org-reload)

(setq org-html-htmlize-output-type 'css)

(defun org-publish-command () (org-publish-project "mainsite"))
(defun my/org-publish (backend plist filename pub-dir)
  (with-temp-buffer 
    (insert-file-contents filename)
    (let
      ((elem (org-element-context)))
      (if (and (eq 'keyword (org-element-type elem))
               (string-equal "MANUAL" (org-element-property :key elem))
               (read (org-element-property :value elem)))
        (princ (concat filename " manually published"))
        (funcall backend plist filename pub-dir)))))

(defun my/org-html-publish-to-html (plist filename pub-dir)
  (my/org-publish 'org-html-publish-to-html plist filename pub-dir))

(setq org-publish-project-alist
      '(
        ("org-mainsite"
         :base-directory "/publish/input"
         :base-extension "org"
         :exclude "level-[0-9]*.org\\|other/orgmode.org"
         :publishing-directory "/publish/output"
         :html-html5-fancy t
         :html-validation-link nil
         :html-postamble "<hr><a href=\"/public-notes/index.html\">Home Page</a><span style=\"float: right\"><a href=\"/public-notes/blog.xml\"><i class=\"fas fa-rss\"></i></a> <a href=\"https://github.com/iliayar/iliayar\"><i class=\"fab fa-github\"></i></a></span>"
         :recursive t
         :publishing-function my/org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
         )
        ("rss-mainsite"
         :base-directory "/publish/input"
         :base-extension "org"
         :exclude ".*"
         :include ("blog.org")
         :publishing-directory "/publish/output"
         :rss-extension "xml"
         :section-numbers nil
         :html-link-home "https://iliayar.ru/public-notes/"
         :html-link-use-abs-url t
         :html-link-org-files-as-html t
         :output-file "rss"
         :recursive nil
         :publishing-function org-rss-publish-to-rss
         )
        ("static-mainsite"
         :base-directory "/publish/input"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|pdf"
         :publishing-directory "/publish/output"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("mainsite" :components ("org-mainsite" "rss-mainsite" "static-mainsite"))
        ))
