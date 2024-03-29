(message "Loading Emacs init file")

(load "/publish/packages.el")

(require 'org)
(require 'ox-latex)
(require 'htmlize)
(require 'haskell-mode)
(require 'ox-iliayar-html)
(require 'ox-rss)

(org-reload)

(setq org-html-inline-image-rules ' (("file" . #1="\\(?:\\.\\(?:gif\\|\\(?:jpe?\\|pn\\|sv\\)g\\)\\)")
                                     ("http" . #1#)
                                     ("https" . #1#)))


(setq org-html-htmlize-output-type 'css)
(setq org-html-htmlize-font-prefix "org-")

(setq org-latex-packages-alist '(
                                 ("T1, T2A" "fontenc" t)
                                 ("lutf8" "luainputenc" t)
                                 ("english,russian" "babel" t)
                                 ("" "minted" t)
                                 ("" "graphicx" t)
                                 ("" "longtable" t)
                                 ("" "hyperref" t)
                                 ("" "xcolor" t)
                                 ("" "natbib" t)
                                 ("" "amssymb" t)
                                 ("" "stmaryrd" t)
                                 ("" "amsmath" t)
                                 ("" "caption" t)
                                 ("" "mathtools" t)
                                 ("" "amsthm" t)
                                 ("" "tikz" t)
                                 ("" "fancyhdr" t)
                                 ("" "lastpage" t)
                                 ("" "titling" t)
                                 ("" "grffile" t)
                                 ("" "extarrows" t)
                                 ("" "wrapfig" t)
                                 ("" "algorithm" t)
                                 ("" "algorithmic" t)
                                 ("" "lipsum" t)
                                 ("" "rotating" t)
                                 ("" "placeins" t)
                                 ("normalem" "ulem" t)
                                 ("" "amsmath" t)
                                 ("" "textcomp" t)
                                 ("" "svg" t)
                                 ("" "capt-of" t)))
(with-eval-after-load 'ox-latex
  (progn
    (add-to-list 'org-latex-classes
                 (list "general"
                       "
  \\documentclass[english]{article}
  [NO-DEFAULT-PACKAGES]
  [PACKAGES]
  [EXTRA]
  \\usepackage{geometry}
  \\geometry{a4paper,left=2.5cm,top=2cm,right=2.5cm,bottom=2cm,marginparsep=7pt, marginparwidth=.6in}
  \\input{/publish/config/preamble.sty}
  "
                       '("\\section{%s}" . "\\section*{%s}")
                       '("\\subsection{%s}" . "\\subsection*{%s}")
                       '("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                       '("\\paragraph{%s}" . "\\paragraph*{%s}")
                       '("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                       ))
    (add-to-list 'org-latex-classes
                 (list "lectures"
                       "
  \\documentclass[oneside]{book}
  [NO-DEFAULT-PACKAGES]
  [PACKAGES]
  [EXTRA]
  \\addto\\captionsrussian{\\renewcommand{\\chaptername}{Лекция}}
  \\renewcommand{\\leftmark}{}
  \\usepackage[a4paper, total={6in, 8in}]{geometry}
  \\input{/publish/config/preamble.sty}
  \\fancyhead[L]{\\leftmark}
  "
                       '("\\chapter*{%1$s}\\renewcommand{\\leftmark}{%1$s}\\addcontentsline{toc}{chapter}{%1$s}\\stepcounter{chapter}" . "\\chapter{%s}")
                       '("\\section{%s}" . "\\section*{%s}")
                       '("\\subsection{%s}" . "\\subsection*{%s}")
                       '("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                       '("\\paragraph{%s}" . "\\paragraph*{%s}")
                       '("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                       ))))
(setq org-latex-listings 'minted
      org-latex-pdf-process
      '("pdflatex -shell-escape --synctex=1 -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape --synctex=1 -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape --synctex=1 -interaction nonstopmode -output-directory %o %f"))
(setq org-latex-minted-options
      '(("frame" "lines") ("linenos=true") ("mathescape")))
(add-to-list 'org-latex-minted-langs '(ipython "python"))


(defun my/org-publish (backend plist filename pub-dir)
  (with-temp-buffer
    (insert-file-contents filename)
    (let
		((elem (org-element-context)))
      (if (and (eq 'keyword (org-element-type elem))
               (string-equal "PUBNOTE" (org-element-property :key elem)))
          (cond ((string-equal "ignore" (org-element-property :value elem))
				 (princ (concat filename " manually published")))
				((string-equal "html" (org-element-property :value elem))
				 (iliayar/org-publish-to-html plist filename pub-dir)))
        (funcall backend plist filename pub-dir)))))

(defun my/org-html-publish-to-html (plist filename pub-dir)
  (my/org-publish 'iliayar/org-publish-to-html plist filename pub-dir))
(defun my/org-latex-publish-to-pdf (plist filename pub-dir)
  (my/org-publish 'org-latex-publish-to-pdf plist filename pub-dir))

(setq org-rss-use-entry-url-as-guid nil)

(setq hostname (getenv "MAINSITE_HOSTNAME"))
(setq base-url (getenv "MAINSITE_BASE_URL"))
(setq res-base-url (getenv "MAINSITE_RES_BASE_URL"))
(setq matomo-host (getenv "MAINSITE_MATOMO_HOST"))

(setq conspects-hostname (getenv "CONSPECTS_HOSTNAME"))
(setq conspects-base-url (getenv "CONSPECTS_BASE_URL"))
(setq conspects-res-base-url (getenv "CONSPECTS_RES_BASE_URL"))
(setq conspects-matomo-host (getenv "CONSPECTS_MATOMO_HOST"))

(setq org-publish-project-alist
      `(
        ("rss-mainsite"
         :base-directory "/publish/input"
         :base-extension "org"
         :recursive t
		 :html-link-home ,base-url
		 :html-link-use-abs-url t
		 :rss-extension "xml"
         :publishing-directory "/publish/output"
         :publishing-function (org-rss-publish-to-rss)
		 :section-number nil
		 :exclude ".*"
		 :include ("rss.org")
		 :table-of-contents nil
         )

        ("org-mainsite"
         :base-directory "/publish/input"
         :base-extension "org"
         :recursive t
         :publishing-directory "/publish/output"
         :publishing-function iliayar/org-publish-to-html
		 :html-links-template "links/main"
		 :html-res-base-url ,res-base-url
		 :html-base-url ,base-url
		 :html-base-title ,hostname
		 :html-matomo-host ,matomo-host
         )
        ("static-mainsite"
         :base-directory "/publish/input"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|svg\\|mp3\\|ogg\\|swf\\|pdf\\|jpeg\\|ico"
         :publishing-directory "/publish/output"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("mainsite" :components ("static-mainsite" "org-mainsite" "rss-mainsite"))

        ("static-conspects"
         :base-directory "/publish/input"
         :base-extension "png\\|jpg\\|gif\\|pdf\\|svg\\|jpeg"
         :publishing-directory "/publish/output"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("org-conspects"
         :base-directory "/publish/input"
         :exclude ".*[^E].org\\|.*[^M]..org"
         :publishing-directory "/publish/output"
         :recursive t
         :publishing-function my/org-html-publish-to-html
         :headline-levels 4
		 :html-res-base-url ,conspects-res-base-url
		 :html-base-url ,conspects-base-url
		 :html-base-title ,conspects-hostname
		 :html-matomo-host ,conspects-matomo-host
         )
        ("pdfs-conspects"
         :base-directory "/publish/input"
         :base-extension "org"
         :exclude "README.org\\|level-[0-9]*.org\\|level-subj.org"
         :publishing-directory "/publish/output"
         :recursive t
         :publishing-function my/org-latex-publish-to-pdf

		 ;; Also publish htmls
		 :html-res-base-url ,conspects-res-base-url
		 :html-base-url ,conspects-base-url
		 :html-base-title ,conspects-hostname
		 :html-matomo-host ,conspects-matomo-host
         )
        ("conspects" :components ("static-conspects" "org-conspects" "pdfs-conspects"))
        ))
