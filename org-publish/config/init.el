(message "Loading Emacs init file")

(load "/publish/packages.el")

(require 'org)
(require 'ox-latex)
(require 'ox-rss)
(require 'htmlize)
(require 'haskell-mode)

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
               (org-html-publish-to-html plist filename pub-dir)))
        (funcall backend plist filename pub-dir)))))

(defun my/org-html-publish-to-html (plist filename pub-dir)
  (my/org-publish 'org-html-publish-to-html plist filename pub-dir))
(defun my/org-latex-publish-to-pdf (plist filename pub-dir)
  (my/org-publish 'org-latex-publish-to-pdf plist filename pub-dir))

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
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|pdf\\|jpeg"
         :publishing-directory "/publish/output"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("mainsite" :components ("org-mainsite" "rss-mainsite" "static-mainsite"))
        ("static-conspects"
         :base-directory "/publish/input"
         :base-extension "png\\|jpg\\|gif\\|pdf\\|svg\\|jpeg"
         :publishing-directory "/publish/output"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("org-conspects"
         :base-directory "/publish/input"
         :exclude ".*[^E].org"
         :publishing-directory "/publish/output"
         :recursive t
         :html-postamble "<hr><a href=\"https://ilyay.space\">Home Page</a><span style=\"float: right\"><a href=\"https://t.me/iliayar\"><i class=\"fab fa-telegram-plane\"></i></a> <a href=\"https://github.com/iliayar/ITMO\"><i class=\"fab fa-github\"></i></a></span><br><a href=\"https://conspects.ilyay.space/README.html\">Conspects Home Page</a>"
         :publishing-function my/org-html-publish-to-html
         :headline-levels 4
         ) 
        ("pdfs-conspects"
         :base-directory "/publish/input"
         :base-extension "org"
         :exclude "README.org\\|level-[0-9]*.org\\|level-subj.org"
         :publishing-directory "/publish/output"
         :recursive t
         :publishing-function my/org-latex-publish-to-pdf
         )
        ("conspects" :components ("static-conspects" "org-conspects" "pdfs-conspects"))
        ))
