(require 'use-package)  

(use-package haskell-mode)

(require 'org)
(require 'ox)
(require 'ox-latex)

(setq preamble-path (getenv "PREAMBLE_PATH"))

(setq org-latex-packages-alist '(
                                 ("T1, T2A" "fontenc" t)
                                 ("lutf8" "luainputenc" t)
                                 ;; ("english,russian" "babel" t)
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
                       (format "
  \\documentclass[english]{article}
  [NO-DEFAULT-PACKAGES]
  [PACKAGES]
  [EXTRA]
  \\usepackage{geometry}
  \\geometry{a4paper,left=2.5cm,top=2cm,right=2.5cm,bottom=2cm,marginparsep=7pt, marginparwidth=.6in}
  \\input{%s}
" preamble-path)
                       '("\\section{%s}" . "\\section*{%s}")
                       '("\\subsection{%s}" . "\\subsection*{%s}")
                       '("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                       '("\\paragraph{%s}" . "\\paragraph*{%s}")
                       '("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                       ))
    (add-to-list 'org-latex-classes
                 (list "lectures"
                       (format "
  \\documentclass[oneside]{book}
  [NO-DEFAULT-PACKAGES]
  [PACKAGES]
  [EXTRA]
  \\addto\\captionsrussian{\\renewcommand{\\chaptername}{Лекция}}
  \\renewcommand{\\leftmark}{}
  \\usepackage[a4paper, total={6in, 8in}]{geometry}
  \\input{%s}
  \\fancyhead[L]{\\leftmark}
  "                      preamble-path)
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
