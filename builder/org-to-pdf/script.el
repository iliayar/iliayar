(defun cli--get-extra-args (args)
  (if (string= (car args) "--")
     (cdr args)
     (cli--get-extra-args (cdr args))))

(defun cli-get-extra-args ()
  (cli--get-extra-args command-line-args))

(defun cli--get-opts-assoc (args)
  (if (= (length args) 0) (list)
    (let* ((arg-full (car args))
           (arg (intern (substring arg-full 2))))

      (if (not (string-prefix-p "--" arg-full)) 
        (error (format "Invalid flag: %s" arg-full)))

      (if (= (length args) 1) 
        (list arg t)
        (let* ((value-full (car (cdr args)))
               (value (substring value-full 2)))
            (if (string-prefix-p "--" value-full)
              (cons (list arg  t) (cli--get-opts-assoc (cdr args)))
              (cons (list arg value-full) (cli--get-opts-assoc (cdr (cdr args))))))))))

(defun cli-get-opts-assoc () 
    (cli--get-opts-assoc (cli-get-extra-args)))

(setq opts (cli-get-opts-assoc))

(defun getopt (symb)
  (let* ((item (assoc symb opts)))
    (if (not item) (error (format "Missing arg: %s" symb)))
    (car (cdr item))))

(defun org-to-pdf ()
  (let* ((infile (expand-file-name (getopt 'input)))
         (outfile (expand-file-name (getopt 'output)))
         (comp-dir (make-temp-file "comp" t))
         (texfile (concat comp-dir "/" "output.tex"))
         (outfile-tmp (concat comp-dir "/" "output.pdf"))
         (async nil)
         (subtreep nil)
         (visible-only nil)
         (body-only nil)
         (ext-plist nil))

    (setq org-latex-pdf-process
      (let* ((command 
               (format "pdflatex -shell-escape --synctex=1 -interaction nonstopmode -output-directory %s %%f"
                 comp-dir)))
        (list command command command)))

    (cd (file-name-directory infile))
    
    (find-file infile)
    (org-mode)
    (org-export-to-file 'latex texfile
      async subtreep visible-only body-only ext-plist #'org-latex-compile)

    (copy-file outfile-tmp outfile 'ignore-exists)
    (delete-directory comp-dir 'recursive)))

(org-to-pdf)
