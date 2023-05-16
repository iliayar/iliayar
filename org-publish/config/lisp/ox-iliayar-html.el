(require 'templatel)
(require 's)

(defconst templates-dir (concat (file-name-directory load-file-name) "/templates"))

(defun unhtml (string)
  (s-replace-all
   '(("&" . "&amp;")
     ("<" . "&lt;")
     (">" . "&gt"))
   string))

(defun add-child-to-top-headline (entry-stack child)
  (let* ((entry (car entry-stack))
	 (childs (plist-get entry :childs))
	 (headline (plist-get entry :headline))
	 (new-childs (cons child childs))
	 (new-entry `(:headline ,headline :childs ,new-childs))
	 (new-entry-stack (cons new-entry (cdr entry-stack))))
    new-entry-stack))

(defun squeeze-headlines (n headlines-stack)
  (let ((i n)
	(stack headlines-stack))
    (while (not (= i 0))
      (setq stack
	    (add-child-to-top-headline (cdr stack) (car stack)))
      (setq i (- i 1)))
    stack))

(defun collect-headlines (headlines)
  (let* ((current-level 0)
	 (stack '((:headline nil :childs ()))))
    (mapcar
     (lambda (entry)
       (let* ((headline (nth 1 entry))
	      (level (plist-get headline :level)))
	 (if (< current-level level)
	     (progn
	       (setq current-level (+ current-level 1))
	       (if (not (= current-level level))
		   (error (format
			   "Expect child header to be exact 1 further: got %d and %d"
			   current-level level))))

	   (progn
	     (let ((new-stack
		    (squeeze-headlines (+ 1 (- current-level level)) stack)))
	       (setq stack new-stack))
	     (setq current-level level)))

	 (let* ((new-headline `(:headline ,headline :childs ()))
		(new-stack (cons new-headline stack)))
	   (setq stack new-stack))))
     headlines)
    (car (squeeze-headlines current-level stack))))

(defun org-html--get-template-file (template-name)
  (expand-file-name (concat template-name ".html.jinja") templates-dir))

(defun iliayar/org-template (contents info)
  (templatel-render-file (org-html--get-template-file "template")
			 `(("contents" . ,contents))))

(defun iliayar/make-toc-rec (node)
  (let* ((headline (plist-get node :headline))
	 (childs (reverse (plist-get node :childs)))
	 (title (plist-get headline :raw-value))
	 (rendered-childs (mapcar #'iliayar/make-toc-rec childs)))
    (templatel-render-file
     (org-html--get-template-file "toc/node")
     `(("title" . ,title)
       ("childs" . ,rendered-childs)))))

(defun iliayar/make-toc (info depth)
  (iliayar/make-toc-rec
   (collect-headlines
    (org-export-collect-headlines info depth nil))))

(defun iliayar/org-inner-template (contents info)
  (let* ((depth (plist-get info :with-toc))
	 (toc (if depth (iliayar/make-toc info depth))))
    (templatel-render-file
     (org-html--get-template-file "inner-template")
     `(("contents" . ,contents)
       ("toc" . ,toc))
     :autoescape t)))

(provide 'ox-iliayar-html)

;;;###autoload
(defun iliayar/org-publish-to-html (plist filename pub-dir)
  (org-publish-org-to 'iliayar-html filename
		      (concat (when (> (length org-html-extension) 0) ".")
			      (or (plist-get plist :html-extension)
				  org-html-extension
				  "html"))
		      plist pub-dir))

(org-export-define-derived-backend
 'iliayar-html 'html
 :translate-alist '((template . iliayar/org-template)
		    (inner-template . iliayar/org-inner-template)))
