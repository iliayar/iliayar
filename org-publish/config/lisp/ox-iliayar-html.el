(require 'templatel)
(require 's)
(require 'cl)

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

	 (let* ((new-headline `(:headline ,entry :childs ()))
		(new-stack (cons new-headline stack)))
	   (setq stack new-stack))))
     headlines)
    (car (squeeze-headlines current-level stack))))

(defun use-template (filename vars)
  (templatel-render-file
   (org-html--get-template-file filename)
   vars))

(defun org-html--get-template-file (template-name)
  (expand-file-name (concat template-name ".html.jinja") templates-dir))

;; Renderers

(defun iliayar/org-checkbox (checkbox)
  (let* ((state (pcase checkbox
		  ('on "on")
		  ('off "off")
		  ('trans "trans"))))
    (use-template "list/checkbox" `(("state" . ,state)))))

(defun iliayar/org-item (item contents info)
  (let* ((checkbox (org-element-property :checkbox item))
	 (checkbox-html (if checkbox (iliayar/org-checkbox checkbox)))
	 (list (org-export-get-parent item))
	 (type (org-element-property :type list))
	 (tag (let ((tag (org-element-property :tag item)))
		(if tag (org-export-data tag info))))
	 (counter (org-element-property :counter item))
	 (tag-html (or tag counter))
	 (template (pcase type
		     ('ordered "list/item-ordered")
		     ('unordered "list/item-unordered")
		     ('descriptive "list/item-descriptive"))))
    (use-template template `(("contents" . ,contents)
			     ("checkbox" . ,checkbox-html)
			     ("tag" . ,tag-html)))))

(defun iliayar/org-plain-list (plain-list contents info)
  (pcase (org-element-property :type plain-list)
    (`ordered (use-template "list/ordered" `(("contents" . ,contents))))
    (`unordered (use-template "list/unordered" `(("contents" . ,contents))))
    (`descriptive (use-template "list/descriptive" `(("contents" . ,contents))))))

(defun iliayar/org-format-code (code refs num-start)
  (let ((res '()))
	(org-export-format-code
	 code
	 (lambda (loc line-num ref)
	   (push
		`(("content" . ,loc)
		  ("num" . ,line-num)
		  ("ref" . ,ref))
		res)
	   "")
	 num-start refs)
	(reverse res)))

(defun iliayar/org-src-block (src-block contents info)
  (let* ((language (org-element-property :language src-block))
	 (code-info (org-export-unravel-code src-block))
	 (code (car code-info))
	 (refs (cdr code-info))
	 (safe-code (unhtml code))
	 (number-lines (org-element-property :number-lines src-block))
	 (number-lines-t (car number-lines))
	 (number-lines-v (cdr number-lines))
	 (num-start (case number-lines-t
		      (continued (org-export-get-loc src-block info))
		      (new number-lines-v)))
	 (code-lines (iliayar/org-format-code safe-code refs num-start))
	 (caption (org-export-get-caption src-block))
	 (caption (when caption (org-trim (org-export-data caption info)))))
    (cond ((equal language "mermaid")
	   (use-template "mermaid"
			 `(("code" . ,safe-code))))
	  (t (use-template "src-block-new"
			   `(("code_lines" . ,code-lines)
			     ("has_lines" . ,num-start)
			     ("lang" . ,language)
			     ("caption" . ,caption)))))))

(defun iliayar/org-inline-src-block (inline-src-block contents info)
  (let* ((language (org-element-property :language inline-src-block))
	 (code-text (org-element-property :value inline-src-block)))
    (use-template "inline-src-block" `(("code" . ,code-text)))))

(defun iliayar/org-plain-text (contents info) contents)

(defun iliayar/org-section (section contents info)
  (let* ()
    (use-template "section"
				  `(("contents" . ,contents)))))

(defun iliayar/org-paragraph (paragraph contents info)
  (let* ((parent (org-export-get-parent paragraph))
	 (parent-type (org-element-type parent))
	 (item-parent (equal parent-type 'item))
	 (footnote-parent (equal parent-type 'footnote-definition))
	 (first? (not (org-export-get-previous-element paragraph info)))
	 (list-paragraph (and item-parent first?))
	 (footnote-paragraph (and footnote-parent first?))
	 (not-paragraph? (or list-paragraph footnote-paragraph)))
    (if not-paragraph? contents
      (use-template "paragraph"
		    `(("contents" . ,contents))))))

(defun make-todo (todo-keyword)
  (let ((keyword (pcase todo-keyword
		   ('todo "todo"))))
    (if keyword
	(use-template "headline-todo"
		      `(("keyword" . ,keyword))))))

(defun iliayar/org-headline (headline contents info)
  (let* ((title (org-export-data (org-element-property :title headline) info))
		 (reference (org-export-get-reference headline info))
		 (level (org-element-property :level headline))
		 (prefix (s-repeat level "#"))
		 (todo-keyword (org-element-property :todo-type headline))
		 (todo-content (if todo-keyword (make-todo todo-keyword)))
		 (cut (org-element-property :CUT headline))
		 (template-name
		  (cond
		   (cut "headline-cut")
		   (t "headline"))))
	(use-template template-name
				  `(("title" . ,title)
					("ref" . ,reference)
					("level" . ,level)
					("contents" . ,contents)
					("prefix" . ,prefix)
					("todo_keyword" . ,todo-content)))))

(defun iliayar/org-template (contents info)
  (let* ((title-raw (plist-get info :title))
	 (title (if title-raw (org-export-data title-raw info)))
	 (base-url (plist-get info :html-base-url))
	 (res-base-url (plist-get info :html-res-base-url))
	 (matomo-host (plist-get info :html-matomo-host))
	 (base-title (plist-get info :html-base-title)))
    (use-template "template"
		  `(("contents" . ,contents)
		    ("title" . ,title)
		    ("base_url" . ,base-url)
		    ("res_base_url" . ,res-base-url)
		    ("matomo_host" . ,matomo-host)
		    ("base_title" . ,base-title)))))

(defun iliayar/make-toc-rec (node info)
  (let* ((headline (plist-get node :headline))
	 (reference (org-export-get-reference headline info))
	 (childs (reverse (plist-get node :childs)))
	 (title (org-export-data (org-element-property :title headline) info))
	 (rendered-childs (mapcar (lambda (node) (iliayar/make-toc-rec node info)) childs)))
    (use-template "toc/node"
		  `(("title" . ,title)
		    ("childs" . ,rendered-childs)
		    ("ref" . ,reference)))))

(defun iliayar/make-toc (info depth)
  (iliayar/make-toc-rec
   (collect-headlines
    (org-export-collect-headlines info depth nil))
   info))

(defun iliayar/org-inner-template (contents info)
  (let* ((depth (plist-get info :with-toc))
	 (toc (if depth (iliayar/make-toc info depth)))
	 (base-url (plist-get info :html-base-url))
	 (base-title (plist-get info :html-base-title))
	 (footnotes (iliayar/org-footnote-section info))
	 (links-template (plist-get info :html-links-template))
	 (links (when links-template 
              (use-template links-template 
                            `(("base_url" . ,base-url))))))
    (use-template "inner-template"
		  `(("contents" . ,contents)
		    ("toc" . ,toc)
		    ("footnotes" . ,footnotes)
		    ("links" . ,links)
		    ("base_url" . ,base-url)
		    ("base_title" . ,base-title)))))

(defun iliayar/org-bold (bold contents info)
  (use-template "bold" `(("contents" . ,contents))))

(defun iliayar/org-italic (italic contents info)
  (use-template "italic" `(("contents" . ,contents))))

(defun iliayar/org-underline (underline contents info)
  (use-template "underline" `(("contents" . ,contents))))

(defun iliayar/org-strike-through (strike-through contents info)
  (use-template "strike-through" `(("contents" . ,contents))))

(defun iliayar/org-horizontal-rule (horizontal-rule contents info)
  (use-template "horizontal-rule" `()))

(defun iliayar/org-code (code contents info)
  (let* ((code-text (org-element-property :value code))
	 (code-text-html (unhtml code-text)))
    (use-template "code" `(("contents" . ,code-text-html)))))

;; FIXME: Is verbatim is realy the same?
(defun iliayar/org-verbatim (verbatim contents info)
  (iliayar/org-code verbatim contents info))

(defun iliayar/org-link (link contents info)
  (let* ((path (org-element-property :path link))
	 (type (org-element-property :type link))
	 (description (or (org-string-nw-p contents) path))
	 (extension (file-name-extension path))
	 (path-with-type (concat type ":" path))
	 (is-image (or (string-prefix-p "svg" extension)
		       (string-prefix-p "png" extension)
		       (string-prefix-p "jpeg" extension))))
    (cond
     ((and is-image (equal type "file"))
      (use-template "image"
		    `(("path" . ,path))))

     (is-image
      (use-template "image"
		    `(("path" . ,path))))

     ((equal type "file")
      (let ((path (s-replace ".org" ".html" path)))
	(use-template "link"
		      `(("contents" . ,description)
			("path" . ,path)))))

     ((equal type "coderef")
      (let ((path (concat "#coderef-" path)))
	(use-template "link"
		      `(("contents" . ,description)
			("path" . ,path)))))

     (t (use-template "link"
		      `(("contents" . ,description)
			("path" . ,path-with-type)))))))

(defun iliayar/org-table (table contents info)
  (use-template "table/body" `(("contents" . ,contents))))

(defun iliayar/org-table-row (table-row contents info)
  (let* ((row-group (org-export-table-row-group table-row info))
	 (table (org-export-get-parent-table table-row))
	 (table-has-header (org-export-table-has-header-p table info)))
    (when (equal (org-element-property :type table-row) 'standard)
      ;; FIXME: Row group can be more than 1 line and there can be any
      ;; number of groups. Wrap it in theader, tbody tags
      (cond ((and table-has-header (equal 1 row-group))
	     (use-template "table/header-row" `(("contents" . ,contents))))
	    (t (use-template "table/row" `(("contents" . ,contents))))))))

(defun iliayar/org-table-cell (table-cell contents info)
  (use-template "table/cell" `(("contents" . ,contents))))

(defun iliayar/org-subscript (subscript contents info)
  (use-template "subscript" `(("contents" . ,contents))))

(defun iliayar/org-superscript (subscript contents info)
  (use-template "superscript" `(("contents" . ,contents))))

(defun iliayar/org-center-block (center-block contents info)
  (let* ((caption (org-export-get-caption center-block))
	 (caption (when caption (org-trim (org-export-data caption info)))))
    (use-template "center" `(("contents" . ,contents)
			     ("caption" . ,caption)))))

(defun iliayar/org-dynamic-block (dynamic-block contents info)
  (use-template "todo" `(("contents" . ,contents))))

(defun iliayar/org-fixed-width (fixed-width contents info)
  (use-template "todo" `(("contents" . ,contents))))

(defun iliayar/org-example-block (example-block contents info)
  (use-template "todo" `(("contents" . ,contents))))

(defun iliayar/org-inline-task (inline-task contents info)
  (use-template "todo" `(("contents" . ,contents))))

(defun iliayar/org-verse-block (verse-block contents info)
  (use-template "todo" `(("contents" . ,contents))))

(defun iliayar/org-quote-block (quote-block contents info)
  (use-template "todo" `(("contents" . ,contents))))

(defun iliayar/org-drawer (drawer contents info)
  (use-template "todo" `(("contents" . ,contents))))

(defun iliayar/org-property-drawer (property-drawer contents info)
  (use-template "todo" `(("contents" . ,contents))))

(defun iliayar/org-special-block (special-block contents info)
  (let* ((type (org-element-property :type special-block)))
    (cond ((t (use-template
	       "todo" `(("contents" . ,contents)
			("entity_type" . ,type))))))))

(defun iliayar/org-clock (clock contents info)
  (let* ((timestamp (org-timestamp-translate (org-element-property :value clock))))
    (use-template "inline-todo" `(("contents" . ,contents)))))

(defun iliayar/org-radio-target (radio-target contents info)
  (use-template "inline-todo" `(("entity_type" . "radio-target"))))

(defun iliayar/org-target (target contents info)
  (use-template "inline-todo" `(("entity_type" . "target"))))

(defun iliayar/org-footnote-section (info)
  (let* ((fn-alist (org-export-collect-footnote-definitions info))
	 (fn-alist
	  (loop for (n type raw) in fn-alist collect
		(let ((content (org-trim (org-export-data raw info))))
		  `(("id" . ,n)
		    ("content" . ,content))))))
    (if (= 0 (length fn-alist))
	nil
      (use-template "footnote-section" `(("footnotes" . ,fn-alist)))
      )))

(defun iliayar/org-footnote-reference (footnote-reference contents info)
  (let ((n (org-export-get-footnote-number footnote-reference info)))
    (use-template "footnote-reference" `(("id" . ,n)))))

(defun iliayar/org-node-property (node-property contents info)
  (let* ((key (org-element-property :key node-property))
	 (value (org-element-property :value node-property))
	 (text (format "%s:%s" key value))))
  (use-template
   "inline-todo" `(("entity_type" . "target")
		   ("contents" . ,text))))

(defun iliayar/org-planning (planning contents info)
  (use-template "todo" `(("entity_type" . "planning"))))

(defun iliayar/org-line-break (line-break contents info)
  (use-template "line-break" `()))

(defun iliayar/org-timestamp (timestamp contents info)
  (let* ((timestamp-text (org-timestamp-translate timestamp)))
    (use-template "timestamp" `(("text" . ,timestamp-text)))))

(defun iliayar/org-statistics-cookie (statistics-cookie contents info)
  (let* ((cookie-text (org-element-property :value statistics-cookie)))
    (use-template "statistics-cookie" `(("text" . ,cookie-text)))))

(defun iliayar/org-export-block (export-block contents info)
  (when (equal (org-element-property :type export-block) "HTML")
    (org-remove-indentation (org-element-property :value export-block))))

(defun iliayar/org-export-snippet (export-snippet contents info)
  (when (eq (org-export-snippet-backend export-snippet) 'html)
    (org-element-property :value export-snippet)))

(defun iliayar/org-keyword (keyword contents info)
  (let* ((key (org-element-property :key keyword))
	 (value (org-element-property :value keyword)))
    (cond
     ;; NOTE: Can generate scoped TOC here. Need it?
     ((equal key "HTML") value))))

(defun iliayar/org-latex-environment (latex-environment contents info)
  (let* ((latex-frag (org-remove-indentation
		      (org-element-property :value latex-environment))))
    (use-template "latex" `(("latex_frag" . ,latex-frag)))))

(defun iliayar/org-latex-fragment (latex-fragment contents info)
  (let* ((latex-frag (org-remove-indentation
		      (org-element-property :value latex-fragment))))
    (use-template "latex" `(("latex_frag" . ,latex-frag)))))


(defun iliayar/org-entity (entity contents info)
  (org-element-property :html entity))

(provide 'ox-iliayar-html)

;;;###autoload
(defun iliayar/org-publish-to-html (plist filename pub-dir)
  (org-publish-org-to 'iliayar-html filename
		      (concat (when (> (length org-html-extension) 0) ".")
			      (or (plist-get plist :html-extension)
				  org-html-extension
				  "html"))
		      plist pub-dir))

(defcustom iliayar/org-base-url
  "https://iliay.ar"
  "Home page"
  :type '(string))

(defcustom iliayar/org-res-base-url
  "https://iliay.ar"
  "URL of resources"
  :type '(string))

(defcustom iliayar/org-base-title
  "iliay.ar"
  "Title"
  :type '(string))

(defcustom iliayar/org-matomo-host
  "matomo.iliay.ar"
  "Matomo host"
  :type '(string))

(defcustom iliayar/org-links-template
  nil
  "Template file with link in footer"
  :type '(string))

;; NOTE: List of entities taken from
;; https://github.com/yyr/org-mode/blob/master/lisp/ox-html.el
(org-export-define-derived-backend
    'iliayar-html 'html
  :translate-alist '((template . iliayar/org-template)
		     (inner-template . iliayar/org-inner-template)
		     (headline . iliayar/org-headline)
		     (section . iliayar/org-section)
		     (paragraph . iliayar/org-paragraph)
		     (plain-text . iliayar/org-plain-text)
		     (src-block . iliayar/org-src-block)
		     (plain-list . iliayar/org-plain-list)
		     (item . iliayar/org-item)
		     (bold . iliayar/org-bold)
		     (italic . iliayar/org-italic)
		     (strike-through . iliayar/org-strike-through)
		     (underline . iliayar/org-underline)
		     (code . iliayar/org-code)
		     (verbatim . iliayar/org-verbatim)
		     (link . iliayar/org-link)
		     (table . iliayar/org-table)
		     (table-row . iliayar/org-table-row)
		     (table-cell . iliayar/org-table-cell)
		     (horizontal-rule . iliayar/org-horizontal-rule)
		     (center-block . iliayar/org-center-block)
		     (dynamic-block . iliayar/org-dynamic-block)
		     (example-block . iliayar/org-example-block)
		     (export-block . iliayar/org-export-block)
		     (export-snippet . iliayar/org-export-snippet)
		     (line-break . iliayar/org-line-break)
		     (fixed-width . iliayar/org-fixed-width)
		     (verse-block . iliayar/org-verse-block)
		     (keyword . iliayar/org-keyword)
		     (inlinetask . iliayar/org-inlinetask)
		     (timestamp . iliayar/org-timestamp)
		     (quote-block . iliayar/org-quote-block)
		     (clock . iliayar/org-clock)
		     (inline-src-block . iliayar/org-inline-src-block)
		     (drawer . iliayar/org-drawer)
		     (target . iliayar/org-target)
		     (radio-target . iliayar/radio-target)
		     (entity . iliayar/org-entity)
		     (special-block . iliayar/org-special-block)
		     (property-drawer . iliayar/org-property-drawer)
		     (statistics-cookie . iliayar/org-statistics-cookie)
		     (planning . iliayar/org-planning)
		     (node-property . iliayar/org-node-property)
		     (footnote-definition . iliayar/org-footnote-definition)
		     (footnote-reference . iliayar/org-footnote-reference)
 		     (subscript . iliayar/org-subscript)
		     (superscript . iliayar/org-superscript)
		     (latex-enironment . iliayar/org-latex-environment)
		     (latex-fragment . iliayar/org-latex-fragment))
  :options-alist
  '((:html-base-url nil nil iliayar/org-base-url)
    (:html-base-title nil nil iliayar/org-base-title)
    (:html-res-base-url nil nil iliayar/org-res-base-url)
    (:html-matomo-host nil nil iliayar/org-matomo-host)
    (:html-links-template nil nil iliayar/org-links-template)))
