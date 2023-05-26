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
	 (template (pcase type
		     ('ordered "list/item-ordered")
		     ('unordered "list/item-unordered")
		     ('descriptive "list/item-descriptive"))))
    (use-template template `(("contents" . ,contents)
			     ("checkbox" . ,checkbox-html)))))

(defun iliayar/org-plain-list (plain-list contents info)
  (pcase (org-element-property :type plain-list)
    (`ordered (use-template "list/ordered" `(("contents" . ,contents))))
    (`unordered (use-template "list/unordered" `(("contents" . ,contents))))
    (`descriptive (use-template "list/descriptive" `(("contents" . ,contents))))))

(defun iliayar/org-src-block (src-block contents info)
  (let* ((language (org-element-property :language src-block))
	 (code-text (org-remove-indentation
		     (org-element-property :value src-block)))
	 (safe-code-text (unhtml code-text)))
    (cond ((equal language "mermaid")
	   (use-template "mermaid"
			 `(("code" . ,safe-code-text))))
	  (t (use-template "src-block"
			   `(("code" . ,code-text)
			     ("lang" . ,language)))))))

(defun iliayar/org-inline-src-block (inline-src-block contents info)
  (let* ((language (org-element-property :language inline-src-block))
	 (code-text (org-element-property :value inline-src-block)))
    (use-template "inline-src-block" `(("code" . ,code-text)))))

(defun iliayar/org-plain-text (contents info)
  (use-template "plain-text" `(("contents" . ,contents))))

(defun iliayar/org-section (section contents info)
  (let* ()
    (use-template "section"
		  `(("contents" . ,contents)))))

(defun iliayar/org-paragraph (paragraph contents info)
  (let* ((parent (org-export-get-parent paragraph))
	 (parent-type (org-element-type parent))
	 (item-parent (equal parent-type 'item))
	 (list-paragraph (and item-parent
			      (not (org-export-get-previous-element paragraph info)))))
    (use-template "paragraph"
		  `(("contents" . ,contents)
		    ("list_paragraph" . ,list-paragraph)))))

(defun make-todo (todo-keyword)
  (let ((keyword (pcase todo-keyword
		   ('todo "todo"))))
    (if keyword
	(use-template "headline-todo"
		      `(("keyword" . ,keyword))))))

(defun iliayar/org-headline (headline contents info)
  (let* ((title (org-element-property :raw-value headline))
	 (reference (org-export-get-reference headline info))
	 (prefix (s-repeat (org-element-property :level headline) "#"))
	 (todo-keyword (org-element-property :todo-type headline))
	 (todo-content (if todo-keyword (make-todo todo-keyword))))
    (use-template "headline"
		  `(("title" . ,title)
		    ("ref" . ,reference)
		    ("contents" . ,contents)
		    ("prefix" . ,prefix)
		    ("todo_keyword" . ,todo-content)))))

(defun iliayar/org-template (contents info)
  (use-template "template"
		`(("contents" . ,contents))))

(defun iliayar/make-toc-rec (node info)
  (let* ((headline (plist-get node :headline))
	 (reference (org-export-get-reference headline info))
	 (childs (reverse (plist-get node :childs)))
	 (title (org-element-property :raw-value headline))
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
	 (toc (if depth (iliayar/make-toc info depth))))
    (use-template "inner-template"
		  `(("contents" . ,contents)
		    ("toc" . ,toc)))))

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

;; TODO Image links
(defun iliayar/org-link (link contents info)
  (let* ((path (org-element-property :path link))
	 (type (org-element-property :type link))
	 (description (or (org-string-nw-p contents) path)))
    (cond
     ((equal type "file")
      (let ((path (s-replace ".org" ".html" path)))
	(use-template "link"
		      `(("contents" . ,description)
			("path" . ,path)))))

     (t (use-template "link"
		      `(("contents" . ,description)
			("path" . ,path)))))))

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
  (use-template "todo" `(("contents" . ,contents))))

(defun iliayar/org-dynamic-block (dynamic-block contents info)
  (use-template "todo" `(("contents" . ,contents))))

(defun iliayar/org-fixed-width (fixed-width contents info)
  (use-template "todo" `(("contents" . ,contents))))

(defun iliayar/org-example-block (example-block contents info)
  (use-template "todo" `(("contents" . ,contents))))

(defun iliayar/org-example-block (example-block contents info)
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

(defun iliayar/org-footnote-definition (target contents info)
  (use-template "inline-todo" `(("entity_type" . "footnote-definition"))))

(defun iliayar/org-footnote-reference (target contents info)
  (use-template "inline-todo" `(("entity_type" . "footnote-reference"))))

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
		    (foonote-definition . iliayar/org-footnote-definition)
		    (foonote-reference . iliayar/org-footnote-reference)
		    (subscript . iliayar/org-subscript)
		    (superscript . iliayar/org-superscript)
		    (latex-enironment . iliayar/org-latex-environment)
		    (latex-fragment . iliayar/org-latex-fragment)))

