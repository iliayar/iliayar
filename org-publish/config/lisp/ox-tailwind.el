;;; ox-tailwind.el --- Tailwind.css Back-End for Org Export Engine -*- lexical-binding: t -*-

;; Author: Vasco Ferreira <vasco_mmf@hotmail.com>
;; Maintainer: Vasco Ferreira <vasco_mmf@hotmail.com>
;; Created: 07 Mar 2020
;; Version: 0.41
;; Keywords: tailwind.css org-mode html-export
;; Homepage: https://github.com/vascoferreira25/ox-tailwind
;; Package-Requires: ((org) (dash) (s))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This html export backend uses tailwind.css classes to export all the
;; org-mode elements.  This way you will be able to quickly and effortlessly
;; change the layout, colors and look of the website.
;;
;; The html document has the following default layout:
;;
;; ---------------------------------------------------
;; |                      Header                     |
;; ---------------------------------------------------
;; |Table of Contents|                               |
;; |                 |                               |
;; |     Sidebar     |              Body             |
;; |                 |                               |
;; |                 |                               |
;; ---------------------------------------------------
;; |                      Footer                     |
;; ---------------------------------------------------
;;
;; You can change the look of the exported HTML by redefining the values of the
;; classes.  All the classes start with `org-tailwind-class-', for example:
;;
;; - org-tailwind-class-h1
;; - org-tailwind-class-h2
;; - org-tailwind-class-header
;; - org-tailwind-class-footer
;; - org-tailwind-class-verbatim
;; - org-tailwind-class-video
;; - etc...
;;
;; Check this file to know more tailwind.css classes and change them any way
;; you want.
;;
;; Source code blocks use Prism.js.  The Prism.js files included in this repo
;; use all the available languages and the following plugins:
;; 
;; - line highlight
;; - line numbers
;; - autolinker
;; - file highlight
;; - show language
;; - jsonp highlight
;; - inline color
;; - previewers
;; - autoloader
;; - keep markup
;; - command-line
;; - unescaped markup
;; - normalize whitespace
;; - data-uri highlight
;; - toolbar
;; - copy to clipboard button
;; - download button
;; - match braces
;; - diff highlight
;; - filter highlight all
;; - treeview

;;; Dependencies:
(require 'org)
(require 'ox-html)
(require 's)
(require 'dash)

;;; tailwind group

(defgroup org-tailwind nil
  "Classes for the html elements."
  :group 'classes)


;;; Element Classes

;; Headings

(defcustom org-tailwind-class-title
  "mt-12 mb-12 text-4xl text-gray-700 dark:text-gray-400 border-b \
hover:text-blue-400 dark:hover:text-blue-500 border-gray-500"
  "Tailwind.css classes for the Title."
  :type '(string))

(defcustom org-tailwind-class-h1
  "mt-24 mb-6 text-3xl text-gray-700 dark:text-gray-400 border-b \
hover:text-blue-400 dark:hover:text-blue-500 border-gray-500"
  "Tailwind.css classes for Heading 1."
  :type '(string))

(defcustom org-tailwind-class-h2
  "mt-20 mb-6 text-2xl text-gray-700 dark:text-gray-400 border-b \
hover:text-blue-400 dark:hover:text-blue-500 border-gray-500"
  "Tailwind.css classes for Heading 2."
  :type '(string))

(defcustom org-tailwind-class-h3
  "mt-16 mb-6 text-xl text-gray-700 dark:text-gray-400 border-b \
hover:text-blue-400 dark:hover:text-blue-500 border-gray-500"
  "Tailwind.css classes for Heading 3."
  :type '(string))

(defcustom org-tailwind-class-h4
  "mt-14 mb-6 text-lg text-gray-700 dark:text-gray-400 border-b \
hover:text-blue-400 dark:hover:text-blue-500 border-gray-500"
  "Tailwind.css classes for Heading 4."
  :type '(string))

(defcustom org-tailwind-class-h5
  "mt-12 mb-6 text-lg text-gray-700 dark:text-gray-400 border-b \
hover:text-blue-400 dark:hover:text-blue-500 border-gray-500"
  "Tailwind.css classes for Heading 5."
  :type '(string))

(defcustom org-tailwind-class-h6
  "mt-10 mb-6 text-base text-gray-700 dark:text-gray-400 border-b \
hover:text-blue-400 dark:hover:text-blue-500 border-gray-500"
  "Tailwind.css classes for Heading 6."
  :type '(string))

(defcustom org-tailwind-class-h7
  "mt-8 mb-6 text-base text-gray-700 dark:text-gray-400 border-b \
hover:text-blue-400 dark:hover:text-blue-500 border-gray-500"
  "Tailwind.css classes for Heading 7."
  :type '(string))


;; Text elements

(defcustom org-tailwind-class-bold
  "font-bold"
  "Tailwind.css classes for the HTML BOLD attribute."
  :type '(string))

(defcustom org-tailwind-class-italic
  "italic"
  "Tailwind.css classes for the HTML ITALIC attribute."
  :type '(string))

(defcustom org-tailwind-class-underlined
  "underline"
  "Tailwind.css classes for the HTML UNDERLINE attribute."
  :type '(string))

(defcustom org-tailwind-class-code
  "px-2 rounded text-green-500 bg-gray-200 dark:bg-gray-600 \
dark:text-yellow-500"
  "Tailwind.css classes for the HTML UNDERLINE attribute."
  :type '(string))

(defcustom org-tailwind-class-verbatim
  "px-2 rounded text-red-400 bg-gray-200 dark:bg-gray-600 \
dark:text-red-500"
  "Tailwind.css classes for the HTML VERBATIM attribute."
  :type '(string))

(defcustom org-tailwind-class-link
  "text-blue-500 hover:underline dark:text-blue-400"
  "Tailwind.css classes for the HTML LINK attribute."
  :type '(string))

(defcustom org-tailwind-class-paragraph
  "my-2"
  "Tailwind.css classes for the HTML PARAGRAPH."
  :type '(string))

(defcustom org-tailwind-class-image-div
  "mx-8 my-12 max-w-full max-h-full"
  "Tailwind.css classes for the HTML image DIV."
  :type '(string))

(defcustom org-tailwind-class-image
  "mx-auto mb-2 max-w-full max-h-full rounded border \
border-gray-500"
  "Tailwind.css classes for the HTML IMAGE."
  :type '(string))

(defcustom org-tailwind-class-image-description
  "italic border-t border-gray-500"
  "Tailwind.css classes for the HTML image DESCRIPTION."
  :type '(string))

(defcustom org-tailwind-class-video-div
  "mx-8 my-12 max-w-full max-h-full"
  "Tailwind.css classes for the HTML video DIV."
  :type '(string))

(defcustom org-tailwind-class-video
  "mx-auto mb-2 rounded border border-gray-500"
  "Tailwind.css classes for the HTML VIDEO."
  :type '(string))

(defcustom org-tailwind-class-video-description
  "italic border-t border-gray-500"
  "Tailwind.css classes for the HTML video DESCRIPTION."
  :type '(string))

(defcustom org-tailwind-class-toc-title
  "text-md my-2 hover:bg-gray-300 dark:hover:bg-gray-600 \
block dark:border-gray-500"
  "Tailwind.css classes for the HTML Table of Contents title.
Use a single `\\' if you have line breaks in the string."
  :type '(string))

(defcustom org-tailwind-class-toc-items
  "text-md my-1 p-1 hover:bg-gray-100 dark:hover:bg-gray-600 \
dark:border-gray-500"
  "Tailwind.css classes for the HTML Table of Contents items.
Use a single `\\' if you have line breaks in the string."
  :type '(string))

(defcustom org-tailwind-class-current-toc
  "border-l-4 border-blue-400 dark:border-blue-500 bg-gray-100 \
  dark:bg-gray-600"
  "Tailwind.css classes for the CURRENT HTML Table of Contents item."
  :type '(string))

(defcustom org-tailwind-class-horizontal-rule
  "border-b-2 border-gray-300 dark:border-gray-700"
  "Tailwind.css classes for the HTML horizontal rule."
  :type '(string))


;; Lists

(defcustom org-tailwind-class-ordered-list
  "list-decimal my-2"
  "Tailwind.css classes for the HTML ORDERED list."
  :type '(string))

(defcustom org-tailwind-class-ordered-list-item
  "ml-10"
  "Tailwind.css classes for the HTML ordered list ITEM."
  :type '(string))

(defcustom org-tailwind-class-unordered-list
  "list-disc my-2"
  "Tailwind.css classes for the HTML UNORDERED list."
  :type '(string))

(defcustom org-tailwind-class-unordered-list-item
  "ml-10"
  "Tailwind.css classes for the HTML unordered list ITEM."
  :type '(string))

(defcustom org-tailwind-class-description-list
  "my-2 ml-8"
  "Tailwind.css classes for the HTML DESCRIPTION list."
  :type '(string))

(defcustom org-tailwind-class-description-list-title
  "font-bold border-b mr-72 dark:border-gray-700"
  "Tailwind.css classes for the HTML DESCRIPTION title."
  :type '(string))

(defcustom org-tailwind-class-description-list-item
  "ml-10"
  "Tailwind.css classes for the HTML description list ITEM."
  :type '(string))


;; Table

(defcustom org-tailwind-class-table-container
  "overflow-x-auto my-12"
  "Tailwind.css classes for the HTML table NAME."
  :type '(string))

(defcustom org-tailwind-class-table
  "table-auto m-auto my-2"
  "Tailwind.css classes for the HTML TABLE."
  :type '(string))

(defcustom org-tailwind-class-table-description
  "mx-48 text-center italic border-t border-gray-500"
  "Tailwind.css classes for the HTML table NAME."
  :type '(string))

(defcustom org-tailwind-class-table-header-row
  "text-gray-600 border-b-2 border-gray-400 bg-gray-100 \
  dark:text-gray-400 dark:bg-gray-600"
  "Tailwind.css classes for the HTML table HEADER-ROW."
  :type '(string))

(defcustom org-tailwind-class-table-header-cell
  "px-4 py-2 font-bold text-center"
  "Tailwind.css classes for the HTML table HEADER-CELL."
  :type '(string))

(defcustom org-tailwind-class-table-body-row
  "hover:bg-blue-100 dark:hover:bg-gray-600"
  "Tailwind.css classes for the HTML table BODY-ROW."
  :type '(string))

(defcustom org-tailwind-class-table-last-body-row
  "border-b-2 border-gray-400 hover:bg-blue-100 dark:hover:bg-gray-600"
  "Tailwind.css classes for the HTML table BODY-ROW."
  :type '(string))

(defcustom org-tailwind-class-table-body-cell
  "px-4 py-2"
  "Tailwind.css classes for the HTML table BODY-CELL."
  :type '(string))

(defcustom org-tailwind-class-table-empty-body-cell
  "px-2 py-2"
  "Tailwind.css classes for the HTML table EMPTY BODY-CELL."
  :type '(string))

;; Footnotes
(defcustom org-tailwind-class-footnotes-section
  "<div id=\"footnotes\">
  <h2 class=\"mt-20 mb-6 text-2xl text-gray-700 dark:text-gray-400 border-b \
  hover:text-green-500 dark:hover:text-blue-500 border-gray-500\">%s</h2> \
  <div id=\"text-footnotes\">
  %s
  </div>
  </div>"
  "Tailwind.css classes for the HTML footnotes section."
  :type '(string))

(defcustom org-tailwind-class-footnotes-format
  "<sup>%s</sup>"
  "Tailwind.css classes for the HTML footnotes format."
  :type '(string))

(defcustom org-tailwind-class-footnotes-separator
  "<sup>, </sup>"
  "Tailwind.css classes for the HTML footnotes separator."
  :type '(string))

;; Blocks

(defcustom org-tailwind-class-example-container
  "my-12 rounded border border-gray-800 shadow-xl"
  "Tailwind.css classes for the HTML EXAMPLE-BLOCK CONTAINER."
  :type '(string))

(defcustom org-tailwind-class-example
  "rounded rainbow-braces"
  "Tailwind.css classes for the HTML EXAMPLE-BLOCK."
  :type '(string))

(defcustom org-tailwind-class-src-container
  "my-3 shadow-xl"
  "Tailwind.css classes for the HTML SRC-BLOCK CONTAINER."
  :type '(string))

(defcustom org-tailwind-class-pre
  "rainbow-braces match-braces rounded-b"
  "Tailwind.css classes for the HTML SRC-BLOCK."
  :type '(string))

(defcustom org-tailwind-class-blockquote-container
  "my-3"
  "Tailwind.css classes for the HTML BLOCKQUOTE container."
  :type '(string))

(defcustom org-tailwind-class-blockquote
  "mx-8 my-2 px-4 border-l-8 rounded border border-gray-500 \
  bg-gray-300 dark:bg-gray-600 dark:text-gray-300"
  "Tailwind.css classes for the HTML BLOCKQUOTE block."
  :type '(string))

(defcustom org-tailwind-class-blockquote-author
  "mx-8 italic border-t border-gray-500"
  "Tailwind.css classes for the HTML blockquote AUTHOR."
  :type '(string))


;; Special Blocks

(defcustom org-tailwind-class-mermaid-container
  "mx-8 my-12"
  "Tailwind.css classes for the HTML MERMAID container."
  :type '(string))

(defcustom org-tailwind-class-mermaid-block
  "mx-auto my-2 max-w-full max-h-full bg-white dark:bg-darkgray rounded border border-gray-500"
  "Tailwind.css classes for the HTML MERMAID block."
  :type '(string))

(defcustom org-tailwind-class-mermaid-block-title
  "italic border-t border-gray-500"
  "Tailwind.css classes for the HTML MERMAID block."
  :type '(string))

(defcustom org-tailwind-class-details-block
  "my-4 py-2 px-8 rounded border-l-8 border border-gray-300 \
  dark:border-gray-600"
  "Tailwind.css classes for the HTML DETAILS block."
  :type '(string))

(defcustom org-tailwind-class-details-title
  "text-gray-500 font-bold"
  "Tailwind.css classes for the HTML details block TITLE."
  :type '(string))

(defcustom org-tailwind-class-anki-block
  "mb-2 px-10 text-xs"
  "Tailwind.css classes for the HTML DETAILS block."
  :type '(string))

(defcustom org-tailwind-class-anki-title
  "text-gray-300 dark:text-gray-600"
  "Tailwind.css classes for the HTML details block TITLE."
  :type '(string))

(defcustom org-tailwind-class-tip-block
  "my-12 p-8 rounded border-l-8 border border-blue-500 \
  shadow-xl"
  "Tailwind.css classes for the HTML TIP block."
  :type '(string))

(defcustom org-tailwind-class-tip-title
  "text-blue-500 font-bold"
  "Tailwind.css classes for the HTML tip block TITLE."
  :type '(string))

(defcustom org-tailwind-class-warning-block
  "my-12 p-8 rounded border-l-8 border border-yellow-500 \
  shadow-xl"
  "Tailwind.css classes for the HTML WARNING block."
  :type '(string))

(defcustom org-tailwind-class-warning-title
  "text-yellow-500 font-bold"
  "Tailwind.css classes for the HTML warning block TITLE."
  :type '(string))

(defcustom org-tailwind-class-danger-block
  "my-12 p-8 rounded border-l-8 border border-red-500 \
  shadow-xl"
  "Tailwind.css classes for the HTML DANGER block."
  :type '(string))

(defcustom org-tailwind-class-danger-title
  "text-red-500 font-bold"
  "Tailwind.css classes for the HTML danger block TITLE."
  :type '(string))


;; Page Divs

(defcustom org-tailwind-class-body
  "flex flex-col h-screen text-gray-700 dark:bg-darkgray \
  dark:text-gray-400"
  "Tailwind.css classes for the HTML BODY."
  :type '(string))

(defcustom org-tailwind-class-header
  "w-full border-b border-gray-500
  shadow-md items-center h-16"
  "Tailwind.css classes for the HTML HEADER."
  :type '(string))

(defcustom org-tailwind-class-sidebar
  "mx-12 md:mx-16 shadow-2xl rounded mt-12 p-12 md:p-16 lg:m-0 \
  lg:border-r lg:border-gray-500 lg:fixed lg:pt-2 lg:w-80 lg:px-2 \
  lg:overflow-y-auto lg:inset-y-0 lg:mt-16"
  "Tailwind.css classes for the HTML SIDEBAR."
  :type '(string))

(defcustom org-tailwind-class-content
  "flex flex-col lg:flex-row overflow-y-auto"
  "Tailwind.css classes for the HTML CONTENT."
  :type '(string))

(defcustom org-tailwind-class-content-container
  "flex-grow px-8 py-12 sm:px-12 md:px-16 lg:ml-80 lg:px-20 \
  lg:overflow-x-auto xl:px-32 2xl:px-48"
  "Tailwind.css classes for the HTML contents CONTAINER."
  :type '(string))

(defcustom org-tailwind-class-inner-container
  "relative -top-16 p-16 pb-32 mb-12 shadow-2xl rounded \
  xl:p-24"
  "Tailwind.css classes for the HTML inner container."
  :type '(string))

(defcustom org-tailwind-class-footer
  "hidden fixed bottom-0 w-full border-t border-solid \
  border-gray-500 h-8 text-center bg-white"
  "Tailwind.css classes for the HTML FOOTER."
  :type '(string))

(defcustom org-tailwind-class-search-bar
  "float-right mx-4 w-1/6 rounded px-4 py-1 border-solid \
  border border-gray-700 dark:border-gray-500 text-gray-400 \
  dark:text-gray-900 focus:border-green-500 \
  dark:focus:border-blue-500 focus:text-gray-500 \
  dark:focus:text-gray-500 dark:bg-midgray"
  "Tailwind.css classes for the HTML SEARCH BAR."
  :type '(string))

(defcustom org-tailwind-class-search-bar-results-list
  "z-50 absolute w-5/6 sm:w-4/6 md:w-3/6 lg:w-2/6 xl:w-1/6
  right-0 mt-12 mr-20 bg-white dark:bg-midgray p-4 shadow-lg border
  border-solid border-gray-500 rounded"
  "Tailwind.css classes for the HTML RESULTS LIST."
  :type '(string))

(defcustom org-tailwind-class-search-bar-results-item
  "p-2 block rounded hover:bg-gray-300 dark:hover:bg-darkgray"
  "Tailwind.css classes for the HTML RESULTS ITEM.
Do not break the line with while inserting a `newline'.  Use `\' at
  the end."
  :type '(string))


;; Other elements

(defcustom org-tailwind-class-checkbox
  "form-tick appearance-none h-6 w-6 mr-2 border border-gray-300 \
  rounded checked:bg-blue-600 checked:border-transparent \
  focus:outline-none align-text-bottom"
  "Tailwind.css classes for Checkbox."
  :type '(string))

(defcustom org-tailwind-class-file-name
  "relative top-10 flex rounded-t py-4 px-16 text-sm xl:px-24 z-10"
  "Tailwind.css classes for File Name."
  :type '(string))


;;; Templates

(defcustom org-tailwind-file-name-use-link t
  "Whether to use a link or a paragraph for the file-name field."
  :type '(boolean))


(defcustom org-tailwind-file-name-link
  "<a class=\"%s\" href=\"org-protocol://open-file?file=%s\">%s</a>"
  "The link to open the file in Emacs - preferably with org-protocol."
  :type '(string))


(defcustom org-tailwind-head-files
  "<!-- Tailwind CSS -->
  <link href=\"./css/tailwind.css\" rel=\"stylesheet\"/>

  <!-- Highlight Css -->
  <link href=\"./css/highlight.css\" rel=\"stylesheet\" />

  <!-- Mathjax -->
  <script id=\"MathJax-script\" async src=\"./js/mathjax/tex-mml-chtml.js\"></script>

  <!-- Your CSS file should come here -->
  <link href=\"./css/style.css\" rel=\"stylesheet\" />

  <!-- Toc tree file -->
  <script src=\"./js/toc_tree.js\"></script>
  "
  "Links to be imported on the head of the HTML file."
  :type '(string))

(defcustom org-tailwind-header
  "<nav class=\"flex items-center justify-between flex-wrap p-4\">
  <div class=\"flex items-center flex-no-shrink mr-6\">
  <span class=\"font-semibold text-xl tracking-tight\">
  <a href=\"./index.html\">Notes</a>
  </span>
  </div>
  <div class=\"block flex-grow lg:flex lg:items-center lg:w-auto\">
  <div class=\"text-sm lg:flex-grow\">
  <a id=\"top-button\" href=\"#top\" class=\"absolute right-0 bottom-0 mb-2 mr-8 z-50 bg-gray-500
  bg-opacity-80 hover:bg-gray-700 text-white font-bold rounded
  h-10 w-16 flex items-center justify-center\">
  Top
  </a>
  <input id=\"search-bar\" onkeyup=\"search()\"
  onfocusin='showResults()'
  class=\"%s\" placeholder=\"Search...\"/>
  <ul id=\"search-bar-results\"
  class=\"%s\" style=\"display: none;\"></ul>
  <button id=\"toggle-light-btn\" type=\"button\" class=\"float-right rounded px-4 py-1 border bg-black bg-gray-100
  dark:bg-gray-300 dark:text-gray-700\">Toggle dark-mode</button>
  </div>
  </div>
  </nav>"
  "Contents of header in HTML."
  :type '(string))

(defcustom org-tailwind-sidebar
  "<div id=\"toc\"></div>"
  "Contents of sidebar in HTML."
  :type '(string))

(defcustom org-tailwind-footer
  "<p>Exported with org-tailwind</p>"
  "Contents of footer in HTML."
  :type '(string))

(defcustom org-tailwind-bottom-files
  "<script src=\"./js/highlight.js\"></script>
  <script src=\"./js/utils.js\"></script>
  <script type=\"module\">
    import mermaid from './js/mermaid/mermaid.esm.min.mjs';
  </script>"
  "Javascript files to be imported at the bottom of the HTML file."
  :type '(string))

(defcustom org-tailwind-headlines
  "h1,h2,h3,h4"
  "The level of the headlines to be included in the toc."
  :type '(string))

(defcustom org-tailwind-html-template
  "<!doctype html>
<html lang=\"en\" class=\"dark\">
<head>
  <meta charset=\"utf-8\">
  <title>%s</title>
  %s
</head>
<body class=\"%s\">

<div id=\"header\" class=\"%s\">
%s
</div>

<div id=\"content\" class=\"top %s\" onclick=\"hideResults()\">
  <div id=\"sidebar\" class=\"%s\">
  %s
  </div>

  <div id=\"content-container\" class=\"%s\" onscroll=\"scrollSpy()\">
    <div id=\"top\"></div>
    <div id=\"file-name\" class=\"%s\">
      <img class=\"hidden w-6 h-6 mr-2\" src=\"./img/file_icon.png\">
      %s
    </div>
    <div id=\"inner-container\" class=\"%s\">
      <h1 id=\"toc-link-title\" class=\"%s\">
      %s
      </h1>
      %s
    </div>
  </div>
</div>

<div id=\"footer\" class=\"%s\">
%s
</div>

%s

</body>
</html>
"
  "Default HTML template.
The default template needs the format place for the following
values, in this order:

- page title
- head files
- body classes
- header classes
- header contents
- content classes
- sidebar classes
- sidebar contents
- content-container classes
- inner-container classes
- contents
- footer classes
- footer contents
- bottom page javascript
- bottom page files."
  :type '(string))


;;; Transcode Elements

(defun org-tailwind--file-name-link-or-paragraph (info)
  "Write the file name as a link or paragraph.
Depends on the variable `org-tailwind-file-name-use-link'."
  (if org-tailwind-file-name-use-link
      (format org-tailwind-file-name-link
              org-tailwind-class-link
              (plist-get info :input-file)
              (plist-get info :input-buffer))
    (format "<p>%s</p>"
            (plist-get info :input-buffer))))


(defun org-tailwind-inner-template (contents info)
  "Define the template for the CONTENTS inside Headings.
By not doing anything to the contents, it exports the elements at the root level."
  (concat contents (org-html-footnote-section info)))


(defun org-tailwind-template (contents info)
  "Format the HTML Template and add the CONTENTS of the export."
  (format org-tailwind-html-template
          (-first-item (plist-get info :title))
          org-tailwind-head-files
          org-tailwind-class-body
          org-tailwind-class-header
          (format org-tailwind-header
                  org-tailwind-class-search-bar
                  org-tailwind-class-search-bar-results-list)
          org-tailwind-class-content
          org-tailwind-class-sidebar
          org-tailwind-sidebar
          org-tailwind-class-content-container
          ;; add the file name before the content box
          org-tailwind-class-file-name
          (org-tailwind--file-name-link-or-paragraph info)
          org-tailwind-class-inner-container
          ;; Contents go here
          org-tailwind-class-title
          (-first-item (plist-get info :title))
          contents
          org-tailwind-class-footer
          org-tailwind-footer
          org-tailwind-bottom-files))


(defun org-tailwind-bold (bold contents info)
  "Transcode BOLD from Org to HTML."
  (format "<strong class=\"%s\">%s</strong>" org-tailwind-class-bold contents))


(defun org-tailwind-italic (italic contents info)
  "Transcode ITALIC from Org to HTML."
  (format "<em class=\"%s\">%s</em>" org-tailwind-class-italic contents))


(defun org-tailwind-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to HTML."
  (format "<del class=\"%s\">%s</del>" org-tailwind-class-italic contents))


(defun org-tailwind-underlined (underlined contents info)
  "Transcode UNDERLINED from Org to HTML."
  (format "<span class=\"%s\">%s</span>" org-tailwind-class-underlined contents))


(defun org-tailwind-code (code contents info)
  "Transcode CODE from Org to HTML."
  (let* ((code-text (org-element-property :value code))
        (escaped-text (let* ((r1 (replace-regexp-in-string "<" "&lt;" code-text))
                             (r2 (replace-regexp-in-string ">" "&gt;" r1)))
                        r2)))
    (format "<code class=\"%s\">%s</code>" org-tailwind-class-code escaped-text)))


(defun org-tailwind-verbatim (verbatim contents info)
  "Transcode VERBATIM from Org to HTML."
  (let* ((code-text (org-element-property :value verbatim))
         (escaped-text (let* ((r1 (replace-regexp-in-string "<" "&lt;" code-text))
                              (r2 (replace-regexp-in-string ">" "&gt;" r1)))
                         r2)))
    (format "<code class=\"%s\">%s</code>" org-tailwind-class-verbatim escaped-text)))


(defun org-tailwind-verbatim (verbatim contents info)
  "Transcode VERBATIM from Org to HTML."
  (let* ((code-text (org-element-property :value verbatim))
         (escaped-text (let* ((r1 (replace-regexp-in-string "<" "&lt;" code-text))
                              (r2 (replace-regexp-in-string ">" "&gt;" r1)))
                         r2)))
    (format "<code class=\"%s\">%s</code>" org-tailwind-class-verbatim escaped-text)))


(defun org-tailwind-horizontal-rule (_horizontal-rule _contents info)
  "Transcode HORIZONTAL-RULE from Org to HTML."
  (format "<hr class=\"%s\">"
          org-tailwind-class-horizontal-rule))


(defun org-tailwind-checkbox (checkbox)
  "Format a checkbox item into the corresponding HTML tag."
  (let ((checkbox-tag "<input type=\"checkbox\" class=\"%s\" disabled %s>"))
    (pcase checkbox
      (`on (format checkbox-tag
                   org-tailwind-class-checkbox "checked"))
      (`off (format checkbox-tag
                    org-tailwind-class-checkbox "unchecked"))
      (_ (format checkbox-tag
                 org-tailwind-class-checkbox "unchecked")))))


(defun org-tailwind-paragraph (paragraph contents info)
  "Transcode PARAGRAPH from Org to HTML."
  (let* ((parent (org-element-property :parent paragraph))
         (parent-type (org-element-property :type parent))
         (ancestor (org-element-property :parent parent))
         (ancestor-type (org-element-property :type ancestor))
         (checkbox (org-element-property :checkbox parent))
         (is-checkbox-p (unless (equal checkbox nil) t))
         (is-image-p (string-match-p "<img " contents))
		 (is-video-p (string-match-p "<video " contents)))
    (cond
     ;; For Mermaid.js return raw text
     ((equal parent-type "mermaid") contents)
     ;; For Descriptive types, surround with `dd' tag
     ((equal ancestor-type 'descriptive)
      (format "<dd class=\"%s\">%s</dd>"
              org-tailwind-class-description-list-item
              contents))
     ;; Put image inside a div with description
     (is-image-p (format "<div class=\"%s\">%s</div>"
                         org-tailwind-class-image-div
                         contents))
	 ;; Put video inside a div with description
     (is-video-p (format "<div class=\"%s\">%s</div>"
                         org-tailwind-class-video-div
                         contents))
     ;; If is a checkbox
     (is-checkbox-p
      (format "<p class=\"%s\">%s %s</p>"
              org-tailwind-class-paragraph
              (org-tailwind-checkbox checkbox)
              contents))
     ;; Normal paragraph
     (t (format "<p class=\"%s\">\n%s</p>"
                org-tailwind-class-paragraph
                contents)))))


(defun org-tailwind-target (target _contents info)
  "Transcode a TARGET object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let* ((ref (org-element-property :value target)))
    (format "<span id=\"%s\"></span>" ref)))


(defun org-tailwind-link (link contents info)
  "Transcode LINK from Org to HTML."
  (let* ((path (org-element-property :path link))
         (raw-link (org-element-property :raw-link link))
         (type (org-element-property :type link))
         (parent (org-element-property :parent link))
         (description (org-element-property :name parent))
         (timeline (org-tailwind--get-attribute :attr_timeline parent))
         (file-extension (file-name-extension path))
         (link-tag "<a class=\"%s\" href=\"%s\">%s</a>")
         (video-tag "<video class=\"%s\" controls><source src=\"%s\" type=\"video/%s\"/></video>%s")
         (link-description "<p class=\"%s\">%s</p>")
         (image-tag "<img class=\"%s\" src=\"%s\"/>%s"))
    (cond
     ;; Is the link a video
     ((or (equal file-extension "mp4")
          (equal file-extension "avi")
          (equal file-extension "mkv")
          (equal file-extension "webm")
          (equal file-extension "mpeg4")
          (equal file-extension "3gp"))
      (format video-tag
              org-tailwind-class-video
              (if timeline
                  (format "%s#t=%s" path timeline)
                path)
              file-extension
              (if description
                  (format link-description
                          org-tailwind-class-video-description
                          description)
                "")))
     ;; Is the link an image
     ((or (equal file-extension "png")
          (equal file-extension "svg")
          (equal file-extension "tiff")
          (equal file-extension "tif")
          (equal file-extension "jpg")
          (equal file-extension "jpeg")
          (equal file-extension "webp")
          (equal file-extension "gif")
          (equal file-extension "bmp"))
      (format image-tag
              org-tailwind-class-image
              path
              (if description
                  (format link-description
                          org-tailwind-class-image-description
                          description)
                "")))
     ((equal type "org-protocol")
      (format link-tag
              org-tailwind-class-link
              raw-link
              contents))
     ;; Is the link an org file
     ((equal file-extension "org")
      (format link-tag
              org-tailwind-class-link
              (concat type ":" (replace-regexp-in-string "\\.org" ".html" path))
              contents))
     ;; Internal link
     ((equal type "fuzzy")
      (format link-tag
              org-tailwind-class-link
              (concat "#" raw-link)
              (if contents
                  contents
                (concat "#" raw-link))))
     ;; Any other link
     (t (format link-tag
                org-tailwind-class-link
                (concat type ":" path)
                (if contents
                    contents
                  (concat type ":" path)))))))


(defun org-tailwind-blockquote (blockquote contents info)
  "Transcode BLOCKQUOTE from Org to HTML."
  (format
   "<div class=\"%s\"><blockquote class=\"%s\">%s</blockquote>%s</div>"
   org-tailwind-class-blockquote-container
   org-tailwind-class-blockquote
   contents
   (if (org-element-property :name blockquote)
       (format "<p class=\"%s\">%s</p>"
               org-tailwind-class-blockquote-author
               (org-element-property :name blockquote))
     "")))


(defvar org-tailwind--src-block-open
  "<div class=\"%s\">%s<pre class=\"%s\" %s>"
  "Opening tag of code block for Prism.js.
It has two format places:
- Tailwind.css classes
- other attribute.")


(defvar org-tailwind--src-block-close
  "<code class=\"language-%s\">%s</code></pre></div>"
  "Closing tag of code block for Prism.js.
It has two format places:
- language
- code text")


(defvar org-tailwind--src-block-file-name
  "<div class=\"h-8 flex border border-sky-500\"><p class=\"ml-2 mr-16 my-1 w-full text-center dark:text-gray-700\">%s</p></div>"
  "Make it look like the code is in a mac OS code editor.")


(defun org-tailwind--get-attribute (attribute block)
  "Get the ATTRIBUTE from an org BLOCK element."
  (car (org-element-property attribute block)))


(defun org-tailwind-command-line-block (src-block contents info)
  "Transcode SRC-BLOCK with command line language to a custom Prism.js code block."
  (let* ((code-text (org-element-property :value src-block))
         (language (org-element-property :language src-block))
         (filepath (org-tailwind--get-attribute :attr_filepath src-block))
         (file-name (org-tailwind--get-attribute :attr_filename src-block))
         (username (org-tailwind--get-attribute :attr_username src-block))
         (hostname (org-tailwind--get-attribute :attr_hostname src-block))
         (highlight-lines (org-tailwind--get-attribute :attr_highlight src-block)))
    (concat
     (format org-tailwind--src-block-open
             org-tailwind-class-src-container
	     (if file-name
		 (format org-tailwind--src-block-file-name
			 file-name)
	       "")
             (concat "command-line " org-tailwind-class-pre)
             (concat
              "data-filter-output=\"(out)\" "
              ;; Add user and host names
              (format " data-user=\"%s\" data-host=\"%s\""
                      (if username username "user")
                      (if hostname hostname "localhost"))
              (if highlight-lines
                  (format " data-line=\"%s\"" highlight-lines)
                "")
              ;; Add file src
              (if filepath
                  (format " data-src=\"%s\" data-download-link" filepath)
                "")
              ))
     (format org-tailwind--src-block-close
             (if (equal language "ps") "powershell" language)
             code-text))))


(defun org-tailwind-src-block (src-block contents info)
  "Transcode SRC-BLOCK from Org to HTML."
  (let* ((code-text (org-element-property :value src-block))
         (escaped-text (let* ((r1 (replace-regexp-in-string "<" "&lt;" code-text))
                              (r2 (replace-regexp-in-string ">" "&gt;" r1)))
                         r2))
         (language (org-element-property :language src-block))
         (filepath (org-tailwind--get-attribute :attr_filepath src-block))
         (file-name (org-tailwind--get-attribute :attr_filename src-block))
         (highlight-lines (org-tailwind--get-attribute :attr_highlight src-block))
         (fetch-code (org-tailwind--get-attribute :attr_fetch src-block)))
    (concat
     (format org-tailwind--src-block-open
	     org-tailwind-class-src-container
	     (if file-name
		 (format org-tailwind--src-block-file-name
			 file-name)
	       "")
             (concat "line-numbers " org-tailwind-class-pre)
             (concat " "
                     (if highlight-lines
                         (format " data-line=\"%s\"" highlight-lines)
                       "")
                     (if fetch-code
                         (format " data-jsonp=\"%s\" language-%s"
                                 fetch-code language)
                       "")
                     ;; Add file src
                     (if filepath
                         (format " data-src=\"%s\" data-download-link" filepath)
                       "")))
     (format org-tailwind--src-block-close
             language
             escaped-text))))


(defun org-tailwind-src-block-select (src-block contents info)
  "Transcode SRC-BLOCK from Org to HTML."
  (let* ((language (org-element-property :language src-block)))
    (cond
     ;; If it is a command line language,
     ((or (equal language "sh")
          (equal language "shell")
          (equal language "bash")
          (equal language "ps")
          (equal language "powershell"))
      (org-tailwind-command-line-block src-block contents info))
     ;; If it is a programming language
     (t (org-tailwind-src-block src-block contents info)))))

(defun org-tailwind-example-block (example-block contents info)
  "Transcode EXAMPLE-BLOCK from Org to HTML."
  (let ((code (org-html-format-code example-block info))
        (code-text (org-element-property :value example-block))
        (language (org-element-property :language example-block))
        (highlight-lines (org-element-property :highlight example-block)))
    (format "<div class=\"%s\">
<pre class=\"%s\">
<code class=\"language-%s\">%s</code>
</pre>
</div>"
            org-tailwind-class-example-container
            org-tailwind-class-example
            (if language language "none")
            code-text)))


(defun org-tailwind-special-block (special-block contents info)
  "Transcode SPECIAL-BLOCK from Org to HTML.
There are 4 types of blocks:
- Details
- Tip
- Warning
- Danger"
  ;; HACK: In order not to add the p tags in paragraph if the special-block's
  ;; type is `mermaid', change the paragraph block directly
  (let* ((type (org-element-property :type special-block))
         (name (org-element-property :name special-block)))
    (cond ((equal type "mermaid")
           (format "<div class=\"%s\"><div class=\"mermaid %s\">%s</div><p class=\"%s\">%s</p></div>"
                   org-tailwind-class-mermaid-container
                   org-tailwind-class-mermaid-block
                   contents
                   org-tailwind-class-mermaid-block-title
                   (if name name "")))
          ;; TODO: process title as summary
          ((equal type "ANKI")
           (format "<details class=\"%s\"><summary class=\"%s\">%s</summary>%s</details>"
                   org-tailwind-class-anki-block
                   org-tailwind-class-anki-title
                   (if name name "Anki Card")
                   contents))
          ((equal type "details")
           (format "<details class=\"%s\"><summary class=\"%s\">%s</summary>%s</details>"
                   org-tailwind-class-details-block
                   org-tailwind-class-details-title
                   (if name name "Details")
                   contents))
          ((equal type "tip")
           (format "<div class=\"tip %s\"><p class=\"%s\">%s</p>%s</div>"
                   org-tailwind-class-tip-block
                   org-tailwind-class-tip-title
                   (if name name "")
                   contents))
          ((equal type "warning")
           (format "<div class=\"warning %s\"><p class=\"%s\">%s</p>%s</div>"
                   org-tailwind-class-warning-block
                   org-tailwind-class-warning-title
                   (if name name "")
                   contents))
          ((equal type "danger")
           (format "<div class=\"danger %s\"><p class=\"%s\">%s</p>%s</div>"
                   org-tailwind-class-danger-block
                   org-tailwind-class-danger-title
                   (if name name "")
                   contents))
          (t (org-html-special-block special-block contents info)))))


(defun org-tailwind-format-header (level text contents)
  "Return the corresponding HTML heading level of the Org LEVEL."
  (let ((header-contents (if contents contents ""))
        (header "<h%s class=\"%s\">%s</h%s>%s" )
        (level (+ level 1)))
    (cond
     ((eq level 1) (format header level org-tailwind-class-h2
                           text level header-contents))
     ((eq level 2) (format header level org-tailwind-class-h3
                           text level header-contents))
     ((eq level 3) (format header level org-tailwind-class-h4
                           text level header-contents))
     ((eq level 4) (format header level org-tailwind-class-h5
                           text level header-contents))
     ((eq level 5) (format header level org-tailwind-class-h6
                           text level header-contents))
     (t (format header level org-tailwind-class-h7 text level
                header-contents)))))


(defun org-tailwind-headline (headline contents info)
  "Transcode HEADLINE from Org to HTML."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((text (org-element-property :raw-value headline))
           (level (org-export-get-relative-level headline info)))
      (org-tailwind-format-header level text contents))))


(defun org-tailwind-table-cell (table-cell contents info)
  "Transcode TABLE-CELL from Org to HTML.
If TABLE-CELL is part of the table header, return the HTML table
cell with `th'.  Return `td' otherwise."
  (let* ((header-p (org-export-table-row-in-header-p
                   (org-export-get-parent table-cell) info))
         (text-alignment (org-export-table-cell-alignment
                          table-cell
                          info)))
    (if header-p
        (format "<th class=\"%s\">%s</th>" org-tailwind-class-table-header-cell contents)
      (if contents
          (format "<td class=\"%s %s\">%s</td>"
                  org-tailwind-class-table-body-cell
                  (concat "text-"
                          (cond
                           ((equal text-alignment 'left) "left")
                           ((equal text-alignment 'center) "center")
                           ((equal text-alignment 'right) "right")))
                  contents)
        (format "<td class=\"%s\"></td>"
                org-tailwind-class-table-empty-body-cell)))))

(defun org-tailwind-table-row (table-row contents info)
  "Transcode TABLE-ROW from Org to HTML.
First, check the group of the TABLE-ROW, in this case it will be
either `thead' or `tbody'.  Second, check if it is the first row
of the group.  If it is, add the corresponding opening HTML tag.
In the case of being the last row of the group, add the closing
tag.  If it is neither, return the row without group tags."
  (let* ((header '("<thead>" . "</thead>"))
         (body '("<tbody>" . "</tbody>"))
         (row '("<tr class=\"%s\">" . "</tr>"))
         (row-group (org-export-table-row-group table-row info))
         (is-first-row-p (org-export-table-row-starts-rowgroup-p table-row info))
         (is-last-row-p (org-export-table-row-ends-rowgroup-p table-row info)))
    ;; As there are multiple types of rows, e.g., separator rows,
    ;; check if the row is a normal row
    (when (eq (org-element-property :type table-row) 'standard)
      ;; If row is in the table header
      (if (eq row-group 1)
          (cond
           ;; Is the first row of the header
           ((not (eq is-first-row-p nil))
            (concat (car header)
                    (format (car row) org-tailwind-class-table-header-row)
                    contents
                    (cdr row)))
           ;; Is the last row of the header
           ((not (eq is-last-row-p nil))
            (concat (format (car row) org-tailwind-class-table-header-row)
                    contents
                    (cdr row)
                    (cdr header)))
           ;; Is in the middle of header
           (t (concat (format (car row) org-tailwind-class-table-header-row)
                      contents
                      (cdr row))))
        ;; If row is in the table body
        (cond
         ;; Is the first row of the body
         ((not (eq is-first-row-p nil))
          (concat (car body)
                  (format (car row) org-tailwind-class-table-body-row)
                  contents
                  (cdr row)))
         ;; Is the last row of the body
         ((not (eq is-last-row-p nil))
          (concat (format (car row) org-tailwind-class-table-last-body-row)
                  contents
                  (cdr row)
                  (cdr body)))
         ;; Is in the middle of the body
         (t (concat (format (car row) org-tailwind-class-table-body-row)
                    contents
                    (cdr row))))))))

(defun org-tailwind-table (table contents info)
  "Transcode TABLE from Org to HTML."
  (let ((name (org-element-property :name table)))
    (format "<div class=\"%s\"><table class=\"%s\">%s</table>%s</div>"
            org-tailwind-class-table-container
            org-tailwind-class-table
            contents
            (if name
                (format "<p class=\"%s\">%s</p>"
                        org-tailwind-class-table-description
                        name)
              ""))))

(defun org-tailwind-plain-list (plain-list contents info)
  "Transcode PLAIN-LIST from Org to HTML.
There are three types of lists:
- Ordered Lists;
- Unordered Lists and
- Description Lists."
  (let* ((type (pcase (org-element-property :type plain-list)
                 (`ordered "ol")
                 (`unordered "ul")
                 (`descriptive "dl")
                 (other (error "Unknown HTML list type: %s" other)))))
    (format "<%s class=\"%s\">%s</%s>"
            type
            (cond ((equal type "ol") org-tailwind-class-ordered-list)
                  ((equal type "ul") org-tailwind-class-unordered-list)
                  ((equal type "dl") org-tailwind-class-description-list))
            contents
            type)))

(defun org-tailwind-format-list-item (contents type checkbox info
					   &optional term-counter-id headline)
  "Format a list item into the corresponding HTML tag."
  (cond
   ((equal type `ordered)
    (format "<li class=\"%s\">%s</li>"
            org-tailwind-class-ordered-list-item contents))
   ((equal type `unordered)
    (format "<li class=\"%s\">%s</li>"
            org-tailwind-class-unordered-list-item contents))
   ((equal type `descriptive)
    (format "<dt class=\"%s\">%s</dt>%s"
            org-tailwind-class-description-list-title
            term-counter-id
            contents))))

(defun org-tailwind-item (item contents info)
  "Transcode an ITEM element from Org to HTML."
  (let* ((plain-list (org-export-get-parent item))
         (type (org-element-property :type plain-list))
         (counter (org-element-property :counter item))
         (checkbox (org-element-property :checkbox item))
         (tag (let ((tag (org-element-property :tag item)))
                (and tag (org-export-data tag info)))))
    (org-tailwind-format-list-item
     contents type checkbox info (or tag counter))))

(defun org-tailwind-section (section contents info)
  "Return the contents of SECTION as is."
  contents)


;;; Search bar toc tree

(defun org-tailwind--json-toc-item (file title heading index)
  "Transform HEADING and TITLE to a json object."
  (format
   "\t{\n\t\t\"name\": \"%s\", \n\t\t\"index\": \"%s\", \n\t\t\"parent\": \"%s\", \n\t\t\"file\": \"%s\"\n\t},"
   (replace-regexp-in-string "\"" "'" heading)
   index
   title
   (replace-regexp-in-string "\\.org$" ".html" file)))


(defun org-tailwind--json-toc-all-items (headings title file)
  "Transform all the HEADINGS into an object with the heading and the TITLE."
  (string-join
   (-map
    (lambda (heading)
      (org-tailwind--json-toc-item
       file
       title
       (-first-item heading)
       (-last-item heading)))
    headings)
   "\n"))


(defun org-tailwind--json-toc (filename file-tree output-directory)
  "Read all the exported files and search for headings.
The JSON format will"

  ;; Create an empty list to append all the headings
  (setq headings '())

  (setq headings-max-level (+ 1 (s-count-matches "," org-tailwind-headlines)))

  ;; Create a temporary buffer to store the text of the file
  (with-temp-buffer
    ;; Paste all the text inside the file to the temp buffer
    (insert-file-contents filename)
    ;; Start the index count
    ;; this will be used to target the element position in html
    (setq count 0)
    ;; Get the title
    (goto-char (point-min))
    (if (re-search-forward "^#\\+title:\s\\(.*\\)$" nil t)
        (progn
          (setq headings (append headings `((,(match-string 1) ,count))))
          (setq count (+ count 1))))
    ;; Search for the org headings
    (goto-char (point-min))
    (while (re-search-forward
            (format "^\\*\\{1,%s\\}\s.*$" headings-max-level)
            nil
            t)
      ;; Split the org headings and text
      (let ((cur-line (string-join (cdr (split-string (match-string 0) "\s")) " ")))
        (setq headings (append headings `((,cur-line ,count))))
        (setq count (+ count 1)))))

  (setq title (-first-item (-first-item headings)))
  (setq json-headings
        (org-tailwind--json-toc-all-items headings title filename))

  ;; Write the json toc tree to file
  (write-region json-headings nil file-tree 'append))


(defun org-tailwind--json-toc-all-files (notes-directory output-directory)
  "Read all the files in the NOTES-DIRECTORY and create a toc tree."
  
  (setq files (directory-files notes-directory nil ".org"))
  
  (setq file-tree (concat output-directory "/js/" "toc_tree.js"))

  ;; Clear the file
  (write-region "" nil file-tree)

  ;; Open the js object
  (write-region "const tocTree = [\n" nil file-tree 'append)

  ;; Append toc
  (-map
   (lambda (file)
     (let ((file-path (concat "./" file)))
       (org-tailwind--json-toc file-path file-tree output-directory))) files)

  ;; Close the js object
  (write-region "\n];" nil file-tree 'append))


;;;###autoload
(defun org-tailwind-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a TAILWIND HTML buffer.
Export as `org-html-export-as-html' does, with slimhtml
org-export-backend.
If narrowing is active in the current buffer, only export its
narrowed part.
If a region is active, export that region.
A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.
When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.
When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.
When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.
EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.
Export is done in a buffer named \"*Org TAILWIND export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'tailwind "*Org TAILWIND Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun org-tailwind-export-to-html (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML file.
Export as `org-html-export-as-html' does, with slimhtml
org-export-backend.
If narrowing is active in the current buffer, only export its
narrowed part.
If a region is active, export that region.
A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.
When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.
When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.
When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.
EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.
Return output file's name."
  (interactive)
  (let* ((extension (concat
                     (when (> (length org-html-extension) 0) ".")
                     (or (plist-get ext-plist :html-extension)
                         org-html-extension
                         "html")))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'tailwind file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-tailwind-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-tailwind--json-toc-all-files (file-name-directory filename) pub-dir)
  (org-publish-org-to 'tailwind filename
                      (concat (when (> (length org-html-extension) 0) ".")
                              (or (plist-get plist :html-extension)
                                  org-html-extension
                                  "html"))
                      plist pub-dir))

;;;###autoload
(defun org-tailwind-publish-to-html-without-toc (plist filename pub-dir)
  "Publish an org file to HTML without toc to build faster.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'tailwind filename
                      (concat (when (> (length org-html-extension) 0) ".")
                              (or (plist-get plist :html-extension)
                                  org-html-extension
                                  "html"))
                      plist pub-dir))

;;;###autoload
(defun org-tailwind-build-toc ()
  "Build the toc for the current buffer.
Open one if the notes and then run this function."
  (interactive)
  (org-tailwind--json-toc-all-files (file-name-directory (buffer-file-name)) "./dist/"))

(provide 'ox-tailwind)
;;; ox-tailwind.el ends here

;;; Define backend
(org-export-define-derived-backend 'tailwind 'html
  :translate-alist '((bold . org-tailwind-bold)
                     (code . org-tailwind-code)
                     (example-block . org-tailwind-example-block)
                     (headline . org-tailwind-headline)
                     (horizontal-rule . org-tailwind-horizontal-rule)
                     (inline-src-block . org-html-inline-src-block)
                     (inner-template . org-tailwind-inner-template)
                     (italic . org-tailwind-italic)
                     (item . org-tailwind-item)
                     (link . org-tailwind-link)
                     (paragraph . org-tailwind-paragraph)
                     (plain-list . org-tailwind-plain-list)
                     (plain-text . org-tailwind-plain-text)
                     (quote-block . org-tailwind-blockquote)
                     (section . org-tailwind-section)
                     (special-block . org-tailwind-special-block)
                     (src-block . org-tailwind-src-block-select)
                     (strike-through . org-tailwind-strike-through)
                     (table . org-tailwind-table)
                     (table-cell . org-tailwind-table-cell)
                     (table-row . org-tailwind-table-row)
                     (target . org-tailwind-target)
                     (template . org-tailwind-template)
                     (underline . org-tailwind-underlined)
                     (verbatim . org-tailwind-verbatim))
  :menu-entry
  '(?x "Export to HTML with org-tailwind Back-End"
       ((?H "As HTML buffer" org-tailwind-export-as-html)
        (?h "As HTML file" org-tailwind-export-to-html)
        (?o "As HTML file and open"
            (lambda (a s v b)
              (if a (org-tailwind-export-to-html t s v b)
                (org-open-file (org-tailwind-export-to-html nil s v b)))))))
  :options-alist
  '((:html-footnote-format nil nil org-tailwind-class-footnotes-format)
    (:html-footnote-separator nil nil org-tailwind-class-footnotes-separator)
    (:html-footnotes-section nil nil org-tailwind-class-footnotes-section)))

