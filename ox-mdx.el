;;; ox-md.el --- Markdown Back-End for Org Export Engine

;; Copyright (C) 2012, 2013  Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou@gmail.com>
;; Keywords: org, wp, markdown

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a Markdown back-end (vanilla flavour) for
;; Org exporter, based on `html' back-end.
;;
;; It provides two commands for export, depending on the desired
;; output: `org-md-export-as-markdown' (temporary buffer) and
;; `org-md-export-to-markdown' ("md" file).

;;; Code:

(eval-when-compile (require 'cl))
(require 'ox-md)



;;; User-Configurable Variables

(defgroup org-export-mdx nil
  "Options specific to Markdown export back-end."
  :tag "Org Extended Markdown"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))


;;; Define Back-End

(org-export-define-derived-backend 'mdx 'md
  :export-block '("MDX" "EXTENDED MARKDOWN")
  :menu-entry
  '(?d "Export to Extended Markdown"
       ((?M "To temporary buffer"
	    (lambda (a s v b) (org-mdx-export-as-markdown a s v)))
	(?m "To file" (lambda (a s v b) (org-mdx-export-to-markdown a s v)))
	(?o "To file and open"
	    (lambda (a s v b)
	      (if a (org-mdx-export-to-markdown t s v)
		(org-open-file (org-mdx-export-to-markdown nil s v)))))))
  :translate-alist '((code . org-mdx-verbatim)
                     (example-block . org-mdx-src-block)
                     ;;(fixed-width . org-mdx-src-block)
                     (inline-src-block . org-md-verbatim)
                     (inline-src-block . org-mdx-inline-src-block)
                     (link . org-md-link)
                     (src-block . org-mdx-src-block)
		     (underline . org-mdx-verbatim)
		     (verbatim . org-mdx-verbatim)))


;;; Filters

;;; Transcode Functions

;;;; Bold

;;;; Code and Verbatim

(defun org-mdx-inline-src-block (src contents info)
  (format "`%s`" (org-element-property :value code)))

(defun org-mdx-verbatim (verbatim contents info)
  "Do nothing to verbatim content."
  (org-element-property :value verbatim))

(defun org-mdx-src-block (src-block contents info)
  (let ((lang (org-element-property :language src-block)))
    (format "```%s\n%s```"
            (or lang "")
            (org-element-property :value src-block))))

;;;; Example Block and Src Block

(defun org-md-example-block (example-block contents info)
  "Transcode EXAMPLE-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (replace-regexp-in-string
   "^" "    "
   (org-remove-indentation
    (org-element-property :value example-block))))


;;;; Link

(defun org-md-link (link contents info)
  "Transcode LINE-BREAK object into Markdown format.
CONTENTS is the link's description.  INFO is a plist used as
a communication channel."
  (let ((--link-org-files-as-html-maybe
	 (function
	  (lambda (raw-path info)
	    ;; Treat links to `file.org' as links to `file.html', if
            ;; needed.  See `org-html-link-org-files-as-html'.
	    (cond
	     ((and org-html-link-org-files-as-html
		   (string= ".org"
			    (downcase (file-name-extension raw-path "."))))
	      (concat (file-name-sans-extension raw-path) "."
		      (plist-get info :html-extension)))
	     (t raw-path)))))
	(type (org-element-property :type link)))
    (cond ((member type '("custom-id" "id"))
	   (let ((destination (org-export-resolve-id-link link info)))
	     (if (stringp destination)	; External file.
		 (let ((path (funcall --link-org-files-as-html-maybe
				      destination info)))
		   (if (not contents) (format "<%s>" path)
		     (format "[%s](%s)" contents path)))
	       (concat
		(and contents (concat contents " "))
		(format "(%s)"
			(format (org-export-translate "See section %s" :html info)
				(mapconcat 'number-to-string
					   (org-export-get-headline-number
					    destination info)
					   ".")))))))
	  ((org-export-inline-image-p link org-html-inline-image-rules)
	   (let ((path (let ((raw-path (org-element-property :path link)))
			 (if (not (file-name-absolute-p raw-path)) raw-path
			   (expand-file-name raw-path)))))
	     (format "![%s](%s)"
		     (let ((caption (org-export-get-caption
				     (org-export-get-parent-element link))))
		       (when caption (org-export-data caption info)))
		     path)))
	  ((string= type "coderef")
	   (let ((ref (org-element-property :path link)))
	     (format (org-export-get-coderef-format ref contents)
		     (org-export-resolve-coderef ref info))))
	  ((equal type "radio")
	   (let ((destination (org-export-resolve-radio-link link info)))
	     (org-export-data (org-element-contents destination) info)))
	  ((equal type "fuzzy")
	   (let ((destination (org-export-resolve-fuzzy-link link info)))
	     (if (org-string-nw-p contents) contents
	       (when destination
		 (let ((number (org-export-get-ordinal destination info)))
		   (when number
		     (if (atom number) (number-to-string number)
		       (mapconcat 'number-to-string number "."))))))))
	  (t (let* ((raw-path (org-element-property :path link))
		    (path (cond
			   ((member type '("http" "https" "ftp"))
			    (concat type ":" raw-path))
			   ((equal type "file")
			    ;; Treat links to ".org" files as ".html",
			    ;; if needed.
			    (setq raw-path
				  (funcall --link-org-files-as-html-maybe
					   raw-path info))
			    ;; If file path is absolute, prepend it
			    ;; with protocol component - "file://".
			    (if (not (file-name-absolute-p raw-path)) raw-path
			      (concat "file://" (expand-file-name raw-path))))
			   (t raw-path))))
	       (if (not contents) (format "<%s>" path)
		 (format "[%s](%s)" contents path)))))))

;;; Interactive function

;;;###autoload
(defun org-mdx-export-as-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Markdown buffer.

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

Export is done in a buffer named \"*Org MD Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (if async
      (org-export-async-start
	  (lambda (output)
	    (with-current-buffer (get-buffer-create "*Org MD Export*")
	      (erase-buffer)
	      (insert output)
	      (goto-char (point-min))
	      (text-mode)
	      (org-export-add-to-stack (current-buffer) 'mdx)))
	`(org-export-as 'mdx ,subtreep ,visible-only))
    (let ((outbuf (org-export-to-buffer
		   'mdx "*Org MD Export*" subtreep visible-only)))
      (with-current-buffer outbuf (text-mode))
      (when org-export-show-temporary-export-buffer
	(switch-to-buffer-other-window outbuf)))))

;;;###autoload
(defun org-mdx-convert-region-to-md ()
  "Assume the current region has org-mode syntax, and convert it to Markdown.
This can be used in any buffer.  For example, you can write an
itemized list in org-mode syntax in a Markdown buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'mdx))


;;;###autoload
(defun org-mdx-export-to-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Markdown file.

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

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (if async
	(org-export-async-start
	    (lambda (f) (org-export-add-to-stack f 'mdx))
	  `(expand-file-name
	    (org-export-to-file 'mdx ,outfile ,subtreep ,visible-only)))
      (org-export-to-file 'mdx outfile subtreep visible-only))))


(provide 'ox-mdx)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

