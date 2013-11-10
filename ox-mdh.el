;;; ox-mdh.el: Higginbotham Extended Markdown Back-End for Org Export Engine

;; Copyright (C) 2013  Daniel Higginbotham

;; Author: Daniel Higginbotham <daniel@flyingmachinestudios.com>
;; Keywords: org, wp, markdown

;; This file is not part of GNU Emacs.

;;; Copyright Notice:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library extends the Markdown backend

;;; Code:

(eval-when-compile (require 'cl))
(require 'ox-md)



;;; User-Configurable Variables

(defgroup org-export-mdh nil
  "Options specific to Markdown export back-end."
  :tag "Org Higginbotham Extended Markdown"
  :group 'org-export-mdh
  :version "24.4"
  :package-version '(Org . "8.0"))


;;; Define Back-End

(org-export-define-derived-backend 'mdh 'md
  :export-block '("MDH" "HIGGINBOTHAM EXTENDED MARKDOWN")
  :options-alist
  '((:mdh-include-preamble nil "mdh-include-preamble" org-mdh-include-preamble t)
    (:mdh-link-title "MDH_LINK_TITLE" nil org-mdh-link-title t)
    (:mdh-preamble "MDH_PREAMBLE" nil org-mdh-preamble t)
    (:mdh-unseparated-element-types "MDH_UNSEPARATED_ELEMENT_TYPES" nil org-mdh-unseparate-element-types t))
  
  :translate-alist '((example-block . org-mdh-src-block)
                     ;;(fixed-width . org-mdh-src-block)
                     (inline-src-block . org-mdh-inline-src-block)
                     (link . org-md-link)
                     (src-block . org-mdh-src-block)
                         (table . org-org-identity)
                         (table-cell . org-org-identity)
                         (table-row . org-org-identity)))

(defcustom org-mdh-include-preamble t
  "Whether to actually include the preamble."
  :group 'org-export-mdh
  :type 'boolean)

(defcustom org-mdh-preamble nil
  "Preamble contents"
  :group 'org-export-mdh
  :type 'string)

(defcustom org-mdh-link-title nil
  "Title to show in a menu"
  :group 'org-export-mdh
  :type 'string)

(defcustom org-mdh-unseparate-element-types '(org-data table table-row item)
  "Org element types not to handle with org-md-separate-elements"
  :group 'org-export-mdh
  :type 'list)

;;; Filters

;;;; Code and Verbatim

(defun org-mdh-inline-src-block (src contents info)
  (format "`%s`" (org-element-property :value code)))

(defun org-mdh-src-block (src-block contents info)
  (let ((lang (org-element-property :language src-block)))
    (format "```%s\n%s```"
            (or lang "")
            (org-element-property :value src-block))))

;;; pre/postamble
;; Derived from ox-reveal.el
;; https://github.com/yjwen/org-reveal
(defun org-mdh--build-pre/postamble (type info)
  "Return document preamble or postamble as a string, or nil."
  (let ((section (plist-get info (intern (format ":mdh-%s" type))))
        (spec (org-html-format-spec info)))
    (when section
      (let ((section-contents
             (if (functionp (intern section)) (funcall (intern section) info)
               ;; else section is a string.
               (format-spec section spec))))
        (when (org-string-nw-p section-contents)
           (org-element-normalize-string section-contents))))))

;;; Interactive function

;;;###autoload
(defun org-md-export-as-markdown (&optional async subtreep visible-only)
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
	      (org-export-add-to-stack (current-buffer) 'mdh)))
	`(org-export-as 'mdh ,subtreep ,visible-only))
    (let ((outbuf (org-export-to-buffer
		   'mdh "*Org MD Export*" subtreep visible-only)))
      (with-current-buffer outbuf (text-mode))
      (when org-export-show-temporary-export-buffer
	(switch-to-buffer-other-window outbuf)))))

;;;###autoload
(defun org-md-convert-region-to-md ()
  "Assume the current region has org-mode syntax, and convert it to Markdown.
This can be used in any buffer.  For example, you can write an
itemized list in org-mode syntax in a Markdown buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'mdh))


;;;###autoload
(defun org-md-export-to-markdown (&optional async subtreep visible-only)
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
	    (lambda (f) (org-export-add-to-stack f 'mdh))
	  `(expand-file-name
	    (org-export-to-file 'mdh ,outfile ,subtreep ,visible-only)))
      (org-export-to-file 'mdh outfile subtreep visible-only))))

(defun org-md-template (contents info)
  (concat
   (org-mdh--build-pre/postamble 'preamble info)
   contents
   (org-mdh--build-pre/postamble 'postamble info)))

(defun org-md-separate-elements (tree backend info)
  "Redefined to allow multiple data types"
  (org-element-map tree org-element-all-elements
    (lambda (elem)
      (unless (member (org-element-type elem) org-mdh-unseparate-element-types)
        (org-element-put-property
         elem :post-blank
         (let ((post-blank (org-element-property :post-blank elem)))
           (if (not post-blank) 1 (max 1 post-blank)))))))
  ;; Return updated tree.
  tree)

(provide 'ox-mdh)
