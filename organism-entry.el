;;; organism-entry.el --- Entry class for organism -*- lexical-binding: t; -*-

;; Author: Billy Lamberta
;; URL: https://github.com/lamberta/organism
;; Package-Requires: ((emacs "27.2") (graphael "0.1.1"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file defines the organism-entry class, which represents an org-mode
;; entry (file or heading) in the organism graph.
;;
;; An organism-entry is an extension of graph-node that represents either:
;; 1. A file-level entry (level 0) - Corresponds to an org file with an ID property
;; 2. A heading entry (level > 0) - Corresponds to an org heading with an ID property
;;
;; Each entry maintains a connection to its location in an org file, allowing
;; for dynamic data retrieval and updates when the underlying files change.
;;
;; Basic usage:
;;
;; ;; Create an entry from an existing org ID
;; (let ((entry (make-instance 'organism-entry :id "20211231-1234-abcd")))
;;   ;; Access basic properties
;;   (organism-entry-title entry)       ; Get entry title
;;   (organism-entry-level entry)       ; Get outline level (0 for file)
;;   (organism-entry-file-p entry)      ; Check if file-level entry
;;   (organism-entry-heading-p entry)   ; Check if heading entry
;;
;;   ;; Access org properties
;;   (organism-entry-property entry "CUSTOM_PROP")
;;
;;   ;; Get tags
;;   (organism-entry-tags entry)           ; Direct tags
;;   (organism-entry-tags entry t)         ; Including inherited tags
;;
;;   ;; Extract links
;;   (organism-entry-links entry)          ; All links
;;   (organism-entry-links entry "id")     ; Only ID links
;;   (organism-entry-links entry nil t))   ; All links including subtree

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'org-element)
(require 'graphael-core)
(require 'organism-utils)

;;; Class definition

(defclass organism-entry (graph-node)
  ((level
     :initarg :level
     :reader organism-entry-level
     :initform 0
     :type integer
     :documentation "Outline level: 0 for file, 1+ for headings")
    (title
      :initarg :title
      :reader organism-entry-title
      :initform nil
      :type (or null string)
      :documentation "Title of the entry (from #+TITLE or heading)")
    (properties
      :initform '()
      :type list
      :documentation "Alist of properties from entry's property drawer"))
  "A class representing an org-mode entry in the organism graph.")

(cl-defmethod cl-print-object ((entry organism-entry) stream)
  "Print a readable representation of ENTRY to STREAM."
  (princ (format "#<organism-entry %s level:%d label:\"%s\">"
           (node-id entry)
           (organism-entry-level entry)
           (node-label entry))
    stream))

;;; Utilities

(defun organism-entry--cleanup-buffer (buffer existing-buffers)
  "Close BUFFER if it's not in EXISTING-BUFFERS list.
Only closes unmodified buffers."
  (when (and buffer
             (buffer-live-p buffer)
             (not (member (buffer-name buffer) existing-buffers))
             (not (buffer-modified-p buffer)))
    (kill-buffer buffer)))

(cl-defmacro organism-entry--with-entry-location (entry &body body &aux (refresh nil))
  "Execute BODY at the location of ENTRY in its file.
When :refresh is specified, reload the buffer before execution."
  (declare (indent 1) (debug t))
  (when (eq (car body) :refresh)
    (setq refresh t body (cdr body)))

  (let ((marker-sym (make-symbol "marker"))
        (buffer-sym (make-symbol "buffer"))
        (existing-buffers-sym (make-symbol "existing-buffers"))
        (result-sym (make-symbol "result")))
    `(let* ((,existing-buffers-sym (mapcar #'buffer-name (buffer-list)))
            (,marker-sym (org-id-find (node-id ,entry) t))
            ,buffer-sym ,result-sym)
       (if (not ,marker-sym)
         (progn
           (organism-debug "Could not find location for ID: %s"
             (node-id ,entry))
           nil)
         (unwind-protect
           (progn
             (setq ,buffer-sym (marker-buffer ,marker-sym))
             (with-current-buffer ,buffer-sym
               (if (and ,refresh (buffer-modified-p))
                 (progn
                   (organism-debug "Buffer %s has unsaved changes, skip refresh"
                     (buffer-name))
                   (setq ,result-sym nil))
                 (when ,refresh
                   (condition-case err
                     (progn
                       (revert-buffer t t t)
                       (org-set-regexps-and-options)
                       (org-element-cache-reset))
                     (error
                       (organism-debug "Error refreshing buffer: %s"
                         (error-message-string err)))))

                 (save-excursion
                   (goto-char ,marker-sym)
                   (setq ,result-sym (progn ,@body))))))
           (progn
             (move-marker ,marker-sym nil)
             (organism-entry--cleanup-buffer ,buffer-sym ,existing-buffers-sym)))
         ,result-sym))))

;;; Initialization and refresh

(cl-defmethod initialize-instance :after ((entry organism-entry) &rest _)
  "Initialize ENTRY with org data from its ID."
  (organism-entry--refresh entry))

(cl-defmethod organism-entry--refresh ((entry organism-entry))
  "Refresh ENTRY data from its ID location in org files.
Returns t if successful, nil if refresh was skipped or failed."
  (organism-entry--with-entry-location entry :refresh
    (let ((element (org-element-at-point))
          (current-file (buffer-file-name))
          (current-heading (when (org-at-heading-p) (org-get-heading t t t t))))

      (organism-debug "Refreshing entry %s at %s, heading: %s"
        (node-id entry) current-file current-heading)

      (with-slots (level title properties) entry
        ;; Set level
        (setf level (if (org-at-heading-p)
                      (org-element-property :level element)
                      0))  ; file-level
        ;; Set title
        (setf title (if (> level 0)
                      (substring-no-properties (org-get-heading t t t t))
                      (or (cadar (org-collect-keywords '("TITLE")))
                        (file-name-base current-file))))
        ;; Set properties
        (setf properties (org-entry-properties))
        ;; Set node label
        (setf (node-label entry) (organism-entry-label entry)))
      t)))

(cl-defmethod organism-entry--refresh :after ((entry organism-entry))
  "Clear caches after refreshing entry."
  (graph-node-attr-put entry :annotation nil)
  (graph-node-attr-put entry :cached-element nil)
  (graph-node-attr-put entry :links-entry-only nil)
  (graph-node-attr-put entry :links-with-subtree nil))

;;; Predicates

(cl-defmethod organism-entry-file-p ((entry organism-entry))
  "Return non-nil if ENTRY is a file-level entry (level 0)."
  (= (organism-entry-level entry) 0))

(cl-defmethod organism-entry-heading-p ((entry organism-entry))
  "Return non-nil if ENTRY is a heading entry (level > 0)."
  (> (organism-entry-level entry) 0))

;;; Basic properties

(cl-defmethod organism-entry-property ((entry organism-entry) (key string) &optional default)
  "Return property drawer value for KEY in ENTRY, or DEFAULT if not found.
KEY is case-insensitive."
  (unless (stringp key)
    (signal 'wrong-type-argument (list 'stringp key)))
  (with-slots (properties) entry
    (alist-get key properties default nil #'cl-equalp)))

(cl-defmethod organism-entry-element ((entry organism-entry))
  "Get the org element at ENTRY's location."
  (or (graph-node-attr-get entry :cached-element)
    (let ((element (organism-entry--with-entry-location entry
                     (org-element-at-point))))
      (when element
        (graph-node-attr-put entry :cached-element element))
      element)))

(cl-defmethod organism-entry-label ((entry organism-entry))
  "Get the label for ENTRY suitable for minibuffer completion."
  (with-slots (properties) entry
    (let ((file-name (file-name-nondirectory
                       (or (alist-get "FILE" properties nil nil #'cl-equalp) "")))
          (title (or (organism-entry-title entry) "Untitled")))
      (if (organism-entry-file-p entry)
        (format "%s (%s)" file-name title)
        (format "%s > %s" file-name title)))))

;;; Content extraction

(cl-defmethod organism-entry-tags ((entry organism-entry) &optional inherited-p)
  "Get tags for ENTRY from cached properties.
When INHERITED-P is non-nil, return all tags including inherited ones."
  (organism-check-type inherited-p 'boolean)
  (let* ((key (if (or inherited-p (organism-entry-file-p entry))
                "ALLTAGS"
                "TAGS"))
          (tags (organism-entry-property entry key)))
    (when tags
      (split-string
        (replace-regexp-in-string "^:\\|:$" ""
          (substring-no-properties tags))
        ":" t))))

(cl-defmethod organism-entry-links ((entry organism-entry) &optional type include-subtree)
  "Return a list of links in ENTRY.
Each link is represented as a plist with :type and :target properties.
TYPE can be nil (get all links), a string (e.g. \"id\"), or a list of strings.
When INCLUDE-SUBTREE is non-nil and ENTRY is a heading, search all subheadings."
  (organism-check-type type '(or string list))
  (organism-check-type include-subtree 'boolean)
  ;; Create cache key based on subtree flag
  (let* ((cache-key (if include-subtree :links-with-subtree :links-entry-only))
         (all-links (graph-node-attr-get entry cache-key)))
    ;; Fetch links if not cached
    (unless all-links
      (setq all-links
        (organism-entry--with-entry-location entry
          (let* ((file-entry-p (organism-entry-file-p entry))
                 (start-point (if file-entry-p (point-min) (point)))
                 (end-point (cond
                              (file-entry-p (point-max))
                              (include-subtree (save-excursion (org-end-of-subtree t t)))
                              (t (save-excursion
                                   (outline-next-heading)
                                   (point))))))
            (goto-char start-point)
            (cl-loop while (and (< (point) end-point)
                             (re-search-forward org-link-any-re end-point t))
              for element = (org-element-context)
              when (eq (org-element-type element) 'link)
              for link-type = (org-element-property :type element)
              for path = (org-element-property :path element)
              for raw-link = (org-element-property :raw-link element)
              collect (list :type link-type :target path :raw-link raw-link)))))
      ;; Cache the results
      (when all-links
        (graph-node-attr-put entry cache-key all-links)))
    ;; Filter by type
    (if (not type)
      all-links
      (cl-remove-if-not
        (lambda (link)
          (let ((link-type (plist-get link :type)))
            (cond
              ((stringp type) (cl-equalp link-type type))
              ((listp type) (member-ignore-case link-type type)))))
        all-links))))

(provide 'organism-entry)
;;; organism-entry.el ends here
