;;; organism-capture.el --- Entry creation and linking  -*- lexical-binding: t; -*-

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

;; This file provides functions for finding and linking to entries
;; in the organism graph.

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-capture)
(require 'graphael-core)
(require 'organism-utils)
(require 'organism-entry)
(require 'organism-graph)

;; Declare free variables defined in main file.
(defvar organism-directory)
(defvar organism-capture-templates-default)

;;; Entry completion

(defvar organism-capture--current-candidates nil
  "Current completion candidates for organism entries.")

(defun organism-capture--candidate-label (entry)
  "Format ENTRY as a candidate string for completion.
Returns a cons cell with displayable label and entry object."
  (let ((max-length 45)  ; Max length for entry titles in completion column.
        (title (or (organism-entry-title entry) "Untitled")))
    (cons
      (truncate-string-to-width title max-length nil ?\s "…")
      entry)))

(defun organism-capture--candidate-annotator (candidate)
  "Annotate CANDIDATE for minibuffer completion selection.
Adds file, type, timestamp and tag information for selected entry."
  (when-let* ((entry (cdr (assoc candidate
                            organism-capture--current-candidates))))
    (or (graph-node-attr-get entry :annotation)
      (let* ((category (organism-entry-category entry))
             (type (if category
                     category
                     (if (organism-entry-file-p entry) "file" "heading")))
             (tags (mapconcat 'identity (organism-entry-tags entry) ":"))
             (timestamp (organism-entry-property entry "CREATED"))
             (summary (organism-entry-property entry "SUMMARY"))
             (file (file-name-nondirectory
                     (or (organism-entry-property entry "FILE") "")))
             (annotation
               (concat
                 " "
                 (propertize (format "%-10s" type) 'face 'font-lock-type-face)
                 "  "
                 (if timestamp
                   (propertize (organism-format-timestamp timestamp "%Y-%m-%d")
                     'face 'font-lock-comment-face)
                   (make-string 10 ?\s))
                 "  "
                 (propertize (truncate-string-to-width tags 30 nil ?\s "…")
                   'face 'font-lock-keyword-face)
                 "  "
                 (propertize (truncate-string-to-width file 35 nil ?\s "…")
                   'face 'font-lock-comment-face)
                 "  "
                 (when summary
                   (propertize (truncate-string-to-width summary 50 nil ?\s "…")
                     'face 'font-lock-type-face)))))
        ;; Store the entry annotation for future use
        (graph-node-attr-put entry :annotation annotation)
        annotation))))

(defun organism-capture--get-candidates (&optional filter-fn)
  "Get all entries as candidates for completion.
If FILTER-FN is provided, only include entries for which it returns non-nil.
Sets `organism-capture--current-candidates' and returns it."
  (unless organism-graph
    (organism-debug "Organism graph not initialized")
    (user-error "Enable organism-mode"))

  (let (candidates)
    (dolist (entry (organism-graph-entries))
      (when (or (null filter-fn) (funcall filter-fn entry))
        (push (organism-capture--candidate-label entry) candidates)))
    (setq organism-capture--current-candidates
      (sort candidates (lambda (a b) (string< (car a) (car b)))))))

(defun organism-capture--completing-read (prompt &optional initial-input filter-fn)
  "Read organism entry using PROMPT with completion.
INITIAL-INPUT provides initial text in the minibuffer.
FILTER-FN, if provided, filters completion candidates."
  (organism-check-type prompt 'string "prompt")
  (let* ((candidates (organism-capture--get-candidates filter-fn))
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                    '(metadata (category . organism-entry)
                       (annotation-function . organism-capture--candidate-annotator))
                    (complete-with-action action candidates string pred)))))
    (completing-read prompt table nil nil initial-input)))

(defun organism-capture--complete-entry (prompt &optional initial-input filter-fn)
  "Complete an organism entry using PROMPT.
Returns selection and entry as multiple values.
INITIAL-INPUT provides initial text in the minibuffer.
FILTER-FN, if provided, filters completion candidates."
  (organism-check-type prompt 'string "prompt")
  (organism-check-type initial-input '(or null string) "initial-input")
  (organism-check-type filter-fn '(or null function) "filter-fn")

  (let* ((selection (organism-capture--completing-read prompt
                      initial-input filter-fn))
         (entry (cdr (assoc selection organism-capture--current-candidates))))
    (cl-values selection entry)))

(defun organism-capture--get-template-key ()
  "Get the template key to use for organism capture.
Use `organism-capture-templates-default' if set, otherwise the first template."
  (cond
    ;; Case 1: User has specified a key that exists
    ((and organism-capture-templates-default
          (assoc organism-capture-templates-default org-capture-templates))
      organism-capture-templates-default)

    ;; Case 2: User specified key doesn't exist
    (organism-capture-templates-default
      (user-error "Template key '%s' not found in org-capture-templates"
        organism-capture-templates-default))

    ;; Case 3: Find the first real template (not a group)
    ((> (length org-capture-templates) 0)
      (let ((template (car org-capture-templates)))
        (if (and (listp template) (>= (length template) 5))
          (car template)
          (user-error "No valid capture templates found - first entry appears to be a group"))))

    ;; Case 4: No templates available
    (t
      (user-error "No org-capture-templates defined. Please configure at least one template"))))

(defun organism-capture--update-graph-after-capture (callback)
  "Set up a one-time capture hook to update graph and run CALLBACK."
  (organism-check-type callback 'function)

  (cl-labels ((finalize-hook ()
                (remove-hook 'org-capture-after-finalize-hook #'finalize-hook)
                (unless org-note-abort
                  (when-let ((marker org-capture-last-stored-marker)
                             (buffer (marker-buffer org-capture-last-stored-marker))
                             (file (buffer-file-name buffer)))
                    ;; Validate file is in organism-directory
                    (if (file-in-directory-p file organism-directory)
                      (progn
                        ;; Visit the marker to find the ID
                        (with-current-buffer buffer
                          (goto-char marker)
                          (when-let ((id (org-id-get)))
                            ;; Register the ID in org-id-locations
                            (org-id-add-location id file)
                            ;; Update graph with the file
                            (organism-debug "Updating graph for new entry: %s at %s" id file)
                            (organism-graph-update-file file)
                            ;; Call the callback with the ID
                            (funcall callback id))))
                      ;; File is outside organism-directory
                      (message "Organism WARNING: Entry created outside organism-directory - not added to graph")
                      (with-current-buffer buffer
                        (goto-char marker)
                        (when-let ((id (org-id-get)))
                          (funcall callback id))))))))
    (add-hook 'org-capture-after-finalize-hook #'finalize-hook)))

;;; Public interface functions

;;;###autoload
(defun organism-find (&optional initial-input filter-fn)
  "Find and open an organism entry by its title or create a new one.
INITIAL-INPUT provides initial text for the completion prompt.
FILTER-FN can filter completion candidates to show only matching entries.

When an existing entry is found, navigates to its location.
When creating a new entry, prompts for confirmation before proceeding."
  (interactive)
  (unless organism-graph
    (user-error "Enable organism-mode"))

  (cl-multiple-value-bind (selection entry)
    (organism-capture--complete-entry "Entry: " initial-input filter-fn)
    (if entry
      (org-id-goto (node-id entry))

      ;; Entry not found - create new
      (when (y-or-n-p (format "Create new entry '%s'? " selection))
        (organism-debug "Creating new entry: %s" selection)
        (let ((org-capture-initial selection))
          (org-capture nil (organism-capture--get-template-key)))))))

;;;###autoload
(defun organism-link (&optional filter-fn)
  "Find an organism entry and insert a link to it at point.
FILTER-FN filters completion candidates if provided."
  (interactive)
  (unless organism-graph
    (user-error "Enable organism-mode"))

  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))

  (cl-multiple-value-bind (selection entry)
    (organism-capture--complete-entry "Link to entry: " nil filter-fn)
    (if entry
      (insert (org-link-make-string (concat "id:" (node-id entry))
                (organism-entry-title entry)))

      ;; Entry not found - create new
      (when (y-or-n-p (format "Create new entry '%s'? " selection))
        (let ((orig-buf (current-buffer))
              (orig-point (point)))
          (organism-capture--update-graph-after-capture
            (lambda (id)
              (with-current-buffer orig-buf
                (goto-char orig-point)
                (insert (org-link-make-string (concat "id:" id) selection)))))
          (let ((org-capture-initial selection))
            (org-capture nil (organism-capture--get-template-key))))))))

;;;###autoload
(defun organism-link-immediate (&optional filter-fn)
  "Insert a link to an organism entry without visiting it.
If no matching entry exists with FILTER-FN, create a placeholder entry."
  (interactive)
  (unless organism-graph
    (user-error "Enable organism-mode"))
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))

  (cl-multiple-value-bind (selection entry)
    (organism-capture--complete-entry "Link to entry: " nil filter-fn)
    (if entry
      (insert (org-link-make-string (concat "id:" (node-id entry))
                (organism-entry-title entry)))

      ;; Entry not found - create new
      (when (y-or-n-p (format "Create new entry '%s'? " selection))
        (let* ((orig-buf (current-buffer))
               (orig-point (point))
               ;; Get template key
               (template-key (organism-capture--get-template-key))
               ;; Create temporary modified template with :immediate-finish
               (org-capture-templates
                 (mapcar (lambda (template)
                           (if (equal (car template) template-key)
                             ;; Add :immediate-finish to the template
                             (append template '(:immediate-finish t))
                             template))
                   org-capture-templates)))

          (organism-capture--update-graph-after-capture
            (lambda (id)
              (with-current-buffer orig-buf
                (goto-char orig-point)
                (insert (org-link-make-string (concat "id:" id) selection)))))

          (let ((org-capture-initial selection))
            (org-capture nil template-key)))))))

(provide 'organism-capture)
;;; organism-capture.el ends here
