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
;;
;; Main functions:
;; - organism-find: Find or create an entry and visit it
;; - organism-link: Insert a link to an entry at point
;; - organism-link-immediate: Insert a link to a placeholder entry without visiting

;;; Code:

(require 'format-spec)
(require 'org)
(require 'org-id)
(require 'org-capture)
(require 'graphael-core)
(require 'organism-utils)
(require 'organism-entry)
(require 'organism-graph)

;; Declare free variables defined in main file.
(defvar organism-directory)
(defvar organism-capture-templates)

;;; Entry Completion

(defconst organism-capture--title-max-length 45
  "Maximum length for entry titles in completion.")

(defvar organism-capture--current-candidates nil
  "Current completion candidates for organism entries.")

(defun organism-capture--candidate-label (entry)
  "Format ENTRY as a candidate string for completion.
Returns a cons cell with displayable label and entry object."
  (cons
    (truncate-string-to-width (organism-entry-title entry)
      organism-capture--title-max-length nil ?\s "…")
    entry))

(defun organism-capture--candidate-annotator (candidate)
  "Annotate CANDIDATE for minibuffer completion selection.
Adds file, type, timestamp and tag information for selected entry."
  (when-let* ((entry (cdr (assoc candidate
                            organism-capture--current-candidates))))
    (or (graph-node-attr-get entry :annotation)
      (let* ((type (if (organism-entry-file-p entry) "file" "heading"))
             (tags (mapconcat 'identity (organism-entry-tags entry) ":"))
             (timestamp (organism-entry-property entry "CREATED"))
             (summary (organism-entry-property entry "SUMMARY"))
             (file (file-name-nondirectory
                     (or (organism-entry-property entry "FILE") "")))
             (annotation
               (concat
                 " "
                 (propertize (format "%-7s" type) 'face 'font-lock-type-face)
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

  (let* ((selection (organism-capture--completing-read prompt initial-input filter-fn))
         (entry (cdr (assoc selection organism-capture--current-candidates))))
    (cl-values selection entry)))

;;; Template Generation

(defun organism-capture--slugify (title)
  "Create a slug string from TITLE.
Converts to lowercase and replaces non-alphanumeric characters with hyphens."
  (if (string-blank-p title)
    "untitled"
    (let ((slug (downcase title)))
      ;; Replace common accented characters
      (setq slug (replace-regexp-in-string
                   "[áàâäãéèêëíìîïóòôöõúùûüçñ]"
                   (lambda (match)
                     (pcase match
                       ((or "á" "à" "â" "ä" "ã") "a")
                       ((or "é" "è" "ê" "ë") "e")
                       ((or "í" "ì" "î" "ï") "i")
                       ((or "ó" "ò" "ô" "ö" "õ") "o")
                       ((or "ú" "ù" "û" "ü") "u")
                       ("ç" "c")
                       ("ñ" "n")))
                   slug))
      ;; Replace non-alphanumeric with hyphens
      (setq slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
      (setq slug (replace-regexp-in-string "-+" "-" slug))
      (setq slug (replace-regexp-in-string "^-\\|-$" "" slug))

      (if (string-empty-p slug) "untitled" slug))))

(defun organism-capture--build-template (template &optional title)
  "Process TEMPLATE to create an org-capture ready template with substitutions.
TEMPLATE is a capture template formatted like `organism-capture-templates'.
TITLE is an optional string used for the note title and filename.
The function replaces format specifiers in the template:
%i = UUID, %t = timestamp, %T = title, %%? = cursor position.

Returns multiple values:
- Processed template ready for `org-capture'
- Generated UUID
- File path destination"
  (organism-check-type template 'list "template")
  (organism-check-type title '(or null string) "title")

  (let* ((id (org-id-uuid))
         (title (or title "untitled"))
         (slug (organism-capture--slugify title))
         (file-name (format "%s-%s.org" (format-time-string "%Y%m%d%H%M%S") slug))
         (file-path (expand-file-name file-name organism-directory))
         (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z"))  ; ISO 8601
         (template (copy-tree template))
         (template-string (nth 4 template))
         (spec (format-spec-make
                 ?i id
                 ?t timestamp
                 ?T title)))
    (organism-debug "Building template for %s at %s" title file-path)

    ;; Replace the file target with our specific file
    (setf (nth 3 template) `(file ,file-path))
    ;; Format the template with our values
    (let ((filled-template (format-spec template-string spec t)))
      (setf (nth 4 template) filled-template))
    ;; Return
    (cl-values template id file-path)))

(defun organism-capture--update-graph-after-capture (file-path id callback)
  "Set up a one-time capture hook to update the graph for FILE-PATH and ID.
After capture completes, run CALLBACK with ID if successful.
The hook registers the ID location and updates the graph when capture completes."
  (organism-check-type file-path 'string)
  (organism-check-type id 'string)
  (organism-check-type callback 'function)
  (organism-debug "Setting up after-finalize hook for %s" id)

  (cl-labels ((finalize-hook ()
                (remove-hook 'org-capture-after-finalize-hook #'finalize-hook)
                (condition-case err
                  (progn
                    (when (and (not org-note-abort)
                               (file-exists-p file-path))
                      (organism-debug "Registering ID location for %s at %s" id file-path)
                      (org-id-add-location id file-path)

                      ;; Update graph and handle possible errors
                      (if (and organism-graph
                               (organism-graph-update-file file-path))
                        (progn
                          (organism-debug "Successfully updated graph for new entry %s" id)
                          (funcall callback id))
                        (organism-debug "Failed to update graph for new entry %s" id))

                      ;; Verify ID was properly registered
                      (unless (org-id-find id 'marker)
                        (organism-debug "Failed to register ID location for %s" id))))
                  (error
                    (organism-debug "Error in capture finalization: %s"
                      (error-message-string err))))))
    (add-hook 'org-capture-after-finalize-hook #'finalize-hook)))

;;; Public Interface Functions

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
        (cl-multiple-value-bind (template id file-path)
          (organism-capture--build-template
            (car organism-capture-templates)  ; Select first template
            selection)
          (organism-capture--update-graph-after-capture file-path id #'ignore)
          ;; Override templates with `organism-capture-templates'
          (let ((org-capture-templates (list template))
                (template-key (car template)))
            (org-capture nil template-key)))))))

;;;###autoload
(defun organism-link (&optional filter-fn)
  "Find an organism entry and insert a link to it at point.
FILTER-FN filters completion candidates if provided.

When an existing entry is found, inserts a link to it immediately.
When creating a new entry, captures a new file and then inserts a link to it."
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
          (cl-multiple-value-bind (template id file-path)
            (organism-capture--build-template
              (car organism-capture-templates)  ; Select first template
              selection)
            (organism-capture--update-graph-after-capture
              file-path id
              (lambda (id)
                (with-current-buffer orig-buf
                  (goto-char orig-point)
                  (insert (org-link-make-string (concat "id:" id) selection)))))
            ;; Override templates with `organism-capture-templates'
            (let ((org-capture-templates (list template))
                  (template-key (car template)))
              (org-capture nil template-key))))))))

;;;###autoload
(defun organism-link-immediate (&optional filter-fn)
  "Insert a link to an organism entry without visiting it.
If no matching entry exists with FILTER-FN, create a placeholder.

Unlike `organism-link', this function creates entries immediately
without using the capture process, so no editing is performed."
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
        ;; For immediate linking, create the file directly
        (cl-multiple-value-bind (template id file-path)
          (organism-capture--build-template
            (car organism-capture-templates)  ; Select first template
            selection)
          (organism-debug "Creating immediate entry '%s' at %s" selection file-path)
          ;; Write template directly to file
          (with-temp-file file-path
            ;; Remove cursor locator from template
            (insert (replace-regexp-in-string "\\(%\\?\\)" ""
                      (nth 4 template))))
          ;; Register and update graph
          (org-id-add-location id file-path)
          (organism-graph-update-file file-path)
          ;; Insert link
          (insert (org-link-make-string (concat "id:" id) selection)))))))

(provide 'organism-capture)
;;; organism-capture.el ends here
