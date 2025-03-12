;;; organism-capture-test.el --- Tests for organism-capture.el -*- lexical-binding: t; -*-

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

;; Test suite for organism-capture.el

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'org-id)
(require 'organism-utils)
(require 'organism-entry)
(require 'organism-graph)
(require 'organism-capture)

;;; Test Setup

(defvar organism-capture-test--temp-dir nil
  "Temporary directory for test files.")

(defvar organism-capture-test--original-settings nil
  "Original settings before test setup.")

(defun organism-capture-test--setup ()
  "Set up test environment for organism-capture tests."
  ;; Save original settings
  (setq organism-capture-test--original-settings
    (list org-directory organism-directory
      (when (boundp 'organism-capture-templates)
        organism-capture-templates)))

  ;; Reset org-id locations
  (setq org-id-locations (make-hash-table :test 'equal))
  (setq org-id-files nil)

  ;; Create temp directory and set up directories
  (setq organism-capture-test--temp-dir
    (make-temp-file "organism-capture-test-" t))
  (setq org-directory organism-capture-test--temp-dir)
  (setq organism-directory organism-capture-test--temp-dir)

  ;; Set up test capture templates
  (setq organism-capture-templates
    '(("t" "Test Template" plain
        (file)
        ":PROPERTIES:\n:ID: %i\n:CREATED: %t\n:END:\n#+TITLE: %T\n\n%?")))

  ;; Initialize organism-graph
  (setq organism-graph nil)
  (organism-graph-start))

(defun organism-capture-test--teardown ()
  "Clean up test environment for organism-capture tests."
  ;; Restore original settings
  (cl-destructuring-bind (org-dir organism-dir templates)
    organism-capture-test--original-settings
    (setq org-directory org-dir)
    (setq organism-directory organism-dir)
    (when templates
      (setq organism-capture-templates templates)))

  ;; Stop organism-graph and clean up temp directory
  (organism-graph-stop)
  (when (and organism-capture-test--temp-dir
             (file-exists-p organism-capture-test--temp-dir))
    (delete-directory organism-capture-test--temp-dir t)))

(defun organism-capture-test--create-entry (id title &optional content tags)
  "Create test org file with ID, TITLE, CONTENT, and TAGS."
  (let* ((file-name (format "%s.org" (organism-capture--slugify title)))
         (tags-str (if tags
                     (format "\n#+FILETAGS: :%s:" (mapconcat #'identity tags ":"))
                     ""))
         (content (or content "Test content"))
         (file-path (expand-file-name
                      file-name organism-capture-test--temp-dir)))

    ;; Create the file
    (with-temp-file file-path
      (insert (format ":PROPERTIES:\n:ID: %s\n:CREATED: %s\n:END:\n#+TITLE: %s%s\n\n%s"
                id
                (format-time-string "%Y-%m-%d")
                title
                tags-str
                content)))

    ;; Register the ID and update the graph
    (org-id-add-location id file-path)
    (organism-graph-update-file file-path)
    ;; Return the entry object
    (organism-graph-get-entry id)))

;;; Utility Tests

(ert-deftest organism-capture-test-slugify ()
  "Test organism-capture--slugify function with various inputs."
  (should (string= (organism-capture--slugify "Hello World!") "hello-world"))
  (should (string= (organism-capture--slugify "Test 123") "test-123"))
  (should (string= (organism-capture--slugify "  spaces  ") "spaces"))
  (should (string= (organism-capture--slugify "Multiple---Dashes") "multiple-dashes"))
  (should (string= (organism-capture--slugify "-trim-edges-") "trim-edges"))
  (should (string= (organism-capture--slugify "") "untitled"))

  ;; Edge cases
  (should (string= (organism-capture--slugify "   ") "untitled"))
  (should (string= (organism-capture--slugify "Café") "cafe"))
  (should (string= (organism-capture--slugify "Naïve") "naive"))

  ;; Long titles shouldn't cause errors
  (let* ((long-title (make-string 150 ?a))
         (slugified (organism-capture--slugify long-title)))
    (should (<= (length slugified) 150))
    (should (string-match-p "^a+$" slugified))))

(ert-deftest organism-capture-test-build-template ()
  "Test organism-capture--build-template function."
  (unwind-protect
    (progn
      (organism-capture-test--setup)
      (let* ((template '("t" "Test Template" plain (file)
                          ":PROPERTIES:\n:ID: %i\n:CREATED: %t\n:END:\n#+TITLE: %T\n\n%?")))

        ;; Test with normal title
        (cl-multiple-value-bind (processed-template id file-path)
          (organism-capture--build-template template "Test Entry")

          ;; Check ID and file path
          (should (graph-uuid-p id))
          (should (string-match-p "test-entry\\.org$" file-path))

          ;; Check template substitutions
          (let ((template-string (nth 4 processed-template)))
            (should (string-match-p (regexp-quote "#+TITLE: Test Entry") template-string))
            (should (string-match-p (concat "ID: " id) template-string)))

          ;; Check file target
          (should (equal (nth 3 processed-template) `(file ,file-path))))

        ;; Test with empty title
        (cl-multiple-value-bind (processed-template id file-path)
          (organism-capture--build-template template "")
          (should (graph-uuid-p id))
          (should (string-match-p "untitled\\.org$" file-path))
          (let ((template-string (nth 4 processed-template)))
            (should (string-match-p "#\\+TITLE:" template-string))))))
    (organism-capture-test--teardown)))

;;; Candidate Formatting Tests

(ert-deftest organism-capture-test-candidates ()
  "Test candidate formatting and annotation functions."
  (unwind-protect
    (progn
      (organism-capture-test--setup)

      ;; Create test entries
      (let* ((id1 (org-id-uuid))
             (entry1 (organism-capture-test--create-entry id1 "First Entry" "Content" '("tag1" "tag2"))))
        ;; Test candidate label formatting
        (let ((label1 (organism-capture--candidate-label entry1)))
          (should (consp label1))
          (should (string= (org-trim (car label1)) "First Entry"))
          (should (eq (cdr label1) entry1)))

        ;; Test candidates retrieval
        (let ((candidates (organism-capture--get-candidates)))
          (should (>= (length candidates) 1))
          (should (cl-find-if (lambda (c)
                                (string= (org-trim (car c)) "First Entry"))
                    candidates)))

        ;; Test filtered candidates
        (let ((candidates (organism-capture--get-candidates
                            (lambda (entry)
                              (string= (organism-entry-title entry) "First Entry")))))
          (should (= (length candidates) 1)))))
    (organism-capture-test--teardown)))

;;; Mock Utilities

(defmacro organism-capture-test--with-mocks (bindings &rest body)
  "Execute BODY with temporary function definitions.
BINDINGS is an alist of (FUNCTION-NAME . FUNCTION-DEFINITION)."
  (declare (indent 1))
  (let ((orig-defs (cl-loop for (func . _) in bindings
                     collect (list (gensym) func))))
    `(let ,(mapcar (lambda (def)
                     `(,(car def) (symbol-function ',(cadr def))))
             orig-defs)
       (unwind-protect
         (progn
           ,@(mapcar (lambda (binding)
                       `(cl-letf (((symbol-function ',(car binding)) ,(cdr binding)))
                          nil))
               bindings)
           ,@body)
         ;; Restore original functions
         ,@(mapcar (lambda (def)
                     `(fset ',(cadr def) ,(car def)))
             orig-defs)))))

;;; Integration Tests

(ert-deftest organism-capture-test-find ()
  "Test organism-find with existing and new entries."
  (unwind-protect
    (progn
      (organism-capture-test--setup)

      ;; Test with existing entry
      (let* ((id (org-id-uuid))
             (entry (organism-capture-test--create-entry id "Test Entry" "Content"))
             (goto-called nil)
             (goto-id nil))

        ;; Test finding existing entry
        (cl-letf (((symbol-function 'organism-capture--complete-entry)
                    (lambda (&rest _) (cl-values "Test Entry" entry)))
                   ((symbol-function 'org-id-goto)
                     (lambda (id) (setq goto-called t goto-id id))))

          (organism-find)
          (should goto-called)
          (should (equal goto-id id))))

      ;; Test with new entry
      (let ((new-title "New Entry")
            (template-used nil)
            (capture-called nil))

        (cl-letf (((symbol-function 'organism-capture--complete-entry)
                    (lambda (&rest _) (cl-values new-title nil)))
                   ((symbol-function 'y-or-n-p) (lambda (_) t))
                   ((symbol-function 'organism-capture--build-template)
                     (lambda (template _)
                       (setq template-used template)
                       (cl-values template "test-id" (expand-file-name "test-file.org" organism-capture-test--temp-dir))))
                   ((symbol-function 'organism-capture--update-graph-after-capture)
                     (lambda (&rest _) t))
                   ((symbol-function 'org-capture)
                     (lambda (&rest _) (setq capture-called t))))

          (organism-find)
          (should template-used)
          (should capture-called))))
    (organism-capture-test--teardown)))

(ert-deftest organism-capture-test-link ()
  "Test organism-link and organism-link-immediate functions."
  (unwind-protect
    (progn
      (organism-capture-test--setup)

      ;; Test linking to existing entry
      (let* ((id (org-id-uuid))
             (entry (organism-capture-test--create-entry id "Test Entry" "Content"))
             (test-buffer (generate-new-buffer "*organism-test*")))

        (with-current-buffer test-buffer
          (org-mode)
          ;; Test with existing entry
          (cl-letf (((symbol-function 'organism-capture--complete-entry)
                      (lambda (&rest _) (cl-values "Test Entry" entry))))

            (organism-link)
            (should (string-match-p (format "\\[\\[id:%s\\]\\[Test Entry\\]\\]" id)
                      (buffer-string))))

          ;; Test with immediate creation
          (erase-buffer)
          (cl-letf (((symbol-function 'organism-capture--complete-entry)
                      (lambda (&rest _) (cl-values "New Entry" nil)))
                     ((symbol-function 'y-or-n-p) (lambda (_) t))
                     ((symbol-function 'organism-capture--build-template)
                       (lambda (&rest _)
                         (cl-values '("t" "Test" plain (file) "test") "test-id"
                           (expand-file-name "test-file.org" organism-capture-test--temp-dir) )))
                     ((symbol-function 'with-temp-file)
                       (lambda (_ &rest _) t))
                     ((symbol-function 'org-id-add-location) (lambda (&rest _) t))
                     ((symbol-function 'organism-graph-update-file) (lambda (&rest _) t)))

            (organism-link-immediate)
            (should (string-match-p "\\[\\[id:test-id\\]\\[New Entry\\]\\]"
                      (buffer-string)))))

        ;; Clean up test buffer
        (kill-buffer test-buffer)))
    (organism-capture-test--teardown)))

(ert-deftest organism-capture-test-update-graph-hook ()
  "Test organism-capture--update-graph-after-capture hook."
  (unwind-protect
    (progn
      (organism-capture-test--setup)
      ;; Create test file and set up hook
      (let* ((id (org-id-uuid))
             (file-path (expand-file-name "test-capture.org"
                          organism-capture-test--temp-dir))
             (callback-called nil)
             (callback-id nil))
        ;; Create the file
        (with-temp-file file-path
          (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n#+TITLE: Test\n\n" id)))

        ;; Set up the hook
        (organism-capture--update-graph-after-capture
          file-path id (lambda (received-id)
                         (setq callback-called t
                           callback-id received-id)))

        ;; Simulate successful capture
        (let ((org-note-abort nil))
          (run-hooks 'org-capture-after-finalize-hook))

        (should callback-called)
        (should (equal callback-id id))
        (should (member file-path org-id-files))))
    (organism-capture-test--teardown)))

(provide 'organism-capture-test)
;;; organism-capture-test.el ends here
