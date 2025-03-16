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

;;; Test setup

(defvar organism-capture-test--temp-dir nil
  "Temporary directory for test files.")

(defvar organism-capture-test--original-settings nil
  "Original settings before test setup.")

(defun organism-capture-test--setup ()
  "Set up test environment for organism-capture tests."
  ;; Save original settings
  (setq organism-capture-test--original-settings
    (list org-directory
          organism-directory
          org-capture-templates
          (when (boundp 'organism-capture-templates-default)
            organism-capture-templates-default)))

  ;; Reset org-id locations
  (setq org-id-locations (make-hash-table :test 'equal))
  (setq org-id-files nil)

  ;; Create temp directory and set up directories
  (setq organism-capture-test--temp-dir
    (make-temp-file "organism-capture-test-" t))
  (setq org-directory organism-capture-test--temp-dir)
  (setq organism-directory organism-capture-test--temp-dir)

  ;; Set up test capture templates
  (setq org-capture-templates
    '(("t" "Test Template" plain
        (file)
        ":PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED: %<%Y-%m-%dT%H:%M:%S%z>\n:END:\n#+TITLE: %^{Title}\n\n%?")))

  ;; Set default template key
  (setq organism-capture-templates-default "t")

  ;; Initialize organism-graph
  (setq organism-graph nil)
  (organism-graph-start))

(defun organism-capture-test--teardown ()
  "Clean up test environment for organism-capture tests."
  ;; Restore original settings
  (cl-destructuring-bind (org-dir organism-dir templates template-default)
    organism-capture-test--original-settings
    (setq org-directory org-dir)
    (setq organism-directory organism-dir)
    (setq org-capture-templates templates)
    (when template-default
      (setq organism-capture-templates-default template-default)))

  ;; Stop organism-graph and clean up temp directory
  (organism-graph-stop)
  (when (and organism-capture-test--temp-dir
             (file-exists-p organism-capture-test--temp-dir))
    (delete-directory organism-capture-test--temp-dir t)))

(defun organism-capture-test--create-entry (id title &optional content tags)
  "Create test org file with ID, TITLE, CONTENT, and TAGS."
  (let* ((file-name (format "%s.org" (organism-utils-slugify title)))
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
                (format-time-string "%Y-%m-%dT%H:%M:%S%z")
                title
                tags-str
                content)))

    ;; Register the ID and update the graph
    (org-id-add-location id file-path)
    (organism-graph-update-file file-path)
    ;; Return the entry object
    (organism-graph-get-entry id)))

;;; Utility tests

(ert-deftest organism-capture-test-get-template-key ()
  "Test organism-capture--get-template-key function."
  (unwind-protect
    (progn
      (organism-capture-test--setup)
      ;; Test with default set and matching template
      (let ((organism-capture-templates-default "t"))
        (should (string= (organism-capture--get-template-key) "t")))
      ;; Test with non-existent default key
      (let ((organism-capture-templates-default "nonexistent"))
        (should-error (organism-capture--get-template-key)))
      ;; Test with no default key
      (let ((organism-capture-templates-default nil))
        (should (string= (organism-capture--get-template-key) "t"))))
    (organism-capture-test--teardown)))

;;; Candidate formatting tests

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

(ert-deftest organism-capture-test-annotator-with-category ()
  "Test that organism-capture--candidate-annotator properly handles categories."
  (unwind-protect
    (progn
      (organism-capture-test--setup)
      ;; Create test entries with and without categories
      (let* ((id1 (org-id-uuid))
             (id2 (org-id-uuid))
              ;; Entry with default category
              (entry1 (organism-capture-test--create-entry id1 "Regular Entry" "Content"))
              ;; Entry with custom category
              (file-path (expand-file-name "with-category.org"
                           organism-capture-test--temp-dir)))

        ;; Create file with explicit category
        (with-temp-file file-path
          (insert
            (format ":PROPERTIES:\n:ID: %s\n:CATEGORY: CustomCat\n:CREATED: %s\n:END:\n#+TITLE: Categorized\n\nContent"
              id2
              (format-time-string "%Y-%m-%dT%H:%M:%S%z"))))

        ;; Register the ID and update the graph
        (org-id-add-location id2 file-path)
        (organism-graph-update-file file-path)
        (let ((entry2 (organism-graph-get-entry id2)))
          ;; Setup organism-capture--current-candidates for testing
          (setq organism-capture--current-candidates
            (list (cons (car (organism-capture--candidate-label entry1)) entry1)
                  (cons (car (organism-capture--candidate-label entry2)) entry2)))

          ;; Test annotation with default category
          (let* ((label1 (car (organism-capture--candidate-label entry1)))
                 (annotation1 (organism-capture--candidate-annotator label1)))
            (should annotation1)
            (should (string-match-p "file" annotation1)))

          ;; Test annotation with custom category
          (let* ((label2 (car (organism-capture--candidate-label entry2)))
                 (annotation2 (organism-capture--candidate-annotator label2)))
            (should annotation2)
            (should (string-match-p "CustomCat" annotation2))
            (should-not (string-match-p "file" annotation2))))))
    (organism-capture-test--teardown)))

;;; Mock utilities

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

;;; Integration tests

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
            (capture-called nil)
            (template-key nil))

        (cl-letf (((symbol-function 'organism-capture--complete-entry)
                    (lambda (&rest _) (cl-values new-title nil)))
                   ((symbol-function 'y-or-n-p) (lambda (_) t))
                   ((symbol-function 'organism-capture--get-template-key)
                     (lambda () "t"))
                   ((symbol-function 'org-capture)
                     (lambda (_arg key)
                       (setq capture-called t
                             template-key key))))

          (organism-find)
          (should capture-called)
          (should (string= template-key "t")))))
    (organism-capture-test--teardown)))

(ert-deftest organism-capture-test-link ()
  "Test organism-link function."
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

          ;; Test with new entry
          (erase-buffer)
          (cl-letf (((symbol-function 'organism-capture--complete-entry)
                      (lambda (&rest _) (cl-values "New Entry" nil)))
                     ((symbol-function 'y-or-n-p) (lambda (_) t))
                     ((symbol-function 'organism-capture--get-template-key)
                       (lambda () "t"))
                     ((symbol-function 'organism-capture--update-graph-after-capture)
                       (lambda (callback)
                         ;; Simulate successful capture and callback
                         (funcall callback "test-id")))
                     ((symbol-function 'org-capture)
                       (lambda (_arg _key) t)))

            (organism-link)
            (should (string-match-p "\\[\\[id:test-id\\]\\[New Entry\\]\\]"
                      (buffer-string)))))

        ;; Clean up test buffer
        (kill-buffer test-buffer)))
    (organism-capture-test--teardown)))

(ert-deftest organism-capture-test-link-immediate ()
  "Test organism-link-immediate function."
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

            (organism-link-immediate)
            (should (string-match-p (format "\\[\\[id:%s\\]\\[Test Entry\\]\\]" id)
                      (buffer-string))))

          ;; Test with immediate creation
          (erase-buffer)
          (cl-letf (((symbol-function 'organism-capture--complete-entry)
                      (lambda (&rest _) (cl-values "New Entry" nil)))
                     ((symbol-function 'y-or-n-p) (lambda (_) t))
                     ((symbol-function 'organism-capture--get-template-key)
                       (lambda () "t"))
                     ((symbol-function 'organism-capture--update-graph-after-capture)
                       (lambda (callback)
                         ;; Simulate successful capture and callback
                         (funcall callback "test-id")))
                     ((symbol-function 'org-capture)
                       (lambda (_arg _key) t)))

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
      ;; Set up test variables
      (let* ((id (org-id-uuid))
             (callback-called nil)
             (callback-id nil)
             ;; Create a mock last stored marker
             (test-buffer (generate-new-buffer "*organism-test-file*"))
             (marker (with-current-buffer test-buffer
                       (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n#+TITLE: Test\n\n" id))
                       (goto-char (point-min))
                       (point-marker)))
             (test-file (expand-file-name "test-file.org" organism-capture-test--temp-dir)))

        ;; Set mock buffer file name
        (with-current-buffer test-buffer
          (setq-local buffer-file-name test-file))

        ;; Set up capture variables
        (let ((org-note-abort nil)
              (org-capture-last-stored-marker marker))

          ;; Set up the callback function
          (organism-capture--update-graph-after-capture
            (lambda (received-id)
              (setq callback-called t
                    callback-id received-id)))

          ;; Run the capture hook
          (run-hooks 'org-capture-after-finalize-hook)

          ;; Check if callback was called with the right ID
          (should callback-called)
          (should (equal callback-id id)))

        ;; Clean up
        (kill-buffer test-buffer)))
    (organism-capture-test--teardown)))

(provide 'organism-capture-test)
;;; organism-capture-test.el ends here
