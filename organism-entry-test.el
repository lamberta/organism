;;; organism-entry-test.el --- Tests for organism-entry.el -*- lexical-binding: t; -*-

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

;; Test suite for organism-entry.el

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'organism-utils)
(require 'organism-entry)

;;; Test Setup

(defvar organism-entry-test--temp-dir nil)
(defvar organism-entry-test--files nil)

(defun organism-entry-test--setup ()
  "Set up test environment."
  ;; Reset org-id locations
  (setq org-id-locations (make-hash-table :test 'equal))
  (setq org-id-files nil)
  ;; Create temp directory
  (setq organism-entry-test--temp-dir (make-temp-file "organism-test-" t))
  (setq organism-entry-test--files nil))

(defun organism-entry-test--teardown ()
  "Clean up test environment."
  (when (and organism-entry-test--temp-dir (file-exists-p organism-entry-test--temp-dir))
    (delete-directory organism-entry-test--temp-dir t))
  (setq organism-entry-test--files nil))

(defun organism-entry-test--register-id (id file)
  "Register ID directly to FILE in the org-id-locations hash."
  (puthash id (file-truename file) org-id-locations)
  (add-to-list 'org-id-files file))

(defun organism-entry-test--create-file (filename content)
  "Create test file with FILENAME and CONTENT."
  (let ((file-path (expand-file-name filename organism-entry-test--temp-dir)))
    (with-temp-file file-path
      (insert content))
    (push file-path organism-entry-test--files)
    file-path))

(defun organism-entry-test--create-org-file (id title content)
  "Create test org file with ID, TITLE and CONTENT."
  (let ((file-path (file-truename  ; Use file-truename to normalize path
                     (organism-entry-test--create-file
                       (format "%s.org" (or title "test"))
                       (format ":PROPERTIES:\n:ID:       %s\n:END:\n#+TITLE: %s\n\n%s"
                         (or id (org-id-uuid))
                         (or title "Test File")
                         (or content ""))))))
    ;; Manually register the file ID
    (organism-entry-test--register-id id file-path)
    file-path))

;;; Basic Initialization Tests

(ert-deftest organism-entry-test-init ()
  "Test organism-entry creation and initialization."
  (let ((id (org-id-uuid))
        (file-path nil))
    (unwind-protect
      (progn
        (organism-entry-test--setup)
        (setq file-path (organism-entry-test--create-org-file id "Test" "Content"))

        ;; Create an entry
        (let ((entry (make-instance 'organism-entry :id id :level 0)))
          ;; Basic structure tests
          (should (object-of-class-p entry 'organism-entry))
          (should (object-of-class-p entry 'graph-node))
          (should (string= (node-id entry) id))
          (should (= (organism-entry-level entry) 0))
          (should (string= (organism-entry-title entry) "Test"))
          ;; Verify properties
          (should (alist-get "FILE" (slot-value entry 'properties) nil nil #'cl-equalp))
          (should (string= (organism-entry-property entry "FILE") file-path))
          ;; Test predicates
          (should (organism-entry-file-p entry))
          (should-not (organism-entry-heading-p entry))))
      ;; Cleanup
      (organism-entry-test--teardown))))

(ert-deftest organism-entry-test-heading ()
  "Test organism-entry with headline entries."
  (let ((file-id (org-id-uuid))
        (heading-id (org-id-uuid))
        (file-path nil))
    (unwind-protect
      (progn
        (organism-entry-test--setup)
        (setq file-path (organism-entry-test--create-org-file file-id "Test"
                          (format "* Heading\n  :PROPERTIES:\n  :ID: %s\n  :END:\n  Content" heading-id)))

        ;; Manually register the heading ID
        (organism-entry-test--register-id heading-id file-path)
        ;; Create heading entry
        (let ((entry (make-instance 'organism-entry :id heading-id)))
          ;; Basic structure tests
          (should (object-of-class-p entry 'organism-entry))
          (should (= (organism-entry-level entry) 1))
          (should (string= (organism-entry-title entry) "Heading"))
          ;; Test predicates
          (should-not (organism-entry-file-p entry))
          (should (organism-entry-heading-p entry))))
      ;; Cleanup
      (organism-entry-test--teardown))))

;;; Property and Attribute Tests

(ert-deftest organism-entry-test-property ()
  "Test organism-entry property access."
  (let ((file-id (org-id-uuid))
        (file-path nil))
    (unwind-protect
      (progn
        (organism-entry-test--setup)
        ;; Create file with properties
        (setq file-path (organism-entry-test--create-org-file
                          file-id "Properties Test"
                          "#+FILETAGS: :test:tag:\n\n* Heading\n  :PROPERTIES:\n  :CUSTOM: value\n  :END:"))
        ;; Create entry and test property access
        (let ((entry (make-instance 'organism-entry :id file-id)))
          ;; Test organism-entry-property
          (should (string= (organism-entry-property entry "FILE") file-path))
          (should (string= (organism-entry-property entry "ALLTAGS") ":test:tag:"))
          (should-not (organism-entry-property entry "NONEXISTENT"))
          (should (string= (organism-entry-property entry "NONEXISTENT" "default") "default"))))
      ;; Cleanup
      (organism-entry-test--teardown))))

(ert-deftest organism-entry-test-label ()
  "Test organism-entry-label function."
  (let ((file-id (org-id-uuid))
        (heading-id (org-id-uuid)))
    (unwind-protect
      (progn
        (organism-entry-test--setup)
        ;; Create test file with heading
        (let ((file-path (organism-entry-test--create-org-file
                           file-id "Test File"
                           (format "* Heading\n  :PROPERTIES:\n  :ID: %s\n  :END:" heading-id))))
          ;; Test file label
          (let ((file-entry (make-instance 'organism-entry :id file-id)))
            (should (string-match-p "Test File.org (Test File)" (organism-entry-label file-entry))))
          ;; Test heading label
          (organism-entry-test--register-id heading-id file-path)
          (let ((heading-entry (make-instance 'organism-entry :id heading-id)))
            (should (string-match-p "Test File.org > Heading" (organism-entry-label heading-entry))))))
      ;; Cleanup
      (organism-entry-test--teardown))))

(ert-deftest organism-entry-test-element ()
  "Test basic organism-entry-element functionality."
  (let ((file-id (org-id-uuid))
        (heading-id (org-id-uuid))
        (file-path nil))
    (unwind-protect
      (progn
        (organism-entry-test--setup)
        (setq file-path (organism-entry-test--create-org-file
                          file-id "Element Test"
                          (format "* Heading\n  :PROPERTIES:\n  :ID: %s\n  :END:" heading-id)))
        (organism-entry-test--register-id heading-id file-path)
        ;; Test file-level element
        (let* ((file-entry (make-instance 'organism-entry :id file-id))
               (file-element (organism-entry-element file-entry)))
          (should file-element)
          (should (eq (org-element-type file-element) 'property-drawer)))
        ;; Test heading-level element
        (let* ((heading-entry (make-instance 'organism-entry :id heading-id))
               (heading-element (organism-entry-element heading-entry)))
          (should heading-element)
          (should (eq (org-element-type heading-element) 'headline))
          (should (= (org-element-property :level heading-element) 1))))
      ;; Cleanup
      (organism-entry-test--teardown))))

(ert-deftest organism-entry-test-element-caching ()
  "Test caching in organism-entry-element."
  (let ((file-id (org-id-uuid))
        (file-path nil))
    (unwind-protect
      (progn
        (organism-entry-test--setup)
        ;; Create test file
        (setq file-path (organism-entry-test--create-org-file
                          file-id "Element Test" "Test content"))
        (organism-entry-test--register-id file-id file-path)
        ;; Create entry
        (let ((entry (make-instance 'organism-entry :id file-id)))
          ;; First call should cache the element
          (let ((element1 (organism-entry-element entry)))
            (should element1)
            (should (eq (org-element-type element1) 'property-drawer))
            ;; Verify cache is populated
            (should (graph-node-attr-get entry :cached-element))
            ;; Second call should return cached element
            (let ((element2 (organism-entry-element entry)))
              (should (eq element1 element2)))
            ;; Refresh should clear cache
            (organism-entry--refresh entry)
            (should-not (graph-node-attr-get entry :cached-element)))))
      ;; Cleanup
      (organism-entry-test--teardown))))

;;; Content Extraction Tests

(ert-deftest organism-entry-test-tags ()
  "Test organism-entry tag handling."
  (let ((file-id (org-id-uuid))
        (file-path nil))
    (unwind-protect
      (progn
        (organism-entry-test--setup)
        ;; Create file with tags using property drawer format
        (setq file-path (organism-entry-test--create-file
                          (format "Tags Test.org")
                          (format ":PROPERTIES:\n:ID:       %s\n:END:\n#+TITLE: Tags Test\n#+FILETAGS: :test:tag:\n\n* Heading\n  :PROPERTIES:\n  :ID:       %s\n  :TAGS: :local:\n  :END:"
                            file-id
                            (org-id-uuid))))
        (organism-entry-test--register-id file-id file-path)
        ;; Create entry and test tags
        (let ((entry (make-instance 'organism-entry :id file-id)))
          ;; Check tag extraction
          (let ((tags (organism-entry-tags entry)))
            (should tags)
            (should (member "test" tags))
            (should (member "tag" tags)))))
      ;; Cleanup
      (organism-entry-test--teardown))))

(ert-deftest organism-entry-test-tags-caching ()
  "Test organism-entry refreshing file tags."
  (let ((file-id (org-id-uuid))
        (file-path nil))
    (unwind-protect
      (progn
        (organism-entry-test--setup)
        ;; Create initial file with tags
        (setq file-path (organism-entry-test--create-file "TagsTest.org"
                          (format ":PROPERTIES:\n:ID:       %s\n:END:\n#+TITLE: Tags Test\n#+FILETAGS: :one:two:\n\nContent" file-id)))
        (organism-entry-test--register-id file-id file-path)

        ;; Create entry and verify initial tags
        (let ((entry (make-instance 'organism-entry :id file-id)))
          (let ((initial-tags (organism-entry-tags entry)))
            (should (= (length initial-tags) 2))
            (should (member "one" initial-tags))
            (should (member "two" initial-tags)))

          ;; Modify file tags
          (with-temp-file file-path
            (insert (format ":PROPERTIES:\n:ID:       %s\n:END:\n#+TITLE: Tags Test\n#+FILETAGS: :one:two:three:\n\nContent" file-id)))
          ;; Refresh and verify updated tags
          (organism-entry--refresh entry)
          (let ((updated-tags (organism-entry-tags entry)))
            (should (= (length updated-tags) 3))
            (should (member "one" updated-tags))
            (should (member "two" updated-tags))
            (should (member "three" updated-tags)))))
      ;; Cleanup
      (organism-entry-test--teardown))))

(ert-deftest organism-entry-test-links ()
  "Test organism-entry link extraction."
  (let ((file-id (org-id-uuid))
        (target-id-1 (org-id-uuid))
        (target-id-2 (org-id-uuid)))
    (unwind-protect
      (progn
        (organism-entry-test--setup)
        ;; Create file with various links
        (organism-entry-test--create-org-file
          file-id "Link Test"
          (format "This is a [[id:%s][test link]] and [[https://example.com][a URL]].

* Heading
  [[id:%s][Another link]]"
            target-id-1 target-id-2))

        ;; Create entry and test links
        (let ((entry (make-instance 'organism-entry :id file-id)))
          ;; File-level entry gets all links by default
          (let ((links (organism-entry-links entry "id")))
            (should links)
            (should (= (length links) 2)) ; Both ID links in the file
            (should (cl-some (lambda (link) (equal (plist-get link :target) target-id-1)) links))
            (should (cl-some (lambda (link) (equal (plist-get link :target) target-id-2)) links)))

          ;; All links
          (let ((links (organism-entry-links entry nil)))
            (should (= (length links) 3)) ; Two ID links + URL link
            (should (cl-some (lambda (link) (equal (plist-get link :type) "https")) links)))

          ;; Test for a heading entry
          (let* ((heading-entry (make-instance 'organism-entry
                                  :id (org-id-uuid)
                                  :level 1))
                 (links-in-heading (organism-entry-links heading-entry "id")))
            ;; This is an empty/invalid entry, so it should return nil or empty list
            (should-not links-in-heading))))
      ;; Cleanup
      (organism-entry-test--teardown))))

;;; Refresh and Caching Tests

(ert-deftest organism-entry-test-link-caching ()
  "Test caching in organism-entry-links."
  (let ((file-id (org-id-uuid))
        (file-path nil))
    (unwind-protect
      (progn
        (organism-entry-test--setup)
        ;; Create file with various links
        (setq file-path (organism-entry-test--create-file
                          "Links.org"
                          (format ":PROPERTIES:\n:ID: %s\n:END:\n#+TITLE: Link Test\n\n[[id:12345][ID Link]]\n[[https://example.com][URL Link]]\n[[file:test.txt][File Link]]"
                            file-id)))
        (organism-entry-test--register-id file-id file-path)
        ;; Test file entry
        (let ((file-entry (make-instance 'organism-entry :id file-id)))
          ;; First call populates cache
          (let ((all-links (organism-entry-links file-entry)))
            (should (= (length all-links) 3))
            ;; Verify cache is populated
            (should (graph-node-attr-get file-entry :links-entry-only))
            ;; Modify the cached value to test it's being used
            (let ((modified-links (list (list :type "modified" :target "test" :raw-link "test"))))
              (graph-node-attr-put file-entry :links-entry-only modified-links)
              ;; Next call should use the modified cache
              (let ((cached-links (organism-entry-links file-entry)))
                (should (equal cached-links modified-links))
                (should (= (length cached-links) 1))
                (should (equal (plist-get (car cached-links) :type) "modified"))))
            ;; Verify refresh clears cache
            (organism-entry--refresh file-entry)
            (should-not (graph-node-attr-get file-entry :links-entry-only)))))
      ;; Cleanup
      (organism-entry-test--teardown))))

(ert-deftest organism-entry-test-link-cache-invalidation ()
  "Test link cache invalidation on file changes."
  (let ((file-id (org-id-uuid))
         (file-path nil))
    (unwind-protect
      (progn
        (organism-entry-test--setup)
        ;; Create file with one link
        (setq file-path (organism-entry-test--create-file
                          "Changing.org"
                          (format ":PROPERTIES:\n:ID: %s\n:END:\n#+TITLE: Cache Test\n\n[[id:12345][ID Link]]"
                            file-id)))
        (organism-entry-test--register-id file-id file-path)
        ;; Build initial cache
        (let ((entry (make-instance 'organism-entry :id file-id)))
          ;; Get links to populate cache
          (let ((initial-links (organism-entry-links entry)))
            (should (= (length initial-links) 1))
            ;; Add a new link to the file
            (with-temp-file file-path
              (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n#+TITLE: Cache Test\n\n[[id:12345][ID Link]]\n\n[[https://example.com][New Link]]"
                        file-id)))
            ;; Without refresh, should get cached result
            (let ((cached-links (organism-entry-links entry)))
              (should (= (length cached-links) 1)))
            ;; After refresh, should get updated links
            (organism-entry--refresh entry)
            (let ((updated-links (organism-entry-links entry)))
              (should (= (length updated-links) 2))))))
      ;; Cleanup
      (organism-entry-test--teardown))))

(ert-deftest organism-entry-test-refresh ()
  "Test organism-entry refreshing."
  (let ((file-id (org-id-uuid))
        (file-path nil))
    (unwind-protect
      (progn
        (organism-entry-test--setup)
        ;; Create initial file with property drawer
        (setq file-path (organism-entry-test--create-file
                          "Test.org"
                          (format ":PROPERTIES:\n:ID:       %s\n:END:\n#+TITLE: Initial\n\nContent" file-id)))
        (organism-entry-test--register-id file-id file-path)
        ;; Create entry and verify initial state
        (let ((entry (make-instance 'organism-entry :id file-id)))
          (should (string= (organism-entry-title entry) "Initial"))
          ;; Modify file content, maintaining property drawer format
          (with-temp-file file-path
            (insert (format ":PROPERTIES:\n:ID:       %s\n:END:\n#+TITLE: Updated\n\nNew content" file-id)))
          ;; Refresh and verify updated state
          (organism-entry--refresh entry)
          (should (string= (organism-entry-title entry) "Updated"))))
      ;; Cleanup
      (organism-entry-test--teardown))))

(provide 'organism-entry-test)
;;; organism-entry-test.el ends here
