;;; organism-graph-test.el --- Tests for organism-graph.el -*- lexical-binding: t; -*-

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

;; Test suite for organism-graph.el

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'organism-utils)
(require 'organism-entry)
(require 'organism-graph)

;;; Test Setup

(defvar organism-graph-test--temp-dir nil)
(defvar organism-graph-test--files nil)

(defun organism-graph-test--setup ()
  "Set up test environment."
  ;; Reset org-id locations
  (setq org-id-locations (make-hash-table :test 'equal))
  (setq org-id-files nil)
  ;; Create temp directory
  (setq organism-graph-test--temp-dir (make-temp-file "organism-graph-test-" t))
  (setq organism-graph-test--files nil)
  ;; Ensure organism-graph is nil
  (setq organism-graph nil))

(defun organism-graph-test--teardown ()
  "Clean up test environment."
  (when (and organism-graph-test--temp-dir (file-exists-p organism-graph-test--temp-dir))
    (delete-directory organism-graph-test--temp-dir t))
  (setq organism-graph-test--files nil)
  (setq organism-graph nil))

(defun organism-graph-test--register-id (id file)
  "Register ID directly to FILE in the org-id-locations hash."
  (puthash id (file-truename file) org-id-locations)
  (add-to-list 'org-id-files file))

(defun organism-graph-test--create-file (filename content)
  "Create test file with FILENAME and CONTENT."
  (let ((file-path (expand-file-name filename organism-graph-test--temp-dir)))
    (with-temp-file file-path
      (insert content))
    (push file-path organism-graph-test--files)
    file-path))

(defun organism-graph-test--create-org-file (id title content)
  "Create test org file with ID, TITLE and CONTENT."
  (let ((file-path (file-truename  ; Use file-truename to normalize path
                     (organism-graph-test--create-file
                       (format "%s.org" (or title "test"))
                       (format ":PROPERTIES:\n:ID:       %s\n:END:\n#+TITLE: %s\n\n%s"
                         (or id (org-id-uuid))
                         (or title "Test File")
                         (or content ""))))))
    ;; Manually register the file ID
    (organism-graph-test--register-id id file-path)
    file-path))

;;; Basic Graph Tests

(ert-deftest organism-graph-test-build ()
  "Test building the organism graph."
  (let ((old-org-directory org-directory)
        (old-organism-directory organism-directory)
        (id1 (org-id-uuid))
        (id2 (org-id-uuid))
        (id3 (org-id-uuid)))
    (unwind-protect
      (progn
        (organism-graph-test--setup)
        ;; Set directories to test directory
        (setq org-directory organism-graph-test--temp-dir)
        (setq organism-directory organism-graph-test--temp-dir)
        ;; Create test files with linking content
        (organism-graph-test--create-org-file
          id1 "File1"
          (format "This links to [[id:%s][file 2]]." id2))

        (organism-graph-test--create-org-file
          id2 "File2"
          (format "This links to [[id:%s][file 3]]." id3))

        (organism-graph-test--create-org-file
          id3 "File3"
          (format "This links back to [[id:%s][file 1]]." id1))

        ;; Initialize and check structure
        (organism-graph-start)

        ;; Verify graph content
        (should (= (graph-node-count organism-graph) 3))
        (should (= (graph-edge-count organism-graph) 3))

        ;; Check specific nodes
        (should (graph-node-p organism-graph id1))
        (should (graph-node-p organism-graph id2))
        (should (graph-node-p organism-graph id3))

        ;; Check entry properties
        (let ((entry1 (organism-graph-get-entry id1)))
          (should entry1)
          (should (string= (organism-entry-title entry1) "File1"))
          (should (organism-entry-file-p entry1))
          (should-not (organism-entry-heading-p entry1))))
      ;; Cleanup
      (setq org-directory old-org-directory)
      (setq organism-directory old-organism-directory)
      (organism-graph-test--teardown))))

(ert-deftest organism-graph-test-start ()
  "Test graph initialization and cleanup."
  (let ((old-org-directory org-directory)
        (old-organism-directory organism-directory))
    (unwind-protect
      (progn
        (organism-graph-test--setup)
        ;; Set directories to test directory
        (setq org-directory organism-graph-test--temp-dir)
        (setq organism-directory organism-graph-test--temp-dir)
        ;; Test initialization on empty directory
        (let ((graph (organism-graph-start)))
          (should graph)
          (should (= (graph-node-count graph) 0))
          (should (= (graph-edge-count graph) 0)))
        ;; Test cleanup
        (should (organism-graph-stop))
        (should-not organism-graph))
      ;; Cleanup
      (setq org-directory old-org-directory)
      (setq organism-directory old-organism-directory)
      (organism-graph-test--teardown))))

(ert-deftest organism-graph-test-rescan ()
  "Test rescanning the entire graph."
  (let ((old-org-directory org-directory)
        (old-organism-directory organism-directory)
        (id1 (org-id-uuid))
        (id2 (org-id-uuid))
        (file-path nil))
    (unwind-protect
      (progn
        (organism-graph-test--setup)
        ;; Set directories to test directory
        (setq org-directory organism-graph-test--temp-dir)
        (setq organism-directory organism-graph-test--temp-dir)
        ;; Create initial files
        (setq file-path
          (organism-graph-test--create-org-file id1 "File1" "No links yet."))
        (organism-graph-test--create-org-file id2 "File2" "Target file")

        ;; Start graph
        (organism-graph-start)
        (should (= (graph-node-count organism-graph) 2))
        (should (= (graph-edge-count organism-graph) 0))

        ;; Modify file1 to add a link to file2
        (with-temp-file file-path
          (insert (format
                    ":PROPERTIES:\n:ID: %s\n:END:\n#+TITLE: File1\n\nNow links to [[id:%s][File2]]."
                    id1 id2)))

        ;; Refresh entire graph
        (organism-graph-rescan)

        ;; Check that edge was created during refresh
        (should (= (graph-edge-count organism-graph) 1))
        (let ((node1 (organism-graph-get-entry id1)))
          (should node1)
          (let ((linked (organism-graph-linked-entries node1)))
            (should (= (length linked) 1))
            (should (equal (node-id (car linked)) id2)))))
      ;; Cleanup
      (setq org-directory old-org-directory)
      (setq organism-directory old-organism-directory)
      (organism-graph-test--teardown))))

(ert-deftest organism-graph-test-file-removal ()
  "Test detection and removal of deleted files."
  (let ((old-org-directory org-directory)
        (old-organism-directory organism-directory)
        (id1 (org-id-uuid))
        (id2 (org-id-uuid))
        (file1-path nil)
        (file2-path nil))
    (unwind-protect
      (progn
        (organism-graph-test--setup)
        ;; Set directories to test directory
        (setq org-directory organism-graph-test--temp-dir)
        (setq organism-directory organism-graph-test--temp-dir)

        ;; Create test files with links between them
        (setq file1-path (organism-graph-test--create-org-file
                           id1 "File1"
                           (format "Links to [[id:%s][File2]]." id2)))
        (setq file2-path (organism-graph-test--create-org-file
                           id2 "File2"
                           (format "Links back to [[id:%s][File1]]." id1)))

        ;; Start graph and verify initial state
        (organism-graph-start)
        (should (= (graph-node-count organism-graph) 2))
        (should (= (graph-edge-count organism-graph) 2))

        (should (file-exists-p file1-path))
        (should (file-exists-p file2-path))
        ;; Delete one file
        (delete-file file2-path)

        ;; Refresh graph to detect the deletion
        (organism-graph-rescan)

        ;; Verify that the node and its connections were removed
        (should (= (graph-node-count organism-graph) 1))
        (should (= (graph-edge-count organism-graph) 0))
        (should (graph-node-p organism-graph id1))
        (should-not (graph-node-p organism-graph id2)))

      ;; Cleanup
      (setq org-directory old-org-directory)
      (setq organism-directory old-organism-directory)
      (organism-graph-test--teardown))))

(defun organism-graph-test-create-linked-files (id1 id2)
  "Create two test files with IDs ID1 and ID2, with a link from ID1 to ID2."
  (let ((file1-path (organism-graph-test--create-org-file
                      id1 "File1"
                      (format "Links to [[id:%s][File2]]." id2)))
        (file2-path (organism-graph-test--create-org-file
                      id2 "File2" "Target file")))
    (list file1-path file2-path)))

(ert-deftest organism-graph-test-update-file ()
  "Test updating entries when a file changes."
  (let ((old-org-directory org-directory)
        (old-organism-directory organism-directory)
        (id1 (org-id-uuid))
        (id2 (org-id-uuid))
        (file-path nil))
    (unwind-protect
      (progn
        (organism-graph-test--setup)
        ;; Set directories to test directory
        (setq org-directory organism-graph-test--temp-dir)
        (setq organism-directory organism-graph-test--temp-dir)
        ;; Create both files first to ensure they're registered
        (setq file-path
          (organism-graph-test--create-org-file id1 "File1" "No links yet."))
        (organism-graph-test--create-org-file id2 "File2" "Target file")
        ;; Initialize graph first with both files
        (organism-graph-start)
        (should (= (graph-node-count organism-graph) 2))
        (should (= (graph-edge-count organism-graph) 0))
        ;; Now modify file1 to add a link to file2
        (with-temp-file file-path
          (insert (format
                    ":PROPERTIES:\n:ID: %s\n:END:\n#+TITLE: File1\n\nNow links to [[id:%s][File2]]."
                    id1 id2)))
        ;; Update file and check for changes
        (organism-graph-update-file file-path)
        ;; Check that edge was created
        (should (= (graph-edge-count organism-graph) 1))
        ;; Verify the link exists
        (let ((node1 (organism-graph-get-entry id1)))
          (should node1)
          (let ((linked (organism-graph-linked-entries node1)))
            (should (= (length linked) 1))
            (should (equal (node-id (car linked)) id2)))))
      ;; Cleanup
      (setq org-directory old-org-directory)
      (setq organism-directory old-organism-directory)
      (organism-graph-test--teardown))))

(ert-deftest organism-graph-test-broken-links ()
  "Test handling of broken links."
  (let ((old-org-directory org-directory)
        (old-organism-directory organism-directory)
        (id1 (org-id-uuid))
        (missing-id (org-id-uuid)))
    (unwind-protect
      (progn
        (organism-graph-test--setup)
        ;; Set directories to test directory
        (setq org-directory organism-graph-test--temp-dir)
        (setq organism-directory organism-graph-test--temp-dir)
        ;; Create test file with broken link
        (organism-graph-test--create-org-file
          id1 "File1"
          (format "This links to a nonexistent ID: [[id:%s][missing]]." missing-id))
        ;; Initialize graph - should not error
        (organism-graph-start)
        (should (= (graph-node-count organism-graph) 1))
        (should (= (graph-edge-count organism-graph) 0))
        ;; Check entry properties
        (let ((entry1 (organism-graph-get-entry id1)))
          (should entry1)
          (should (= (length (organism-graph-linked-entries entry1)) 0))))
      ;; Cleanup
      (setq org-directory old-org-directory)
      (setq organism-directory old-organism-directory)
      (organism-graph-test--teardown))))

(ert-deftest organism-graph-test-heading-entries ()
  "Test entries in headings."
  (let ((old-org-directory org-directory)
        (old-organism-directory organism-directory)
        (file-id (org-id-uuid))
        (heading1-id (org-id-uuid))
        (heading2-id (org-id-uuid)))
    (unwind-protect
      (progn
        (organism-graph-test--setup)
        ;; Set directories to test directory
        (setq org-directory organism-graph-test--temp-dir)
        (setq organism-directory organism-graph-test--temp-dir)
        ;; Create test file with headings - ONE LINK ONLY
        (let ((file-path (organism-graph-test--create-org-file file-id
                           "HeadingTest"
                           (format "* Heading 1\n  :PROPERTIES:\n  :ID: %s\n  :END:\n  Content 1\n\n* Heading 2\n  :PROPERTIES:\n  :ID: %s\n  :END:\n  Content 2\n  Links to heading one: [[id:%s][Heading 1]]."
                             heading1-id heading2-id heading1-id))))
          ;; Manually register heading IDs
          (organism-graph-test--register-id heading1-id file-path)
          (organism-graph-test--register-id heading2-id file-path)
          ;; Initialize graph
          (organism-graph-start)
          (should (= (graph-node-count organism-graph) 3))
          ;; Check heading entries
          (let ((file-entry (organism-graph-get-entry file-id))
                (heading1 (organism-graph-get-entry heading1-id))
                (heading2 (organism-graph-get-entry heading2-id)))
            (should file-entry)
            (should heading1)
            (should heading2)
            ;; Check types
            (should (organism-entry-file-p file-entry))
            (should (organism-entry-heading-p heading1))
            (should (organism-entry-heading-p heading2))
            ;; Check titles
            (should (string= (organism-entry-title heading1) "Heading 1"))
            (should (string= (organism-entry-title heading2) "Heading 2"))
            ;; Verify links exist from heading2 to heading1
            (should (member heading1 (organism-graph-linked-entries heading2))))))
      ;; Cleanup
      (setq org-directory old-org-directory)
      (setq organism-directory old-organism-directory)
      (organism-graph-test--teardown))))

(ert-deftest organism-graph-test-linked-entries ()
  "Test organism-graph-linked-entries function."
  (let ((old-org-directory org-directory)
        (old-organism-directory organism-directory)
        (id1 (org-id-uuid))
        (id2 (org-id-uuid))
        (id3 (org-id-uuid)))
    (unwind-protect
      (progn
        (organism-graph-test--setup)
        ;; Set directories to test directory
        (setq org-directory organism-graph-test--temp-dir)
        (setq organism-directory organism-graph-test--temp-dir)
        ;; Create a network of linked files
        (organism-graph-test--create-org-file
          id1 "File1"
          (format "Links to [[id:%s][file 2]] and [[id:%s][file 3]]." id2 id3))

        (organism-graph-test--create-org-file
          id2 "File2"
          (format "Links to [[id:%s][file 3]]." id3))

        (organism-graph-test--create-org-file
          id3 "File3"
          "No outgoing links.")

        ;; Initialize graph
        (organism-graph-start)
        (should (= (graph-edge-count organism-graph) 3))

        ;; Test outgoing links
        (let ((entry1 (organism-graph-get-entry id1)))
          (let ((outgoing (organism-graph-linked-entries entry1)))
            (should (= (length outgoing) 2))
            (should (cl-every (lambda (entry)
                                (member (node-id entry) (list id2 id3)))
                      outgoing))))
        ;; Test incoming links
        (let ((entry3 (organism-graph-get-entry id3)))
          (let ((incoming (organism-graph-linked-entries entry3 t)))
            (should (= (length incoming) 2))
            (should (cl-every (lambda (entry)
                                (member (node-id entry) (list id1 id2)))
                      incoming)))))
      ;; Cleanup
      (setq org-directory old-org-directory)
      (setq organism-directory old-organism-directory)
      (organism-graph-test--teardown))))

(ert-deftest organism-graph-test-stats ()
  "Test statistics tracking during graph operations."
  (let ((old-org-directory org-directory)
        (old-organism-directory organism-directory)
        (id1 (org-id-uuid))
        (id2 (org-id-uuid))
        (id3 (org-id-uuid))
        (file-path nil))
    (unwind-protect
      (progn
        (organism-graph-test--setup)
        ;; Set directories to test directory
        (setq org-directory organism-graph-test--temp-dir)
        (setq organism-directory organism-graph-test--temp-dir)

        ;; Create test files with links
        (setq file-path (organism-graph-test--create-org-file
                          id1 "File1"
                          (format "Links to [[id:%s][File2]]." id2)))
        (organism-graph-test--create-org-file
          id2 "File2"
          (format "Links to [[id:%s][File3]]." id3))
        (organism-graph-test--create-org-file
          id3 "File3"
          (format "Links back to [[id:%s][File1]]." id1))

        ;; Start graph
        (organism-graph-start)

        ;; Delete one file to test removal tracking
        (delete-file file-path)

        ;; Create stats struct to capture refresh operations
        (let ((stats (make-organism-graph-stats)))
          ;; Update stats with refresh operations
          (organism-graph--handle-removed-entries stats)

          ;; Check that removal was tracked
          (should (= (organism-graph-stats-entries-removed stats) 1))
          (should (> (organism-graph-stats-edges-removed stats) 0))))

      ;; Cleanup
      (setq org-directory old-org-directory)
      (setq organism-directory old-organism-directory)
      (organism-graph-test--teardown))))

(provide 'organism-graph-test)
;;; organism-graph-test.el ends here
