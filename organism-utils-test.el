;;; organism-utils-test.el --- Tests for organism-utils.el -*- lexical-binding: t; -*-

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

;; Test suite for organism-utils.el

;;; Code:

(require 'ert)
(require 'organism-utils)

;;; Setup/Teardown

(defvar organism-utils-test--temp-dir nil)
(defvar organism-directory nil "Mock directory for testing.")

(defun organism-utils-test--setup ()
  "Set up test environment."
  (setq organism-utils-test--temp-dir (make-temp-file "organism-utils-test-" t))
  (setq org-id-locations (make-hash-table :test 'equal))
  (setq org-id-files nil))

(defun organism-utils-test--teardown ()
  "Clean up test environment."
  (when (and organism-utils-test--temp-dir (file-exists-p organism-utils-test--temp-dir))
    (delete-directory organism-utils-test--temp-dir t)))

(defun organism-utils-test--create-file (filename content)
  "Create a test file with FILENAME and CONTENT."
  (let ((file-path (expand-file-name filename organism-utils-test--temp-dir)))
    (with-temp-file file-path
      (insert content))
    file-path))

;;; Test Cases

(ert-deftest organism-utils-test-debug-function ()
  "Test that organism-debug executes without errors."
  (should (progn (organism-debug "Test message") t))
  (should (progn (organism-debug "Test message") t)))

(ert-deftest organism-utils-test-check-type ()
  "Test organism-check-type function."
  ;; Valid types
  (should-not (organism-check-type "test" 'string))
  (should-not (organism-check-type 42 'integer))
  (should-not (organism-check-type nil 'integer)) ; nil is allowed

  ;; Invalid types
  (should-error (organism-check-type "test" 'integer))
  (should-error (organism-check-type 42 'string))

  ;; Test with argument name
  (condition-case err
    (organism-check-type "test" 'integer "test-arg")
    (wrong-type-argument
      (should (string-match-p "test-arg" (error-message-string err))))))

(ert-deftest organism-utils-test-get-ids-in-file ()
  "Test organism-get-ids-in-file function."
  (unwind-protect
    (progn
      (organism-utils-test--setup)
      (let* ((file1-path (organism-utils-test--create-file
                           "test1.org" "Content"))
             (file2-path (organism-utils-test--create-file
                           "test2.org" "Content"))
              (id1 "id-123")
              (id2 "id-456")
              (id3 "id-789"))

        ;; Register some IDs to files
        (puthash id1 file1-path org-id-locations)
        (puthash id2 file1-path org-id-locations)
        (puthash id3 file2-path org-id-locations)

        ;; Test retrieval
        (let ((ids-file1 (organism-get-ids-in-file file1-path))
              (ids-file2 (organism-get-ids-in-file file2-path)))
          (should (= (length ids-file1) 2))
          (should (member id1 ids-file1))
          (should (member id2 ids-file1))
          (should (= (length ids-file2) 1))
          (should (member id3 ids-file2)))

        ;; Test with nonexistent file - should return nil
        (should-not (organism-get-ids-in-file "/nonexistent/file"))))
    (organism-utils-test--teardown)))

(ert-deftest organism-utils-test-get-files-with-ids ()
  "Test organism-get-files-with-ids function."
  (unwind-protect
    (progn
      (organism-utils-test--setup)
      (let ((organism-directory organism-utils-test--temp-dir))
        (let* ((file1-path (organism-utils-test--create-file
                             "test1.org" "Content"))
               (file2-path (organism-utils-test--create-file
                             "test2.org" "Content"))
                (outside-path "/tmp/outside-file.org")
                (id1 "id-123")
                (id2 "id-456"))
          ;; Register IDs to files
          (puthash id1 file1-path org-id-locations)
          (puthash id2 file2-path org-id-locations)

          ;; Test retrieval
          (let ((files (organism-get-files-with-ids)))
            (should (= (length files) 2))
            (should (member file1-path files))
            (should (member file2-path files)))
          ;; Test with file outside directory
          (puthash "outside-id" outside-path org-id-locations)
          (let ((files (organism-get-files-with-ids)))
            (should (= (length files) 2))
            (should-not (member outside-path files))))))
    (organism-utils-test--teardown)))

(provide 'organism-utils-test)
;;; organism-utils-test.el ends here
