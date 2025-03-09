;;; organism-display.el --- Display utilities for organism -*- lexical-binding: t; -*-

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

;; This file provides display and statistics functions for organism.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'graphael-core)
(require 'graphael-operations)
(require 'organism-utils)
(require 'organism-entry)
(require 'organism-graph)

(defun organism-display-stats ()
  "Display detailed statistics about the organism graph."
  (interactive)
  (unless organism-graph
    (user-error "Organism graph not initialized"))

  (let* ((stats (graph-stats organism-graph))
         (buffer (get-buffer-create "*Organism Graph Stats*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "#+TITLE: Organism Graph Statistics\n\n")
        ;; Basic statistics
        (insert "* Summary\n\n")
        (insert (format "- Entries: %d\n" (graph-stats-result-nodes stats)))
        (insert (format "- Connections: %d\n" (graph-stats-result-edges stats)))
        (insert (format "- Density: %.3f\n" (graph-stats-result-density stats)))
        (insert (format "- Avg Edge Weight: %.2f\n" (graph-stats-result-avg-edge-weight stats)))
        (insert (format "- Memory Usage: %.2f KB\n\n" (graph-stats-result-size-kb stats)))
        ;; Entry type breakdown
        (organism-display--render-entry-types)
        ;; Most connected entries
        (insert "* Most Connected Entries\n\n")
        (organism-display--render-connected-entries)))

    (switch-to-buffer buffer)
    (read-only-mode 1)
    (goto-char (point-min))
    (org-fold-show-all)))

(defun organism-display--render-entry-types ()
  "Render entry type statistics."
  (insert "* Entry Types\n\n")
  (let ((file-entries 0)
        (heading-entries 0))
    (dolist (entry (organism-graph-entries))
      (if (organism-entry-file-p entry)
        (cl-incf file-entries)
        (cl-incf heading-entries)))
    (insert (format "- Files: %d\n" file-entries))
    (insert (format "- Headings: %d\n\n" heading-entries))))

(defun organism-display--render-connected-entries ()
  "Render connected entries in the stats buffer."
  (let* ((entries (organism-graph-entries))
         (connected-entries
           (cl-remove-if (lambda (entry)
                           (zerop (length (organism-graph-linked-entries entry t))))
             entries))
         (sorted-entries
           (seq-sort-by (lambda (entry)
                          (length (organism-graph-linked-entries entry t)))
             #'> connected-entries)))

    (if (not connected-entries)
      (insert "No connected entries found.\n")
      (seq-do (lambda (entry)
                (organism-display--render-entry entry))
        (seq-take sorted-entries (min 10 (length sorted-entries)))))))

(defun organism-display--render-entry (entry)
  "Render a single ENTRY with its connections."
  (let* ((file (organism-entry-property entry "FILE"))
         (title (organism-entry-title entry))
         (outgoing (organism-graph-linked-entries entry nil))
         (incoming (organism-graph-linked-entries entry t))
         (total-connections (length incoming)))
    ;; Entry heading with link
    (insert (format "** [[file:%s][%s%s]] (%d connections)\n\n"
              file
              (file-name-nondirectory file)
              (if (organism-entry-heading-p entry)
                (format " > %s" title)
                (format " (%s)" title))
              total-connections))
    ;; Outgoing links
    (when outgoing
      (insert "*** Outgoing links\n\n")
      (organism-display--render-link-list outgoing)
      (insert "\n"))
    ;; Incoming links (backlinks)
    (let ((backlinks (cl-set-difference incoming outgoing
                       :test (lambda (a b)
                               (string= (node-id a) (node-id b))))))
      (when backlinks
        (insert "*** Incoming links (backlinks)\n\n")
        (organism-display--render-link-list backlinks)
        (insert "\n")))))

(defun organism-display--render-link-list (entries)
  "Format a list of ENTRIES as links."
  (dolist (entry entries)
    (let* ((file (organism-entry-property entry "FILE"))
           (title (organism-entry-title entry))
           (display-name (concat (file-name-nondirectory file)
                           (if (organism-entry-heading-p entry)
                             (format " > %s" title)
                             (format " (%s)" title)))))
      (insert (format "- [[file:%s][%s]]\n" file display-name)))))

(provide 'organism-display)
;;; organism-display.el ends here
