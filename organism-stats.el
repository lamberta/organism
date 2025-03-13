;;; organism-stats.el --- Stats utilities for organism -*- lexical-binding: t; -*-

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

;; Stats display and helper functions for organism.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'graphael-core)
(require 'graphael-operations)
(require 'organism-defs)
(require 'organism-utils)
(require 'organism-entry)
(require 'organism-graph)


(defun organism-stats-collect (&optional operation-stats)
  "Collect statistics about the organism graph.
When OPERATION-STATS is provided, merge operation metrics with graph metrics."
  (unless organism-graph
    (user-error "Organism graph not initialized"))

  (let* ((graph-stats (graph-stats organism-graph))
         (stats (make-organism-graph-stats
                  :entries-total (graph-stats-result-nodes graph-stats)
                  :edges-total (graph-stats-result-edges graph-stats)
                  :density (graph-stats-result-density graph-stats)
                  :avg-edge-weight (graph-stats-result-avg-edge-weight graph-stats)
                  :memory-kb (graph-stats-result-size-kb graph-stats)))
          (file-entries 0)
          (heading-entries 0))

    ;; Count entry types
    (dolist (entry (organism-graph-entries))
      (if (organism-entry-file-p entry)
        (cl-incf file-entries)
        (cl-incf heading-entries)))

    (setf (organism-graph-stats-entries-files stats) file-entries)
    (setf (organism-graph-stats-entries-headings stats) heading-entries)

    ;; Merge operation stats if provided
    (when operation-stats
      (setf (organism-graph-stats-entries-added stats)
        (organism-graph-stats-entries-added operation-stats))
      (setf (organism-graph-stats-entries-removed stats)
        (organism-graph-stats-entries-removed operation-stats))
      (setf (organism-graph-stats-entries-processed stats)
        (organism-graph-stats-entries-processed operation-stats))
      (setf (organism-graph-stats-entries-skipped stats)
        (organism-graph-stats-entries-skipped operation-stats))
      (setf (organism-graph-stats-edges-added stats)
        (organism-graph-stats-edges-added operation-stats))
      (setf (organism-graph-stats-edges-removed stats)
        (organism-graph-stats-edges-removed operation-stats))
      (setf (organism-graph-stats-elapsed-time stats)
        (organism-graph-stats-elapsed-time operation-stats))
      (setf (organism-graph-stats-success stats)
        (organism-graph-stats-success operation-stats)))
    stats))

(defun organism-stats-display ()
  "Display detailed statistics about the organism graph."
  (interactive)
  (unless organism-graph
    (user-error "Organism graph not initialized"))

  (let* ((stats (organism-stats-collect))
         (buffer (get-buffer-create "*Organism Graph Stats*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "#+TITLE: Organism Graph Statistics\n\n")
        ;; Basic stats
        (insert "* Summary\n\n")
        (insert (format "- Entries: %d\n" (organism-graph-stats-entries-total stats)))
        (insert (format "- Connections: %d\n" (organism-graph-stats-edges-total stats)))
        (insert (format "- Density: %.3f\n" (organism-graph-stats-density stats)))
        (insert (format "- Avg Edge Weight: %.2f\n" (organism-graph-stats-avg-edge-weight stats)))
        (insert (format "- Memory Usage: %.2f KB\n\n" (organism-graph-stats-memory-kb stats)))
        ;; Entry type breakdown
        (insert "* Entry Types\n\n")
        (insert (format "- Files: %d\n" (organism-graph-stats-entries-files stats)))
        (insert (format "- Headings: %d\n\n" (organism-graph-stats-entries-headings stats)))
        ;; Most connected entries
        (insert "* Most Connected Entries\n\n")
        (organism-stats--render-connected-entries)))

    (switch-to-buffer buffer)
    (read-only-mode 1)
    (goto-char (point-min))
    (org-fold-show-all)))

;; Display helper functions
(defun organism-stats--render-connected-entries ()
  "Render connected entries in the stats buffer."
  (let* ((entries (organism-graph-entries))
         (connected-entries
           (cl-remove-if
             (lambda (entry)
               (zerop (length (organism-graph-linked-entries entry t))))
             entries))
         (sorted-entries
           (seq-sort-by (lambda (entry)
                          (length (organism-graph-linked-entries entry t)))
             #'> connected-entries)))

    (if (not connected-entries)
      (insert "No connected entries found.\n")
      (seq-do (lambda (entry)
                (organism-stats--render-entry entry))
        (seq-take sorted-entries (min 10 (length sorted-entries)))))))

(defun organism-stats--render-entry (entry)
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
      (organism-stats--render-link-list outgoing)
      (insert "\n"))
    ;; Incoming links (backlinks)
    (let ((backlinks (cl-set-difference incoming outgoing
                       :test (lambda (a b)
                               (string= (node-id a) (node-id b))))))
      (when backlinks
        (insert "*** Incoming links (backlinks)\n\n")
        (organism-stats--render-link-list backlinks)
        (insert "\n")))))

(defun organism-stats--render-link-list (entries)
  "Format a list of ENTRIES as links."
  (dolist (entry entries)
    (let* ((file (organism-entry-property entry "FILE"))
           (title (organism-entry-title entry))
           (display-name (concat (file-name-nondirectory file)
                           (if (organism-entry-heading-p entry)
                             (format " > %s" title)
                             (format " (%s)" title)))))
      (insert (format "- [[file:%s][%s]]\n" file display-name)))))

(provide 'organism-stats)
;;; organism-stats.el ends here
