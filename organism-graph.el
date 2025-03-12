;;; organism-graph.el --- Graph representation of org-mode notes -*- lexical-binding: t; -*-

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

;; This file provides the core graph representation for organism.
;;
;; The workflow is:
;; - Initialize graph at startup
;; - Update entries when files change
;; - Keep the graph in sync with underlying org files
;;
;; The in-memory graph maintains a representation of org-mode entries
;; (files and headings) and the connections between them. The source
;; of truth is always the org files on disk.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'graphael-core)
(require 'organism-utils)
(require 'organism-entry)

;;; Variables

;; Declare free variables defined in main file.
(defvar organism-directory)
(defvar organism-exclude-file-regexp)
(defvar organism-file-match)

(defvar organism-graph nil
  "The global organism graph object.
This is the in-memory representation of entries and their connections.")

(defvar organism-graph--processing nil
  "Flag to prevent recursive graph processing during updates.
Used to avoid infinite loops when updating interconnected entries.")

;;; Core Graph Functions

(defun organism-graph--file-matches-criteria-p (file)
  "Return t if FILE should be included in the organism graph."
  (and (file-exists-p file)
       (string-match-p organism-file-match file)
       (or (null organism-exclude-file-regexp)
           (not (string-match-p organism-exclude-file-regexp file)))
       (file-in-directory-p file organism-directory)))

(defun organism-graph--process-entries (action-message)
  "Process all entries in the graph, refreshing and connecting them.
ACTION-MESSAGE is used in the debug output."
  (let ((entry-count 0)
        (connection-count 0)
        (skipped-count 0))
    ;; Refresh each entry
    (dolist (entry (organism-graph-entries))
      (if (organism-entry--refresh entry)
        (cl-incf entry-count)
        (cl-incf skipped-count))
      ;; Update connections
      (when-let ((connections (organism-graph--try-update-connections entry)))
        (cl-incf connection-count)))

    (organism-debug "%s: %d entries (%d skipped) with %d connections"
      action-message entry-count skipped-count connection-count)
    (list entry-count skipped-count connection-count)))

(defun organism-graph--build ()
  "Build the graph by adding entries for all IDs in org-id-locations."
  (organism-debug "Building graph from org-id-locations with %d entries"
    (hash-table-count org-id-locations))

  (let ((organism-graph--processing t))
    ;; Add all entries to the graph
    (maphash (lambda (id file)
               (when (organism-graph--file-matches-criteria-p file)
                 (organism-graph--try-create-entry id)))
      org-id-locations)
    ;; Process entries
    (organism-graph--process-entries "Graph build complete")))

(defun organism-graph-refresh ()
  "Refresh all entries in the organism graph, clear caches, etc."
  (unless organism-graph
    (user-error "Organism graph not started"))

  (organism-debug "Refreshing organism graph...")
  (let ((organism-graph--processing t))
    (organism-graph--process-entries "Refreshed"))
  t)

(defun organism-graph-start ()
  "Initialize and build the organism graph from org files."
  (if organism-graph
    (organism-graph-stop))

  (organism-debug "Creating new organism graph")
  (setq organism-graph (make-instance 'graph))

  (org-id-locations-load)  ; Load existing ID locations

  ;; Update ID locations by scanning matching org files
  (organism-debug "Scanning for IDs in %s" organism-directory)
  (let ((files (seq-filter #'organism-graph--file-matches-criteria-p
                 (directory-files-recursively organism-directory "\\.org$"))))
    (organism-debug "Found %d matching org files to scan" (length files))
    (org-id-update-id-locations files))

  ;; Build the graph from entry IDs
  (organism-graph--build)
  organism-graph)

(defun organism-graph-stop ()
  "Clean up the organism graph and release resources.
Releases all memory used by the graph. Returns t if cleanup was performed,
nil if there was no graph to clean up."
  (when organism-graph
    (organism-debug "Cleaning up organism graph with %d nodes"
      (graph-node-count organism-graph))
    (setq organism-graph nil)
    t))

(defun organism-graph--try-create-entry (id)
  "Try to create an entry with ID, handling any errors.
Returns the created entry if successful, nil otherwise."
  (condition-case err
    (organism-graph-get-or-create-entry id)
    (error
      (organism-debug "Error creating entry for ID %s: %s"
        id (error-message-string err))
      nil)))

(defun organism-graph--try-update-connections (entry)
  "Try to update connections for ENTRY, handling any errors.
Returns the number of successful connection updates, or nil on error."
  (condition-case err
    (organism-graph-update-connections entry)
    (error
      (organism-debug "Error creating edges for %s: %s"
        (node-id entry) (error-message-string err))
      nil)))

(defun organism-graph-update-file (file)
  "Update the graph to reflect changes in FILE.
Refreshes entries in the given file and updates their connections.
Returns the number of entries updated."
  (when (and organism-graph
          (not organism-graph--processing)
          (file-exists-p file))
    (let ((organism-graph--processing t)
          (updated-count 0))

      (organism-debug "Processing file: %s" file)

      ;; Process all entries in this file
      (dolist (id (organism-get-ids-in-file file))
        (when-let ((entry (organism-graph--try-create-entry id)))
          (organism-entry--refresh entry)
          (when (organism-graph--try-update-connections entry)
            (cl-incf updated-count))))

      (organism-debug "File update complete: %d entries updated" updated-count)
      updated-count)))

;;; Entry and Edge Management

(defun organism-graph-get-or-create-entry (id)
  "Get entry with ID from the graph, or create it if it doesn't exist.
Returns the existing or newly created organism-entry, or nil if
the ID corresponds to a non-entry node type."
  (if-let ((existing (graph-node-get organism-graph id)))
    (if (object-of-class-p existing 'organism-entry)
      existing
      (organism-debug "Node %s exists but is not an organism-entry" id)
      nil)
    (let ((entry (make-instance 'organism-entry :id id)))
      (graph-node-add organism-graph :node entry)
      entry)))

(defun organism-graph-get-entry (id)
  "Get entry with ID from the graph, or nil if it doesn't exist.
Returns nil if ID doesn't exist or isn't an organism-entry."
  (when-let ((node (graph-node-get organism-graph id)))
    (when (object-of-class-p node 'organism-entry)
      node)))

(defun organism-graph-entries ()
  "Return a list of all entries in the graph.
Only includes organism-entry objects, filtering out any other node types."
  (let ((entries nil))
    (maphash (lambda (_id node)
               (when (object-of-class-p node 'organism-entry)
                 (push node entries)))
      (graph-nodes organism-graph))
    entries))

(defun organism-graph-update-connections (entry)
  "Update all connections for ENTRY in the graph.
Removes all existing outgoing edges and creates new ones based on
current links in the entry. Returns the number of connections created."
  (let ((from-id (node-id entry))
        (connection-count 0))
    ;; First remove all outgoing edges from this entry
    (dolist (edge (graph-node-edges organism-graph from-id))
      (graph-edge-remove organism-graph edge))

    ;; Then add edges for all ID links
    (dolist (link (organism-entry-links entry "id"))
      (let ((target-id (plist-get link :target)))
        (when (and target-id (not (string-empty-p target-id)))
          (when (organism-graph-create-edge entry target-id
                  (list :link-text (plist-get link :raw-link)))
            (cl-incf connection-count)))))
    connection-count))

(defun organism-graph-create-edge (from-entry to-id &optional edge-attrs)
  "Create an edge from FROM-ENTRY to TO-ID in the graph.
Optional EDGE-ATTRS is a plist of attributes for the edge.
Returns t if edge was created, nil otherwise."
  (if (not (graph-node-p organism-graph to-id))
    (progn
      ;; Show warning about broken link
      (let* ((link-text (plist-get edge-attrs :link-text))
             (from-title (organism-entry-title from-entry)))
        (message "Organism WARNING: Broken link from %s (%s) to nonexistent entry ID: %s%s"
          (node-id from-entry)
          (or from-title "untitled")
          to-id
          (if link-text (format " [%s]" link-text) "")))
      ;; Return nil if target doesn't exist
      nil)
    ;; Target exists, try to create edge
    (let ((from-id (node-id from-entry)))
      ;; Check if edge already exists
      (if (cl-some (lambda (edge)
                     (and (string= (edge-from edge) from-id)
                       (string= (edge-to edge) to-id)))
            (graph-node-edges organism-graph from-id))
        nil  ; Edge already exists
        (progn
          (graph-edge-add organism-graph
            :from from-id
            :to to-id
            :label "link"
            :attrs edge-attrs)
          t)))))

;;; Query Functions

(defun organism-graph-linked-entries (entry &optional include-incoming)
  "Return entries linked from ENTRY.
Returns a list of organism-entry objects that ENTRY links to.
When INCLUDE-INCOMING is non-nil, also return entries that link to ENTRY."
  (let* ((id (node-id entry))
         (outgoing (cl-remove-if-not
                     (lambda (node)
                       (object-of-class-p node 'organism-entry))
                     (graph-neighbors organism-graph id)))
          (incoming (when include-incoming
                      (cl-remove-if-not
                        (lambda (node)
                          (object-of-class-p node 'organism-entry))
                        (graph-neighbors organism-graph id t)))))
    (if include-incoming
      (append outgoing incoming)
      outgoing)))

(provide 'organism-graph)
;;; organism-graph.el ends here
