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
(require 'organism-defs)
(require 'organism-utils)
(require 'organism-entry)

;; Forward declaration for free variables

(defvar organism-directory)

;;; Variables

(defvar organism-graph--processing nil
  "Flag to prevent recursive graph processing during updates.
Used to avoid infinite loops when updating interconnected entries.")

;;; Core Graph Functions

(defun organism-graph--handle-removed-entries (stats)
  "Remove entries whose files no longer exist.
Updates STATS with removal information."
  (let ((entries-to-remove nil)
        (edges-removed 0))
    ;; First identify entries to remove
    (dolist (entry (organism-graph-entries))
      (let ((file (organism-entry-property entry "FILE")))
        (when (or (not file)
                  (not (file-exists-p file))
                  (not (organism-file-matches-criteria-p file)))
          (push entry entries-to-remove))))

    ;; Then remove them
    (dolist (entry entries-to-remove)
      (let* ((node-id (node-id entry))  ;; Use proper variable name
             (file (organism-entry-property entry "FILE"))
             (title (organism-entry-title entry))
             (edge-count (+ (length (graph-node-edges organism-graph node-id))
                            (length (graph-node-edges organism-graph node-id t)))))
        ;; Track edge count before removal
        (cl-incf edges-removed edge-count)

        ;; Remove the entry
        (graph-node-remove organism-graph entry)
        (organism-debug "Removed entry %s (%s) - file no longer exists: %s"
          node-id (or title "untitled") (or file "unknown"))
        (cl-incf (organism-graph-stats-entries-removed stats))))

    ;; Update edge count in stats
    (cl-incf (organism-graph-stats-edges-removed stats) edges-removed)

    (when (> (organism-graph-stats-entries-removed stats) 0)
      (organism-debug "Removed %d entries with %d edges for nonexistent files"
        (organism-graph-stats-entries-removed stats)
        edges-removed))))

(defun organism-graph--scan-files ()
  "Scan organism directory for org files with IDs and update org-id-locations."
  (let ((files (seq-filter #'organism-file-matches-criteria-p
                 (directory-files-recursively organism-directory "\\.org$"))))
    (organism-debug "Scanning %d matching org files for IDs" (length files))
    (org-id-update-id-locations files)))

(defun organism-graph--process-entry (entry stats)
  "Process ENTRY, update its connections, and update STATS."
  (if (condition-case err
        (progn
          ;; Refresh the entry's data
          (organism-entry--refresh entry)

          ;; Update connections and track edge count
          (let* ((old-edges (graph-node-edges organism-graph (node-id entry)))
                 (old-edge-ids (mapcar #'edge-id old-edges))
                 (new-edge-ids (progn
                                 (organism-graph-update-connections entry)
                                 (mapcar #'edge-id
                                   (graph-node-edges organism-graph (node-id entry))))))
            ;; Count truly new edges (not just updates)
            (let ((added-count 0)
                  (removed-count 0))
              (dolist (id new-edge-ids)
                (unless (member id old-edge-ids)
                  (cl-incf added-count)))
              (dolist (id old-edge-ids)
                (unless (member id new-edge-ids)
                  (cl-incf removed-count)))

              ;; Update stats with actual changes
              (cl-incf (organism-graph-stats-edges-added stats) added-count)
              (cl-incf (organism-graph-stats-edges-removed stats) removed-count)))
          t)
        (error
          (organism-debug "Error processing entry %s: %s"
            (node-id entry) (error-message-string err))
          (setf (organism-graph-stats-success stats) nil)
          nil))
    ;; Success
    (cl-incf (organism-graph-stats-entries-processed stats))
    ;; Failed
    (cl-incf (organism-graph-stats-entries-skipped stats))))

(defun organism-graph-rescan ()
  "Refresh the organism graph to match the filesystem state.
Returns t if sync was complete, nil if some entries couldn't be processed."
  (unless organism-graph
    (user-error "Organism graph not started"))

  (let ((organism-graph--processing t)
        (start-time (current-time))
        (stats (make-organism-graph-stats))
        (initial-edge-count (graph-edge-count organism-graph)))

    (organism-debug "Starting full graph refresh...")

    ;; 1. Remove entries for nonexistent files
    (organism-graph--handle-removed-entries stats)
    ;; 2. Scan for files and update ID locations
    (organism-graph--scan-files)
    ;; 3. Add entries for new IDs
    (maphash (lambda (id file)
               (when (and (organism-file-matches-criteria-p file)
                          (not (graph-node-p organism-graph id)))
                 (condition-case err
                   (progn
                     (organism-graph-get-or-create-entry id)
                     (cl-incf (organism-graph-stats-entries-added stats)))
                   (error
                     (organism-debug "Error adding entry for ID %s: %s"
                       id (error-message-string err))
                     (setf (organism-graph-stats-success stats) nil)))))
      org-id-locations)
    ;; 4. Process and update all entries
    (dolist (entry (organism-graph-entries))
      (if (condition-case err
            (progn
              (organism-entry--refresh entry)
              (organism-graph-update-connections entry)
              t)
            (error
              (organism-debug "Error processing entry %s: %s"
                (node-id entry) (error-message-string err))
              (setf (organism-graph-stats-success stats) nil)
              nil))
        (cl-incf (organism-graph-stats-entries-processed stats))
        (cl-incf (organism-graph-stats-entries-skipped stats))))
    ;; Calculate net edge changes
    (let ((edge-difference (- (graph-edge-count organism-graph)
                              initial-edge-count)))
      (when (not (zerop edge-difference))
        (if (> edge-difference 0)
          (setf (organism-graph-stats-edges-added stats) edge-difference)
          (setf (organism-graph-stats-edges-removed stats)
            (abs edge-difference)))))

    ;; Set elapsed time in stats
    (setf (organism-graph-stats-elapsed-time stats)
      (float-time (time-since start-time)))

    ;; Display a descriptive message
    (message "%s" (organism-utils-format-status stats "Graph refresh"))
    ;; Return success status
    (organism-graph-stats-success stats)))

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
               (when (organism-file-matches-criteria-p file)
                 (organism-graph--try-create-entry id)))
      org-id-locations)
    ;; Process entries
    (organism-graph--process-entries "Graph build complete")))

(defun organism-graph-start ()
  "Initialize and build the organism graph from org files."
  (if organism-graph
    (organism-graph-stop))

  (organism-debug "Creating new organism graph")
  (let ((start-time (current-time))
        (stats (make-organism-graph-stats)))

    (setq organism-graph (make-instance 'graph))
    (org-id-locations-load)  ; Load existing ID locations

    ;; Update ID locations by scanning matching org files
    (organism-debug "Scanning for IDs in %s" organism-directory)
    (let ((files (seq-filter #'organism-file-matches-criteria-p
                   (directory-files-recursively organism-directory "\\.org$"))))
      (organism-debug "Found %d matching org files to scan" (length files))
      (org-id-update-id-locations files))

    ;; Build the graph from entry IDs
    (maphash (lambda (id file)
               (when (organism-file-matches-criteria-p file)
                 (when (organism-graph--try-create-entry id)
                   (cl-incf (organism-graph-stats-entries-added stats)))))
      org-id-locations)

    ;; Process all entries and track connections
    (dolist (entry (organism-graph-entries))
      (if (condition-case err
            (progn
              (organism-entry--refresh entry)
              (organism-graph-update-connections entry)
              t)
            (error
              (organism-debug "Error processing entry %s: %s"
                (node-id entry) (error-message-string err))
              (setf (organism-graph-stats-success stats) nil)
              nil))
        (cl-incf (organism-graph-stats-entries-processed stats))
        (cl-incf (organism-graph-stats-entries-skipped stats))))

    ;; Set elapsed time in stats
    (setf (organism-graph-stats-elapsed-time stats)
          (float-time (time-since start-time)))

    (message "%s" (organism-utils-format-status stats "Graph build"))
    organism-graph))

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
