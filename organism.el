;;; organism.el --- Notes network for org-mode -*- lexical-binding: t; -*-

;; Author: Billy Lamberta
;; URL: https://github.com/lamberta/organism
;; Version: 0.0.1
;; Created: Mar 2025
;; Updated: Mar 2025
;; Package-Requires: ((emacs "27.2") (graphael "0.1.1"))
;; Keywords: convenience, hypermedia, outlines

;; This file is not part of GNU Emacs

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

;; Organism builds on org-mode to create, manage, and analyze
;; interconnected notes as a graph structure.
;;
;; This package maintains a graph representation of org-mode files,
;; where files and headings with IDs become entries in a graph, and links
;; between them become edges.
;;
;; To use organism, enable the minor mode with M-x organism-mode
;; or add (organism-mode 1) to your configuration.

;;; Code:

(require 'org)
(require 'org-id)
(require 'graphael-core)
(require 'graphael-operations)

(require 'organism-utils)
(require 'organism-entry)
(require 'organism-graph)
(require 'organism-display)

(defgroup organism nil
  "Graph-based org-mode note management."
  :group 'org
  :prefix "organism-")

(defcustom organism-directory org-directory
  "Base directory for organism files.
This is the root directory where organism will scan for org files."
  :type 'directory
  :group 'organism)

(defcustom organism-file-match "\\`[^.].*\\.org\\'"
  "Regular expression matching org files to include in the graph."
  :type 'regexp
  :group 'organism)

(defcustom organism-exclude-file-regexp nil
  "Regular expression matching org files to exclude from the graph."
  :type '(choice (const :tag "Don't exclude any files" nil)
           (regexp :tag "Regular expression"))
  :group 'organism)

(defcustom organism-auto-update t
  "Whether to automatically update the graph when a file is saved."
  :type 'boolean
  :group 'organism)

(defcustom organism-debug-enabled nil
  "Whether to show debug messages."
  :type 'boolean
  :group 'organism)

(defvar organism-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for Organism mode.")

;;;###autoload
(define-minor-mode organism-mode
  "Toggle Organism mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it. From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When Organism mode is enabled, the graph of org notes is built and
maintained, enabling analysis and navigation through the note structure."
  :init-value nil
  :lighter " Organism"
  :group 'organism
  :global t
  :keymap organism-mode-map
  (if organism-mode
    (progn
      (organism-debug "Initializing organism graph...")
      (organism-graph-initialize)
      (when organism-auto-update
        (add-hook 'after-save-hook #'organism-after-save-hook)))
    (progn
      (organism-debug "Cleaning up organism graph...")
      (organism-graph-cleanup)
      (remove-hook 'after-save-hook #'organism-after-save-hook))))

(defun organism-after-save-hook ()
  "Update the organism graph when a relevant file is saved."
  (when (and
          organism-mode
          (derived-mode-p 'org-mode)
          (buffer-file-name)
          (string-match-p organism-file-match (buffer-file-name))
          (or (null organism-exclude-file-regexp)
              (not (string-match-p organism-exclude-file-regexp (buffer-file-name))))
          (file-in-directory-p (buffer-file-name) organism-directory))
    (organism-debug "Updating graph for file: %s" (buffer-file-name))
    (organism-graph-update-file (buffer-file-name))))

(provide 'organism)
;;; organism.el ends here
