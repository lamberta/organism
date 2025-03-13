;;; organism-utils.el --- Utility functions for organism -*- lexical-binding: t; -*-

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

;; This file provides common utility functions for the organism package.

;;; Code:

(require 'cl-lib)
(require 'org-id)
(require 'organism-defs)

;; Forward declaration for free variables

(defvar organism-directory)
(defvar organism-exclude-file-regexp)
(defvar organism-file-match)

;;; Development

(defun organism-debug (format-string &rest args)
  "Output debug message if `organism-debug-enabled' is non-nil.
FORMAT-STRING and ARGS are passed to `message'."
  (when (bound-and-true-p organism-debug-enabled)
    (apply #'message (concat "Organism: " format-string) args)))

(defun organism-check-type (value allowed-type &optional arg-name)
  "Check that VALUE is of ALLOWED-TYPE, signal error if not.
ARG-NAME is used in the error message if provided."
  (unless (or (null value) (cl-typep value allowed-type))
    (signal 'wrong-type-argument
      (list allowed-type value (or arg-name "argument")))))

;;; Formatting

(defun organism-format-timestamp (timestamp &optional format-string)
  "Convert TIMESTAMP to a formatted date string.
TIMESTAMP should be in ISO 8601 format or any supported by `date-to-time`."
  (let ((fmt-string (or format-string "%b %e %Y %l:%M%P"))
        (time (date-to-time timestamp)))
    (format-time-string fmt-string time)))

;; Formatting minibuffer status message

(defun organism-utils--pluralize (count singular &optional plural)
  "Return COUNT with SINGULAR or PLURAL form based on COUNT."
  (let ((plural-form (or plural (concat singular "s"))))
    (format "%d %s" count (if (= count 1) singular plural-form))))

(defun organism-utils-format-status (stats action-name)
  "Format a user-facing message string based on STATS with ACTION-NAME."
  (let* ((elapsed (organism-graph-stats-elapsed-time stats))
         (entries-added (organism-graph-stats-entries-added stats))
         (entries-removed (organism-graph-stats-entries-removed stats))
         (entries-processed (organism-graph-stats-entries-processed stats))
         (edges-added (organism-graph-stats-edges-added stats))
         (edges-removed (organism-graph-stats-edges-removed stats))
         (changes-parts nil))
    (cl-labels ((add-change (count word action &optional plural)
                  (when (> count 0)
                    (push (format "%s %s"
                            (organism-utils--pluralize count word plural)
                            action)
                      changes-parts))))
      ;; Build changes parts
      (add-change entries-removed "entry" "removed" "entries")
      (add-change entries-added "entry" "added" "entries")
      (add-change edges-removed "connection" "removed")
      (add-change edges-added "connection" "added")
      ;; Format the status string
      (format "%s complete (%.2fs): %s%s"
        action-name
        elapsed
        (organism-utils--pluralize entries-processed "entry" "entries")
        (if changes-parts
          (format " (%s)" (string-join (nreverse changes-parts) ", "))
          " processed")))))

;;; Entries

(defun organism-file-matches-criteria-p (file)
  "Return t if FILE should be included in the organism graph."
  (and (file-exists-p file)
       (string-match-p organism-file-match file)
       (or (null organism-exclude-file-regexp)
           (not (string-match-p organism-exclude-file-regexp file)))
       (file-in-directory-p file organism-directory)))

(defun organism-get-ids-in-file (file-name)
  "Get all property IDs in a FILE-NAME using `org-id-locations' lookup.
Returns a list of ID strings or nil if no IDs found or FILE-NAME is invalid."
  (unless (and file-name (file-exists-p file-name))
    (organism-debug "Invalid or nonexistent file: %s" file-name)
    nil)

  (let ((normalized-file (file-truename file-name))
        (ids nil))
    (maphash (lambda (id path)
               (when (equal (file-truename path) normalized-file)
                 (push id ids)))
      org-id-locations)
    ids))

(defun organism-get-files-with-ids ()
  "Return a list of files with at least one ID.
Only includes files that exist and are within `organism-directory'."
  (let ((files nil))
    (maphash (lambda (_id file)
               (when (and (file-exists-p file)
                          (file-in-directory-p file organism-directory))
                 (cl-pushnew file files :test #'equal)))
      org-id-locations)
    files))

(provide 'organism-utils)
;;; organism-utils.el ends here
