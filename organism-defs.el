;;; organism-defs.el --- Shared definitions for the organism package -*- lexical-binding: t; -*-

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

;; Shared definitions for the organism package.
;; This file contains shared constants, structures, utility functions, and
;; forward declarations that need to be available to all modules.

;;; Code:

(require 'cl-lib)

;;; Variables

(defvar organism-graph nil
  "The global organism graph object.
This is the in-memory representation of entries and their connections.")

;;; Structs

(cl-defstruct organism-graph-stats
  "Graph statistics."
  (entries-total 0)
  (entries-files 0)
  (entries-headings 0)
  (entries-added 0)
  (entries-removed 0)
  (entries-processed 0)
  (entries-skipped 0)
  (edges-total 0)
  (edges-added 0)
  (edges-removed 0)
  (density 0.0)
  (avg-edge-weight 0.0)
  (memory-kb 0.0)
  (elapsed-time 0.0)
  (success t))

(provide 'organism-defs)
;;; organism-defs.el ends here
