# Organism - Notes network for org-mode

*Organism* can create, manage, and analyze org-mode notes as
interconnected graph structure.

## Install

This project depends on
[Graphael](https://github.com/lamberta/graphael) for its Emacs Lisp
graph data structures and algorithms.

Clone the repositories:

```bash
$ git clone https://github.com/lamberta/graphael.git

$ git clone https://github.com/lamberta/organism.git
```

Load the packages in your `.emacs` configuration:

```elisp
(use-package graphael
  :load-path "/path/to/graphael"
  :ensure nil)

(use-package organism
  :load-path "/path/to/organism"
  :ensure nil
  :after (graphael org)
  :custom
  (organism-directory "~/org")  ; default to org-directory
  (organism-debug-enabled t))
```

## Basic usage

Start the minor mode with `M-x organism-mode` or add
`(organism-mode 1)` to your configuration.

### Create and find entries

Entries are Org files or headings that have an ID property and stored
in `organism-directory` (which defaults to `org-directory`). Use
`org-id-get-create` to add an ID property. Links between entry IDs are
how nodes connect in the Organism graph.

Inspired by [org-roam](https://github.com/org-roam/org-roam), there
are several commands for working with entries:

- `organism-find`: Find an existing entry or create a new one.
- `organism-link`: Insert a link to an entry. Create the entry, if needed.
- `organism-link-immediate`: Create a new entry and insert a link
  without editing.

Organism uses annotations to improve its minibuffer completion. For a
better experience, install packages such as `vertico`, `marginalia`,
and `orderless`.

### Sync

*Organism* uses the file system as the source of truth and builds its
internal graph representation when you start `organism-mode`. Entries
are automatically updated when Emacs saves a file. But if a file is
added, deleted, or modified outside of Emacs, rebuild the graph using
`organism-rescan`.

## Developer commands

Byte-compile Elisp files for compilation warnings:

```bash
$ make
```

Run tests:

```bash
$ make test
```

Use a different version of Emacs:

```bash
$ EMACS=/path/to/bin/emacs make test
```

## License

This project is licensed under the GNU General Public License v3.0 -
see the LICENSE file for details.
