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
