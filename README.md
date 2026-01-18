# A Emacs conversion tool based on nix-converter

[![build-then-test](https://github.com/theobori/nix-converter.el/actions/workflows/build-then-test.yml/badge.svg)](https://github.com/theobori/nix-converter.el/actions/workflows/build-then-test.yml)

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

nix-converter.el is a Emacs package which contains a conversion tool based on nix-converter, it provides an Emacs Lisp API that communicates with the nix-converter CLI. The idea is to be able to convert Nix code into a configuration language and vice versa from Emacs. nix-converter.el, for example, converts the text of the active region into a destination language.

## Getting started

To use the project you need [Emacs](https://www.gnu.org/software/emacs/) with a version higher or equal than `30.1` and [GNU Make](https://www.gnu.org/software/make/) if you want to build and install it manually.

## Installation

The package has not yet been published on a Package Archive. To install it manually, download the code from this [GitHub repository](https://github.com/theobori/nix-converter.el) and then load it. To do this, you can use the following command lines.

```bash
make install
```

Then you can evaluate the following ELisp expression.

```emacs-lisp
(add-to-list 'load-path (file-name-concat user-emacs-directory "manual-packages" "nix-converter"))
```

If you're using [straight.el](https://github.com/radian-software/straight.el) you can use the code snippet below.

```Emacs-lisp
(use-package nix-converter
:straight (nix-converter :type git :host github :repo "theobori/nix-converter.el")
:custom
;; Extra nix-converter command line flags
 (nix-converter-command-line-flags '("--unsafe-keys")))
```

## Examples

Here are a few examples of how to use the API.

### Convert active region

You can use the dedicated command `nix-converter-convert-region`. This command will delete the active region and insert the conversion result. If you'd rather write the conversion result to the dedicated `nix-converter-buffer-name`, you can run the `nix-converter-run-with-region` command.

### Ad-hoc conversion

You can use the dedicated command `nix-converter-run-with-content` or you can call the function as shown below.

```emacs-lisp
;; Convert from Nix to JSON
(nix-converter-run-with-content "[ 123 3 1 ]" "json" t)
```

## Contribute

If you want to help the project, you can follow the guidelines in [CONTRIBUTING.md](./CONTRIBUTING.md).
