;;; nix-converter.el --- A conversion tool based on nix-converter  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Théo Bori

;; Author: Théo Bori <theobori@disroot.org>
;; Maintainer: Théo Bori <theobori@disroot.org>
;; Keywords: tools
;; URL: https://github.com/theobori/nix-converter.el
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.2"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary

;; nix-converter.el is a conversion tool for Emacs. It allows you to
;; use the nix-converter CLI tool from Emacs using ELisp.

;;; Code

(defgroup nix-converter ()
  "A conversion tool based on nix-converter"
  :group 'tools
  :group 'external
  :link "https://github.com/theobori/nix-converter.el")

;;;; User options

(defcustom nix-converter-executable (executable-find "nix-converter")
  "Nix-converter executable."
  :type 'string
  :group 'nix-converter)

(defcustom nix-converter-command-line-flags nil
  "List of command line flags for nix-converter."
  :type '(repeat string)
  :group 'nix-converter)

(defconst nix-converter-languages '("json" "yaml" "toml")
  "Possible nix-converter values for the language argument.")

(defconst nix-converter-error-regexp "^[0-9]\\{4\\}/[0-9]\\{2\\}/[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}"
  "Regular expression to match any nix-converter error.")

(defconst nix-converter-default-argument-surrounder "\""
  "Default nix-converter argument surrounder.")

(defun nix-converter--make-safe-argument (argument &optional surrounder)
  "Make a command line ARGUMENT safe by surrounding it with a SURROUNDER."
  (setq surrounder (or surrounder nix-converter-default-argument-surrounder))
  (concat surrounder argument surrounder))

(defun nix-converter--build-command-line-flags (language &optional file from-nix &rest flags)
  "Build the nix-converter command line flags, it returns a formatted
command line as string representing each parameters bound to its
argument."
  (unless (member language nix-converter-languages)
    (error "LANGUAGE must be one of the following values `%S" nix-converter-languages))
  (let ((flag-list nil))
    (when file
      (setq flag-list (append flag-list (list "--filename" (nix-converter--make-safe-argument file)))))
    (when from-nix
      (setq flag-list (append flag-list '("--from-nix"))))
    ;; Adding LANGUAGE and FLAGS at this level
    (setq flag-list (append flag-list (list "--language" (nix-converter--make-safe-argument language)) flags))
    (when nix-converter-command-line-flags
      (setq flag-list (append flag-list nix-converter-command-line-flags)))
    (string-join flag-list " ")))

(defun nix-converter--build-command-line (language &optional file content from-nix &rest flags)
  "Build the full nix-converter command line."
  (when (and (string-empty-p file) (string-empty-p content))
    (error "FILE or CONTENT must be non-empty strings"))
  (let* ((flags (apply
		 'nix-converter--build-command-line-flags language file from-nix flags))
	 (command-line-list (list nix-converter-executable flags)))
    (when content
      (setq command-line-list
	    (append
	     (list "echo" "-n" (nix-converter--make-safe-argument content "'") "|")
	     command-line-list)))
    (string-join command-line-list " ")))

(defun nix-converter--run (language &optional file content from-nix &rest flags)
  "Run nix-converter with the main CLI arguments and returns the output as
string."
  (unless nix-converter-executable
    (error "Unable to find nix-converter executable"))
  (let* ((command-line (apply
			'nix-converter--build-command-line language file content from-nix flags))
	 ;; Here we are using the `shell-command-to-string' function
	 ;; because a shell interpreter is needed. Indeed, the echo
	 ;; function could be needed if the is non-nil.
	 (output (string-trim (shell-command-to-string command-line))))
    (when (string-match-p nix-converter-error-regexp output)
      (error "The following error occured during the nix-converter execution: %s" output))
    output))

(defun nix-converter-run-with-file (file language &optional from-nix &rest flags)
  "Run nix-converter with a filepath as input and returns the output as
string.
FILE is the filepath.
LANGUAGE is the second language, see `nix-converter-languages' for more defails.
If FROM-NIX is non-nil, it will convert from Nix to LANGUAGE.
FLAGS are additional command line flags."
  (apply 'nix-converter--run language file nil from-nix flags))

(defun nix-converter-run-with-content (content language &optional from-nix &rest flags)
  "Run nix-converter with the content to convert as input and returns the
output as string.
CONTENT is the content to convert.
LANGUAGE is the second language, see `nix-converter-languages' for more defails.
If FROM-NIX is non-nil, it will convert from Nix to LANGUAGE.
FLAGS are additional command line flags."
  (apply 'nix-converter--run language nil content from-nix flags))

(defun nix-converter-convert-region (language &optional from-nix)
  ;; TODO
  )

(provide 'nix-converter)

;;; nix-converter.el ends here
