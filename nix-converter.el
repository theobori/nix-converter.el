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

(defcustom nix-converter-buffer-name nil
  "Dedicated buffer for the nix-converter execution result"
  :type '(string)
  :group 'nix-converter)

(defconst nix-converter-languages '("json" "yaml" "toml")
  "Possible nix-converter values for the language argument.")

(defconst nix-converter-default-language "json"
  "Default nix-converter language.")

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
    (unless (string-empty-p file)
      (setq flag-list (append flag-list (list "--filename" (nix-converter--make-safe-argument file)))))
    (unless (string-empty-p from-nix)
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
	 ;; function/program could be needed if the is non-nil.
	 (output (string-trim (shell-command-to-string command-line))))
    (when (string-match-p nix-converter-error-regexp output)
      (error "The following error occured during the nix-converter execution: %s" output))
    output))

(defun nix-converter--text-prompt (prompt &rest default)
  "Read a string with a formatted prompt, then returns the read value."
  (read-string (format-prompt prompt default) nil nil default))

(defun nix-converter--default-prompt ()
  "Returns a list of `nix-converter--text-prompt' function calls. It read
values for the language and the from-nix nix-converter parameters."
  (list
   (nix-converter--text-prompt "Language" nix-converter-default-language)
   (nix-converter--text-prompt "Is the source language Nix ? (No by default)")))

(defun nix-converter--insert-to-buffer (msg)
  "Creates a temporary buffer then inserts MSG if it's a string"
  (when (stringp msg)
    (with-output-to-temp-buffer nix-converter-buffer-name
      (pop-to-buffer nix-converter-buffer-name)
      (insert msg))))

(defun nix-converter-run-with-file (file language &optional from-nix &rest flags)
  "Run nix-converter with a filepath as input and returns the conversion
result as string.
FILE is the filepath.
LANGUAGE is the second language, see `nix-converter-languages' for more defails.
If FROM-NIX is non-nil, it will convert from Nix to LANGUAGE.
FLAGS are additional command line flags."
  (interactive
   (append
    (list (read-file-name "File:"))
    (nix-converter--default-prompt)))
  (let ((result (apply 'nix-converter--run language file nil from-nix flags)))
    (when (called-interactively-p)
      (nix-converter--insert-to-buffer result))
    result))

(defun nix-converter-run-with-content (content language &optional from-nix &rest flags)
  "Run nix-converter with the content to convert as input and returns the
conversion result as string.
CONTENT is the content to convert.
LANGUAGE is the second language, see `nix-converter-languages' for more defails.
If FROM-NIX is non-nil, it will convert from Nix to LANGUAGE.
FLAGS are additional command line flags."
  (interactive
   (append
    (list (nix-converter--text-prompt "Language expression"))
    (nix-converter--default-prompt)))
  (let ((result (apply 'nix-converter--run language nil content from-nix flags)))
    (when (called-interactively-p)
      (nix-converter--insert-to-buffer result))
    result))

;; See https://github.com/protesilaos/denote/blob/a31969fea285a0fc7593fec8ab905ecabb2d7c5e/denote.el#L5274-L5288
(defun nix-converter--get-active-region-content ()
  "Return the text of the active region, else nil."
  (when-let* ((_ (region-active-p))
              (beg (region-beginning))
              (end (region-end))
              (contents (buffer-substring-no-properties beg end))
              (_ (not (string-blank-p contents))))
    (string-trim contents)))

(defun nix-converter--delete-active-region-content ()
  "Delete the content of the active region, if any."
  (when-let* ((_ (region-active-p))
              (beg (region-beginning))
              (end (region-end)))
    (delete-region beg end)))

(defun nix-converter-run-with-region (language &optional from-nix)
  "Run nix-converter with a region as input and returns the conversion
result as string.
LANGUAGE is the second language, see `nix-converter-languages' for more defails.
If FROM-NIX is non-nil, it will convert from Nix to LANGUAGE."
  (interactive (nix-converter--default-prompt))
  (let* ((content (nix-converter--get-active-region-content))
	 (result (nix-converter-run-with-content content language from-nix)))
    (when (called-interactively-p)
      (nix-converter--insert-to-buffer result))
    result))

(defun nix-converter-convert-region (language &optional from-nix)
  "Convert the current region with the nix-converter result.
LANGUAGE is the second language, see `nix-converter-languages' for more defails.
If FROM-NIX is non-nil, it will convert from Nix to LANGUAGE."
  (interactive (nix-converter--default-prompt))
  (let ((result (nix-converter-run-with-region language from-nix)))
    (nix-converter--delete-active-region-content)
    (insert result)))

(provide 'nix-converter)

;;; nix-converter.el ends here
