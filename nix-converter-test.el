;;; nix-converter-test.el --- Units tests for nix-converter  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Théo Bori

;; Author: Théo Bori <theobori@disroot.org>
;; Maintainer: Théo Bori <theobori@disroot.org>
;; Keywords: tools
;; URL: https://github.com/theobori/nix-converter.el

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

;; Units tests for nix-converter

;;; Code

(require 'ert)

;;;; Units tests for nix-converter.el

(require 'nix-converter)

(ert-deftest test-nix-converter--build-command-line-flags ()
  "Test that `nix-converter--build-command-line-flags' build correct
command line flags."
  (should (string-equal
	   "--filename \"test.json\" --from-nix --language \"json\""
	   (nix-converter--build-command-line-flags "json" "test.json" t)))
  (should (string-equal
	   "--filename \"test.yaml\" --language \"yaml\""
	   (nix-converter--build-command-line-flags "yaml" "test.yaml")))
  (should (string-equal
	   "--language \"toml\""
	   (nix-converter--build-command-line-flags "toml"))))

(ert-deftest test-nix-converter--build-command-line-flags-fail ()
  "Test that `nix-converter--build-command-line-flags' has a correct error
handling."
  (should-error (nix-converter--build-command-line-flags "invalid-langage")))

(ert-deftest test-nix-converter--build-command-line ()
  "Test that `nix-converter--build-command-line' build correct command
line."
  (should (string-equal
	   (format "echo -n '[ 1 2 3 ]' | %s --from-nix --language \"json\"" nix-converter-executable)
	   (nix-converter--build-command-line "json" nil "[ 1 2 3 ]" t)))
  (should-error (nix-converter--build-command-line "json" nil nil))
  (should-error (nix-converter--build-command-line "json" "" ""))
  (should-error (nix-converter--build-command-line "json" "" nil))
  (should-error (nix-converter--build-command-line "json" nil "")))

(ert-deftest test-nix-converter--run ()
  "Test that `nix-converter--run' is able to handle nix-converter execution
error"
  (should-error (nix-converter--run "json" nil "[1,2,]")))

(ert-deftest test-nix-converter--yes-or-no ()
  "Test that `nix-converter--yes-or-no' returns correct results."
  (should (nix-converter--yes-or-no "yes"))
  (should (nix-converter--yes-or-no "  YES      "))
  (should (nix-converter--yes-or-no "y"))
  (should-not (nix-converter--yes-or-no "no"))
  (should-not (nix-converter--yes-or-no "anything")))

(provide 'nix-converter-test)

;;; nix-converter-test.el ends here
