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

(defcustom nix-converter-executable (executable-find "nix-converter")
  "Nix-converter executable."
  :type 'string
  :group 'nix-converter)

(provide 'nix-converter)

;;; nix-converter.el ends here
