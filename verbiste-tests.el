;;; verbiste-tests.el --- Tests for verbiste.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Aidan Pace

;; Author: Aidan Pace <apace@defrecord.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for verbiste.el

;;; Code:

(require 'ert)
(require 'verbiste)

;; Test customization variables
(ert-deftest verbiste-test-custom-vars ()
  "Test that customization variables are properly defined."
  (should (boundp 'verbiste-data-dir))
  (should (stringp verbiste-data-dir))
  (should (boundp 'verbiste-french-conjugator-path))
  (should (stringp verbiste-french-conjugator-path))
  (should (boundp 'verbiste-french-deconjugator-path))
  (should (stringp verbiste-french-deconjugator-path)))

;; Test internal helper functions
(ert-deftest verbiste-test-ensure-executable ()
  "Test the executable checker."
  (should (functionp #'verbiste--ensure-executable))
  (should (equal (verbiste--ensure-executable "non-existent-program") nil)))

;; Test XML loading functionality (when files are available)
(ert-deftest verbiste-test-load-xml-file ()
  "Test XML loading function (only if test data is available)."
  (skip-unless (and (file-readable-p (expand-file-name "verbs-fr-sample.xml" 
                                                      default-directory))))
  (let ((verbiste-data-dir default-directory))
    (should (verbiste--load-xml-file "verbs-fr-sample.xml"))
    (should (listp (verbiste--load-xml-file "verbs-fr-sample.xml")))))

;; Test mode definition
(ert-deftest verbiste-test-mode ()
  "Test verbiste-mode is properly defined."
  (should (fboundp 'verbiste-mode))
  (should (keymapp verbiste-mode-map)))

(provide 'verbiste-tests)
;;; verbiste-tests.el ends here