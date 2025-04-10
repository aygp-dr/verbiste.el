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

;; Test main French functions
(ert-deftest verbiste-test-french-functions ()
  "Test that French functions are properly defined."
  (should (fboundp 'verbiste-french-conjugation))
  (should (commandp 'verbiste-french-conjugation))
  (should (fboundp 'verbiste-french-deconjugation))
  (should (commandp 'verbiste-french-deconjugation)))

;; Test French verbs loading function
(ert-deftest verbiste-test-load-french-verbs ()
  "Test loading French verbs function."
  (skip-unless (and (file-readable-p (expand-file-name "verbs-fr-sample.xml" 
                                                     default-directory))))
  (let ((verbiste-data-dir default-directory))
    (should (fboundp 'verbiste--load-french-verbs))
    (when (file-readable-p (expand-file-name "verbs-fr.xml" verbiste-data-dir))
      (should (listp (verbiste--load-french-verbs))))))

;; Test installation check function
(ert-deftest verbiste-test-installation-check ()
  "Test installation check function."
  (should (fboundp 'verbiste-check-installation))
  (should (commandp 'verbiste-check-installation)))

;; Test keymap bindings
(ert-deftest verbiste-test-keymap-bindings ()
  "Test that keymap bindings are properly set."
  (should (keymapp verbiste-mode-map))
  (should (eq (lookup-key verbiste-mode-map (kbd "C-c v f c")) 'verbiste-french-conjugation))
  (should (eq (lookup-key verbiste-mode-map (kbd "C-c v f d")) 'verbiste-french-deconjugation))
  (should (eq (lookup-key verbiste-mode-map (kbd "C-c v s")) 'verbiste-display-similar-verbs))
  (should (eq (lookup-key verbiste-mode-map (kbd "C-c v r")) 'verbiste-browse-random-verbs)))

;; Test verb cluster functions
(ert-deftest verbiste-test-cluster-functions ()
  "Test the verb cluster functions."
  (should (fboundp 'verbiste--load-verb-clusters))
  (should (fboundp 'verbiste--get-similar-verbs))
  (should (fboundp 'verbiste--get-random-verbs))
  (should (fboundp 'verbiste-display-similar-verbs))
  (should (fboundp 'verbiste-browse-random-verbs))
  
  ;; Skip tests if the clusters file doesn't exist
  (skip-unless (file-readable-p verbiste-clusters-file))
  
  ;; Test loading clusters
  (let ((clusters (verbiste--load-verb-clusters)))
    (should (listp clusters))
    (should (consp (car clusters))))
  
  ;; Test random verb selection
  (let ((random-verbs (verbiste--get-random-verbs 5)))
    (should (listp random-verbs))
    (should (<= (length random-verbs) 5))
    (should (stringp (car random-verbs))))
  
  ;; Define a utility function for cosine similarity calculation
  (defun verbiste-test--cosine-similarity (v1 v2)
    "Calculate the cosine similarity between vectors V1 and V2."
    (let* ((dot-product (apply #'+ (cl-mapcar #'* v1 v2)))
           (magnitude1 (sqrt (apply #'+ (mapcar (lambda (x) (* x x)) v1))))
           (magnitude2 (sqrt (apply #'+ (mapcar (lambda (x) (* x x)) v2))))
           (magnitudes (* magnitude1 magnitude2)))
      (if (zerop magnitudes)
          0.0
        (/ dot-product magnitudes))))
  
  ;; Test embedding similarity calculation
  (let* ((v1 '(0.1 0.2 0.3))
         (v2 '(0.2 0.3 0.4))
         (v3 '(-0.1 -0.2 -0.3))
         (sim1-2 (verbiste-test--cosine-similarity v1 v2))
         (sim1-3 (verbiste-test--cosine-similarity v1 v3)))
    ;; Similar vectors should have higher similarity
    (should (> sim1-2 0.9))
    ;; Opposite vectors should have negative similarity
    (should (< sim1-3 0)))
  
  ;; Test similar verb retrieval with different key formats
  (let ((mock-clusters 
         ;; Mix of symbol and string keys to test robustness
         `((dominer . (,(list (cons 'verb "diriger") (cons 'similarity 0.92))
                       ,(list (cons "verb" "contrôler") (cons "similarity" 0.87))))
           ("parler" . (,(list (cons 'verb "discuter") (cons 'similarity 0.94))
                        ,(list (cons "verb" "converser") (cons "similarity" 0.88)))))))
    
    ;; Mock the cluster loading function temporarily
    (cl-letf (((symbol-function 'verbiste--load-verb-clusters) 
               (lambda () mock-clusters)))
      
      ;; Test with symbol key
      (let ((similar-verbs (verbiste--get-similar-verbs "dominer")))
        (should similar-verbs)
        (should (= (length similar-verbs) 2))
        (should (stringp (caar similar-verbs)))
        ;; Only test that some string is returned, not exact values
        (should (stringp (caar similar-verbs)))
        (should (numberp (cdar similar-verbs))))
      
      ;; Test with string key
      (let ((similar-verbs (verbiste--get-similar-verbs "parler")))
        (should similar-verbs)
        (should (= (length similar-verbs) 2))
        (should (stringp (caar similar-verbs)))
        (should (numberp (cdar similar-verbs))))
      
      ;; Test random verbs
      (let ((random-verbs (verbiste--get-random-verbs 5)))
        (should random-verbs)
        (should (<= (length random-verbs) 2)) ;; Only 2 verbs in our mock data
        (should (stringp (car random-verbs)))))))

(provide 'verbiste-tests)
;;; verbiste-tests.el ends here