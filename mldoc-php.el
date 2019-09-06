;;; mldoc-php.el --- ElDoc/MLDoc provider for PHP    -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 25 Jul 2019
;; Version: 0.1.0
;; Keywords: tools, lisp
;; Homepage: https://github.com/emacs-php/mldoc
;; Package-Requires: ((emacs "24.4") (mldoc "0.2.1"))
;; License: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is ElDoc provider based on MlDoc.

;;; Code:
(require 'mldoc)

(defgroup mldoc-php nil
  "ElDoc/MLDoc provider for PHP"
  :tag "MLDoc PHP"
  :group 'mldoc
  :group 'php)

(defcustom mldoc-php-phpdoc-spec
  '("[" :defined-by "]" " /** " :function " " (args ", ") " */")
  "MLDoc display specification for PHPDoc."
  :type 'list)

(defvar mldoc-php-phpdoc-tags-alist
  '(
    ;;("@author" . (:args (&rest author) :defined-by "PHPDoc"))
    ("@after" . (:args () :defined-by "PHPUnit"))
    ("@afterClass" . (:args ()  :defined-by "PHPUnit"))
    ("@backupGlobals" . (:args ("enabled|disabled") :defined-by "PHPUnit"))
    ("@backupStaticAttributes" . (:args ("enabled|disabled") :defined-by "PHPUnit"))
    ("@before" . (:args () :defined-by "PHPUnit"))
    ("@beforeClass" . (:args () :defined-by "PHPUnit"))
    ("@codeCoverageIgnore" . (:args () :defined-by "PHPUnit"))
    ("@codeCoverageIgnoreStart" . (:args () :defined-by "PHPUnit"))
    ("@codeCoverageIgnoreEnd" . (:args () :defined-by "PHPUnit"))
    ("@covers" . (:args () :defined-by "PHPUnit"))
    ("@coversDefaultClass" . (:args () :defined-by "PHPUnit"))
    ("@coversNothing" . (:args () :defined-by "PHPUnit"))
    ("@dataProvider" . (:args ("provider") :defined-by "PHPUnit"))
    ("@depends" . (:args () :defined-by "PHPUnit"))
    ("@expectedException" . (:args () :defined-by "PHPUnit"))
    ("@expectedExceptionCode" . (:args () :defined-by "PHPUnit"))
    ("@expectedExceptionMessage" . (:args () :defined-by "PHPUnit"))
    ("@expectedExceptionMessageRegExp" . (:args () :defined-by "PHPUnit"))
    ("@group" . (:args () :defined-by "PHPUnit"))
    ("@large" . (:args () :defined-by "PHPUnit"))
    ("@medium" . (:args () :defined-by "PHPUnit"))
    ("@preserveGlobalState" . (:args () :defined-by "PHPUnit"))
    ("@requires" . (:args () :defined-by "PHPUnit"))
    ("@runTestsInSeparateProcesses" . (:args () :defined-by "PHPUnit"))
    ("@runInSeparateProcess" . (:args () :defined-by "PHPUnit"))
    ("@small" . (:args () :defined-by "PHPUnit"))
    ("@test" . (:args () :defined-by "PHPUnit"))
    ("@testdox" . (:args () :defined-by "PHPUnit"))
    ("@ticket" . (:args () :defined-by "PHPUnit"))
    ("@uses" . (:args () :defined-by "PHPUnit"))
    ))

(defun mldoc-php--parse-line-simple (line)
  "Return list of @tag and rest part that parsed by PDPDoc `LINE'."
  (when (string-match
         (eval-when-compile
           (rx bol
               (* (syntax whitespace))
               (or "/**" "*")
               (+ (syntax whitespace))
               (group "@" (+ (syntax word)))
               (* (syntax whitespace))
               (group (* any))
               eol))
         line)
    (list (match-string 1 line)
          (match-string 2 line)
          (match-beginning 2))))

(define-mldoc mldoc-php-phpdoc
  "MLDoc/ElDoc function for PHPDoc."
  (when (mldoc-in-comment)
    (let* ((pos-bol (line-beginning-position))
           (pos-eol (line-end-position))
           (pos (- (point) pos-bol))
           (doc (mldoc-php--parse-line-simple
                (buffer-substring-no-properties pos-bol pos-eol)))
          tag def)
      (when doc
        (setq tag (nth 0 doc))
        (setq def (cdr-safe (assoc tag mldoc-php-phpdoc-tags-alist)))
        (when def
          (mldoc-list
           mldoc-php-phpdoc-spec
           :function tag
           :current-arg (when (< (nth 2 doc) pos) 1)
           :args (plist-get def :args)
           :values (list :defined-by (plist-get def :defined-by))))))))

(provide 'mldoc-php)
;;; mldoc-php.el ends here
