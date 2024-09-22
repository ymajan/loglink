;;; org-roam-logseq.el --- Org-roam Logseq converter -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2023, Ivan Danov
;; Author: Ivan Danov
;; URL: https://github.com/idanov/org-roam-logseq.el/
;; Keywords: org-mode, roam, logseq
;; Version: 0.1.1
;; Package-Requires: ((emacs "26.1") (org-roam "2.0") (f "0.20.0"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package provides functionality to convert between Org-roam and Logseq formats.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-roam)
(require 'f)

;; logseq folder same as org-roam directory
(defvar logseq-folder org-roam-directory
  "The root folder for Logseq files.")

(defvar logseq-pages (f-expand "pages" logseq-folder)
  "The folder for Logseq pages.")

(defvar logseq-journals (f-expand "journals" logseq-folder)
  "The folder for Logseq journals.")

;; exclude files in logseq/bak/ folder via regex
(defvar logseq-exclude-pattern
  (concat "^" (regexp-quote (file-truename logseq-folder)) "/logseq/bak/.*$")
  "Regex pattern to exclude files in the logseq/bak/ folder.")

(defun logseq-journal-p (file)
  "Return t if FILE is a Logseq journal file."
  (string-prefix-p logseq-journals (expand-file-name file)))

(provide 'org-roam-logseq)

;;; org-roam-logseq.el ends here
