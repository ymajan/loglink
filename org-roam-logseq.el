;;; org-roam-logseq.el --- Org-roam Logseq converter -*- coding: utf-8; lexical-binding: t; -*-

(require 'cl-lib)
(require 'org)
(require 'org-roam)
(require 'f)
;;(use-package f
;;  :ensure t)
;; logseq folder same as org-roam directory
(defvar ymajan/logseq-folder org-roam-directory)

(defvar ymajan/logseq-pages (f-expand (f-join ymajan/logseq-folder "pages")))
(defvar ymajan/logseq-journals (f-expand (f-join ymajan/logseq-folder "journals")))

;; exclude files in logseq/bak/ folder via regex
(defvar ymajan/logseq-exclude-pattern (string-join (list "^" (file-truename ymajan/logseq-folder) "/logseq/bak/.*$")))

(defun ymajan/logseq-journal-p (file) (string-match-p (concat "^" ymajan/logseq-journals) file))
