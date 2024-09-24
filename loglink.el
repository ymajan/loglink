;;; loglink.el --- Org-roam Logseq converter -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2024, Jaynam Shah
;; Author: Jaynam Shah
;; URL: https://github.com/ymajan/loglink/tree/main

;; Based on:
;; Copyright (C) 2023, Ivan Danov
;; Author: Ivan Danov
;; URL: https://github.com/idanov/org-roam-logseq.el/
;; Tags: org-mode, roam, logseq
;; Version: 0.1.1
;; Prerequisites: ((emacs "26.1") (org-roam "2.0") (f "0.20.0"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BACK UP YOUR LOGSEQ DIR BEFORE RUNNING THIS!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LICENSE
;;
;; This code is a derivative work of
;;    https://gist.github.com/zot/ddf1a89a567fea73bc3c8a209d48f527
;; which is dual-licensed with MIT and GPL licenses by William R. Burdick Jr.
;;
;; The license of the derivative work here is MIT.
;;
;; Logseq compatibility:
;; - put ids and titles at the tops of non-journal files
;; - change fuzzy links from [[PAGE]] to [[id:2324234234][PAGE]]
;; - change fuzzy links from [[ALIAS]] to [[id:2324234234][ALIAS]]
;; - also change file links to id links, provided that the links
;;   expand to file names that have ids in the roam database.
;;
;; NOTE:
;; - it converts the links only if they are not alias links due to a bug in Logseq:
;;    https://github.com/logseq/logseq/issues/9342

;; This package provides functionality to convert between Org-roam and Logseq formats.

;;; Code:

(require 'org-roam)
(require 'f)

;; logseq folder same as org-roam directory
(defvar loglink-folder org-roam-directory
  "The root folder for Logseq files.")

(defvar loglink-pages (f-expand "pages" loglink-folder)
  "The folder for Logseq pages.")

(defvar loglink-journals (f-expand "journals" loglink-folder)
  "The folder for Logseq journals.")

;; exclude files in logseq/bak/ folder via regex
(defvar loglink-exclude-pattern
  (concat "^" (regexp-quote (file-truename loglink-folder)) "/logseq/bak/.*$")
  "Regex pattern to exclude files in the logseq/bak/ folder.")

(defun loglink-journal-p (file)
  "Return t if FILE is a Logseq journal file."
  (string-prefix-p loglink-journals (expand-file-name file)))

(defun loglink-ensure-file-id (file)
  "Ensure FILE has an ID and a title."
  (setq file (f-expand file))
  (if (loglink-journal-p file)
      ;; do nothing for journal files
      ;; TODO double check this is actually desired behaviour
      (message "Skipping journal file: %s" file)
    '(nil . nil)
    ;; Else, proceed to ensure ID and title
    (let* ((buf (get-file-buffer file))
           (was-modified (buffer-modified-p buf))
           (new-buf nil)
           has-data
           org
           changed
           sec-end)
      (when (not buf)
        (setq buf (find-file-noselect file))
        (setq new-buf t))
      (set-buffer buf)
      (setq org (org-element-parse-buffer))
      (setq has-data (cddr org))
      (goto-char 1)
      (when (not (and (eq 'section (org-element-type (nth 2 org))) (org-roam-id-at-point)))
        ;; this file has no file id
        (setq changed t)
        (when (eq 'headline (org-element-type (nth 2 org)))
          ;; if there's no section before the first headline, add one
          (insert "\n")
          (goto-char 1))
        (org-id-get-create)
        (setq org (org-element-parse-buffer)))
      (when (nth 3 org)
        (when (not (org-collect-keywords ["title"]))
          ;; no title -- ensure there's a blank line at the section end
          (setq changed t)
          (setq sec-end (org-element-property :end (nth 2 org)))
          (goto-char (1- sec-end))
          (when (and (not (equal "\n\n" (buffer-substring-no-properties (- sec-end 2) sec-end))))
            (insert "\n")
            (goto-char (1- (point)))
            (setq org (org-element-parse-buffer)))
          ;; in case of no title, make the title the same as the filename
          (let ((title (file-name-sans-extension (file-name-nondirectory file))))
            (insert (format "#+title: %s" title)))
          ))
      ;; ensure org-roam knows about the new id and/or title
      (when changed (save-buffer))
      (cons new-buf buf))))

(defun loglink-convert-logseq-file (buf)
  "Convert fuzzy and file:../pages logseq links in the BUF to id links."
  (save-excursion
    (let* (changed
           link)
      (set-buffer buf)
      (goto-char 1)
      (while (search-forward "[[" nil t)
        (setq link (org-element-context))
        (setq newlink (loglink-reformat-link link))
        (when newlink
          (setq changed t)
          (goto-char (org-element-property :begin link))
          (delete-region (org-element-property :begin link) (org-element-property :end link))
          ;; note, this format string is reall =[[%s][%s]]= but =%= is a markup char so one's hidden
          (insert newlink)
          (message "Converting logseq file %s link from %s to %s" (buffer-file-name buf) (org-element-property :raw-link link) newlink)))
      ;; ensure org-roam knows about the changed links
      (when changed (save-buffer)))))

(defun loglink-reformat-link (link)
  (let (filename
        title
        id
        linktext
        newlink)
    (when (eq 'link (org-element-type link))
      (when (equal "fuzzy" (org-element-property :type link))
        (setq title (org-element-property :raw-link link))
        ;; fetch the filename by scanning the db for title and alias (in that order)
        (setq filename (caar (org-roam-db-query [:select :distinct [nodes:file]
                                                 :from nodes
                                                 :where (= nodes:title $s1)
                                                 :union
                                                 :select :distinct [nodes:file]
                                                 :from aliases
                                                 :left-join nodes
                                                 :on (= aliases:node-id nodes:id)
                                                 :where (= aliases:alias $s1)] title)))
        (setq linktext (if-let ((contents-begin (org-element-property :contents-begin link))
                                (contents-end (org-element-property :contents-end link)))
                           (buffer-substring-no-properties contents-begin contents-end)
                         (org-element-property :raw-link link)
                         )))
      (when (equal "file" (org-element-property :type link))
        ;; TODO create a workaround for Logseq's bug with aliases
        (setq filename (f-expand (replace-regexp-in-string "\\..//" "/" (org-element-property :path link))))
        (if (org-element-property :contents-begin link)
            (setq linktext (buffer-substring-no-properties
                            (org-element-property :contents-begin link)
                            (org-element-property :contents-end link)))
          (setq linktext (buffer-substring-no-properties
                          (+ (org-element-property :begin link) 2)
                          (- (org-element-property :end link) 2)))))
      (when (and filename (f-exists-p filename))
        (setq id (caar (org-roam-db-query [:select id :from nodes :where (like file $s1)]
                                          filename)))
        (when id
          (setq newlink (format "[[id:%s][%s]]%s"
                                id
                                linktext
                                (if (> (org-element-property :post-blank link))
                                    (make-string (org-element-property :post-blank link) ?\s)
                                  "")))
          (when (not (equal newlink
                            (buffer-substring-no-properties
                             (org-element-property :begin link)
                             (org-element-property :end link))))
            newlink))))))

(defun loglink-roam-file-modified-p (file-path)
  (and (not (string-match-p loglink-exclude-pattern (file-truename file-path)))
       (let ((content-hash (org-roam-db--file-hash file-path))
             (db-hash (caar (org-roam-db-query [:select hash :from files
                                                :where (= file $s1)] file-path))))
         (not (string= content-hash db-hash)))))

(defun loglink-modified-logseq-files ()
  (emacsql-with-transaction (org-roam-db)
    (seq-filter 'loglink-roam-file-modified-p
                (org-roam--list-files loglink-folder))))

(defun loglink-check-logseq ()
  (interactive)
  (setq files (org-roam--list-files loglink-logseq-folder))
  (message "loglink-check-logseq is processing %d" (length files))
  (org-roam-logseq-patch files)
  )

(defun loglink-check-logseq-unsynced ()
  (interactive)
  (setq files (org-roam--list-files loglink-logseq-folder))
  (setq files-in-db (apply #'append (org-roam-db-query [:select file :from files])))
  (setq unsynced-files (cl-set-difference files files-in-db :test #'file-equal-p))
  (message "loglink-check-logseq-unsynced is processing %d" (length unsynced-files))
  (org-roam-logseq-patch unsynced-files)
  )

(defun org-roam-logseq-patch (files)
  (let (created bufs unmodified cur bad buf)
    ;; make sure all the files have file ids
    (dolist (file-path files)
      (setq file-path (f-expand file-path))
      (setq cur (loglink-ensure-file-id file-path))
      (setq buf (cdr cur))
      (push buf bufs)
      (when (and (not (loglink-logseq-journal-p file-path))
                 (not buf))
        (push file-path bad))
      (when (not (buffer-modified-p buf))
        (push buf unmodified))
      (when (car cur)
        (push buf created)))
    ;; patch fuzzy links
    (mapc 'loglink-convert-logseq-file
          (seq-filter 'identity bufs))
    (dolist (buf unmodified)
      (when (buffer-modified-p buf)
        (save-buffer unmodified)))
    (mapc 'kill-buffer created)
    (when bad
      (message "Bad items: %s" bad))
    nil))

(defun org-roam-logseq-hook ()
  "Process any non-empty org-roam files on accessing if they have logseq links."
  (when (and (org-roam-file-p)
             (/= (buffer-size (current-buffer)) 0))
    (progn
      (loglink-ensure-file-id (buffer-file-name (current-buffer)))
      (loglink-convert-logseq-file (current-buffer)))))

(add-hook 'org-mode-hook #'org-roam-logseq-hook)

(provide 'loglink)

;;; loglink.el ends here
