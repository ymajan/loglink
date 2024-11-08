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
;; Prerequisites: ((emacs "29.1") (org-roam "2.0") (f "0.20.0"))

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

;; useful when you FUCK up
(defun loglink-remove-titles-in-directory (directory)
  "Remove titles from all pages in the specified DIRECTORY."
  (interactive "DDirectory: ")
  (let ((files (directory-files directory t "\\.org$")))
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (when (re-search-forward "^#\\+TITLE:.*\n" nil t)
          (replace-match "")
          (save-buffer))
        (kill-buffer)))))

;; Your logseq directory should be inside your org-roam directory,
;; put the directory you use here
(defvar loglink-logseq-folder org-roam-directory)

;; You probably don't need to change these values
(defvar loglink-logseq-pages (f-expand (f-join loglink-logseq-folder "pages")))
(defvar loglink-logseq-journals (f-expand (f-join loglink-logseq-folder "journals")))
;; ignore files matching loglink-logseq-exclude-pattern
;; default: exclude all files in the logseq/bak/ folder
(defvar loglink-logseq-exclude-pattern (string-join (list "^" (file-truename loglink-logseq-folder) "/logseq/bak/.*$")))

(defun loglink-logseq-journal-p (file)
  "Return non-nil if FILE is a Logseq journal file.
Checks if FILE path starts with the logseq-journals directory path.
FILE should be a string representing the full file path."
  (string-match-p (concat "^" loglink-logseq-journals) file))

(defun loglink-ensure-file-id (file)
  "Ensure FILE has an ID and title in Org-roam format.
Visit FILE, create an ID if missing, and ensure proper title formatting.
If no title exists, use the filename as title.

FILE should be an absolute or relative path to an existing org file.

Return a cons cell (NEW-BUF . BUFFER) where:
- NEW-BUF is t if a new buffer was created, nil otherwise
- BUFFER is the buffer containing the file

Side effects:
- Creates or modifies buffer for FILE
- May modify FILE content (ID, title, formatting)
- May save FILE if changes were made"
  (let* ((abs-file (f-expand file))
         (buf (get-file-buffer abs-file))
         (new-buf nil)
         (changed nil))
    (unless (f-exists-p abs-file)
      (error "File does not exist: %s" abs-file))

    ;; Buffer handling
    (unless buf
      (setq buf (find-file-noselect abs-file))
      (setq new-buf t))

    (with-current-buffer buf
      (let ((org (org-element-parse-buffer))
            sec-end)
        ;; Ensure file has an ID
        (goto-char (point-min))
        (unless (and (eq 'section (org-element-type (nth 2 org)))
                     (org-roam-id-at-point))
          (setq changed t)
          ;; Add section if needed
          (when (eq 'headline (org-element-type (nth 2 org)))
            (insert "\n")
            (goto-char (point-min)))
          (org-id-get-create)
          (setq org (org-element-parse-buffer)))

        ;; Ensure file has a title
        (when (nth 3 org)
          (unless (org-collect-keywords ["title"])
            (setq changed t)
            ;; Ensure proper spacing
            (setq sec-end (org-element-property :end (nth 2 org)))
            (goto-char (1- sec-end))
            (unless (equal "\n\n" (buffer-substring-no-properties
                                   (- sec-end 2)
                                   sec-end))
              (insert "\n")
              (goto-char (1- (point))))

            ;; Add title
            (let ((title (file-name-sans-extension
                          (file-name-nondirectory abs-file))))
              (insert (format "#+title: %s" title)))))

        ;; Save if modified
        (when changed
          (save-buffer))))

    (cons new-buf buf)))

(defun loglink-convert-logseq-file (buf)
  "convert fuzzy and file:../pages logseq links in the file to id links"
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
          (message "Convering logseq file %s link from %s to %s" (buffer-file-name buf) (org-element-property :raw-link link) newlink)))
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
  (and (not (string-match-p loglink-logseq-exclude-pattern (file-truename file-path)))
       (let ((content-hash (org-roam-db--file-hash file-path))
             (db-hash (caar (org-roam-db-query [:select hash :from files
                                                :where (= file $s1)] file-path))))
         (not (string= content-hash db-hash)))))

(defun loglink-modified-logseq-files ()
  (emacsql-with-transaction (org-roam-db)
    (seq-filter 'loglink-roam-file-modified-p
                (org-roam--list-files loglink-logseq-folder))))

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
    ;; Make sure all the files have file ids
    (dolist (file-path files)
      (setq file-path (f-expand file-path))
      (setq cur (loglink-ensure-file-id file-path))
      (setq buf (cdr cur))
      (push buf bufs)
      ;; Remove the condition that excludes journal files from the 'bad' list
      (when (not buf)
        (push file-path bad))
      (when (not (buffer-modified-p buf))
        (push buf unmodified))
      (when (car cur)
        (push buf created)))
    ;; Patch fuzzy links
    (mapc 'loglink-convert-logseq-file
          (seq-filter 'identity bufs))
    (dolist (buf unmodified)
      (when (buffer-modified-p buf)
        (save-buffer buf)))
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

(add-hook 'window-setup-hook #'loglink-check-logseq)


(provide 'loglink)

;;; loglink.el ends here
