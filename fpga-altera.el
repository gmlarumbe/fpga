;;; fpga-altera.el --- FPGA Altera Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;; Quartus tags
;; Projects list for the `larumbe/fpga-tags-altera-list' variables:
;; Name of the project (+plus)
;; 1) Path of the altera dir (without name)
;; 2) Name of the tcl file used to get the file list (files_and_libraries.tcl)
;; 3) Path where GTAGS file will be created
;; 4) Name of the file that will be read by global to generate GTAGS (e.g. gtags.files)
(defvar fpga-altera-tags-list                 nil)
(defvar fpga-altera-tags-tcl-dir              nil)
(defvar fpga-altera-tags-tcl-file             nil)
(defvar fpga-altera-tags-gtags-dirs-directory nil)
(defvar fpga-altera-tags-gtags-dirs-file      nil)

(defvar fpga-altera-tags-tcl-file-regexp "\\(.*_FILE\\|SEARCH_PATH\\) ")
(defvar fpga-altera-tags-tcl-file-regexp-file "\\(.*_FILE\\) ")
(defvar fpga-altera-tags-tcl-file-regexp-dir "\\(.*SEARCH_PATH\\) ")

;; Functions and variables for directory expansion (retrieve files from a dir on each line for gtags processing)
(defvar fpga-altera-tags-tcl-env-archons-path  nil)
(defvar fpga-altera-tags-tcl-env-archons-regex nil)
;; Output of `echo $ARCHONS_PATH' at LFP CEE obelix environment

(defun fpga-altera-tags-append-files-from-dir (dir)
  "Append list of files from DIR to FILE.
Used on `tempfile' from `files_and_libraries.tcl' to expand directories
Global needs the file name, hence this function"
  (save-excursion
    (mapcar
     (lambda (x)
       (goto-char (point-max))
       (insert (concat x "\n")))
     (directory-files dir t))))


(defun fpga-altera-tags-find-repeated-included-files ()
  "Find repeated files in current buffer (meant for gtags.files).
There are duplicates in `fpga-altera-tags-append-files-from-dir' if files and
dirs are included.  This function checks if there is a repeated file in
gtags.files for GTAGS not to have a duplicate tag.
Checks Works in current buffer."
  (let ((file-to-check))
    (goto-char (point-min))
    (while (< (point) (point-max))
      (save-excursion
        (setq file-to-check (concat (file-name-base (thing-at-point 'filename)) "." (file-name-extension (thing-at-point 'filename))))
        (move-end-of-line 1)
        (while (re-search-forward (concat file-to-check "$") nil t) ; If file is included more than once we keep only the first one
          (beginning-of-line)
          (kill-line 1)))
      (forward-line))))


(defun fpga-altera-tags-create-file-list ()
  "Create `gtags.files' from altera project tcl file."
  (save-window-excursion
    (with-temp-buffer
      ;; INFO: Debugging with-temp-buffer:
      ;; (view-buffer-other-window (current-buffer))      ; Option A: preferred (not valid since temp buffer cannot be modified)
      ;; (clone-indirect-buffer-other-window "*debug*" t) ; Option B: used here (however, cannot save temp buffer while debugging)
      ;; End of INFO
      (insert-file-contents (file-name-concat fpga-altera-tags-tcl-dir fpga-altera-tags-tcl-file))
      ;; Start Regexp replacement for file
      (keep-lines fpga-altera-tags-tcl-file-regexp (point-min) (point-max)) ; Get only files
      (goto-char (point-min))
      (while (re-search-forward "^#" nil t)   ; Remove comments
        (beginning-of-line)
        (kill-line 1))
      ;; Replace files
      (larumbe/replace-regexp-whole-buffer
       (concat "set_global_assignment -name " fpga-altera-tags-tcl-file-regexp-file)
       (concat (file-name-as-directory fpga-altera-tags-tcl-dir)))
      ;; Replace SEARCH_PATH dirs
      (goto-char (point-min))
      (while (re-search-forward fpga-altera-tags-tcl-file-regexp-dir nil t)
        (kill-line 0) ; Kill until the beginning of line
        (insert (file-name-as-directory fpga-altera-tags-tcl-dir))
        (fpga-altera-tags-append-files-from-dir (thing-at-point 'filename)))
      ;; Replace $env(ARCHONS_PATH) dirs
      (goto-char (point-min))
      (while (re-search-forward fpga-altera-tags-tcl-env-archons-regex nil t)
        (kill-line 0) ; Kill until the beginning of line
        (insert fpga-altera-tags-tcl-env-archons-path))
      ;; Cleanup file
      (larumbe/replace-regexp-whole-buffer " +" "")  ; Delete whitespaces in PATHs
      (goto-char (point-min))
      (while (re-search-forward "\\.$" nil t) ; Remove search paths with previous or current dir
        (beginning-of-line)                  ; Equivalent to `flush-lines' but
        (kill-line 1))                       ; for non-interactive use
      (fpga-altera-tags-find-repeated-included-files) ; Remove repeated files (due to previous directory expansion)
      ;; Make sure expansion is made relative to SVN sandbox path (same as gtags.file path)
      (larumbe/buffer-expand-filenames nil fpga-altera-tags-gtags-dirs-directory)
      (write-file (file-name-concat fpga-altera-tags-gtags-dirs-directory fpga-altera-tags-gtags-dirs-file)))))


(defun fpga-altera-tags-set-active-project ()
  "Retrieve project list and set variables accordingly.
Copied from `larumbe/fpga-tags-vivado-set-active-xpr' for Vivado xpr."
  (let ((project)
        (files-list))
    ;; Get Project name
    (setq project (completing-read "Select project: " (mapcar 'car fpga-altera-tags-list))) ;; Read previous variable and get list of first element of each assoc list
    (setq files-list (cdr (assoc project fpga-altera-tags-list)))
    ;; Set parameters accordingly
    (setq fpga-altera-tags-tcl-dir              (nth 0 files-list))
    (setq fpga-altera-tags-tcl-file             (nth 1 files-list))
    (setq fpga-altera-tags-gtags-dirs-directory (nth 2 files-list))
    (setq fpga-altera-tags-gtags-dirs-file      (nth 3 files-list))))



;;;###autoload
(defun fpga-altera-tags ()
  "Create `gtags.files' file for a specific Altera project.
Based on a search from `files_and_libraries.tcl' file."
  (interactive)
  (fpga-altera-tags-set-active-project)
  (fpga-altera-tags-create-file-list)
  (larumbe/gtags-create-tags-async-process fpga-altera-tags-gtags-dirs-directory))


;;;; Compilation-re
;; TODO: Intel/Altera
;; (defvar verilog-ext-compile-re-quartus
;;   '())






(provide 'fpga-altera)

;;; fpga-altera.el ends here
