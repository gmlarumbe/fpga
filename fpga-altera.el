;;; fpga-altera.el --- FPGA Altera Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'fpga-utils)


(defun fpga-altera-quartus-files-from-qsf (qsf-file)
  "Get filelist from Quartus QSF-FILE project file."
  (let ((qsf-dir (file-name-directory qsf-file))
        (file-re "set_global_assignment -name \\(?1:[A-Z_]+_FILE\\|SEARCH_PATH\\) \\(?2:[$_/\\.a-zA-Z0-9]+\\)")
        match type file-name file-list)
    (unless (string= (file-name-extension qsf-file) "qsf")
      (user-error "Not a qsf file!"))
    (with-temp-buffer
      (insert-file-contents qsf-file)
      (goto-char (point-min))
      (while (re-search-forward file-re nil :no-error)
        (setq type (match-string-no-properties 1))
        (setq match (match-string-no-properties 2))
        (setq file-name (expand-file-name match qsf-dir))
        (if (not (string= type "SEARCH_PATH"))
            (push file-name file-list)
          ;; Else include directory
          (if (and (file-exists-p file-name)
                   (file-directory-p file-name))
              ;; push individually instead of append whole list to keep reverse order, before final un-reversal
              (dolist (file (directory-files file-name t fpga-source-extension-re))
                (push file file-list))
            (display-warning :warning (format "Error processing qsf: %s\nFile \"%s\" set as SEARCH_PATH is not an existing directory!" qsf-file file-name))))))
    (delete-dups (nreverse file-list))))

(defun fpga-altera-tags-from-qsf (out-dir qsf-file)
  "Generate tags in OUT-DIR from data in QSF-FILE."
  (interactive "DOutput dir: \nFQSF file: ")
  (fpga-tags-create out-dir qsf-file #'fpga-altera-quartus-files-from-qsf))


;;;; Compilation-re
;; TODO: Intel/Altera
;; (defvar verilog-ext-compile-re-quartus
;;   '())






(provide 'fpga-altera)

;;; fpga-altera.el ends here
