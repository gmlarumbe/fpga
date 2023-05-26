;;; fpga-altera.el --- FPGA Altera Utils  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Gonzalo Larumbe

;; Author: Gonzalo Larumbe <gonzalomlarumbe@gmail.com>
;; URL: https://github.com/gmlarumbe/fpga
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; FPGA Utilities for Altera Quartus:
;;  - Automatic tags creation from project QSF file
;;  - ...

;;; Code:


(require 'fpga-utils)


;;;; Custom
(defgroup fpga-altera nil
  "FPGA Altera customization."
  :group 'fpga)

(defcustom fpga-altera-quartus-bin (executable-find "quartus_sh")
  "Path to Quartus executable."
  :type 'string
  :group 'fpga-altera)

(defcustom fpga-altera-quartus-cmd-opts '("-s")
  "Quartus process options."
  :type '(repeat string)
  :group 'fpga-altera)

(defcustom fpga-altera-quartus-buf "*quartus*"
  "Buffer to use for Quartus compilation process."
  :type 'string
  :group 'fpga-altera)

(defcustom fpga-altera-quartus-shell-buf "*quartus-shell*"
  "Buffer to use for Quartus interactive shell process."
  :type 'string
  :group 'fpga-altera)

;; TODO
(defcustom fpga-altera-quartus-syn-script
  '("synth_design -rtl"
    "synth_design"
    "exit")
  "Quartus script to be run for synthesis.
Each string of the list corresponds to one statement of the TCL input file."
  :type '(repeat string)
  :group 'fpga-altera)


;;;; Internal
(defconst fpga-altera-quartus--base-cmd
  (concat fpga-altera-quartus-bin " " (mapconcat #'identity fpga-altera-quartus-cmd-opts " ")))


;;;; Compilation
;; TODO: Intel/Altera
;; (defconst fpga-altera-quartus-compile-re
;;   '())


;;;; Tags
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
              (dolist (file (directory-files file-name t fpga-utils-source-extension-re))
                (push file file-list))
            (display-warning :warning (format "Error processing qsf: %s\nFile \"%s\" set as SEARCH_PATH is not an existing directory!" qsf-file file-name))))))
    (delete-dups (nreverse file-list))))

(defun fpga-altera-quartus-tags (out-dir qsf-file)
  "Generate tags in OUT-DIR from data in QSF-FILE."
  (interactive "DOutput dir: \nFQSF file: ")
  (fpga-utils-tags-create out-dir qsf-file #'fpga-altera-quartus-files-from-qsf))


;;;; Synthesis


;;;; Simulation


;;;; Shell





(provide 'fpga-altera)

;;; fpga-altera.el ends here
