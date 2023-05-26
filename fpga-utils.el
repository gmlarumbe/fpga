;;; fpga-utils.el --- FPGA & ASIC Utils  -*- lexical-binding: t -*-

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

;; FPGA/ASIC Common Utils

;;; Code:

(require 'compile)
(require 'ggtags)

;;;; Custom
(defcustom fpga-utils-source-extension-re (concat "\\." (regexp-opt '("sv" "svh" "v" "vh" "vhd" "vhdl")) "$")
  "FPGA source file extension regexp."
  :type 'string
  :group 'fpga)

(defcustom fpga-utils-tags-creation-fn #'ggtags-create-tags
  "Function to use to create tags."
  :type 'function
  :group 'fpga)


;;;; Faces
(defconst fpga-utils-compilation-msg-code-face 'fpga-utils-compilation-msg-code-face)
(defface fpga-utils-compilation-msg-code-face
  '((t (:foreground "gray55")))
  "Face for compilation message codes."
  :group 'fpga)

(defconst fpga-utils-compilation-bin-face 'fpga-utils-compilation-bin-face)
(defface fpga-utils-compilation-bin-face
  '((t (:foreground "goldenrod")))
  "Face for compilation binaries."
  :group 'fpga)


;;;; Functions
(defun fpga-utils-write-file-from-filelist (outfile filelist)
  "Create OUTFILE with one file of FILELIST per line."
  (with-temp-file outfile
    (dolist (line filelist)
      (insert (concat line "\n")))))

(defun fpga-utils-tags-create (out-dir in-file file-list-fn)
  "Generate tags from filelist.

Tags will be generated in OUT-DIR from the project file of IN-FILE (xpr/qsf).

Third parameter FILE-LIST-FN is the used function to create gtags.files from
IN-FILE."
  (interactive "DOutput dir: \nFInput file: ")
  (let* ((gtags-file-name "gtags.files")
         (gtags-file-path (file-name-concat out-dir gtags-file-name)))
    (fpga-utils-write-file-from-filelist gtags-file-path (funcall file-list-fn in-file))
    (funcall fpga-utils-tags-creation-fn out-dir)))

(defun fpga-utils-shell-delchar-or-maybe-eof (num-chars)
  "Delete character or exit shell.
With `prefix-arg', delete NUM-CHARS characters."
  (interactive "p")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and (eobp)
             (save-excursion
               (skip-chars-backward " ")
               (eq (preceding-char) ?%)))
        (comint-send-string proc "exit\n")
      (delete-char num-chars))))


;;;; Compilation-re
(defvar fpga-utils-compilation-uvm-re
  '((uvm-fatal    "^\\(?1:UVM_FATAL\\) \\(?2:[a-zA-Z0-9\./_-]+\\)(\\(?3:[0-9]+\\))"   2 3 nil 2 nil (1 compilation-error-face))
    (uvm-fatal2   "^\\(?1:UVM_FATAL\\) @"   1 nil nil 2 nil)
    (uvm-error    "^\\(?1:UVM_ERROR\\) \\(?2:[a-zA-Z0-9\./_-]+\\)(\\(?3:[0-9]+\\))"   2 3 nil 2 nil (1 compilation-error-face))
    (uvm-error2   "^\\(?1:UVM_ERROR\\) @"   1 nil nil 2 nil)
    (uvm-warning  "^\\(?1:UVM_WARNING\\) \\(?2:[a-zA-Z0-9\./_-]+\\)(\\(?3:[0-9]+\\))" 2 3 nil 1 nil (1 compilation-warning-face))
    (uvm-warning2 "^\\(?1:UVM_WARNING\\) @" 1 nil nil 1 nil)
    (uvm-info     "^\\(?1:UVM_INFO\\) \\(?2:[a-zA-Z0-9\./_-]+\\)(\\(?3:[0-9]+\\))"    2 3 nil 0 nil (1 compilation-info-face))
    (uvm-info2    "^\\(?1:UVM_INFO\\) @"    1 nil nil 0 nil)))

(defvar fpga-utils-compilation-ovm-re
  '((ovm-fatal    "^\\(?1:OVM_FATAL\\) @ \\(?2:[0-9]+\\): "   1 nil nil 2 nil (2 compilation-line-face))
    (ovm-error    "^\\(?1:OVM_ERROR\\) @ \\(?2:[0-9]+\\): "   1 nil nil 2 nil (2 compilation-line-face))
    (ovm-warning  "^\\(?1:OVM_WARNING\\) @ \\(?2:[0-9]+\\): " 1 nil nil 1 nil (2 compilation-line-face))
    (ovm-info     "^\\(?1:OVM_INFO\\) @ \\(?2:[0-9]+\\): "    1 nil nil 0 nil (2 compilation-line-face))))


(provide 'fpga-utils)

;;; fpga-utils.el ends here
