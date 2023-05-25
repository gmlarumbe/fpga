;;; fpga-utils.el --- FPGA & ASIC Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun fpga-utils-write-file-from-filelist (outfile filelist)
  "Create OUTFILE from FILELIST.
OUTFILE is the output file.
FILELIST is a list of files."
  (with-temp-file outfile
    (dolist (line filelist)
      (insert (concat line "\n")))))

(defun fpga-tags-create (out-dir in-file file-list-fn)
  "Generate tags in OUT-DIR from data in IN-FILE.
Call FILE-LIST-FN to gather file list from IN-FILE."
  (interactive "DOutput dir: \nFInput file: ")
  (let* ((gtags-file-name "gtags.files")
         (gtags-file-path (file-name-concat out-dir gtags-file-name)))
    (fpga-utils-write-file-from-filelist gtags-file-path (funcall file-list-fn in-file))
    (funcall fpga-tags-creation-fn out-dir)))



(provide 'fpga-utils)

;;; fpga-utils.el ends here
