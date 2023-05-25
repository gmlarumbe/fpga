;;; fpga-synopsys.el --- FPGA & ASIC Utils for Synopsys  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;; Compilation-re
(defvar fpga-lattice-compile-re-synplify
  '((synp-error     "^@\\(?1:E\\): \\(?2:[A-Z0-9]+\\) :\"\\(?3:[0-9a-zA-Z./_-]+\\)\":\\(?4:[0-9]+\\):\\(?5:[0-9]+\\):" 3 4 5 2 nil (1 compilation-error-face) (2 fpga-lattice-compile-gray-face))
    (synp-error2    "^@\\(?1:E\\): \\(?2:[A-Z0-9]+\\) [:]?|" 1 nil nil 2 nil (2 fpga-lattice-compile-gray-face))
    (synp-error3    "^@\\(?1:E\\):" 1 nil nil 2 nil)
    (synp-warning   "^@\\(?1:W\\): \\(?2:[A-Z0-9]+\\) :\"\\(?3:[0-9a-zA-Z./_-]+\\)\":\\(?4:[0-9]+\\):\\(?5:[0-9]+\\):" 3 4 5 1 nil (1 compilation-warning-face) (2 fpga-lattice-compile-gray-face))
    (synp-warning2  "^@\\(?1:W\\): \\(?2:[A-Z0-9]+\\) [:]?|" 1 nil nil 1 nil (2 fpga-lattice-compile-gray-face))
    (synp-warning3  "^@\\(?1:W\\):" 1 nil nil 1 nil)
    (synp-note      "^@\\(?1:N\\): \\(?2:[A-Z0-9]+\\) :\"\\(?3:[0-9a-zA-Z./_-]+\\)\":\\(?4:[0-9]+\\):\\(?5:[0-9]+\\):" 3 4 5 0 nil (1 compilation-info-face) (2 fpga-lattice-compile-gray-face))
    (synp-note2     "^@\\(?1:N\\): \\(?2:[A-Z0-9]+\\) [:]?|" 1 nil nil 0 nil (2 fpga-lattice-compile-gray-face))
    ;; Did not find what those meant online, so set as warnings
    (synp-alt-info  "^@\\(?1:A\\): \\(?2:[A-Z0-9]+\\) :\"\\(?3:[0-9a-zA-Z./_-]+\\)\":\\(?4:[0-9]+\\):\\(?5:[0-9]+\\):" 3 4 5 0 nil (1 compilation-info-face) (2 fpga-lattice-compile-gray-face))
    (synp-alt-info2 "^@\\(?1:A\\): \\(?2:[A-Z0-9]+\\) [:]?|" 1 nil nil 0 nil (2 fpga-lattice-compile-gray-face))
    (synp-alt-info3 "^@\\(?1:A\\):" 1 nil nil 0 nil)
    (synp-note3     "^@\\(?1:N\\):" 1 nil nil 0 nil)
    (synp-info      "^@\\(?1:I\\):" nil nil nil 0 nil (1 compilation-line-face))
    (synp-log       "^@\\(?1:L\\):" nil nil nil 0 nil (1 compilation-line-face)))
  "Synopsys Synplify regexps.")


(defvar fpga-misc-compile-re-design-compiler
  '((dc-error     "\\(?1:^Error\\):  \\(?2:[0-9a-zA-Z./_-]+\\):\\(?3:[0-9]+\\): "       2 3   nil 2 nil (1 compilation-error-face))
    (dc-error-2   "\\(?1:^Error\\): .*"                                                 1 nil nil 2 nil)
    (dc-warning   "\\(?1:^Warning\\):  \\(?2:[0-9a-zA-Z./_-]+\\):\\(?3:[0-9]+\\): "     2 3   nil 1 nil (1 compilation-warning-face))
    (dc-warning-2 "\\(?1:^Warning\\): .*"                                               1 nil nil 1 nil)
    (dc-info      "\\(?1:^Information\\):  \\(?2:[0-9a-zA-Z./_-]+\\):\\(?3:[0-9]+\\): " 2 3   nil 0 nil (1 compilation-info-face))
    (dc-info-2    "\\(?1:^Information\\): .*"                                           1 nil nil 0 nil))
  "Synopsys Design Compiler regexps.")


;;;; Synplify-TCL Shell
(defvar fpga-lattice-synplify-shell-bin (executable-find "synplify_premier"))
(defvar fpga-lattice-synplify-shell-cmd-switches
  '("-shell"
    "-licensetype synplifypremierdp"
    "-verbose_log"))
(defvar fpga-lattice-synplify-shell-buffer "*synplify-tcl*")


;;;###autoload
(defun larumbe/synplify-shell ()
  "Invoke a TCL Synplify shell with the proper regexps, suited for compilation."
  (interactive)
  (unless fpga-lattice-synplify-shell-bin
    (error "Could not find synplify in $PATH.  Add it or set `fpga-lattice-synplify-shell-bin'"))
  (let ((command (concat fpga-lattice-synplify-shell-bin " " (mapconcat #'identity fpga-lattice-synplify-shell-cmd-switches " ")))
        (bufname fpga-lattice-synplify-shell-buffer)
        (parser  "synplify"))
    (larumbe/compilation-interactive command bufname parser)
    (fpga-lattice-synplify-shell-completion-at-point-mode 1)
    (company-mode 1)))


(defun fpga-lattice-synplify-shell-tcl-send-line-or-region-and-step ()
  "Send the current line to the inferior shell and step to the next line.
When the region is active, send the region instead."
  (interactive)
  (let (from to end (proc (get-buffer-process fpga-lattice-synplify-shell-buffer)))
    (if (use-region-p)
        (setq from (region-beginning)
              to (region-end)
              end to)
      (setq from (line-beginning-position)
            to (line-end-position)
            end (1+ to)))
    (comint-send-string proc (buffer-substring-no-properties from to))
    (comint-send-string proc "\n")
    (goto-char end)))



(provide 'fpga-synopsys)

;;; fpga-synopsys.el ends here
