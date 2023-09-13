;;; fpga-yosys.el --- FPGA Yosys Utils  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Gonzalo Larumbe

;; Author: Gonzalo Larumbe <gonzalomlarumbe@gmail.com>
;; URL: https://github.com/gmlarumbe/fpga

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

;; FPGA Utilities for Yosys:
;;  - Synthesis compilation with error regexp matching
;;  - Improved Yosys shell with syntax highlighting and autocompletion
;;  - Yosys Script mode with syntax highlighting and autocompletion

;;; Code:


(require 'fpga-utils)

;;;; Custom
(defgroup fpga-yosys nil
  "FPGA Yosys customization."
  :group 'fpga)

(defcustom fpga-yosys-bin (executable-find "yosys")
  "Path to Yosys executable."
  :type 'string
  :group 'fpga-yosys)

(defcustom fpga-yosys-cmd-opts nil
  "Yosys process options."
  :type '(repeat string)
  :group 'fpga-yosys)

(defcustom fpga-yosys-buf "*yosys*"
  "Buffer to use for Yosys compilation process."
  :type 'string
  :group 'fpga-yosys)

(defcustom fpga-yosys-syn-script
  '("read -sv my_top.sv"
    "hierarchy -top my_top"
    "proc"
    "opt"
    "techmap"
    "opt"
    "write_verilog netlist.v")
  "Yosys script to be run for synthesis.

Each string of the list corresponds to one statement, each separated with ;.

Default value shows an example of a Yosys script that reads a top module named
\"my_top\" from file \"my_top.sv\". Tweak accordingly for your needs."
  :type '(repeat string)
  :group 'fpga-yosys)

(defcustom fpga-yosys-shell-buf "*yosys-shell*"
  "Buffer to use for Yosys interactive shell process."
  :type 'string
  :group 'fpga-yosys)

(defcustom fpga-yosys-sby-bin (executable-find "sby")
  "Path to sby executable."
  :type '(repeat string)
  :group 'fpga-yosys)


;;;; Internal
(defconst fpga-yosys--base-cmd
  (let ((cmd-opts (mapconcat #'identity fpga-yosys-cmd-opts " ")))
    (concat fpga-yosys-bin (unless (string-empty-p cmd-opts)
                             " " cmd-opts))))


;;;; Compilation
(defconst fpga-yosys-compile-re
  '(;; INFO: Surelog, Vivado, Intel and Lattice regexps could be added depending on used frontends and backends
    (yosys-step-num "^\\(?1:[0-9\.]+ \\)" nil nil nil 0 nil (1 fpga-utils-compilation-bin-face))
    (read-sv-error  "^\\(?1:[^ \t,]+\\):\\(?2:[0-9]+\\): \\(?3:ERROR\\): " 1 2 nil 2 nil (3 compilation-error-face)))
  "Yosys regexps.")

(fpga-utils-define-compilation-mode fpga-yosys-compilation-mode
  :desc "Yosys"
  :docstring "Yosys Compilation mode."
  :compile-re fpga-yosys-compile-re
  :buf-name fpga-yosys-buf)

;;;###autoload (autoload 'fpga-yosys-compile "fpga.el")
(fpga-utils-define-compile-fn fpga-yosys-compile
  :docstring "Compile Yosys COMMAND with error regexp highlighting."
  :buf fpga-yosys-buf
  :comp-mode fpga-yosys-compilation-mode)


;;;; Synthesis
;;;###autoload (autoload 'fpga-yosys-syn "fpga.el" nil t)
(defun fpga-yosys-syn (&optional output-dir)
  "Run Yosys script from commands in `fpga-yosys-syn-script'.

If optional OUTPUT-DIR is non nil, use it as the default directory for
compilation.  Otherwise use `default-directory'."
  (interactive "DOutput Dir:")
  (when output-dir
    (unless (file-directory-p output-dir)
      (make-directory output-dir :parents)))
  (unless fpga-yosys-bin
    (error "Binary yosys not found in the $PATH"))
  (let* ((compilation-dir (or output-dir default-directory))
         (cmd (concat "cd " compilation-dir " && " fpga-yosys--base-cmd " -p "
                      "\'" (mapconcat #'identity fpga-yosys-syn-script "; ") "\'")))
    (fpga-yosys-compile cmd)))

;;;###autoload (autoload 'fpga-yosys-syn-from-file "fpga.el" nil t)
(defun fpga-yosys-syn-from-file (file &optional output-dir)
  "Open Yosys script from FILE and run its commands.

If optional OUTPUT-DIR is non nil, use it as the default directory for
compilation.  Otherwise use `default-directory'."
  (interactive "FScript File: \nDOutput Dir: ")
  (when output-dir
    (unless (file-directory-p output-dir)
      (make-directory output-dir :parents)))
  (unless fpga-yosys-bin
    (error "Binary yosys not found in the $PATH"))
  (let* ((compilation-dir (or output-dir default-directory))
         (cmd (concat "cd " compilation-dir " && " fpga-yosys--base-cmd " " file)))
    (fpga-yosys-compile cmd)))


;;;; Yosys shell
(defconst fpga-yosys-shell-commands
  '("abc" "abc9" "abc9_exe" "abc9_ops" "add" "aigmap" "alumacc" "anlogic_eqn"
    "anlogic_fixcarry" "assertpmux" "async2sync" "attrmap" "attrmvcp" "autoname"
    "blackbox" "bmuxmap" "bugpoint" "bwmuxmap" "cd" "check" "chformal" "chparam"
    "chtype" "clean" "clean_zerowidth" "clk2fflogic" "clkbufmap" "connect"
    "connect_rpc" "connwrappers" "coolrunner2_fixup" "coolrunner2_sop" "copy"
    "cover" "cutpoint" "debug" "delete" "deminout" "demuxmap" "design" "dffinit"
    "dfflegalize" "dfflibmap" "dffunmap" "dump" "echo" "ecp5_gsr" "edgetypes"
    "efinix_fixcarry" "equiv_add" "equiv_induct" "equiv_make" "equiv_mark"
    "equiv_miter" "equiv_opt" "equiv_purge" "equiv_remove" "equiv_simple"
    "equiv_status" "equiv_struct" "eval" "exec" "expose" "extract"
    "extract_counter" "extract_fa" "extract_reduce" "extractinv" "flatten"
    "flowmap" "fmcombine" "fminit" "formalff" "freduce" "fsm" "fsm_detect"
    "fsm_expand" "fsm_export" "fsm_extract" "fsm_info" "fsm_map" "fsm_opt"
    "fsm_recode" "fst2tb" "gatemate_foldinv" "glift" "greenpak4_dffinv" "help"
    "hierarchy" "hilomap" "history" "ice40_braminit" "ice40_dsp" "ice40_opt"
    "ice40_wrapcarry" "insbuf" "iopadmap" "jny" "json" "lattice_gsr" "log"
    "logger" "ls" "ltp" "lut2mux" "maccmap" "memory" "memory_bmux2rom"
    "memory_bram" "memory_collect" "memory_dff" "memory_libmap" "memory_map"
    "memory_memx" "memory_narrow" "memory_nordff" "memory_share" "memory_unpack"
    "miter" "mutate" "muxcover" "muxpack" "nlutmap" "onehot" "opt" "opt_clean"
    "opt_demorgan" "opt_dff" "opt_expr" "opt_ffinv" "opt_lut" "opt_lut_ins"
    "opt_mem" "opt_mem_feedback" "opt_mem_priority" "opt_mem_widen" "opt_merge"
    "opt_muxtree" "opt_reduce" "opt_share" "paramap" "peepopt" "plugin"
    "pmux2shiftx" "pmuxtree" "portlist" "prep" "printattrs" "proc" "proc_arst"
    "proc_clean" "proc_dff" "proc_dlatch" "proc_init" "proc_memwr" "proc_mux"
    "proc_prune" "proc_rmdead" "proc_rom" "qbfsat" "qwp" "read" "read_aiger"
    "read_blif" "read_ilang" "read_json" "read_liberty" "read_rtlil"
    "read_verilog" "recover_names" "rename" "rmports" "sat" "scatter" "scc"
    "scratchpad" "script" "select" "setattr" "setparam" "setundef" "share"
    "shell" "show" "shregmap" "sim" "simplemap" "splice" "splitcells"
    "splitnets" "sta" "stat" "submod" "supercover" "synth" "synth_achronix"
    "synth_anlogic" "synth_coolrunner2" "synth_easic" "synth_ecp5"
    "synth_efinix" "synth_fabulous" "synth_gatemate" "synth_gowin"
    "synth_greenpak4" "synth_ice40" "synth_intel" "synth_intel_alm" "synth_machxo2"
    "synth_lattice" "synth_nexus" "synth_quicklogic" "synth_sf2" "synth_xilinx"
    "synthprop" "tcl" "techmap" "tee" "test_abcloop" "test_autotb" "test_cell"
    "test_pmgen" "torder" "trace" "tribuf" "uniquify" "verific"
    "verilog_defaults" "verilog_defines" "viz" "wbflip" "wreduce" "write_aiger"
    "write_blif" "write_btor" "write_cxxrtl" "write_edif" "write_file"
    "write_firrtl" "write_ilang" "write_intersynth" "write_jny" "write_json"
    "write_rtlil" "write_simplec" "write_smt2" "write_smv" "write_spice"
    "write_table" "write_verilog" "write_xaiger" "xilinx_dffopt" "xilinx_dsp"
    "xilinx_srl" "xprop" "zinit"))

(defconst fpga-yosys-shell-commands-re
  (regexp-opt fpga-yosys-shell-commands 'symbols))

(defconst fpga-yosys-punctuation-re "[;]")

(defconst fpga-yosys-shell-font-lock
  (append `((,fpga-yosys-shell-commands-re 0 font-lock-keyword-face)
            (,fpga-utils-shell-switch-re (1 fpga-utils-compilation-msg-code-face) (2 font-lock-constant-face))
            (,fpga-yosys-punctuation-re 0 fpga-utils-punctuation-face))))

;;;###autoload (autoload 'fpga-yosys-shell "fpga.el" nil t)
(fpga-utils-define-shell-mode fpga-yosys-shell
  :bin fpga-yosys-bin
  :base-cmd fpga-yosys--base-cmd
  :shell-commands fpga-yosys-shell-commands
  :compile-re fpga-yosys-compile-re
  :buf fpga-yosys-shell-buf
  :font-lock-kwds fpga-yosys-shell-font-lock)


;;;; Yosys script mode
(defun fpga-yosys-ys-capf ()
  "Yosys YS completion at point."
  (let* ((b (save-excursion (skip-chars-backward "a-zA-Z0-9_-") (point)))
         (e (point))
         (allcomp `(,@fpga-yosys-shell-commands)))
    `(,b ,e ,allcomp)))


(defconst fpga-yosys-ys-font-lock-defaults
  `(((,fpga-yosys-shell-commands-re . font-lock-keyword-face)
     (,fpga-yosys-punctuation-re . fpga-utils-punctuation-face)
     (,fpga-utils-shell-switch-re . ((1 fpga-utils-compilation-msg-code-face) (2 font-lock-constant-face)))))
  "Font lock defaults for `fpga-yosys-ys-mode'.")

(defconst fpga-yosys-ys-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">#" table)
    table)
  "Syntax table used in Yosys Script buffers.")

(defvar fpga-yosys-ys-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-p") 'fpga-yosys-shell-send-line-or-region-and-step)
    map)
  "Keymap for `fpga-yosys-ys-mode'.")


;;;###autoload (autoload 'fpga-yosys-ys-mode "fpga.el")
(define-derived-mode fpga-yosys-ys-mode prog-mode "YS"
  (setq-local font-lock-defaults fpga-yosys-ys-font-lock-defaults)
  (setq-local completion-at-point-functions #'fpga-yosys-ys-capf)
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+[\t ]*"))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.ys\\'" 'fpga-yosys-ys-mode))



(provide 'fpga-yosys)

;;; fpga-yosys.el ends here
