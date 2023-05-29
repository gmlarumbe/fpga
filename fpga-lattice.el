;;; fpga-lattice.el --- FPGA Lattice Utils  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Gonzalo Larumbe

;; Author: Gonzalo Larumbe <gonzalomlarumbe@gmail.com>
;; URL: https://github.com/gmlarumbe/fpga
;; Version: 0.1.0

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

;; FPGA Utilities for Lattice Diamond:
;;  - Compilation with error regexp matching
;;  - Improved Diamond shell with syntax highlighting and autocompletion
;;
;;; Code:

(require 'fpga-utils)


;;;; Custom
(defgroup fpga-lattice nil
  "FPGA Lattice customization."
  :group 'fpga)

(defcustom fpga-lattice-diamond-bin (executable-find "diamondc")
  "Path to Diamond executable."
  :type 'string
  :group 'fpga-lattice)

(defcustom fpga-lattice-diamond-cmd-opts nil
  "Diamond process options."
  :type '(repeat string)
  :group 'fpga-lattice)

(defcustom fpga-lattice-diamond-buf "*diamond*"
  "Buffer to use for Diamond compilation process."
  :type 'string
  :group 'fpga-lattice)

(defcustom fpga-lattice-diamond-shell-buf "*diamond-shell*"
  "Buffer to use for Diamond interactive shell process."
  :type 'string
  :group 'fpga-lattice)


;;;; Internal
(defconst fpga-lattice-diamond--base-cmd
  (concat fpga-lattice-diamond-bin " " (mapconcat #'identity fpga-lattice-diamond-cmd-opts " ")))


;;;; Compilation-re
(defconst fpga-lattice-diamond-compile-re
  '((lattice-error     "\\(?1:^ERROR\\) -" 1 nil nil 2 nil (1 compilation-error-face))
    (lattice-warning   "\\(?1:^WARNING\\) - \\(?2:[a-z0-9]+:\\) \\(?3:[a-zA-Z0-9\./_-]+\\)(\\(?4:[0-9]+\\)):" 3 4 nil 1 nil (1 compilation-warning-face) (2 fpga-utils-compilation-bin-face))
    (lattice-warning2  "\\(?1:^WARNING\\) - \\(?2:[a-z0-9]+:\\)" 1 nil nil 1 nil (1 compilation-warning-face) (2 fpga-utils-compilation-bin-face))
    (lattice-warning3  "\\(?1:^WARNING\\) -" 1 nil nil 1 nil (1 compilation-warning-face)))
  "Lattice Diamond regexps.")

(fpga-utils-define-compilation-mode fpga-lattice-diamond-compilation-mode
  :desc "Diamond"
  :docstring "Diamond Compilation mode."
  :compile-re fpga-lattice-diamond-compile-re
  :buf-name fpga-lattice-diamond-buf)

(fpga-utils-define-compile-fn fpga-lattice-diamond-compile
  :docstring "Compile Diamond COMMAND with error regexp highlighting."
  :buf fpga-lattice-diamond-buf
  :comp-mode fpga-lattice-diamond-compilation-mode)


;;;; Diamond Shell
;; Lattice Diamond User Guide Tcl Scripting section
(defvar fpga-lattice-diamond-shell-commands
  '(;; Help
    "help"
    ;; Lattice Diamond Tcl Console extended commands
    "history" "reset" "clear" "save_script" "set_prompt"
    ;; Project manager extended Tcl commands
    "prj_project" "prj_src" "prj_impl" "prj_strgy" "prj_run" "prj_syn" "prj_dev" "prj_incr"
    ;; Sys
    "sys_install"
    ;; Ncd extended Tcl commands
    "ncd_port" "ncd_inst" "ncd_net" "ncd_attr"
    ;; Ngd extended Tcl commands
    "ngd_port" "ngd_inst" "ngd_net" "ngd_attr"
    ;; Reveal Inserter extended Tcl commands
    "rvl_project" "rvl_core" "rvl_trace" "rvl_tu" "rvl_te"
    ;; Clarity Designer extended Tcl commands
    "sbp_design" "sbp_resource" "sbp_builder"
    ;; Reveal Analyzer extended Tcl commands
    "rva_trace" "rva_core" "rva_tu" "rva_te" "rva_tokenmgr" "rva_trigoptn" "rva_project" "rva_pcs"
    ;; Power Calculator extended Tcl commands
    "pwc_command" "pwc_device" "pwc_parameters" "pwc_thermal" "pwc_settings"
    "pwc_supply" "pwc_logicblocks" "pwc_clocks" "pwc_inout" "pwc_blockram"
    "pwc_dspblock" "pwc_plldll" "pwc_maco" "pwc_serdes" "pwc_mipidphy"
    "pwc_writereport" "pwc_efb" "pwc_misc" "pwc_power" "pwc_esb"
    ;; Programmer extended Tcl commands
    "pgr_project" "pgr_program"
    ;; Platform Manager II extended Tcl commands
    "psb_vmon" "psb_trim" "psb_vid" "psb_imon" "psb_tmon" "psb_fan"
    "psb_lbd_flow" "psb_lbd_sqn" "psb_lbd_svsy" "psb_lbd_timer" "psb_lbd_impt" "psb_usp_glb"
    "psb_usp_port" "psb_usp_node" "psb_usp_asc" "psb_usp_gpio" "psb_usp_hvport" "psb_global"
    "psb_summary" "psb_faultlogger"
    ;; Incremental Design Flow Database extended Tcl commands
    "icf_data" "icf_part"
    ;; Compile Lattice FPGA simulation libraries.
    "cmpl_libs"
    ;; ECO extended Tcl commands
    "eco_design" "eco_add" "eco_delete" "eco_unbind"
    "eco_clone" "eco_swap" "eco_rename" "eco_place"
    "eco_route" "eco_config"))

(defconst fpga-lattice-diamond-shell-commands-font-lock
  (regexp-opt fpga-lattice-diamond-shell-commands 'symbols))

(defconst fpga-lattice-diamond-shell-font-lock
  (append `((,fpga-lattice-diamond-shell-commands-font-lock 0 font-lock-keyword-face)
            (,fpga-utils-shell-switch-re (1 fpga-utils-compilation-msg-code-face) (2 font-lock-constant-face)))))

;;;###autoload (autoload 'fpga-lattice-diamond-shell "fpga-altera.el")
(fpga-utils-define-shell-mode fpga-lattice-diamond-shell
  :bin fpga-lattice-diamond-bin
  :base-cmd fpga-lattice-diamond--base-cmd
  :shell-commands fpga-lattice-diamond-shell-commands
  :compile-re fpga-lattice-diamond-compile-re
  :buf fpga-lattice-diamond-shell-buf
  :font-lock-kwds fpga-lattice-diamond-shell-font-lock)


(provide 'fpga-lattice)

;;; fpga-lattice.el ends here
