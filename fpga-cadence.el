;;; fpga-cadence.el --- FPGA & ASIC Cadence Utils  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 Gonzalo Larumbe

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

;; FPGA Cadence Utilities
;;  - Compilation mode with syntax highlighting for Xcelium
;;  - vManager VSIF mode
;;
;;; Code:


(require 'fpga-utils)


;;;; Custom
(defgroup fpga-cadence nil
  "FPGA Cadence customization."
  :group 'fpga)

(defcustom fpga-cadence-xrun-buf "*xrun*"
  "Buffer to use for Xcelium compilation process."
  :type 'string
  :group 'fpga-cadence)


;;;; Compilation
(defvar fpga-cadence-xrun-compile-re
  '((xrun-fatal    "\\(?1:^[a-z]+\\): \\(?2:\\*F\\),\\(?3:[0-9A-Z]+\\)\\(?:\\(?:\\[[0-9A-Z_,]+\\]\\)? (\\(?4:[^ \t,]+\\),\\(?5:[0-9]+\\)|\\(?6:[0-9]+\\)\\)" 4 5 6 2 nil (1 fpga-utils-compilation-bin-face) (2 compilation-error-face) (3 fpga-utils-compilation-msg-code-face))
    (xrun-fatal2   "\\(?1:^[a-z]+\\): \\(?2:\\*F\\),\\(?3:[0-9A-Z]+\\)\\(?:\\(?:\\[[0-9A-Z_,]+\\]\\)? (\\(?4:[^ \t,]+\\),\\(?5:[0-9]+\\)\\)" 4 5 nil 2 nil (1 fpga-utils-compilation-bin-face) (2 compilation-error-face) (3 fpga-utils-compilation-msg-code-face))
    (xrun-fatal3   "\\(?1:^[a-z]+\\): \\(?2:\\*F\\),\\(?3:[0-9A-Z]+\\):" 1 nil nil 2 nil (1 fpga-utils-compilation-bin-face) (2 compilation-error-face) (3 fpga-utils-compilation-msg-code-face))
    (xrun-error    "\\(?1:^[a-z]+\\): \\(?2:\\*E\\),\\(?3:[0-9A-Z]+\\)\\(?:\\(?:\\[[0-9A-Z_,]+\\]\\)? (\\(?4:[^ \t,]+\\),\\(?5:[0-9]+\\)|\\(?6:[0-9]+\\)\\)" 4 5 6 2 nil (1 fpga-utils-compilation-bin-face) (2 compilation-error-face) (3 fpga-utils-compilation-msg-code-face))
    (xrun-error2   "\\(?1:^[a-z]+\\): \\(?2:\\*E\\),\\(?3:[0-9A-Z]+\\)\\(?:\\(?:\\[[0-9A-Z_,]+\\]\\)? (\\(?4:[^ \t,]+\\),\\(?5:[0-9]+\\)\\)" 4 5 nil 2 nil (1 fpga-utils-compilation-bin-face) (2 compilation-error-face) (3 fpga-utils-compilation-msg-code-face))
    (xrun-error3   "\\(?1:^[a-z]+\\): \\(?2:\\*E\\),\\(?3:[0-9A-Z]+\\):" 1 nil nil 2 nil (1 fpga-utils-compilation-bin-face) (2 compilation-error-face) (3 fpga-utils-compilation-msg-code-face))
    (xrun-warning  "\\(?1:^[a-z]+\\): \\(?2:\\*W\\),\\(?3:[0-9A-Z]+\\)\\(?:\\(?:\\[[0-9A-Z_,]+\\]\\)? (\\(?4:[^ \t,]+\\),\\(?5:[0-9]+\\)|\\(?6:[0-9]+\\)\\)" 4 5 6 1 nil (1 fpga-utils-compilation-bin-face) (2 compilation-warning-face) (3 fpga-utils-compilation-msg-code-face))
    (xrun-warning2 "\\(?1:^[a-z]+\\): \\(?2:\\*W\\),\\(?3:[0-9A-Z]+\\)\\(?:\\(?:\\[[0-9A-Z_,]+\\]\\)? (\\(?4:[^ \t,]+\\),\\(?5:[0-9]+\\)\\)" 4 5 nil 1 nil (1 fpga-utils-compilation-bin-face) (2 compilation-warning-face) (3 fpga-utils-compilation-msg-code-face))
    (xrun-warning3 "\\(?1:^[a-z]+\\): \\(?2:\\*W\\),\\(?3:[0-9A-Z]+\\):" 1 nil nil 1 nil (1 fpga-utils-compilation-bin-face) (2 compilation-warning-face) (3 fpga-utils-compilation-msg-code-face))
    (xrun-note     "\\(?1:^[a-z]+\\): \\(?2:\\*N\\),\\(?3:[0-9A-Z]+\\)\\(?:\\(?:\\[[0-9A-Z_,]+\\]\\)? (\\(?4:[^ \t,]+\\),\\(?5:[0-9]+\\)|\\(?6:[0-9]+\\)\\)" 4 5 6 0 nil (1 fpga-utils-compilation-bin-face) (2 compilation-info-face) (3 fpga-utils-compilation-msg-code-face))
    (xrun-note2    "\\(?1:^[a-z]+\\): \\(?2:\\*N\\),\\(?3:[0-9A-Z]+\\)\\(?:\\(?:\\[[0-9A-Z_,]+\\]\\)? (\\(?4:[^ \t,]+\\),\\(?5:[0-9]+\\)\\)" 4 5 nil 0 nil (1 fpga-utils-compilation-bin-face) (2 compilation-info-face) (3 fpga-utils-compilation-msg-code-face))
    (xrun-note3    "\\(?1:^[a-z]+\\): \\(?2:\\*N\\),\\(?3:[0-9A-Z]+\\):" 1 nil nil 0 nil (1 fpga-utils-compilation-bin-face) (2 compilation-info-face) (3 fpga-utils-compilation-msg-code-face)))
  "Xcelium compilation regexp.
- xrun-error: errors with line number and column number
- xrun-error2: errors with line number
- xrun-error3: errors without file/line")

(fpga-utils-define-compilation-mode fpga-cadence-xrun-compilation-mode
  :desc "Xcelium"
  :docstring "Xcelium Compilation mode."
  :compile-re (append fpga-cadence-xrun-compile-re
                      fpga-utils-compilation-uvm-re
                      fpga-utils-compilation-ovm-re)
  :buf-name fpga-cadence-xrun-buf)

;;;###autoload (autoload 'fpga-cadence-xrun-compile "fpga.el")
(fpga-utils-define-compile-fn fpga-cadence-xrun-compile
  :docstring "Compile Xcelium COMMAND with error regexp highlighting."
  :buf fpga-cadence-xrun-buf
  :comp-mode fpga-cadence-xrun-compilation-mode)


;;;; vManager VSIF mode
(defconst fpga-cadence-vsif-ntf-containers '("session" "group" "test" "extend"))
(defconst fpga-cadence-vsif-session-container-attributes
  '("abort_dependent_jobs_on_nonzero_exit" "abort_dependent_runs_on_failure"
    "automation_file" "auto_load_vplan_refinement_vmgr" "auto_load_refinement_vmgr"
    "auto_load_vplan_vmgr" "create_debug_logs" "create_optimized_coverage_data"
    "default_dispatch_parameters" "description" "drm" "enable_session_recovery_vmgr"
    "file_policy" "free_hosts" "group_automation_file" "host_lock_timeout"
    "incremental_merge_type_vmgr" "incremental_merge_slice_size_vmgr"
    "incremental_merge_timeout_to_merge_vmgr" "incremental_merge_runs_merged_vmgr"
    "incremental_merge_last_time_merged_vmgr" "use_incremental_merged_model_for_analysis_vmgr"
    "propose_failure_clusters_vmgr" "master_submission_policy" "local_top_dir"
    "max_runs_in_parallel" "MERGE_CONFIG_VMGR" "model_dir" "output_mode"
    "post_session_dispatch_parameters" "pre_session_dispatch_parameters"
    "post_session_drm" "pre_session_drm" "post_session_script" "pre_session_script"
    "top_dir" "verification_scope"))
(defconst fpga-cadence-vsif-group-container-attributes '("bundle_group" "repetitions"))
(defconst fpga-cadence-vsif-group-test-containers-attributes
  '("bundle_group" "code_coverage" "count" "copied_files"
    "depends_on" "details" "drm_job_priority_vmgr" "dut_name"
    "estimated_duration_vmgr" "exit_on" "fully_directed" "gui_mode"
    "hdl_files" "ifv_assertions" "ifv_effort" "ifv_engine"
    "ifv_halo" "linked_files" "pre_group_script" "pre_run_script"
    "primary_run" "post_commands" "post_group_script" "post_run_script"
    "post_simulate_script" "repetitions" "revision" "run_mode"
    "run_script" "run_on_emulation_hardware_vmgr" "xejob_args_vmgr" "runs_dispatch_parameters"
    "scan_script" "seed" "sim_args" "sv_seed"
    "sve_name" "test_command" "timeout" "top_files"
    "verbosity" "waveform" "write_metrics" "use_ida_record"
    "ida_db_start_time" "ida_probe_options" "ida_recording_window"))
(defconst fpga-cadence-vsif-queuing_policy-options '("long2short" "short2long" "round_robin" "vsif_order"))

(defconst fpga-cadence-vsif-ntf-containers-re
  (regexp-opt fpga-cadence-vsif-ntf-containers 'symbols))
(defconst fpga-cadence-vsif-session-container-attributes-re
  (regexp-opt fpga-cadence-vsif-session-container-attributes 'symbols))
(defconst fpga-cadence-vsif-group-container-attributes-re
  (regexp-opt fpga-cadence-vsif-group-container-attributes 'symbols))
(defconst fpga-cadence-vsif-group-test-containers-attributes-re
  (regexp-opt fpga-cadence-vsif-group-test-containers-attributes 'symbols))
(defconst fpga-cadence-vsif-queuing_policy-options-re
  (regexp-opt fpga-cadence-vsif-queuing_policy-options 'symbols))

(defconst fpga-cadence-vsif-ntf-containers-re-font-lock
  (concat "\\s-*" fpga-cadence-vsif-ntf-containers-re "\\s-+\\_<\\([a-zA-Z0-9_-]+\\)\\_>\\s-*"))
(defconst fpga-cadence-vsif-cpp-directives-simple-re-font-lock
  (concat "\\s-*" "\\(#[a-zA-Z0-9_]+\\)")) ; Only one word (e.g. #endif)
(defconst fpga-cadence-vsif-cpp-directives-complex-re-font-lock
  (concat "\\s-*" "\\(#[a-zA-Z0-9_]+\\)\\s-+\\(.*\\)")) ; More than one word (e.g. #ifdef asdf)
(defconst fpga-cadence-vsif-group-test-containers-attributes-re-font-lock
  (concat "\\s-*" fpga-cadence-vsif-group-test-containers-attributes-re "\\s-*:\\s-*\\([a-zA-Z0-9_-]+\\)\\s-*;"))
(defconst fpga-cadence-vsif-queuing_policy-options-re-font-lock
  (concat "\\s-*\\(queuing_policy\\)\\s-*:\\s-*" fpga-cadence-vsif-queuing_policy-options-re "\\s-*;"))
(defconst fpga-cadence-vsif-user-options-re-font-lock (concat "\\s-*\\([a-zA-Z0-9_]+\\)\\s-*:\\s-*"))
(defconst fpga-cadence-vsif-punctuation-re-font-lock "[:;]")
(defconst fpga-cadence-vsif-attribute-fake-comment-re-font-lock "\\(^.*\\)\\(--\\s-*.*\\)$") ; INFO: These comments do not belong to syntax table comments, only font-locking

(defvar fpga-cadence-vsif-font-lock-defaults
  `(((,fpga-cadence-vsif-ntf-containers-re-font-lock . ((1 font-lock-keyword-face) (2 font-lock-function-name-face)))
     (,fpga-cadence-vsif-group-test-containers-attributes-re-font-lock . (1 font-lock-variable-name-face))
     (,fpga-cadence-vsif-queuing_policy-options-re-font-lock . (1 font-lock-variable-name-face))
     (,fpga-cadence-vsif-cpp-directives-simple-re-font-lock . (1 font-lock-preprocessor-face))
     (,fpga-cadence-vsif-cpp-directives-complex-re-font-lock . ((1 font-lock-preprocessor-face) (2 font-lock-variable-name-face)))
     (,fpga-cadence-vsif-user-options-re-font-lock . (1 font-lock-variable-name-face))
     (,fpga-cadence-vsif-punctuation-re-font-lock . fpga-utils-punctuation-face)
     (,fpga-utils-curly-braces-re . fpga-utils-curly-braces-face)
     (,fpga-cadence-vsif-attribute-fake-comment-re-font-lock . (2 font-lock-comment-face)))))

;;;###autoload (autoload 'fpga-cadence-vsif-mode "fpga.el")
(define-derived-mode fpga-cadence-vsif-mode c-mode "VSIF"
  "A mode for VSIF vManager regression files."
  (setq-local font-lock-defaults fpga-cadence-vsif-font-lock-defaults))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.vsif\\'") 'fpga-cadence-vsif-mode))


(provide 'fpga-cadence)

;;; fpga-cadence.el ends here

