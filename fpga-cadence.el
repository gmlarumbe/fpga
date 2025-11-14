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

(defcustom fpga-cadence-genus-bin (executable-find "genus")
  "Path to Genus executable."
  :type 'string
  :group 'fpga-cadence)

(defcustom fpga-cadence-genus-cmd-opts nil
  "Genus process options."
  :type '(repeat string)
  :group 'fpga-cadence)

(defcustom fpga-cadence-genus-buf "*genus*"
  "Buffer to use for Genus compilation process."
  :type 'string
  :group 'fpga-cadence)

(defcustom fpga-cadence-genus-syn-script
  '("read_hdl -language vhdl my_top.vhd"
    "elaborate my_top"
    "syn_generic"
    "syn_map"
    "syn_opt")
  "Genus script to be run for synthesis.

Each string of the list corresponds to one statement, each separated with ;.

Default value shows an example of a Genus script that reads a top module named
\"my_top\" from file \"my_top.sv\". Tweak accordingly for your needs."
  :type '(repeat string)
  :group 'fpga-cadence)

(defcustom fpga-cadence-genus-shell-buf "*genus-shell*"
  "Buffer to use for Genus interactive shell process."
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

;;;; Genus

:;;;; Internal
(defconst fpga-cadence-genus--base-cmd
  (string-trim-right
   (format "%s %s" fpga-cadence-genus-bin
           (when fpga-cadence-genus-cmd-opts
             (mapconcat #'identity fpga-cadence-genus-cmd-opts " ")))))

;;;;; Compilation

(defconst fpga-cadence-genus-compile-re
  '((genus-info "^\\(?1:[0-9\.]+ \\)"
                nil nil nil 0 nil
                (1 compilation-info-face))
    (genus-warning "^\\(?1:[^ \t,]+\\):\\(?2:[0-9]+\\): \\(?3:ERROR\\): "
                   1 2 nil 2 nil
                   (3 compilation-warning-face))
    (genus-error "^\\(?1:[^ \t,]+\\):\\(?2:[0-9]+\\): \\(?3:ERROR\\): "
                 1 2 nil 2 nil
                 (3 compilation-error-face)))
  "Genus regexps.")

(fpga-utils-define-compilation-mode fpga-cadence-genus-compilation-mode
  :desc "Genus"
  :docstring "Genus Compilation mode."
  :compile-re fpga-cadence-genus-compile-re
  :buf-name fpga-cadence-genus-buf)

;;;###autoload (autoload 'fpga-cadence-genus-compile "fpga.el")
(fpga-utils-define-compile-fn fpga-cadence-genus-compile
  :docstring "Compile Genus COMMAND with error regexp highlighting."
  :buf fpga-cadence-genus-buf
  :comp-mode fpga-cadence-genus-compilation-mode)

;;;;; Synthesis

;;;###autoload (autoload 'fpga-cadence-genus-syn "fpga.el" nil t)
(defun fpga-cadence-genus-syn (&optional output-dir)
  "Run Genus script from commands in `fpga-cadence-genus-syn-script'.

If optional OUTPUT-DIR is non nil, use it as the default directory for
compilation.  Otherwise use `default-directory'."
  (interactive "DOutput Dir:")
  (when output-dir
    (unless (file-directory-p output-dir)
      (make-directory output-dir :parents)))
  (unless fpga-cadence-genus-bin
    (error "Binary genus not found in the $PATH"))
  (fpga-cadence-genus-compile
   (format "cd %s && %s -execute \'%s\'"
           (or output-dir default-directory)
           fpga-cadence-genus--base-cmd
           (mapconcat #'identity fpga-cadence-genus-syn-script "; "))))

;;;###autoload (autoload 'fpga-cadence-genus-syn-from-file "fpga.el" nil t)
(defun fpga-cadence-genus-syn-from-file (file &optional output-dir)
  "Open Genus script from FILE and run its commands.

If optional OUTPUT-DIR is non nil, use it as the default directory for
compilation.  Otherwise use `default-directory'."
  (interactive "FScript File: \nDOutput Dir: ")
  (when output-dir
    (unless (file-directory-p output-dir)
      (make-directory output-dir :parents)))
  (unless fpga-cadence-genus-bin
    (error "Binary genus not found in the $PATH"))
  (fpga-cadence-genus-compile
   (format "cd %s && %s %s"
           (or output-dir default-directory)
           fpga-cadence-genus--base-cmd
           file)))

;;;;; Genus shell

(defconst fpga-cadence-genus-shell-commands
  '("add_analyzed_test_points" "add_assign_buffer_options" "add_buffer"
    "add_clock_gates_obs" "add_clock_gates_test_connection"
    "add_core_wrapper_cell" "add_hard_repair" "add_jtag_boundary_scan"
    "add_jtag_macro" "add_lbist" "add_lockup_element" "add_los_pipeline"
    "add_opcg" "add_opcg_hold_mux" "add_pmbist" "add_pmbist_access_method"
    "add_shadow_logic" "add_shift_register_test_points" "add_test_compression"
    "add_test_point" "add_tieoffs" "add_to_collection" "add_user_test_point"
    "add_wir_signal_bits" "alias" "all_clocks" "all_connected"
    "all_constraint_modes" "all_fanin" "all_fanout" "all_inputs" "all_outputs"
    "all_registers" "analyze_atpg_testability" "analyze_initmode_processing"
    "analyze_scan_compressibility" "append_to_collection" "apply_power_intent"
    "apropos" "assemble_design" "bitblast_all_ports" "bitblast_ports"
    "cdnshelp" "change_link" "check_atpg_rules" "check_constraints"
    "check_cpf" "check_cwd_object" "check_design" "check_dft_pad_cfg"
    "check_dft_rules" "check_dft_setup" "check_floorplan" "check_flow"
    "check_library" "check_placement" "check_power_intent"
    "check_power_structure" "check_timing_intent" "clear"
    "commit_dft_power_intent" "commit_module_model" "commit_power_intent"
    "compare_collection" "compare_collections" "compare_crossing"
    "compare_sdc" "compress_scan_chains" "connect" "connect_opcg_segments"
    "connect_scan_chains" "connect_serial_scan_chains"
    "convert_polygon_to_boxes" "convert_to_opcg_scan" "convert_to_scan"
    "copy_collection" "copy_library_domain_attributes" "cpi_insert_auto_ls"
    "create_analysis_view" "create_base_cell_set" "create_chains_database"
    "create_clock" "create_command_help" "create_component"
    "create_component_binding" "create_component_implementation"
    "create_component_library" "create_component_operator"
    "create_component_package" "create_component_parameter"
    "create_component_pin" "create_constraint_mode" "create_cwd_check"
    "create_delay_corner" "create_derived_design" "create_design"
    "create_floorplan" "create_flow" "create_flow_step"
    "create_generated_clock" "create_hinst" "create_hnet" "create_hport"
    "create_hport_bus" "create_inst" "create_library_domain"
    "create_library_set" "create_module" "create_msg" "create_net"
    "create_opcond" "create_place_halo" "create_ple_model" "create_port"
    "create_port_bus" "create_primitive" "create_property_alias"
    "create_rc_corner" "create_region" "create_route_halo" "create_route_rule"
    "create_route_type" "create_row" "create_single_row" "create_snapshot"
    "create_testcase" "create_timing_bin" "create_timing_condition"
    "create_track" "current_design" "current_instance"
    "cut_power_domain_by_overlaps" "date" "declone_clock_gate" "decrypt"
    "dedicate_module" "define_attribute" "define_cost_group"
    "define_dft_cfg_mode" "define_feature"
    "define_hier_test_core_concat_order" "define_hier_test_scan_mapping"
    "define_jtag_boundary_scan_segment" "define_jtag_instruction"
    "define_jtag_instruction_register" "define_jtag_macro"
    "define_jtag_tap_port" "define_mbist_clock" "define_metric" "define_msg"
    "define_opcg_domain" "define_opcg_domain_macro_parameters"
    "define_opcg_mode" "define_opcg_osc_source" "define_opcg_trigger"
    "define_pmbist_direct_access" "define_pmbist_port"
    "define_scan_abstract_segment" "define_scan_chain"
    "define_scan_fixed_segment" "define_scan_floating_segment"
    "define_scan_preserved_segment" "define_shift_enable"
    "define_shift_register_segment" "define_test_bus_port" "define_test_clock"
    "define_test_mode" "define_test_signal" "delete_buffer_tree"
    "delete_clock_gate" "delete_flow" "delete_flow_config" "delete_flow_step"
    "delete_metric" "delete_metric_header" "delete_metric_run" "delete_obj"
    "delete_rows" "delete_unloaded_undriven"
    "dft_translate_functional_clocks_for_atpg_cut_points" "disconnect"
    "duplicate_register" "edit_flow" "elaborate" "enable_metrics" "encrypt"
    "end_flow" "eval_1801" "eval_common_ui" "exit" "export_oa_db"
    "filter_collection" "fix_dft_violations" "fix_pad_cfg"
    "fix_scan_path_inversions" "flatten_complex_ports"
    "floorplan_resize_blockage" "floorplan_resize_boundary"
    "foreach_in_collection" "generate_ilm" "get_cells" "get_clock_ports"
    "get_clocks" "get_computed_shapes" "get_constant" "get_db" "get_designs"
    "get_fanin" "get_fanout" "get_feature" "get_flow_config"
    "get_generated_clocks" "get_leaf_cells" "get_leaf_nets" "get_leaf_pins"
    "get_lib_cells" "get_lib_pins" "get_libs" "get_license" "get_license_list"
    "get_license_names" "get_license_version" "get_logical_name" "get_metric"
    "get_metric_alias" "get_metric_config" "get_metric_definition"
    "get_metric_header" "get_nets" "get_obj_in_area" "get_object_name"
    "get_object_type" "get_pins" "get_ports" "get_read_files"
    "get_remove_assign_options" "get_scan_elements"
    "get_socv_reporting_nsigma_multiplier" "group" "group_path" "gui_hide"
    "gui_raise" "gui_reset" "gui_show" "gui_update"
    "hdl_set_vlog_file_extension" "help"
    "identify_domain_crossing_pins_for_cgic_and_scan_abstracts"
    "identify_multibit_cell_abstract_scan_segments"
    "identify_shared_wrapper_cells_in_design"
    "identify_shift_register_scan_segments" "identify_test_mode_registers"
    "import_oa_db" "include" "incr_read_1801" "index_collection" "init_design"
    "init_ml" "insert_buffer" "is_attribute" "is_collection_object"
    "is_common_ui_mode" "is_feature" "is_flow" "license_reset_wait"
    "list_property" "man" "map_dft_unmapped_logic" "map_pmbist_cgc_to_cgic"
    "map_pmbist_ffn" "map_pmbist_ffsync" "map_pmbist_map2mux"
    "merge_clock_gate" "merge_to_multibit_cells" "move_blockage"
    "move_boundary" "move_instance" "move_obj" "move_port" "optimize_iso_ls"
    "path_adjust" "path_group" "phys_enable_ocv" "phys_reset_ocv" "place_dft"
    "place_dft_sequentials" "pop_snapshot_stack" "predict_floorplan"
    "preserve_analog_pins_from_undriven_handling" "proc" "push_snapshot_stack"
    "quit" "read_clip" "read_congestion_map" "read_db" "read_def" "read_dfm"
    "read_dft_abstract_model" "read_dft_jtag_boundary_file"
    "read_dft_pmbist_interface_files" "read_flow" "read_hdl" "read_ilm"
    "read_ilm_from_files" "read_libs" "read_metric" "read_metric_html_package"
    "read_mmmc" "read_module_model" "read_netlist" "read_parasitics"
    "read_physical" "read_pmbist_memory_view" "read_power_intent" "read_qrc"
    "read_saif" "read_sdc" "read_sdf" "read_sdp_file" "read_tcf" "read_vcd"
    "rebalance_clock_gating" "redirect" "release_license"
    "remove_assigns_without_opt" "remove_clock_reconvergence"
    "remove_from_collection" "rename_obj" "rename_snapshot"
    "report_analysis_views" "report_area" "report_auto_ungroup_hierarchies"
    "report_boundary_opt" "report_case_analysis"
    "report_cell_delay_calculation" "report_chains_longest_wires"
    "report_chains_total_wirelength" "report_chains_wires"
    "report_clock_gates" "report_clock_groups" "report_clock_tree_structure"
    "report_clocks" "report_congestion" "report_constraint"
    "report_core_wrapper_cell" "report_cwd_check" "report_delay_calculation"
    "report_design_rules" "report_dft_abstract_models" "report_dft_trace_back"
    "report_dft_violations" "report_dont_touch" "report_dp" "report_flow"
    "report_gates" "report_hierarchy" "report_instance"
    "report_logic_levels_histogram" "report_loop" "report_low_power_intent"
    "report_memory" "report_memory_cells" "report_messages" "report_metric"
    "report_min_pulse_width" "report_mode" "report_module"
    "report_multibit_inferencing" "report_net_cap_calculation"
    "report_net_delay_calculation" "report_net_res_calculation" "report_nets"
    "report_obj" "report_opcg_clock_domain_info" "report_opcg_equivalents"
    "report_ple" "report_port" "report_power" "report_power_intent"
    "report_power_intent_instances" "report_property" "report_prototype"
    "report_qor" "report_runtime" "report_scan_chains"
    "report_scan_compressibility" "report_scan_registers" "report_scan_setup"
    "report_sdb_annotation" "report_sequential" "report_signoff_timing"
    "report_slew_calculation" "report_summary" "report_test_power"
    "report_timing" "report_timing_derate" "report_timing_summary"
    "report_ungroup_modules" "report_units" "report_utilization"
    "report_yield" "reset_case_analysis" "reset_clock"
    "reset_clock_gating_check" "reset_clock_latency" "reset_clock_transition"
    "reset_db" "reset_design" "reset_disable_clock_gating_check"
    "reset_disable_timing" "reset_driving_cell" "reset_flow"
    "reset_generated_clock" "reset_ideal_latency" "reset_ideal_network"
    "reset_ideal_transition" "reset_input_delay" "reset_metric_config"
    "reset_metric_run_id" "reset_opcg_equivalent" "reset_output_delay"
    "reset_path_adjust" "reset_path_group" "reset_power_intent"
    "reset_scan_equivalent" "reset_sense" "reset_timing_derate" "resume"
    "run_embedded_script" "run_flow" "run_metric_category"
    "run_parallel_commands" "save_snapshot" "schedule_flow" "set_activity"
    "set_analysis_view" "set_annotated_check" "set_annotated_delay"
    "set_annotated_transition" "set_case_analysis" "set_cell_padding"
    "set_cglar_specification" "set_clock_exclusivity" "set_clock_gating_check"
    "set_clock_groups" "set_clock_latency" "set_clock_sense"
    "set_clock_transition" "set_clock_uncertainty"
    "set_compatible_test_clocks" "set_data_check" "set_db"
    "set_disable_clock_gating_check" "set_disable_timing" "set_dont_touch"
    "set_dont_touch_network" "set_dont_use" "set_drive" "set_driving_cell"
    "set_false_path" "set_fanout_load" "set_feature" "set_flow_config"
    "set_flowkit_read_db_args" "set_flowkit_write_db_args"
    "set_hierarchy_separator" "set_ideal_latency" "set_ideal_network"
    "set_ideal_transition" "set_input_delay" "set_input_transition"
    "set_interactive_constraint_modes" "set_lib_pins" "set_load"
    "set_logic_dc" "set_logic_one" "set_logic_zero" "set_max_capacitance"
    "set_max_delay" "set_max_dynamic_power" "set_max_fanout"
    "set_max_leakage_power" "set_max_time_borrow" "set_max_transition"
    "set_metric" "set_metric_alias" "set_metric_header" "set_min_capacitance"
    "set_min_delay" "set_min_fanout" "set_min_pulse_width"
    "set_min_transition" "set_mode" "set_module_model" "set_multi_cpu_usage"
    "set_multicycle_path" "set_opcg_equivalent" "set_operating_conditions"
    "set_output_delay" "set_path_adjust" "set_path_specification"
    "set_port_fanout_number" "set_proc_verbose" "set_resistance"
    "set_scan_equivalent" "set_sense" "set_socv_rc_variation_factor"
    "set_socv_reporting_nsigma_multiplier" "set_timing_derate"
    "set_top_module" "set_units" "set_wire_load_mode" "set_wire_load_model"
    "set_wire_load_selection_group" "share_clock_gate" "sizeof_collection"
    "sort_collection" "specify_cell_pad" "split_clock_gate" "split_db"
    "stop_suspend" "stringify" "suppress_messages" "suspend" "syn_generic"
    "syn_map" "syn_opt" "test_super_thread_servers" "time_info" "ungroup"
    "uniquify" "uniquify_filename" "unsuppress_msg" "update_analysis_view"
    "update_clock_gate" "update_congestion_map" "update_constraint_mode"
    "update_delay_corner" "update_group" "update_inst" "update_instance_data"
    "update_library_set" "update_metric_definition" "update_names"
    "update_opcond" "update_rc_corner" "update_scan_chains"
    "update_timing_condition" "vbasename" "vcd" "vdirname" "vdirs" "vfind"
    "vls" "vpopd" "vpushd" "vpwd" "write_clp_script" "write_congestion_map"
    "write_db" "write_def" "write_design" "write_dft_abstract_model"
    "write_dft_atpg" "write_dft_atpg_other_vendor_files" "write_dft_bsdl"
    "write_dft_compression_macro" "write_dft_compression_test_points"
    "write_dft_constraints" "write_dft_deterministic_test_points"
    "write_dft_jtag_boundary_file" "write_dft_jtag_boundary_verification"
    "write_dft_lbist_macro" "write_dft_lbist_test_points"
    "write_dft_lbist_testbench" "write_dft_pmbist_interface_files"
    "write_dft_pmbist_testbench" "write_dft_rtl_model"
    "write_dft_rtl_opcg_domain_macro" "write_dft_rtl_opcg_trigger_macro"
    "write_flow" "write_flow_template" "write_lec_data"
    "write_memory_physical_map" "write_metric" "write_ml_design"
    "write_ml_timing_path" "write_mmmc" "write_module_model" "write_netlist"
    "write_parasitics" "write_power_intent" "write_reports" "write_saif"
    "write_scandef" "write_sdc" "write_sdf" "write_sdp_file" "write_snapshot"
    "write_sv_wrapper" "write_tcf" "write_template" "write_tempus"))

(defconst fpga-cadence-genus-shell-commands-re
  (regexp-opt fpga-cadence-genus-shell-commands 'symbols))

(defconst fpga-cadence-genus-punctuation-re "[;]")

(defconst fpga-cadence-genus-shell-font-lock
  (append
   `((,fpga-cadence-genus-shell-commands-re
      0 font-lock-keyword-face)
     (,fpga-utils-shell-switch-re
      (1 fpga-utils-compilation-msg-code-face)
      (2 font-lock-constant-face))
     (,fpga-cadence-genus-punctuation-re
      0 fpga-utils-punctuation-face))))

;;;###autoload (autoload 'fpga-cadence-genus-shell "fpga.el" nil t)
(fpga-utils-define-shell-mode fpga-cadence-genus-shell
  :bin fpga-cadence-genus-bin
  :base-cmd fpga-cadence-genus--base-cmd
  :shell-commands fpga-cadence-genus-shell-commands
  :compile-re fpga-cadence-genus-compile-re
  :buf fpga-cadence-genus-shell-buf
  :font-lock-kwds fpga-cadence-genus-shell-font-lock)

;;;;; Genus script mode

(defun fpga-cadence-genus-capf ()
  "Genus GN completion at point."
  (let* ((b
          (save-excursion
            (skip-chars-backward "a-zA-Z0-9_-")
            (point)))
         (e (point))
         (allcomp `(,@fpga-cadence-genus-shell-commands)))
    `(,b ,e ,allcomp)))

(defconst fpga-cadence-genus-font-lock-defaults
  `(((,fpga-cadence-genus-shell-commands-re . font-lock-keyword-face)
     (,fpga-cadence-genus-punctuation-re . fpga-utils-punctuation-face)
     (,fpga-utils-shell-switch-re . ((1 fpga-utils-compilation-msg-code-face)
                                     (2 font-lock-constant-face)))))
  "Font lock defaults for `fpga-cadence-genus-mode'.")

(defconst fpga-cadence-genus-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">#" table)
    table)
  "Syntax table used in Genus Script buffers.")

(defvar fpga-cadence-genus-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c C-p"
                #'fpga-cadence-genus-shell-send-line-or-region-and-step)
    map)
  "Keymap for `fpga-cadence-genus-mode'.")

;;;###autoload (autoload 'fpga-cadence-genus-mode "fpga.el")
(define-derived-mode fpga-cadence-genus-mode tcl-mode "GN"
  "Major mode for Cadence Genus script files.
See: \\{fpga-cadence-genus-mode-map}"
  (set-syntax-table fpga-cadence-genus-mode-syntax-table)
  (setq-local font-lock-defaults fpga-cadence-genus-font-lock-defaults)
  (setq-local completion-at-point-functions #'fpga-cadence-genus-capf)
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+[\t ]*"))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.gn\\'" 'fpga-cadence-genus-mode))

(provide 'fpga-cadence)

;;; fpga-cadence.el ends here
