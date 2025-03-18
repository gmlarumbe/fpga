;;; fpga-altera.el --- FPGA Altera Utils  -*- lexical-binding: t -*-

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

;; FPGA Utilities for Altera Quartus:
;;  - Automatic tags creation from project QSF file
;;  - Synthesis compilation with error regexp matching
;;  - Improved Quartus shell with syntax highlighting and autocompletion
;;  - Quartus SDC mode with syntax highlighting and autocompletion
;;  - Quartus QSF mode with syntax highlighting and autocompletion

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

(defcustom fpga-altera-quartus-cmd-opts nil
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

(defcustom fpga-altera-quartus-syn-script
  '("load_package flow"
    "execute_flow -compile")
  "Quartus script to be run for synthesis.
Each string of the list corresponds to one statement of the TCL input file."
  :type '(repeat string)
  :group 'fpga-altera)

(defcustom fpga-altera-quartus-compile-keep-term-color nil
  "Set to non-nil to keep original terminal quartus_sh colors.
If nil is kept, regexps will be parsed in compilation derived mode, but
compilation will need to end before showing the output in its corresponding
buffer."
  :type 'boolean
  :group 'fpga-altera)


;;;; Internal
(defconst fpga-altera-quartus--base-cmd
  (concat fpga-altera-quartus-bin " " (mapconcat #'identity fpga-altera-quartus-cmd-opts " ")))

(defconst fpga-altera-quartus--sed-cmd-remove-shell-color "sed -e 's/\x1b\[[0-9;]*m//g'"
  "Sed command to remove ANSI color from shell output.
This is needed since the command \"quartus_sh\" colorizes the output by default.

https://superuser.com/questions/380772/removing-ansi-color-codes-from-text-stream.")

;;;; Compilation
(defconst fpga-altera-quartus-compile-re
  '((altera-error "^\\(?1:^Error\\) \\(?2:([0-9]+)\\): .*File: \\(?3:[a-zA-Z0-9\._/\\]+\\) Line: \\(?4:[0-9]+\\)" 3 4 nil 2 nil (1 compilation-error-face) (2 fpga-utils-compilation-msg-code-face))
    (altera-error2 "^\\(?1:^Error\\) \\(?2:([0-9]+)\\): " nil nil nil 2 nil (1 compilation-error-face) (2 fpga-utils-compilation-msg-code-face))
    (altera-error3 "^\\(?1:^Error:\\) " nil nil nil 2 nil (1 compilation-error-face))
    (altera-critical "^\\(?1:^Critical Warning\\) \\(?2:([0-9]+)\\): .*File: \\(?3:[a-zA-Z0-9\._/\\]+\\) Line: \\(?4:[0-9]+\\)" 3 4 nil 1 nil (1 compilation-error-face) (2 fpga-utils-compilation-msg-code-face))
    (altera-critical2 "^\\(?1:^Critical Warning\\) \\(?2:([0-9]+)\\): " nil nil nil 1 nil (1 compilation-error-face) (2 fpga-utils-compilation-msg-code-face))
    (altera-critical3 "^\\(?1:^Critical Warning:\\) " nil nil nil 1 nil (1 compilation-error-face))
    (altera-warning "^\\(?1:^Warning\\) \\(?2:([0-9]+)\\): .*File: \\(?3:[a-zA-Z0-9\._/\\]+\\) Line: \\(?4:[0-9]+\\)" 3 4 nil 1 nil (1 compilation-warning-face) (2 fpga-utils-compilation-msg-code-face))
    (altera-warning2 "^\\(?1:^Warning\\) \\(?2:([0-9]+)\\): " nil nil nil 1 nil (1 compilation-warning-face) (2 fpga-utils-compilation-msg-code-face))
    (altera-warning3 "^\\(?1:^Warning:\\) " nil nil nil 1 nil (1 compilation-warning-face))
    (altera-info "^\\(?1:^Info\\) \\(?2:([0-9]+)\\): .*File: \\(?3:[a-zA-Z0-9\._/\\]+\\) Line: \\(?4:[0-9]+\\)" 3 4 nil 0 nil (1 compilation-info-face) (2 fpga-utils-compilation-msg-code-face))
    (altera-info2 "^\\(?1:^Info\\) \\(?2:([0-9]+)\\): " nil nil nil 0 nil (1 compilation-info-face) (2 fpga-utils-compilation-msg-code-face))
    (altera-info3 "^\\(?1:^Info:\\) " nil nil nil 0 nil (1 compilation-info-face)))
  "Altera Quartus regexps.")

(fpga-utils-define-compilation-mode fpga-altera-quartus-compilation-mode
  :desc "Quartus"
  :docstring "Quartus Compilation mode."
  :compile-re fpga-altera-quartus-compile-re
  :buf-name fpga-altera-quartus-buf)

;;;###autoload (autoload 'fpga-altera-quartus-compile "fpga.el")
(fpga-utils-define-compile-fn fpga-altera-quartus-compile
  :docstring "Compile Quartus COMMAND with error regexp highlighting."
  :buf fpga-altera-quartus-buf
  :comp-mode fpga-altera-quartus-compilation-mode)


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

;;;###autoload (autoload 'fpga-altera-quartus-tags "fpga.el" nil t)
(defun fpga-altera-quartus-tags (out-dir qsf-file)
  "Generate tags in OUT-DIR from data in QSF-FILE."
  (interactive "DOutput dir: \nFQSF file: ")
  (fpga-utils-tags-create out-dir qsf-file #'fpga-altera-quartus-files-from-qsf))


;;;; Synthesis
;;;###autoload (autoload 'fpga-altera-quartus-syn "fpga.el" nil t)
(defun fpga-altera-quartus-syn (qpf-file)
  "Open Quartus project from QPF-FILE and run `fpga-altera-quartus-syn-script'."
  (interactive "FQPF File: ")
  (unless (string= (file-name-extension qpf-file) "qpf")
    (error "Selected file is not a QPF"))
  (unless fpga-altera-quartus-bin
    (error "Binary quartus_sh not found in the $PATH"))
  (unless fpga-altera-quartus-syn-script
    (error "Empty script to be sourced for quartus"))
  (let* ((project-dir (file-name-directory (expand-file-name qpf-file)))
         (project-name (file-name-nondirectory qpf-file))
         (tmp-dir (file-name-concat temporary-file-directory "fpga/quartus"))
         (script (progn
                   (make-directory tmp-dir :parents)
                   (make-temp-file (concat tmp-dir "/syn_") nil nil (concat "project_open " project-name "\n"
                                                                            (mapconcat #'identity fpga-altera-quartus-syn-script "\n")))))
         (cmd (concat "cd " project-dir " && " fpga-altera-quartus--base-cmd " -t " script (unless fpga-altera-quartus-compile-keep-term-color
                                                                                             (concat " | " fpga-altera-quartus--sed-cmd-remove-shell-color)))))
    (fpga-altera-quartus-compile cmd)))


;;;; Quartus SDC mode
;; SDC and TimeQuest API Reference Manual:
;; - https://www.intel.com/content/dam/support/jp/ja/programmable/support-resources/bulk-container/pdfs/literature/manual/mnl-sdctmq-1.pdf
(defconst fpga-altera-quartus-sdc-commands
  '(;; sdc
    "all_clocks" "all_inputs" "all_outputs"
    "all_registers" "create_clock" "create_generated_clock"
    "derive_clocks" "get_cells" "get_clocks"
    "get_nets" "get_pins" "get_ports"
    "remove_clock_groups" "remove_clock_latency" "remove_clock_uncertainty"
    "remove_input_delay" "remove_output_delay" "reset_design"
    "set_clock_groups" "set_clock_latency" "set_clock_uncertainty"
    "set_false_path" "set_input_delay" "set_max_delay"
    "set_min_delay" "set_multicycle_path" "set_output_delay"
    ;; sdc_ext
    "derive_pll_clocks" "get_assignment_groups" "get_fanins"
    "get_fanouts" "get_keepers" "get_nodes"
    "get_partitions" "get_registers" "remove_clock"
    "set_scc_mode" "set_time_format"
    ;; sta
    "check_timing" "create_slack_histogram" "create_timing_netlist"
    "create_timing_summary" "delete_timing_netlist" "enable_sdc_extension_collections"
    "get_cell_info" "get_clock_domain_info" "get_clock_fmax_info"
    "get_clock_info" "get_datasheet" "get_default_sdc_file_names"
    "get_delay_info" "get_edge_info" "get_net_info"
    "get_node_info" "get_object_info" "get_partition_info"
    "get_path_count" "get_path_info" "get_pin_info"
    "get_point_info" "get_port_info" "get_register_info"
    "get_timing_paths" "read_sdc" "report_clock_fmax_summary"
    "report_clock_transfers" "report_clocks" "report_datasheet"
    "report_min_pulse_width" "report_net_timing" "report_path"
    "report_sdc" "report_timing" "report_ucp"
    "timing_netlist_exist" "update_timing_netlist" "use_timequest_style_escaping"
    "write_sdc"))

(defconst fpga-altera-quartus-sdc-commands-font-lock
  (regexp-opt fpga-altera-quartus-sdc-commands 'symbols))

(defconst fpga-altera-quartus-sdc-font-lock
  `((,fpga-altera-quartus-sdc-commands-font-lock 0 font-lock-keyword-face)
    (,fpga-utils-shell-switch-re (1 fpga-utils-compilation-msg-code-face) (2 font-lock-constant-face))
    (,fpga-utils-brackets-re 0 fpga-utils-brackets-face)
    (,fpga-utils-parenthesis-re 0 fpga-utils-parenthesis-face)
    (,fpga-utils-curly-braces-re 0 fpga-utils-curly-braces-face)
    (,fpga-utils-braces-content-re 1 fpga-utils-braces-content-face)
    (,fpga-utils-punctuation-re 1 fpga-utils-punctuation-face)))

(defun fpga-altera-quartus-sdc-capf ()
  "Quartus SDC completion at point."
  (let* ((b (save-excursion (skip-chars-backward "a-zA-Z0-9_-") (point)))
         (e (point))
         (allcomp `(,@fpga-altera-quartus-sdc-commands)))
    `(,b ,e ,allcomp)))

;;;###autoload (autoload 'fpga-altera-quartus-sdc-mode "fpga.el")
(define-derived-mode fpga-altera-quartus-sdc-mode tcl-mode "SDC"
  (font-lock-add-keywords 'fpga-altera-quartus-sdc-mode fpga-altera-quartus-sdc-font-lock 'append)
  (setq-local completion-at-point-functions #'fpga-altera-quartus-sdc-capf))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.sdc\\'") 'fpga-altera-quartus-sdc-mode))


;;;; Quartus Shell
;; Quartus II Scripting Reference Manual: Chapter 3. Tcl Packages & Commands
(defvar fpga-altera-quartus-shell-packages
  '("advanced_timing"
    "backannotate"
    "chip_planner"
    "database_manager"
    "device"
    "flow"
    "incremental_compilation"
    "insystem_memory_edit"
    "insystem_source_probe"
    "iptclgen"
    "jtag"
    "logic_analyzer_interface"
    "misc"
    "project"
    "report"
    "rtl"
    "sdc"
    "sdc_ext"
    "simulator"
    "sta"
    "stp"
    "timing"
    "timing_assignment"
    "timing_report"
    "list_path"))

;; Quartus II Scripting Reference Manual: Chapter 3. Tcl Packages & Commands
(defvar fpga-altera-quartus-shell-commands
  '(;; Package "advanced_timing"
    "create_p2p_delays" "get_clock_delay_path" "get_delay_path"
    "get_delays_from_clocks" "get_delays_from_keepers" "get_illegal_delay_value"
    "get_max_delay_value" "get_timing_edge_delay" "get_timing_edge_info"
    "get_timing_edges" "get_timing_node_fanin" "get_timing_node_fanout"
    "get_timing_node_info" "get_timing_nodes" "is_legal_delay_value"
    "p2p_timing_cut_exist"
    ;; Package "backannotate"
    "get_back_annotation_assignments" "logiclock_back_annotate"
    ;; Package "chip_planner"
    "add_new_cell" "add_new_io" "add_usage"
    "apply_command" "check_netlist_and_save" "check_node"
    "close_chip_planner" "connect_chain" "convert_signal_probes"
    "create_migrated_script" "delete_sp" "design_has_ace_support"
    "design_has_encrypted_ip" "disable_sp" "discard_all_changes"
    "discard_node_changes" "enable_sp" "export_stack_to"
    "get_info_parameters" "get_iports" "get_node_by_name"
    "get_node_info" "get_node_loc" "get_nodes"
    "get_oports" "get_port_by_type" "get_port_info"
    "get_sp_pin_list" "get_stack" "get_tile_power_setting"
    "list_sps" "make_ape_connection" "make_input_port"
    "make_output_port" "make_sp" "read_netlist"
    "remove_ape_connection" "remove_chain" "remove_input_port"
    "remove_old_cell" "remove_output_port" "remove_usage"
    "routing_path" "set_batch_mode" "set_node_info"
    "set_port_info" "set_tile_power_setting" "undo_command"
    "update_node_loc"
    ;; Package "database_manager"
    "export_database" "generate_bottom_up_scripts" "import_database"
    ;; Package "device"
    "get_family_list" "get_part_info" "get_part_list"
    "report_device_info" "report_family_info" "report_part_info"
    ;; Package "flow"
    "execute_flow" "execute_hc" "execute_module"
    ;; Package "incremental_compilation"
    "auto_partition_design" "create_partition" "delete_all_logiclock"
    "delete_all_partitions" "delete_logiclock" "delete_partition"
    "export_partition" "get_logiclock" "get_logiclock_contents"
    "get_partition" "get_partition_file_list" "import_partition"
    "partition_netlist_exists" "set_logiclock" "set_logiclock_contents"
    "set_partition"
    ;; Package "insystem_memory_edit"
    "begin_memory_edit" "end_memory_edit" "get_editable_mem_instances"
    "read_content_from_memory" "save_content_from_memory_to_file" "update_content_to_memory_from_file"
    "write_content_to_memory"
    ;; Package "insystem_source_probe"
    "end_insystem_source_probe" "get_insystem_source_probe_instance_info" "read_probe_data"
    "read_source_data" "start_insystem_source_probe" "write_source_data"
    ;; Package "iptclgen"
    "compute_pll" "generate_vhdl_simgen_model" "parse_hdl"
    "parse_tcl"
    ;; Package "jtag"
    "close_device" "device_dr_shift" "device_ir_shift"
    "device_lock" "device_run_test_idle" "device_unlock"
    "device_virtual_dr_shift" "device_virtual_ir_shift" "get_device_names"
    "get_hardware_names" "open_device"
    ;; Package "logic_analyzer_interface"
    "begin_logic_analyzer_interface_control" "change_bank_to_output_pin" "end_logic_analyzer_interface_control"
    "get_current_state_of_output_pin" "tristate_output_pin"
    ;; Package "misc"
    "checksum" "disable_natural_bus_naming" "enable_natural_bus_naming"
    "escape_brackets" "foreach_in_collection" "get_collection_size"
    "get_environment_info" "init_tk" "load"
    "load_package" "post_message" "qexec"
    "qexit" "stopwatch"
    ;; Package "project"
    "assignment_group" "create_revision" "delete_revision"
    "execute_assignment_batch" "export_assignments" "get_all_assignment_names"
    "get_all_assignments" "get_all_global_assignments" "get_all_instance_assignments"
    "get_all_parameters" "get_all_quartus_defaults" "get_all_user_option_names"
    "get_assignment_info" "get_assignment_name_info" "get_current_revision"
    "get_global_assignment" "get_instance_assignment" "get_location_assignment"
    "get_name_info" "get_names" "get_parameter"
    "get_project_directory" "get_project_revisions" "get_top_level_entity"
    "get_user_option" "is_project_open" "project_archive"
    "project_close" "project_exists" "project_new"
    "project_open" "project_restore" "remove_all_global_assignments"
    "remove_all_instance_assignments" "remove_all_parameters" "resolve_file_path"
    "revision_exists" "set_current_revision" "set_global_assignment"
    "set_instance_assignment" "set_io_assignment" "set_location_assignment"
    "set_parameter" "set_power_file_assignment" "set_user_option"
    "test_assignment_trait"
    ;; Package "report"
    "add_row_to_table" "create_report_panel" "delete_report_panel"
    "get_fitter_resource_usage" "get_number_of_columns" "get_number_of_rows"
    "get_report_panel_column_index" "get_report_panel_data" "get_report_panel_id"
    "get_report_panel_names" "get_report_panel_row" "get_report_panel_row_index"
    "get_timing_analysis_summary_results" "load_report" "read_xml_report"
    "save_report_database" "unload_report" "write_report_panel"
    "write_xml_report"
    ;; Package "rtl"
    "get_rtl_cell_info" "get_rtl_cells" "get_rtl_fanins"
    "get_rtl_fanouts" "get_rtl_pin_info" "get_rtl_pins"
    "load_rtl_netlist" "unload_rtl_netlist"
    ;; Package "sdc"
    "all_clocks" "all_inputs" "all_outputs"
    "all_registers" "create_clock" "create_generated_clock"
    "derive_clocks" "get_cells" "get_clocks"
    "get_nets" "get_pins" "get_ports"
    "remove_clock_groups" "remove_clock_latency" "remove_clock_uncertainty"
    "remove_disable_timing" "remove_input_delay" "remove_output_delay"
    "reset_design" "set_clock_groups" "set_clock_latency"
    "set_clock_uncertainty" "set_disable_timing" "set_false_path"
    "set_input_delay" "set_input_transition" "set_max_delay"
    "set_min_delay" "set_multicycle_path" "set_output_delay"
    ;; Package "sdc_ext"
    "derive_clock_uncertainty" "derive_pll_clocks" "get_assignment_groups"
    "get_fanins" "get_fanouts" "get_keepers"
    "get_nodes" "get_partitions" "get_registers"
    "remove_annotated_delay" "remove_clock" "reset_timing_derate"
    "set_active_clocks" "set_annotated_delay" "set_max_skew"
    "set_net_delay" "set_scc_mode" "set_time_format"
    "set_timing_derate"
    ;; Package "simulator"
    "compare_vector" "convert_vector" "create_simulation_breakpoint"
    "delete_all_simulation_breakpoint" "delete_simulation_breakpoint" "disable_all_simulation_breakpoint"
    "disable_simulation_breakpoint" "enable_all_simulation_breakpoint" "enable_simulation_breakpoint"
    "fast_write_to_simulation_memory" "force_simulation_value" "get_simulation_memory_info"
    "get_simulation_time" "get_simulation_value" "group_simulation_signal"
    "initialize_simulation" "partition_vector" "read_from_simulation_memory"
    "release_simulation_value" "run_simulation" "set_simulation_clock"
    "write_to_simulation_memory"
    ;; Package "sta"
    "add_to_collection" "check_timing" "create_report_histogram"
    "create_slack_histogram" "create_timing_netlist" "create_timing_summary"
    "delete_timing_netlist" "enable_ccpp_removal" "enable_sdc_extension_collections"
    "get_available_operating_conditions" "get_cell_info" "get_clock_domain_info"
    "get_clock_fmax_info" "get_clock_info" "get_datasheet"
    "get_default_sdc_file_names" "get_edge_info" "get_edge_slacks"
    "get_min_pulse_width" "get_net_info" "get_node_info"
    "get_object_info" "get_operating_conditions" "get_operating_conditions_info"
    "get_partition_info" "get_path" "get_path_info"
    "get_pin_info" "get_point_info" "get_port_info"
    "get_register_info" "get_timing_paths" "locate"
    "query_collection" "read_sdc" "remove_from_collection"
    "report_advanced_io_timing" "report_bottleneck" "report_clock_fmax_summary"
    "report_clock_transfers" "report_clocks" "report_datasheet"
    "report_ddr" "report_exceptions" "report_max_skew"
    "report_metastability" "report_min_pulse_width" "report_net_delay"
    "report_net_timing" "report_partitions" "report_path"
    "report_rskm" "report_sdc" "report_skew"
    "report_tccs" "report_timing" "report_ucp"
    "set_operating_conditions" "timing_netlist_exist" "update_timing_netlist"
    "use_timequest_style_escaping"
    "write_sdc"
    ;; Package "stp"
    "close_session" "open_session" "run"
    "run_multiple_end" "run_multiple_start" "stop"
    ;; Package "timing"
    "compute_slack_on_edges" "create_timing_netlist" "delete_timing_netlist"
    "remove_timing_tables" "report_timing"
    ;; Package "timing_assignment"
    "create_base_clock" "create_relative_clock" "get_clocks"
    "set_clock_latency" "set_clock_uncertainty" "set_input_delay"
    "set_multicycle_assignment" "set_output_delay" "set_timing_cut_assignment"
    ;; Package "timing_report"
    "list_path"
    ;; Other builtins (not in the reference guide)
    "help"))

(defconst fpga-altera-quartus-shell-packages-font-lock
  (regexp-opt fpga-altera-quartus-shell-packages 'symbols))

(defconst fpga-altera-quartus-shell-commands-font-lock
  (regexp-opt fpga-altera-quartus-shell-commands 'symbols))

(defconst fpga-altera-quartus-shell-font-lock
  (append `((,fpga-altera-quartus-shell-commands-font-lock 0 font-lock-keyword-face)
            (,fpga-altera-quartus-shell-packages-font-lock 0 font-lock-function-name-face)
            (,fpga-altera-quartus-sdc-commands-font-lock 0 font-lock-keyword-face)
            (,fpga-utils-shell-switch-re (1 fpga-utils-compilation-msg-code-face) (2 font-lock-constant-face)))))

;;;###autoload (autoload 'fpga-altera-quartus-shell "fpga.el" nil t)
(fpga-utils-define-shell-mode fpga-altera-quartus-shell
  :bin fpga-altera-quartus-bin
  :base-cmd (concat fpga-altera-quartus--base-cmd " -s")
  :shell-commands fpga-altera-quartus-shell-commands
  :compile-re fpga-altera-quartus-compile-re
  :buf fpga-altera-quartus-shell-buf
  :font-lock-kwds fpga-altera-quartus-shell-font-lock)


;;;; Quartus QSF mode
;;;###autoload (autoload 'fpga-altera-quartus-qsf-mode "fpga.el")
(define-derived-mode fpga-altera-quartus-qsf-mode tcl-mode "QSF"
  (font-lock-add-keywords 'fpga-altera-quartus-qsf-mode fpga-altera-quartus-shell-font-lock 'append)
  (add-hook 'completion-at-point-functions #'fpga-altera-quartus-shell-capf :local))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.qsf\\'") 'fpga-altera-quartus-qsf-mode))


(provide 'fpga-altera)

;;; fpga-altera.el ends here
