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
;;  - TODO: Fill

;;; Code:


(require 'fpga-utils)


;;;; Custom
(defgroup fpga-altera nil
  "FPGA Altera customization."
  :group 'fpga)

;; TODO: Check what's the command for the System Console TCL Binary:
;; https://www.intel.com/content/www/us/en/docs/programmable/683819/22-4/starting-system-console.html
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

(fpga-utils-define-compilation-mode fpga-altera-quartus-compilation-mode
                                    "Quartus"
                                    "Quartus Compilation mode."
                                    fpga-altera-quartus-compile-re
                                    fpga-altera-quartus-buf)

(fpga-utils-define-compile-fn fpga-altera-quartus-compile
                              "Compile Quartus COMMAND with error regexp highlighting."
                              fpga-altera-quartus-buf)


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
;; ...
;;  - TODO: Check https://www.intel.com/content/www/us/en/docs/programmable/683432/21-3/compilation-with-quartus-sh-flow.html


;;;; Quartus Shell
;; Quartus II Scripting Reference Manual: Chapter 3. Tcl Packages & Commands
;; TODO: Use some of these for syntax highlighting for qsf/qpf files?
(defvar fpga-altera-quartus-shell-commands
  '(;; Package
    "advanced_timing"
    ;; Functions
    "create_p2p_delays" "get_clock_delay_path" "get_delay_path"
    "get_delays_from_clocks" "get_delays_from_keepers" "get_illegal_delay_value"
    "get_max_delay_value" "get_timing_edge_delay" "get_timing_edge_info"
    "get_timing_edges" "get_timing_node_fanin" "get_timing_node_fanout"
    "get_timing_node_info" "get_timing_nodes" "is_legal_delay_value"
    "p2p_timing_cut_exist"

    ;; Package
    "backannotate"
    ;; Functions
    "get_back_annotation_assignments" "logiclock_back_annotate"

    ;; Package
    "chip_planner"
    ;; Functions
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

    ;; Package
    "database_manager"
    ;; Functions
    "export_database" "generate_bottom_up_scripts" "import_database"

    ;; Package
    "device"
    ;; Functions
    "get_family_list" "get_part_info" "get_part_list"
    "report_device_info" "report_family_info" "report_part_info"

    ;; Package
    "flow"
    ;; Functions
    "execute_flow" "execute_hc" "execute_module"

    ;; Package
    "incremental_compilation"
    ;; Functions
    "auto_partition_design" "create_partition" "delete_all_logiclock"
    "delete_all_partitions" "delete_logiclock" "delete_partition"
    "export_partition" "get_logiclock" "get_logiclock_contents"
    "get_partition" "get_partition_file_list" "import_partition"
    "partition_netlist_exists" "set_logiclock" "set_logiclock_contents"
    "set_partition"

    ;; Package
    "insystem_memory_edit"
    ;; Functions
    "begin_memory_edit" "end_memory_edit" "get_editable_mem_instances"
    "read_content_from_memory" "save_content_from_memory_to_file" "update_content_to_memory_from_file"
    "write_content_to_memory"

    ;; Package
    "insystem_source_probe"
    ;; Functions
    "end_insystem_source_probe" "get_insystem_source_probe_instance_info" "read_probe_data"
    "read_source_data" "start_insystem_source_probe" "write_source_data"

    ;; Package
    "iptclgen"
    ;; Functions
    "compute_pll" "generate_vhdl_simgen_model" "parse_hdl"
    "parse_tcl"

    ;; Package
    "jtag"
    ;; Functions
    "close_device" "device_dr_shift" "device_ir_shift"
    "device_lock" "device_run_test_idle" "device_unlock"
    "device_virtual_dr_shift" "device_virtual_ir_shift" "get_device_names"
    "get_hardware_names" "open_device"

    ;; Package
    "logic_analyzer_interface"
    ;; Functions
    "begin_logic_analyzer_interface_control" "change_bank_to_output_pin" "end_logic_analyzer_interface_control"
    "get_current_state_of_output_pin" "tristate_output_pin"
    ;; Package
    "misc"
    ;; Functions
    "checksum" "disable_natural_bus_naming" "enable_natural_bus_naming"
    "escape_brackets" "foreach_in_collection" "get_collection_size"
    "get_environment_info" "init_tk" "load"
    "load_package" "post_message" "qexec"
    "qexit" "stopwatch"

    ;; Package
    "project"
    ;; Functions
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

    ;; Package
    "report"
    ;; Functions
    "add_row_to_table" "create_report_panel" "delete_report_panel"
    "get_fitter_resource_usage" "get_number_of_columns" "get_number_of_rows"
    "get_report_panel_column_index" "get_report_panel_data" "get_report_panel_id"
    "get_report_panel_names" "get_report_panel_row" "get_report_panel_row_index"
    "get_timing_analysis_summary_results" "load_report" "read_xml_report"
    "save_report_database" "unload_report" "write_report_panel"
    "write_xml_report"

    ;; Package
    "rtl"
    ;; Functions
    "get_rtl_cell_info" "get_rtl_cells" "get_rtl_fanins"
    "get_rtl_fanouts" "get_rtl_pin_info" "get_rtl_pins"
    "load_rtl_netlist" "unload_rtl_netlist"

    ;; Package
    "sdc"
    ;; Functions
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

    ;; Package
    "sdc_ext"
    ;; Functions
    "derive_clock_uncertainty" "derive_pll_clocks" "get_assignment_groups"
    "get_fanins" "get_fanouts" "get_keepers"
    "get_nodes" "get_partitions" "get_registers"
    "remove_annotated_delay" "remove_clock" "reset_timing_derate"
    "set_active_clocks" "set_annotated_delay" "set_max_skew"
    "set_net_delay" "set_scc_mode" "set_time_format"
    "set_timing_derate"

    ;; Package
    "simulator"
    ;; Functions
    "compare_vector" "convert_vector" "create_simulation_breakpoint"
    "delete_all_simulation_breakpoint" "delete_simulation_breakpoint" "disable_all_simulation_breakpoint"
    "disable_simulation_breakpoint" "enable_all_simulation_breakpoint" "enable_simulation_breakpoint"
    "fast_write_to_simulation_memory" "force_simulation_value" "get_simulation_memory_info"
    "get_simulation_time" "get_simulation_value" "group_simulation_signal"
    "initialize_simulation" "partition_vector" "read_from_simulation_memory"
    "release_simulation_value" "run_simulation" "set_simulation_clock"
    "write_to_simulation_memory"

    ;; Package
    "sta"
    ;; Functions
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

    ;; Package
    "stp"
    ;; Functions
    "close_session" "open_session" "run"
    "run_multiple_end" "run_multiple_start" "stop"

    ;; Package
    "timing"
    ;; Functions
    "compute_slack_on_edges" "create_timing_netlist" "delete_timing_netlist"
    "remove_timing_tables" "report_timing"

    ;; Package
    "timing_assignment"
    ;; Functions
    "create_base_clock" "create_relative_clock" "get_clocks"
    "set_clock_latency" "set_clock_uncertainty" "set_input_delay"
    "set_multicycle_assignment" "set_output_delay" "set_timing_cut_assignment"

    ;; Package
    "timing_report"
    ;; Functions
    "list_path"
    ))

;; Quartus 22.1 System Console Tcl and Toolkit Command Reference guide:
(defvar fpga-altera-quartus-shell-system-console-commands
  '(;; 1. System Console Tcl Command Reference
    "add_help" "add_service" "autosweep_add_input_parameter"
    "autosweep_add_output_metric" "autosweep_apply_bestcase" "autosweep_apply_case"
    "autosweep_create_instance" "autosweep_destroy_instance" "autosweep_get_best_case"
    "autosweep_get_case_count" "autosweep_get_case_description" "autosweep_get_case_result"
    "autosweep_get_data" "autosweep_get_input_parameter_range" "autosweep_get_input_parameters"
    "autosweep_get_instances" "autosweep_get_output_metrics" "autosweep_get_progress"
    "autosweep_remove_input_paramater" "autosweep_remove_output_metric" "autosweep_set_input_parameter_range"
    "autosweep_start" "autosweep_stop" "bytestream_receive"
    "bytestream_send" "claim_service" "close_service"
    "debug_get_commands" "debug_get_legacy_service_types" "debug_print_filesystem"
    "design_extract_debug_files" "design_extract_dotty" "design_get_warnings"
    "design_instantiate" "design_link" "design_load"
    "design_update_debug_files" "device_download_sof" "device_get_board"
    "device_get_connections" "device_get_design" "etile_eye_background_scan_done"
    "etile_eye_cancel_background_scan" "etile_eye_get_attribute" "etile_eye_get_data"
    "etile_eye_scan_and_load" "etile_eye_unload" "etile_get_actions"
    "etile_get_parameter" "etile_get_parameters" "etile_read_register"
    "etile_run_action" "etile_set_parameter" "etile_write_register"
    "executor_cancel" "executor_clean_directory" "executor_get_directory"
    "executor_get_environment" "executor_get_exit_value" "executor_get_stderr"
    "executor_get_stdout" "executor_is_cancelled" "executor_is_done"
    "executor_run" "executor_set_environment" "executor_unset_environment"
    "executor_wait_for" "eye_create_instance" "eye_destroy_instance"
    "eye_get_channel" "eye_get_data" "eye_get_instances"
    "eye_get_progress" "eye_get_toolkit_instance" "eye_start"
    "eye_stop" "get_claimed_services" "get_service_paths"
    "get_service_types" "get_services_to_add" "get_version"
    "help" "io_bus_access" "io_bus_get_protocol"
    "is_plugin_enabled" "is_service_open" "is_service_path"
    "issp_get_instance_info" "issp_read_probe_data" "issp_read_source_data"
    "issp_write_source_data" "jtag_debug_loop" "jtag_debug_reset_system"
    "jtag_debug_sample_clock" "jtag_debug_sample_reset" "jtag_debug_sense_clock"
    "log_command_start" "log_command_stop" "loopback_get"
    "loopback_set" "loopback_start" "loopback_stop"
    "marker_get_assignments" "marker_get_info" "marker_get_type"
    "marker_get_types" "marker_node_info" "master_get_register_names"
    "master_get_slaves" "master_get_timeout" "master_read_16"
    "master_read_32" "master_read_8" "master_read_memory"
    "master_read_to_file" "master_set_timeout" "master_write_16"
    "master_write_32" "master_write_8" "master_write_from_file"
    "master_write_memory" "module_get_children" "module_get_keys"
    "module_get_parent" "module_get_values" "monitor_add_range"
    "monitor_get_all_read_intervals" "monitor_get_interval" "monitor_get_missing_event_count"
    "monitor_get_read_interval" "monitor_read_all_data" "monitor_read_data"
    "monitor_set_callback" "monitor_set_enabled" "monitor_set_interval"
    "open_service" "packet_send_command" "plugin_disable"
    "plugin_enable" "processor_clear_breakpoint" "processor_download_elf"
    "processor_gdbserver" "processor_gdbserver_start" "processor_gdbserver_stop"
    "processor_get_register" "processor_get_register_names" "processor_in_debug_mode"
    "processor_reset" "processor_run" "processor_semihosting_start"
    "processor_semihosting_stop" "processor_set_breakpoint" "processor_set_register"
    "processor_step" "processor_stop" "processor_stop_reason"
    "processor_verify_elf" "refresh_connections" "remove_service"
    "semiConsoleNew" "semihosting_start" "semihosting_stop"
    "send_message" "sld_access_dr" "sld_access_ir"
    "sld_lock" "sld_run_test_idle" "sld_send_program"
    "sld_test_logic_reset" "sld_unlock" "stp_run"
    "system_get_keys" "system_get_values" "toolkit_get_toolkit_actions"
    "toolkit_get_toolkit_autosweep_input_parameters" "toolkit_get_toolkit_autosweep_output_metrics" "toolkit_get_toolkit_channel_properties"
    "toolkit_get_toolkit_channels" "toolkit_get_toolkit_display_hint" "toolkit_get_toolkit_display_id_to_name"
    "toolkit_get_toolkit_display_item_property" "toolkit_get_toolkit_eye_channels" "toolkit_get_toolkit_eye_input_parameters"
    "toolkit_get_toolkit_eye_output_metrics" "toolkit_get_toolkit_matching_modules" "toolkit_get_toolkit_matching_services"
    "toolkit_get_toolkit_matching_systems" "toolkit_get_toolkit_parameter_properties" "toolkit_get_toolkit_parameter_property"
    "toolkit_get_toolkit_parameter_value" "toolkit_get_toolkit_parameters" "toolkit_get_toolkit_properties"
    "toolkit_get_toolkit_property" "toolkit_get_toolkit_requirement_ids" "toolkit_get_toolkit_requirement_properties"
    "toolkit_get_toolkit_requirement_property" "toolkit_load_toolkit_instance" "toolkit_log_toolkit_command_start"
    "toolkit_log_toolkit_command_stop" "toolkit_run_toolkit_action" "toolkit_set_toolkit_parameter_value"
    "toolkit_unload_toolkit_instance" "trace_db_delete_snapshot" "trace_db_get_snapshot"
    "trace_db_snapshot_get_event_data" "trace_db_snapshot_get_event_fields" "trace_db_snapshot_get_event_kind"
    "trace_db_snapshot_get_event_timestamp" "trace_db_snapshot_get_events" "trace_decoder_add_key_result"
    "trace_decoder_add_result" "trace_decoder_define_key" "trace_decoder_get_config"
    "trace_decoder_get_data_16" "trace_decoder_get_data_16be" "trace_decoder_get_data_32"
    "trace_decoder_get_data_32be" "trace_decoder_get_data_64" "trace_decoder_get_data_64be"
    "trace_decoder_get_data_8" "trace_decoder_get_length" "trace_decoder_get_timestamp"
    "trace_decoder_set_callback" "trace_decoder_set_config_regs" "trace_decoder_set_summary"
    "trace_get_db_size" "trace_get_max_db_size" "trace_get_monitor_info"
    "trace_get_monitors" "trace_get_status" "trace_load"
    "trace_read_monitor" "trace_save" "trace_set_max_db_size"
    "trace_set_trigger_mode" "trace_start" "trace_stop"
    "trace_write_monitor"
    ;; Properties
    ;; 1.2.1. _hw.tcl Capture Mode
    ;; 1.2.2. _hw.tcl Command Flag Name
    ;; 1.2.3. _hw.tcl Format
    ;; 1.2.4. _hw.tcl Kind
    ;; 1.2.5. _hw.tcl Protocol
    ;; 1.2.6. _hw.tcl Status
    ;; 1.2.7. _hw.tcl Trigger Mode

    ;; 2. Toolkit Tcl Command References
    "add_channel" "add_display_item" "add_parameter"
    "add_requirement" "add_timed_callback" "get_accessible_module"
    "get_accessible_modules" "get_accessible_service" "get_accessible_services"
    "get_accessible_system" "get_channel_display_group" "get_channel_property"
    "get_display_hint" "get_display_item_property" "get_eye_viewer_display_group"
    "get_parameter_property" "get_parameter_value" "get_toolkit_property"
    "remove_timed_callback" "send_message" "set_channel_property"
    "set_current_progress" "set_display_hint" "set_display_item_property"
    "set_eye_data" "set_eye_property" "set_parameter_property"
    "set_parameter_update_callback" "set_parameter_value" "set_requirement_property"
    "set_toolkit_property" "stop_requested"
    ;; 2.2. Properties
    ;; 2.2.1. _hw.tcl Callbacks
    ;; 2.2.2. _hw.tcl Channel Properties
    ;; 2.2.3. _hw.tcl Channel Type
    ;; 2.2.4. _hw.tcl Display Hint
    ;; 2.2.5. _hw.tcl Display Item Properties
    ;; 2.2.6. _hw.tcl Display Item Type
    ;; 2.2.7. _hw.tcl Eye Properties
    ;; 2.2.8. _hw.tcl Parameter Properties
    ;; 2.2.9. _hw.tcl Parameter Type
    ;; 2.2.10. _hw.tcl Requirement Properties
    ;; 2.2.11. _hw.tcl Requirement Type
    ;; 2.2.12. _hw.tcl Toolkit Properties
    ))

;;;###autoload (autoload 'fpga-altera-quartus-shell "fpga-altera.el")
(fpga-utils-define-shell-mode fpga-altera-quartus-shell
  fpga-altera-quartus-bin
  fpga-altera-quartus--base-cmd
  fpga-altera-quartus-shell-commands
  fpga-altera-quartus-compile-re
  fpga-altera-quartus-shell-buf)


;;;; Quartus SDC mode
;; SDC and TimeQuest API Reference Manual:
;; - https://www.intel.com/content/dam/support/jp/ja/programmable/support-resources/bulk-container/pdfs/literature/manual/mnl-sdctmq-1.pdf
(defvar fpga-altera-quartus-sdc-commands
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
  (eval-when-compile (regexp-opt fpga-altera-quartus-sdc-commands 'symbols)))

(defconst fpga-altera-quartus-sdc-font-lock
  `((,fpga-altera-quartus-sdc-commands-font-lock 0 font-lock-keyword-face)))

(defun fpga-altera-quartus-sdc-capf ()
  "Quartus SDC completion at point."
  (let* ((b (save-excursion (skip-chars-backward "a-zA-Z0-9_-") (point)))
         (e (save-excursion (skip-chars-forward "a-zA-Z0-9_-") (point)))
         (str (buffer-substring b e))
         (allcomp (all-completions str fpga-altera-quartus-sdc-commands)))
    (list b e allcomp)))

;;;###autoload
(define-derived-mode fpga-altera-quartus-sdc-mode tcl-mode
  (font-lock-add-keywords 'fpga-altera-quartus-sdc-mode fpga-altera-quartus-sdc-font-lock 'append)
  (add-hook 'completion-at-point-functions #'fpga-altera-quartus-sdc-capf :local)
  (setq mode-name "SDC"))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.sdc\\'") 'fpga-altera-quartus-sdc-mode))


(provide 'fpga-altera)

;;; fpga-altera.el ends here
