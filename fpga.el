;;; fpga.el --- FPGA & ASIC Utils for Emacs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defgroup fpga nil
  "FPGA/ASIC Utils.")

(defcustom fpga-source-extension-re (concat "\\." (regexp-opt '("sv" "svh" "v" "vh" "vhd" "vhdl")) "$")
  "FPGA source file extension regexp."
  :type 'string
  :group 'fpga)

(defcustom fpga-tags-creation-fn #'ggtags-create-tags
  "Function to use to create tags."
  :type 'function
  :group 'fpga)


(require 'fpga-utils)
(require 'fpga-xilinx)
(require 'fpga-altera)
(require 'fpga-lattice)
(require 'fpga-synopsys)

(provide 'fpga)

;;; fpga.el ends here
