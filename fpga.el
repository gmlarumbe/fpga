;;; fpga.el --- FPGA & ASIC Utils for Emacs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defvar fpga-source-extension-regex "\\(.sv$\\|.v$\\|.svh$\\|.vh$\\|.vhd$\\|.vhdl$\\)")

(require 'fpga-xilinx)
(require 'fpga-altera)
(require 'fpga-lattice)
(require 'fpga-synopsys)


(provide 'fpga)

;;; fpga.el ends here
