;;; fpga.el --- FPGA & ASIC Utils -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Gonzalo Larumbe

;; Author: Gonzalo Larumbe <gonzalomlarumbe@gmail.com>
;; URL: https://github.com/gmlarumbe/fpga
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "29.1"))

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
;;
;; This package provides Emacs utilities for FPGA & ASIC tools of major vendors and open source:
;;
;; - Xilinx, Altera, Lattice, Cadence, Siemens, Synopsys and Yosys tools
;;
;; - Synthesis/simulation compilation modes for error regexp matching:
;;    - Colorize error codes and jump to files where errors appeared
;; - Interactive shells with syntax highlighting and auto-completion
;;
;; - Major-modes with syntax highlighting and auto-completion:
;;   - Vivado XDC major-mode
;;   - Quartus SDC and QSF major-modes
;;   - Cadence vManager VSIF major-mode
;;   - Yosys script major-mode
;;
;; - Global Gtags creation from files in Vivado XPR and Quartus QPF project files
;;
;; - And some others...
;;
;;; Code:


;;;; Customization
(defgroup fpga nil
  "FPGA/ASIC Utils."
  :group 'tools)

(defcustom fpga-feature-list '(xilinx altera lattice cadence siemens synopsys yosys)
  "Which FPGA/ASIC features to load."
  :type '(set (const :tag "AMD/Xilinx Vivado tools."
                xilinx)
              (const :tag "Intel/Altera Quartus tools."
                altera)
              (const :tag "Lattice Diamond tools."
                lattice)
              (const :tag "Cadence tools."
                synopsys)
              (const :tag "Siemens tools."
                synopsys)
              (const :tag "Synopsys tools."
                synopsys)
              (const :tag "Yosys tools."
                yosys))
  :group 'fpga)


;;;; Func/macros
(defmacro fpga-when-feature (features &rest body)
  "Macro to run BODY if `fpga' feature is enabled.
FEATURES can be a single feature or a list of features."
  (declare (indent 1) (debug 1))
  `(let (enabled)
     (if (listp ,features)
         (dolist (feature ,features)
           (when (member feature fpga-feature-list)
             (setq enabled t)))
       ;; Else
       (when (member ,features fpga-feature-list)
         (setq enabled t)))
     (when enabled
       ,@body)))


;;;; Core
(require 'fpga-utils)

(fpga-when-feature 'xilinx
  (require 'fpga-xilinx))

(fpga-when-feature 'altera
  (require 'fpga-altera))

(fpga-when-feature 'lattice
  (require 'fpga-lattice))

(fpga-when-feature 'cadence
  (require 'fpga-cadence))

(fpga-when-feature 'siemens
  (require 'fpga-siemens))

(fpga-when-feature 'synopsys
  (require 'fpga-synopsys))

(fpga-when-feature 'yosys
  (require 'fpga-yosys))


;;;; Provide
(provide 'fpga)

;;; fpga.el ends here
