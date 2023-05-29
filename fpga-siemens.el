;;; fpga-siemens.el --- FPGA Siemens Utils  -*- lexical-binding: t -*-

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

;; FPGA/ASIC Utilities for Siemens/Mentor QuestaSim/ModelSim
;;  - Compilation with error regexp matching
;;
;;; Code:


(require 'fpga-utils)

;;;; Custom
(defgroup fpga-siemens nil
  "FPGA Siemens customization."
  :group 'fpga)

(defcustom fpga-siemens-vsim-buf "*vsim*"
  "Buffer to use for QuestaSim/ModelSim compilation process."
  :type 'string
  :group 'fpga-siemens)

;;;; Compilation-re
(defvar fpga-siemens-vsim-uvm-compile-re
  '((uvm-fatal    "^# \\(?1:UVM_FATAL\\) \\(?2:[a-zA-Z0-9\./_-]+\\)(\\(?3:[0-9]+\\))"   2 3 nil 2 nil (1 compilation-error-face))
    (uvm-fatal2   "^# \\(?1:UVM_FATAL\\) @"   1 nil nil 2 nil)
    (uvm-error    "^# \\(?1:UVM_ERROR\\) \\(?2:[a-zA-Z0-9\./_-]+\\)(\\(?3:[0-9]+\\))"   2 3 nil 2 nil (1 compilation-error-face))
    (uvm-error2   "^# \\(?1:UVM_ERROR\\) @"   1 nil nil 2 nil)
    (uvm-warning  "^# \\(?1:UVM_WARNING\\) \\(?2:[a-zA-Z0-9\./_-]+\\)(\\(?3:[0-9]+\\))" 2 3 nil 1 nil (1 compilation-warning-face))
    (uvm-warning2 "^# \\(?1:UVM_WARNING\\) @" 1 nil nil 1 nil)
    (uvm-info     "^# \\(?1:UVM_INFO\\) \\(?2:[a-zA-Z0-9\./_-]+\\)(\\(?3:[0-9]+\\))"    2 3 nil 0 nil (1 compilation-info-face))
    (uvm-info2    "^# \\(?1:UVM_INFO\\) @"    1 nil nil 0 nil)))

(defvar fpga-siemens-vsim-compile-re
  '(;; vlog
    (vlog-error   "^\\*\\* \\(?1:Error\\)\\( (suppressible)\\)?: \\(?2:[a-zA-Z0-9./_-]+\\)(\\(?3:[0-9]+\\)): \\(?4:([a-zA-Z0-9_-]+) \\)?" 2 3 nil 2 nil (1 compilation-error-face))
    (vlog-error2  "^\\*\\* \\(?1:Error\\)\\( (suppressible)\\)?: \\(?2:([a-zA-Z0-9_-]+) \\)?\\(?3:[a-zA-Z0-9./_-]+\\)(\\(?4:[0-9]+\\)): " 3 4 nil 2 nil (1 compilation-error-face) (2 larumbe/compilation-gray-face))
    (vlog-error3  "^\\*\\* \\(?1:Error\\)\\( (suppressible)\\)?: \\(?2:\\(([a-zA-Z0-9_-]+) \\)?\\*\\* while parsing file included at \\(?3:[a-zA-Z0-9./_-]+\\)\\)(\\(?4:[0-9]+\\))\n\\*\\* \\(?5:at\\) \\(?6:[a-zA-Z0-9./_-]+\\)(\\(?7:[0-9]+\\)): " 6 7 nil 2 nil (1 compilation-error-face) (2 larumbe/compilation-gray-face) (3 compilation-error-face) (4 compilation-line-face) (5 larumbe/compilation-gray-face))
    (vlog-error4  "^\\*\\* \\(?1:Error\\)\\( (suppressible)\\)?: \\(?2:([a-zA-Z0-9_-]+) \\)?" nil nil nil 2 nil (1 compilation-error-face) (2 larumbe/compilation-gray-face))
    (vlog-error5  "^\\*\\* \\(?1:Error\\): \\(?2:([a-zA-Z0-9_-]+) \\)?" nil nil nil 2 nil (1 compilation-error-face) (2 larumbe/compilation-gray-face))
    (vlog-error6  "^\\*\\* \\(?1:Error\\): " nil nil nil 2 nil (1 compilation-error-face))
    (vlog-warning "^\\*\\* \\(?1:Warning\\)\\( (suppressible)\\)?: \\(?2:[a-zA-Z0-9./_-]+\\)(\\(?3:[0-9]+\\)): \\(?4:([a-zA-Z0-9_-]+) \\)?" 2 3 nil 1 nil (1 compilation-warning-face))
    (vlog-warning2 "^\\*\\* \\(?1:Warning\\): \\(?2:([a-zA-Z0-9_-]+) \\)?" nil nil nil 1 nil (1 compilation-warning-face) (2 larumbe/compilation-gray-face))
    (vlog-warning3 "^\\*\\* \\(?1:Warning\\): " nil nil nil 1 nil (1 compilation-warning-face))
    (vlog-note "^\\*\\* \\(?1:Note\\): \\(?2:[a-zA-Z0-9./_-]+\\)(\\([0-9]+\\)): " 2 3 nil 0 nil (1 compilation-info-face))
    (vlog-note2 "^\\*\\* \\(?1:Note\\): " nil nil nil 0 nil (1 compilation-info-face))
    ;; vsim
    (vsim-fatal   "^# \\*\\* \\(?1:Fatal\\): \\(?2:([a-zA-Z0-9./_-]+)\\) .*\n#[ ]+\\(?3:Time: [0-9]+ [a-z]s  Iteration: [0-9]+  Instance: [/a-zA-Z0-9_-]+ \\)File: \\(?4:[a-zA-Z0-9./_-]+\\) Line: \\(?5:[0-9]+\\)" 4 5 nil 2 nil (1 compilation-error-face) (2 larumbe/compilation-gray-face) (3 larumbe/compilation-gray-face))
    (vsim-fatal2  "^# \\*\\* \\(?1:Fatal\\): \\(?2:([a-zA-Z0-9./_-]+)\\) \\(?3:[a-zA-Z0-9./_-]+\\)(\\(?4:[0-9]+\\)): " 3 4 nil 2 nil (1 compilation-error-face) (2 larumbe/compilation-gray-face))
    (vsim-fatal3  "^# \\*\\* \\(?1:Fatal\\): \\(?2:[a-zA-Z0-9./_-]+\\)(\\(?3:[0-9]+\\)): " 2 3 nil 2 nil (1 compilation-error-face))
    (vsim-fatal4  "^# \\*\\* \\(?1:Fatal\\): \\(?2:([a-zA-Z0-9./_-]+)\\) " nil nil nil 2 nil (1 compilation-error-face) (2 larumbe/compilation-gray-face))
    (vsim-fatal5  "^# \\*\\* \\(?1:Fatal\\): " nil nil nil 2 nil (1 compilation-error-face))
    (vsim-error   "^# \\*\\* \\(?1:Error\\): \\(?2:([a-zA-Z0-9./_-]+)\\) .*\n#[ ]+\\(?3:Time: [0-9]+ [a-z]s  Iteration: [0-9]+  Instance: [/a-zA-Z0-9_-]+ \\)File: \\(?4:[a-zA-Z0-9./_-]+\\) Line: \\(?5:[0-9]+\\)" 4 5 nil 2 nil (1 compilation-error-face) (2 larumbe/compilation-gray-face) (3 larumbe/compilation-gray-face))
    (vsim-error2  "^# \\*\\* \\(?1:Error\\( (suppressible)\\)?\\): \\(?2:([a-zA-Z0-9./_-]+)\\) \\(?3:[a-zA-Z0-9./_-]+\\)(\\(?4:[0-9]+\\)): " 3 4 nil 2 nil (1 compilation-error-face) (2 larumbe/compilation-gray-face))
    (vsim-error3  "^# \\*\\* \\(?1:Error\\( (suppressible)\\)?\\): \\(?2:([a-zA-Z0-9./_-]+)\\)" nil nil nil 2 nil (1 compilation-error-face) (2 larumbe/compilation-gray-face))
    (vsim-error4  "^# \\*\\* \\(?1:Error\\): \\(?2:([a-zA-Z0-9_-]+) \\)" nil nil nil 2 nil (1 compilation-error-face) (2 larumbe/compilation-gray-face))
    (vsim-error5  "^# \\*\\* \\(?1:Error\\): " nil nil nil 2 nil (1 compilation-error-face))
    (vsim-warning "^# \\*\\* \\(?1:Warning\\): \\(?2:([a-zA-Z0-9./_-]+) \[[A-Z]+\]\\) .*\n#[ ]+\\(?3:Time: [0-9]+ [a-z]s  Iteration: [0-9]+  Instance: [/a-zA-Z0-9_-]+ \\)File: \\(?4:[a-zA-Z0-9./_-]+\\) Line: \\(?5:[0-9]+\\)" 4 5 nil 1 nil (1 compilation-warning-face) (2 larumbe/compilation-gray-face) (3 larumbe/compilation-gray-face))
    (vsim-warning2 "^# \\*\\* \\(?1:Warning\\): \\(?2:([a-zA-Z0-9./_-]+)\\) .*\n#[ ]+\\(?3:Time: [0-9]+ [a-z]s  Iteration: [0-9]+  Region: [/a-zA-Z0-9_-]+ \\)File: \\(?4:[a-zA-Z0-9./_-]+\\)" 4 nil nil 1 nil (1 compilation-warning-face) (2 larumbe/compilation-gray-face) (3 larumbe/compilation-gray-face))
    (vsim-warning3 "^# \\*\\* \\(?1:Warning\\): \\(?2:([a-zA-Z0-9_-]+) \\)" nil nil nil 1 nil (1 compilation-warning-face) (2 larumbe/compilation-gray-face))
    (vsim-warning4 "^# \\*\\* \\(?1:Warning\\): " nil nil nil 1 nil (1 compilation-warning-face))
    (vsim-note "^# \\*\\* \\(?1:Note\\): \\(?2:([a-zA-Z0-9./_-]+)\\) " nil nil nil 0 nil (1 compilation-info-face) (2 larumbe/compilation-gray-face))
    (vsim-note2 "^# \\*\\* \\(?1:Note\\): " nil nil nil 0 nil (1 compilation-info-face))
    ;; VHDL severity
    (vsim-vhdl-failure "^# \\*\\* \\(?1:Failure\\): " nil nil nil 2 nil (1 compilation-error-face))
    (vsim-vhdl-error   "^# \\*\\* \\(?1:Error\\): "   nil nil nil 2 nil (1 compilation-error-face))
    (vsim-vhdl-warning "^# \\*\\* \\(?1:Warning\\): " nil nil nil 1 nil (1 compilation-warning-face))
    (vsim-vhdl-note    "^# \\*\\* \\(?1:Note\\): "    nil nil nil 0 nil (1 compilation-info-face))))

(fpga-utils-define-compilation-mode fpga-siemens-vsim-compilation-mode
  :desc "Vsim"
  :docstring "Vsim Compilation mode."
  :compile-re fpga-siemens-vsim-compile-re
  :buf-name fpga-siemens-vsim-buf)

(fpga-utils-define-compile-fn fpga-siemens-vsim-compile
  :docstring "Compile Vsim COMMAND with error regexp highlighting."
  :buf fpga-siemens-vsim-buf
  :comp-mode fpga-siemens-vsim-compilation-mode)


(provide 'fpga-siemens)

;;; fpga-siemens.el ends here

