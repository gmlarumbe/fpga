;;; fpga-synopsys.el --- FPGA & ASIC Utils for Synopsys  -*- lexical-binding: t -*-

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

;; FPGA/ASIC Synopsys Utilities
;;  - Improved Synopsys shell with syntax highlighting
;;  - Compilation mode with syntax highlighting for Synplify
;;  - Compilation regexps for design compiler (for potential extensions)
;;
;;; Code:

(require 'fpga-utils)


;;;; Custom
(defgroup fpga-synopsys nil
  "FPGA Synopsys customization."
  :group 'fpga)

(defcustom fpga-synopsys-synplify-bin (executable-find "synplify_premier")
  "Path to Synplify executable."
  :type 'string
  :group 'fpga-synopsys)

(defcustom fpga-synopsys-synplify-cmd-opts '("-shell" "-licensetype synplifypremierdp" "-verbose_log")
  "Synplify process options."
  :type '(repeat string)
  :group 'fpga-synopsys)

(defcustom fpga-synopsys-synplify-buf "*synplify*"
  "Buffer to use for Synplify compilation process."
  :type 'string
  :group 'fpga-synopsys)

(defcustom fpga-synopsys-synplify-shell-buf "*synplify-shell*"
  "Buffer to use for Synplify interactive shell process."
  :type 'string
  :group 'fpga-synopsys)


;;;; Internal
(defconst fpga-synopsys-synplify--base-cmd
  (concat fpga-synopsys-synplify-bin " " (mapconcat #'identity fpga-synopsys-synplify-cmd-opts " ")))


;;;; Compilation
(defconst fpga-synopsys-synplify-compile-re
  '((synp-error     "^@\\(?1:E\\): \\(?2:[A-Z0-9]+\\) :\"\\(?3:[0-9a-zA-Z./_-]+\\)\":\\(?4:[0-9]+\\):\\(?5:[0-9]+\\):" 3 4 5 2 nil (1 compilation-error-face) (2 fpga-synplify-compile-gray-face))
    (synp-error2    "^@\\(?1:E\\): \\(?2:[A-Z0-9]+\\) [:]?|" 1 nil nil 2 nil (2 fpga-synplify-compile-gray-face))
    (synp-error3    "^@\\(?1:E\\):" 1 nil nil 2 nil)
    (synp-warning   "^@\\(?1:W\\): \\(?2:[A-Z0-9]+\\) :\"\\(?3:[0-9a-zA-Z./_-]+\\)\":\\(?4:[0-9]+\\):\\(?5:[0-9]+\\):" 3 4 5 1 nil (1 compilation-warning-face) (2 fpga-synplify-compile-gray-face))
    (synp-warning2  "^@\\(?1:W\\): \\(?2:[A-Z0-9]+\\) [:]?|" 1 nil nil 1 nil (2 fpga-synplify-compile-gray-face))
    (synp-warning3  "^@\\(?1:W\\):" 1 nil nil 1 nil)
    (synp-note      "^@\\(?1:N\\): \\(?2:[A-Z0-9]+\\) :\"\\(?3:[0-9a-zA-Z./_-]+\\)\":\\(?4:[0-9]+\\):\\(?5:[0-9]+\\):" 3 4 5 0 nil (1 compilation-info-face) (2 fpga-synplify-compile-gray-face))
    (synp-note2     "^@\\(?1:N\\): \\(?2:[A-Z0-9]+\\) [:]?|" 1 nil nil 0 nil (2 fpga-synplify-compile-gray-face))
    ;; Did not find what those meant online, so set as warnings
    (synp-alt-info  "^@\\(?1:A\\): \\(?2:[A-Z0-9]+\\) :\"\\(?3:[0-9a-zA-Z./_-]+\\)\":\\(?4:[0-9]+\\):\\(?5:[0-9]+\\):" 3 4 5 0 nil (1 compilation-info-face) (2 fpga-synplify-compile-gray-face))
    (synp-alt-info2 "^@\\(?1:A\\): \\(?2:[A-Z0-9]+\\) [:]?|" 1 nil nil 0 nil (2 fpga-synplify-compile-gray-face))
    (synp-alt-info3 "^@\\(?1:A\\):" 1 nil nil 0 nil)
    (synp-note3     "^@\\(?1:N\\):" 1 nil nil 0 nil)
    (synp-info      "^@\\(?1:I\\):" nil nil nil 0 nil (1 compilation-line-face))
    (synp-log       "^@\\(?1:L\\):" nil nil nil 0 nil (1 compilation-line-face)))
  "Synopsys Synplify regexps.")

(fpga-utils-define-compilation-mode fpga-synopsys-synplify-compilation-mode
  :desc "Synplify"
  :docstring "Synplify Compilation mode."
  :compile-re fpga-synopsys-synplify-compile-re
  :buf-name fpga-synopsys-synplify-buf)

(fpga-utils-define-compile-fn fpga-synopsys-synplify-compile
  :docstring "Compile Synplify COMMAND with error regexp highlighting."
  :buf fpga-synopsys-synplify-buf
  :comp-mode fpga-synopsys-synplify-compilation-mode)


;;;; Synplify Shell
;;;###autoload (autoload 'fpga-synopsys-synplify-shell "fpga-synopsys.el")
(fpga-utils-define-shell-mode fpga-synopsys-synplify-shell
  :bin fpga-synopsys-synplify-bin
  :base-cmd fpga-synopsys-synplify--base-cmd
  :shell-commands nil ; No auto-completion/font-lock yet
  :comile-re fpga-synopsys-synplify-compile-re
  :buf fpga-synopsys-synplify-shell-buf
  :font-lock-kwds nil) ; No extra font-lock yet


;;;; Misc
;; Design Compiler
(defconst fpga-synopsys-design-compiler-compile-re
  '((dc-error     "\\(?1:^Error\\):  \\(?2:[0-9a-zA-Z./_-]+\\):\\(?3:[0-9]+\\): " 2 3 nil 2 nil (1 compilation-error-face))
    (dc-error-2   "\\(?1:^Error\\): .*" 1 nil nil 2 nil)
    (dc-warning   "\\(?1:^Warning\\):  \\(?2:[0-9a-zA-Z./_-]+\\):\\(?3:[0-9]+\\): " 2 3 nil 1 nil (1 compilation-warning-face))
    (dc-warning-2 "\\(?1:^Warning\\): .*" 1 nil nil 1 nil)
    (dc-info      "\\(?1:^Information\\):  \\(?2:[0-9a-zA-Z./_-]+\\):\\(?3:[0-9]+\\): " 2 3 nil 0 nil (1 compilation-info-face))
    (dc-info-2    "\\(?1:^Information\\): .*" 1 nil nil 0 nil))
  "Synopsys Design Compiler regexps.")


(provide 'fpga-synopsys)

;;; fpga-synopsys.el ends here
