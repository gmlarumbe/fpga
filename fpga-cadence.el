;;; fpga-cadence.el --- FPGA Cadence Utils  -*- lexical-binding: t -*-

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

;; FPGA Cadence Utilities

;;; Code:


(require 'fpga-utils)

;; Leveraged from verilog-mode (verilog-IES) and extended for UVM/OVM
;;   - xrun-error: errors with line number and column number
;;   - xrun-error2: errors with line number
;;   - xrun-error3: errors without file/line
(defvar larumbe/compilation-error-re-xrun
  '((xrun-fatal    "\\(?1:^[a-z]+\\): \\(?2:\\*F\\),\\(?3:[0-9A-Z]+\\)\\(?:\\(?:\\[[0-9A-Z_,]+\\]\\)? (\\(?4:[^ \t,]+\\),\\(?5:[0-9]+\\)|\\(?6:[0-9]+\\)\\)" 4 5 6 2 nil (1 larumbe/compilation-binary-face) (2 compilation-error-face) (3 larumbe/compilation-gray-face))
    (xrun-fatal2   "\\(?1:^[a-z]+\\): \\(?2:\\*F\\),\\(?3:[0-9A-Z]+\\)\\(?:\\(?:\\[[0-9A-Z_,]+\\]\\)? (\\(?4:[^ \t,]+\\),\\(?5:[0-9]+\\)\\)" 4 5 nil 2 nil (1 larumbe/compilation-binary-face) (2 compilation-error-face) (3 larumbe/compilation-gray-face))
    (xrun-fatal3   "\\(?1:^[a-z]+\\): \\(?2:\\*F\\),\\(?3:[0-9A-Z]+\\):" 1 nil nil 2 nil (1 larumbe/compilation-binary-face) (2 compilation-error-face) (3 larumbe/compilation-gray-face))
    (xrun-error    "\\(?1:^[a-z]+\\): \\(?2:\\*E\\),\\(?3:[0-9A-Z]+\\)\\(?:\\(?:\\[[0-9A-Z_,]+\\]\\)? (\\(?4:[^ \t,]+\\),\\(?5:[0-9]+\\)|\\(?6:[0-9]+\\)\\)" 4 5 6 2 nil (1 larumbe/compilation-binary-face) (2 compilation-error-face) (3 larumbe/compilation-gray-face))
    (xrun-error2   "\\(?1:^[a-z]+\\): \\(?2:\\*E\\),\\(?3:[0-9A-Z]+\\)\\(?:\\(?:\\[[0-9A-Z_,]+\\]\\)? (\\(?4:[^ \t,]+\\),\\(?5:[0-9]+\\)\\)" 4 5 nil 2 nil (1 larumbe/compilation-binary-face) (2 compilation-error-face) (3 larumbe/compilation-gray-face))
    (xrun-error3   "\\(?1:^[a-z]+\\): \\(?2:\\*E\\),\\(?3:[0-9A-Z]+\\):" 1 nil nil 2 nil (1 larumbe/compilation-binary-face) (2 compilation-error-face) (3 larumbe/compilation-gray-face))
    (xrun-warning  "\\(?1:^[a-z]+\\): \\(?2:\\*W\\),\\(?3:[0-9A-Z]+\\)\\(?:\\(?:\\[[0-9A-Z_,]+\\]\\)? (\\(?4:[^ \t,]+\\),\\(?5:[0-9]+\\)|\\(?6:[0-9]+\\)\\)" 4 5 6 1 nil (1 larumbe/compilation-binary-face) (2 compilation-warning-face) (3 larumbe/compilation-gray-face))
    (xrun-warning2 "\\(?1:^[a-z]+\\): \\(?2:\\*W\\),\\(?3:[0-9A-Z]+\\)\\(?:\\(?:\\[[0-9A-Z_,]+\\]\\)? (\\(?4:[^ \t,]+\\),\\(?5:[0-9]+\\)\\)" 4 5 nil 1 nil (1 larumbe/compilation-binary-face) (2 compilation-warning-face) (3 larumbe/compilation-gray-face))
    (xrun-warning3 "\\(?1:^[a-z]+\\): \\(?2:\\*W\\),\\(?3:[0-9A-Z]+\\):" 1 nil nil 1 nil (1 larumbe/compilation-binary-face) (2 compilation-warning-face) (3 larumbe/compilation-gray-face))
    (xrun-note     "\\(?1:^[a-z]+\\): \\(?2:\\*N\\),\\(?3:[0-9A-Z]+\\)\\(?:\\(?:\\[[0-9A-Z_,]+\\]\\)? (\\(?4:[^ \t,]+\\),\\(?5:[0-9]+\\)|\\(?6:[0-9]+\\)\\)" 4 5 6 0 nil (1 larumbe/compilation-binary-face) (2 compilation-info-face) (3 larumbe/compilation-gray-face))
    (xrun-note2    "\\(?1:^[a-z]+\\): \\(?2:\\*N\\),\\(?3:[0-9A-Z]+\\)\\(?:\\(?:\\[[0-9A-Z_,]+\\]\\)? (\\(?4:[^ \t,]+\\),\\(?5:[0-9]+\\)\\)" 4 5 nil 0 nil (1 larumbe/compilation-binary-face) (2 compilation-info-face) (3 larumbe/compilation-gray-face))
    (xrun-note3    "\\(?1:^[a-z]+\\): \\(?2:\\*N\\),\\(?3:[0-9A-Z]+\\):" 1 nil nil 0 nil (1 larumbe/compilation-binary-face) (2 compilation-info-face) (3 larumbe/compilation-gray-face))))



(provide 'fpga-cadence)

;;; fpga-cadence.el ends here

