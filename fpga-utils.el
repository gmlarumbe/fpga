;;; fpga-utils.el --- FPGA & ASIC Utils  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Gonzalo Larumbe

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

;; FPGA/ASIC Common Utils

;;; Code:

(require 'compile)


;;;; Custom
(defcustom fpga-utils-source-extension-re (concat "\\." (regexp-opt '("sv" "svh" "v" "vh" "vhd" "vhdl")) "\\'")
  "FPGA source file extension regexp."
  :type 'string
  :group 'fpga)

(defcustom fpga-utils-shell-file-completion t
  "Wheter to try completion with files in shells."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t))
  :group 'fpga)


;;;; Faces
(defgroup fpga-faces nil
  "FPGA/ASIC faces."
  :group 'fpga)

(defconst fpga-utils-compilation-msg-code-face 'fpga-utils-compilation-msg-code-face)
(defface fpga-utils-compilation-msg-code-face
  '((t :inherit font-lock-comment-face))
  "Face for compilation message codes."
  :group 'fpga-faces)

(defconst fpga-utils-compilation-bin-face 'fpga-utils-compilation-bin-face)
(defface fpga-utils-compilation-bin-face
  '((t :inherit font-lock-function-name-face))
  "Face for compilation binaries."
  :group 'fpga-faces)

(defvar fpga-utils-brackets-face 'fpga-utils-brackets-face)
(defface fpga-utils-brackets-face
  '((t :inherit font-lock-bracket-face))
  "Face for brackets []."
  :group 'fpga-faces)

(defvar fpga-utils-parenthesis-face 'fpga-utils-parenthesis-face)
(defface fpga-utils-parenthesis-face
  '((t :inherit font-lock-bracket-face))
  "Face for parenthesis ()."
  :group 'fpga-faces)

(defvar fpga-utils-curly-braces-face 'fpga-utils-curly-braces-face)
(defface fpga-utils-curly-braces-face
  '((t :inherit font-lock-bracket-face))
  "Face for curly braces {}."
  :group 'fpga-faces)

(defvar fpga-utils-braces-content-face 'fpga-utils-braces-content-face)
(defface fpga-utils-braces-content-face
  '((t :inherit font-lock-number-face))
  "Face for content between braces: arrays, bit vector width and indexing."
  :group 'fpga-faces)

(defvar fpga-utils-punctuation-face 'fpga-utils-punctuation-face)
(defface fpga-utils-punctuation-face
  '((t :inherit font-lock-punctuation-face))
  "Face for punctuation symbols, e.g:
!,;:?'=<>*"
  :group 'fpga-faces)


;;;; Constants
(defconst fpga-utils-brackets-re "\\(\\[\\|\\]\\)")
(defconst fpga-utils-parenthesis-re "[()]")
(defconst fpga-utils-curly-braces-re "[{}]")
(defconst fpga-utils-braces-content-re "\\[\\(?1:[0-9]+\\)\\]")
(defconst fpga-utils-punctuation-re "\\([!,;:?'=<>&^~%\+-]\\|\\*\\|\\.\\|\\/\\|\|\\)")


;;;; Functions
(defun fpga-utils-write-file-from-filelist (outfile filelist)
  "Create OUTFILE with one file of FILELIST per line."
  (with-temp-file outfile
    (dolist (line filelist)
      (insert line "\n"))))

(defun fpga-utils-tags-create (out-dir in-file file-list-fn)
  "Generate tags from filelist.

Tags will be generated in OUT-DIR from the project file of IN-FILE (xpr/qsf).

Third parameter FILE-LIST-FN is the used function to create gtags.files from
IN-FILE."
  (interactive "DOutput dir: \nFInput file: ")
  (unless (executable-find "gtags")
    (error "Error: gtags not available in the PATH"))
  (let* ((gtags-file-name "gtags.files")
         (gtags-file-path (file-name-concat out-dir gtags-file-name))
         (gtags-cmd (mapconcat #'identity `("cd" ,out-dir "&&" "gtags" "-v") " "))
         (gtags-buf "*fpga-gtags*"))
    (fpga-utils-write-file-from-filelist gtags-file-path (funcall file-list-fn in-file))
    (async-shell-command gtags-cmd gtags-buf gtags-buf)))

(defun fpga-utils-shell-delchar-or-maybe-eof (num-chars)
  "Delete character or exit shell.
With `prefix-arg', delete NUM-CHARS characters."
  (interactive "p")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and (eobp)
             (save-excursion
               (skip-chars-backward " ")
               (member (preceding-char) '(?% ?>))))
        (comint-send-string proc "exit\n")
      (delete-char num-chars))))

(cl-defmacro fpga-utils-define-compilation-mode (name &key desc docstring compile-re buf-name)
  "Macro to define a compilation derived mode for a FPGA error regexp.

NAME is the name of the created compilation mode.

The compilation-derived mode will be passed key args DESC and DOCSTRING for
documentation.

COMPILE-RE is be used to map `compilation-error-regexp-alist' and
`compilation-error-regexp-alist-alist'.

BUF-NAME determines the name of the compilation buffer."
  (declare (indent 1) (debug 1))
  `(define-compilation-mode ,name ,desc ,docstring
     (setq-local compilation-error-regexp-alist (mapcar #'car ,compile-re))
     (setq-local compilation-error-regexp-alist-alist ,compile-re)
     (rename-buffer ,buf-name)
     (setq truncate-lines t)
     (goto-char (point-max))))

(cl-defmacro fpga-utils-define-compile-fn (name &key docstring buf comp-mode)
  "Macro to define a function to compile with error regexp highlighting.

DOCSTRING is passed to created function named NAME to document its purpose.

BUF is the name of the used buffer.

COMP-MODE is the name of the compilation derived mode created by macro
`fpga-utils-define-compilation-mode'."
  (declare (indent 1) (debug 1))
  `(defun ,name (command)
     ,docstring
     (when (get-buffer ,buf)
       (if (y-or-n-p (format "Buffer %s is in use, kill its process and start new compilation?" ,buf))
           (kill-buffer ,buf)
         (user-error "Aborted")))
     (compile command)
     (,comp-mode)))

(cl-defmacro fpga-utils-define-shell-mode (name &key bin base-cmd shell-commands compile-re buf font-lock-kwds)
  "Define shell mode named NAME.

BIN is the name of the binary used to run the shell mode.

BASE-CMD is the basic command (binary plus always used optional switches) for
the shell process.

SHELL-COMMANDS is a list of strings with the shell commands, used for font-lock
and completion.

COMPILE-RE is the compilation regexp, same as the one passed to
`fpga-utils-define-compilation-mode'.

BUF is the name of the used buffer.

FONT-LOCK-KWDS determine syntax highlighting for the shell mode."
  (declare (indent 1) (debug 1))
  (let ((mode-fn (intern (concat (symbol-name name) "-mode")))
        (capf-fn (intern (concat (symbol-name name) "-capf")))
        (mode-map (intern (concat (symbol-name name) "-mode-map")))
        (send-line-or-region-fn (intern (concat (symbol-name name) "-send-line-or-region-and-step")))
        (mode-hook (intern (concat (symbol-name name) "-mode-hook"))))

    ;; First define a function for `completion-at-point-functions'
    `(progn
       (defun ,capf-fn ()
         "Completion at point for shell mode."
         (cond (;; Files
                (let* ((comp (thing-at-point 'filename :no-props))
                       (dir (when comp (file-name-directory comp)))
                       (dir-exists (when dir (file-exists-p dir))))
                  (and fpga-utils-shell-file-completion
                       dir-exists))
                (comint-filename-completion))
               (;; Default
                (let* ((line-cmds (split-string (buffer-substring-no-properties (comint-line-beginning-position) (point)) " "))
                       (num-line-cmds (length line-cmds)))
                  (and (>= (point) (comint-line-beginning-position))
                       (eq num-line-cmds 1)))
                (list (save-excursion (skip-chars-backward "/\.a-zA-Z0-9_-") (point))
                      (point)
                      ,shell-commands))))

       ;; Define mode-map
       (defvar ,mode-map
         (let ((map (make-sparse-keymap)))
           (define-key map (kbd "C-d") 'fpga-utils-shell-delchar-or-maybe-eof)
           map)
         "Keymap.")

       ;; Define minor mode for shell
       (define-minor-mode ,mode-fn
         "Shell mode."
         :global nil
         (setq-local compilation-error-regexp-alist (mapcar #'car ,compile-re))
         (setq-local compilation-error-regexp-alist-alist ,compile-re)
         (rename-buffer ,buf)
         (setq truncate-lines t)
         (goto-char (point-max))
         (setq-local comint-dynamic-complete-functions '(,capf-fn)))

       ;; Defin shell function
       (defun ,name ()
         "Spawn an improved shell.
Enables auto-completion and syntax highlighting."
         (interactive)
         (unless ,bin
           (error ,(concat "Could not find " (symbol-name bin) " in $PATH.'")))
         (when (get-buffer ,buf)
           (if (y-or-n-p (format "Buffer %s is in use, kill process and start new shell?" ,buf))
               (kill-buffer ,buf)
             (user-error "Aborted")))
         (let* ((cmd ,base-cmd)
                buf)
           (setq buf (compile cmd t))
           (with-current-buffer buf
             (,mode-fn))))

       ;; Define a shell send line function, meant to be used in tcl buffers
       (defun ,send-line-or-region-fn ()
         "Send the current line to the its shell and step to the next line.
When the region is active, send the region instead."
         (interactive)
         (let (from to end (proc (get-buffer-process ,buf)))
           (if (use-region-p)
               (setq from (region-beginning)
                     to (region-end)
                     end to)
             (setq from (line-beginning-position)
                   to (line-end-position)
                   end (1+ to)))
           (comint-send-string proc (buffer-substring-no-properties from to))
           (comint-send-string proc "\n")
           (goto-char end)))

       ;; Add font-lock keywords for extra syntax highlighting
       (when ,font-lock-kwds
         (add-hook ',mode-hook (lambda () (font-lock-add-keywords nil ,font-lock-kwds 'append)))))))


;;;; Compilation-re
(defvar fpga-utils-compilation-uvm-re
  '((uvm-fatal    "^\\(?1:UVM_FATAL\\) \\(?2:[a-zA-Z0-9\./_-]+\\)(\\(?3:[0-9]+\\))"   2 3 nil 2 nil (1 compilation-error-face))
    (uvm-fatal2   "^\\(?1:UVM_FATAL\\) @"   1 nil nil 2 nil)
    (uvm-error    "^\\(?1:UVM_ERROR\\) \\(?2:[a-zA-Z0-9\./_-]+\\)(\\(?3:[0-9]+\\))"   2 3 nil 2 nil (1 compilation-error-face))
    (uvm-error2   "^\\(?1:UVM_ERROR\\) @"   1 nil nil 2 nil)
    (uvm-warning  "^\\(?1:UVM_WARNING\\) \\(?2:[a-zA-Z0-9\./_-]+\\)(\\(?3:[0-9]+\\))" 2 3 nil 1 nil (1 compilation-warning-face))
    (uvm-warning2 "^\\(?1:UVM_WARNING\\) @" 1 nil nil 1 nil)
    (uvm-info     "^\\(?1:UVM_INFO\\) \\(?2:[a-zA-Z0-9\./_-]+\\)(\\(?3:[0-9]+\\))"    2 3 nil 0 nil (1 compilation-info-face))
    (uvm-info2    "^\\(?1:UVM_INFO\\) @"    1 nil nil 0 nil)))

(defvar fpga-utils-compilation-ovm-re
  '((ovm-fatal    "^\\(?1:OVM_FATAL\\) @ \\(?2:[0-9]+\\): "   1 nil nil 2 nil (2 compilation-line-face))
    (ovm-error    "^\\(?1:OVM_ERROR\\) @ \\(?2:[0-9]+\\): "   1 nil nil 2 nil (2 compilation-line-face))
    (ovm-warning  "^\\(?1:OVM_WARNING\\) @ \\(?2:[0-9]+\\): " 1 nil nil 1 nil (2 compilation-line-face))
    (ovm-info     "^\\(?1:OVM_INFO\\) @ \\(?2:[0-9]+\\): "    1 nil nil 0 nil (2 compilation-line-face))))

(defconst fpga-utils-shell-switch-re "\\_<\\(?1:-\\)\\(?2:[-_a-zA-Z0-9_-]+\\)\\_>")



(provide 'fpga-utils)

;;; fpga-utils.el ends here
