[![MELPA](https://melpa.org/packages/fpga-badge.svg)](https://melpa.org/#/fpga)
[![MELPA Stable](https://stable.melpa.org/packages/fpga-badge.svg)](https://stable.melpa.org/#/fpga)
[![Build Status](https://github.com/gmlarumbe/fpga/workflows/elisp-check/badge.svg)](https://github.com/gmlarumbe/fpga/actions/workflows/elisp-check.yml)

# fpga.el - FPGA & ASIC Utils for Emacs #

This package provides Emacs utilities for FPGA & ASIC tools of major vendors and open source:

* Xilinx, Altera, Lattice, Cadence, Siemens, Synopsys and Yosys tools
* Synthesis/simulation compilation modes for error regexp matching:
   * Colorize error codes and jump to files where errors appeared
* Interactive shells with syntax highlighting and auto-completion
* Major-modes with syntax highlighting and auto-completion:
  * Vivado XDC major-mode
  * Quartus SDC and QSF major-modes
  * Cadence vManager VSIF major-mode
  * Yosys script major-mode
* Global Gtags creation from files in Vivado XPR and Quartus QPF project files
* And some others...

## Installation ##

### MELPA ###

`fpga` is available on MELPA.

### straight.el ###

To install it via [straight](https://github.com/radian-software/straight.el) with `use-package`:

```emacs-lisp
(straight-use-package 'use-package)
(use-package fpga)
```

## Basic config ##

First set which vendors you want tools for and then load the package.

For example, if you need tools for Xilinx/Cadence:

```emacs-lisp
(setq fpga-feature-list '(xilinx cadence))
(require 'fpga)
```

If you need tools for Altera/Siemens:
```emacs-lisp
(setq fpga-feature-list '(altera siemens))
(require 'fpga)
```

With `use-package`:

```emacs-lisp
(use-package fpga
  :init
  (setq fpga-feature-list '(xilinx cadence)))
```

## Features ##

### Synthesis/Simulation compilation functions and modes ###

Compilation functions with their corresponding regexps are provided for each vendor tool:

  * `fpga-xilinx-vivado-compile`
  * `fpga-altera-quartus-compile`
  * `fpga-lattice-diamond-compile`
  * `fpga-cadence-xrun-compile`
  * `fpga-siemens-vsim-compile`
  * `fpga-synopsys-synplify-compile`
  * `fpga-yosys-compile`

These can be used as follows:

``` emacs-lisp
;; Assumes that you are working with Makefiles and that there is a target named
;; `vivado' that runs synthesis/simulation
(fpga-xilinx-vivado-compile "make vivado")
```

The package also provides compilation modes for each tool:

  * `fpga-xilinx-vivado-compilation-mode`
  * `fpga-altera-quartus-compilation-mode`
  * `fpga-lattice-diamond-compilation-mode`
  * `fpga-cadence-xrun-compilation-mode`
  * `fpga-siemens-vsim-compilation-mode`
  * `fpga-synopsys-synplify-compilation-mode`
  * `fpga-yosys-compilation-mode`

These are used by the package to define functions that perform synthesis/simulation compilations.
For example, `M-x fpga-xilinx-vivado-syn RET` will prompt the user for an XPR project file.
Once selected, a Vivado compilation with error message colorized will take place.

As an example, the snippet below has a similar effect as the previous
one for Vivado synthesis/simulation:

``` emacs-lisp
(compile "make vivado")
(fpga-xilinx-vivado-compilation-mode) ; Runs in the *compilation* buffer
```


##### Demo video #####

https://github.com/gmlarumbe/fpga/assets/51021955/e9b59d83-ae78-458a-bf48-360d98bdcef2


### Improved interactive shells ###

Shells with syntax highlighting and autocompletion are provided for the following vendors:

  * `fpga-xilinx-vivado-shell`
  * `fpga-altera-quartus-shell`
  * `fpga-lattice-diamond-shell`
  * `fpga-synopsys-synplify-shell`
  * `fpga-yosys-shell`

##### Demo video #####

https://github.com/gmlarumbe/fpga/assets/51021955/c4be8ebe-26a7-44a3-afe7-82c6928df6f4


### Major-modes ###

The following major modes are provided to edit constraints and project files:

  * `fpga-xilinx-vivado-xdc-mode`
  * `fpga-altera-quartus-sdc-mode` and `fpga-altera-quartus-qsf-mode`
  * `fpga-cadence-vsif-mode`
  * `fpga-yosys-ys-mode`

##### Vivado XDC Mode screenshot #####

<img src="https://github.com/gmlarumbe/fpga/assets/51021955/b9f622c7-9a34-43ad-8323-00157efdb3c5" width=100%>

### Global Gtags creation from project files ###

Running `M-x fpga-xilinx-vivado-tags RET` or `M-x fpga-altera-quartus-tags RET` will prompt for a project file.
It will be parsed and a `gtags.files` will be generated in the selected directory. This file will later be used to gather tags for the project files.

One of the uses of this feature could be filtering out unused files for definitions/references navigation.
It can also be useful to generate the list of files used in a project for further hierarchy extraction.

## Other packages

* [verilog-ts-mode](https://github.com/gmlarumbe/verilog-ts-mode): SystemVerilog Tree-sitter mode
* [vhdl-ts-mode](https://github.com/gmlarumbe/vhdl-ts-mode): VHDL Tree-sitter mode
* [verilog-ext](https://github.com/gmlarumbe/verilog-ext): SystemVerilog Extensions
* [vhdl-ext](https://github.com/gmlarumbe/vhdl-ext): VHDL Extensions
* [wavedrom-mode](https://github.com/gmlarumbe/wavedrom-mode): edit and render WaveJSON files to create timing diagrams
* [vunit-mode](https://github.com/embed-me/vunit-mode.git): Integration of [VUnit](https://github.com/VUnit/vunit) workflow

