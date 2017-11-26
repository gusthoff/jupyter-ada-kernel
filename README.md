Minimal Ada kernel for Jupyter
==============================

Introduction
---------------

This Package contains a Jupyter kernel for the Ada language. The
[Jupyter C kernel](https://github.com/brendan-rius/jupyter-c-kernel) was used
as a starting point for this implementation.


License & Copyright
----------------------

This Package is available "as is" under MIT License. The copyright of the
Jupyter C kernel --- used as a starting point for this implementation --- is
held by Brendan Rius. Unless stated otherwise, the copyright of the Ada kernel
for Jupyter is held by Gustavo A. Hoffmann.


Supported Platforms
----------------------

This Package has been tested on the following compilers / platforms:

- Linux / Python 3.6
- Windows / Python 3.6


Features
-----------

- Support for Ada language in Jupyter notebooks
- Support for running compiler and linker (GNAT) in the background
- Support for specifying multiple source-code files in the same notebook


Known Issues and Limitations
-------------------------------

### Platforms

- This Package has not been tested on the Mac platform.

### Features

- Syntax highlighting requires manual installation.

### Documentation

- Extensive documentation and tutorials are missing.


Installation
---------------

Details about the installation can be found
[on this document](Installation.md).


Usage
--------

Please refer to the Jupyter documentation for details on how to create and use
notebooks.

### Entering and running Ada code

In order to use the Ada kernel, the first step is to select "Ada" from the list
of kernels supported by the Jupyter installation. As soon as the kernel has
started, you may enter Ada code by specifying the filename in the comments:

```ada
--% filename: main.adb

with Ada.Text_IO;

procedure Main is
begin
    Ada.Text_IO.Put_Line("Hello World");
end Main;
```

The filename is required for creating the source-code file for the GNAT
compiler. Click on 'Run' on the toolbar to compile the code.

In order to build and run the binary based on the procedure _Main_, enter
the following line and click on 'Run' on the toolbar:

```ada
--% run: Main
```

Alternatively, for the procedure _Main_, you may enter Ada code, build and run
the binary in the same cell:

```ada
--% run_file: main.adb

with Ada.Text_IO;

procedure Main is
begin
    Ada.Text_IO.Put_Line("Hello World");
end Main;
```

### Output formats

By default, the standard output of the binary will be displayed as raw (plain)
text. However, this mode can be explicitly specified:

```ada
--% run_file: main.adb
--% output: raw

with Ada.Text_IO;

procedure Main is
begin
    Ada.Text_IO.Put_Line("Hello World");
end Main;
```

The standard output can also be displayed in HTML format:

```ada
--% run_file: main.adb
--% output: text/html

with Ada.Text_IO;

procedure Main is
begin
    Ada.Text_IO.Put_Line("<b>Hello World</b>");
end Main;
```

The standard output can also be displayed in Markdown format:

```ada
--% run_file: main.adb
--% output: text/markdown

with Ada.Text_IO;

procedure Main is
begin
    Ada.Text_IO.Put_Line("# Hello World");
end Main;
```

Finally, the format to be used can also be specified directly in the Ada
application. In this case, the output must use the JSON representation
required by `display_data`, which is used by IPython. Details about this can
be found in the
[IPython documentation](https://ipython.org/ipython-doc/3/notebook/nbformat.html#display-data).


### Example of notebook

This Package contains an [example of a notebook](examples/Hello_World.ipynb):

![Pic of notebook](examples/Hello_World.png)

The [HTML output notebook](examples/Hello_Html.ipynb) contains examples of HTML
and Markdown output. It also contains an example that makes use of the internal
JSON representation.
