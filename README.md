Minimal Ada kernel for Jupyter
==============================

1. Introduction
---------------

This Package contains a Jupyter kernel for the Ada language. The
[Jupyter C kernel](https://github.com/brendan-rius/jupyter-c-kernel) was used
as a starting point for this implementation.


2. License & Copyright
----------------------

This Package is available "as is" under MIT License. The copyright of the
Jupyter C kernel --- used as a starting point for this implementation --- is
held by Brendan Rius. Unless stated otherwise, the copyright of the Ada kernel
for Jupyter is held by Gustavo A. Hoffmann.


2. Supported Platforms
----------------------

This Package has been tested on the following compilers / platforms:

- Linux / Python 3.6
- Windows / Python 3.6


3. Features
-----------

- Support for Ada language in Jupyter notebooks
- Support for running compiler and linker (GNAT) in the background
- Support for specifying multiple source-code files in the same notebook


4. Known Issues and Limitations
-------------------------------

### Platforms

- This Package has not been tested on the Mac platform.

### Features

- Syntax highlighting requires manual installation.

### Documentation

- Extensive documentation and tutorials are missing.


5. Installation
---------------

### Requirements

- Make sure that the following packages are installed:
    - gnat
    - python 3
    - pip
    - jupyter
    - jupyter_utils
    - jupyter_client

### Ada kernel

In order to install this package, download it and use pip:

```bash
cd jupyter-ada-kernel
sudo pip install -e `pwd`
sudo install_ada_kernel
```

Use similar command-lines for Windows:

```
pip install -e C:\Users\JohnSmith\Downloads\jupyter-ada-kernel-0.1.0
cd C:\Users\JohnSmith\Downloads\jupyter-ada-kernel-0.1.0\jupyter_ada_kernel
python install_ada_kernel
```

### Ada module for CodeMirror

Jupyter makes use of [CodeMirror](https://codemirror.net/) for syntax
highlighting. Unfortunately, CodeMirror does not support Ada, and the project
maintainers do not accept adding support for new languages. Therefore, in order
to get syntax highlighting for Ada in Jupyter notebooks, the current solution
is to manually install the required files:

- Install Ada mode

    - Locate CodeMirror installation for Jupyter
        - For example, if you use Python 3.6, this is the corresponding location on Linux:

        ```bash
        /usr/local/lib/python3.6/dist-packages/notebook/static/components/codemirror/
        ```

        - On Windows, the location also depends on the environment. For example:

        ```
        C:\Users\JohnSmith\myenv\Lib\site-packages\notebook\static\components\codemirror\
        ```

    - Install Ada mode into CodeMirror module:

        - On Linux:

        ```bash
            cp -r ./jupyter-ada-kernel/codemirror/mode/ada /usr/local/lib/python3.6/dist-packages/notebook/static/components/codemirror/mode/
        ```

        - On Windows:

        ```
            cd C:\Users\JohnSmith\myenv\Lib\site-packages\notebook\static\components\codemirror\mode

            xcopy C:\Users\JohnSmith\Downloads\jupyter-ada-kernel-0.1.0\codemirror\mode\ada .\ada /e /i /h
        ```

        - Make sure to use the correct directory!

- Add Ada mode to the list

    - Locate the file containing the list of modes supported by CodeMirror
        - For example, if you use Python 3.6, this is the corresponding location:

        ```bash
        /usr/local/lib/python3.6/dist-packages/notebook/static/components/codemirror/mode/meta.js
        ```

    - Add the following line to the the list of modes supported by CodeMirror:

        ```javascript
            {name: "Ada", mime: "text/x-ada", mode: "ada", ext: ["ads", "adb", "ada"]},
        ```

    - Alternativey, you may simply overwrite the file:

        ```
        cp ./jupyter-ada-kernel/codemirror/mode/meta.js /usr/local/lib/python3.6/dist-packages/notebook/static/components/codemirror/mode/meta.js
        ```


6. Usage
--------

Please refer to the Jupyter documentation for details on how to create and use
notebooks.

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

In order to build and run the executable based on the procedure _Main_, enter
the following line and click on 'Run' on the toolbar:

```ada
--% run: Main
```

### Example of notebook

This Package contains an [example of a notebook](examples/Hello_World.ipynb):

![Pic of notebook](examples/Hello_World.png)
