Minimal Ada kernel for Jupyter
==============================

Installation
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
