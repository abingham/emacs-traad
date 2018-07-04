# emacs-traad

An Emacs client for [traad](https://github.com/abingham/traad), a client-server
approach to using the [rope](https://github.com/python-rope/rope) Python
refactoring library.

With this client and the `traad` server, you can do Python refactorings from
Emacs. This client includes support for installing traad for you, so you
probably don't even need to think about installing the server separately.

## Quickstart

You can install the Emacs client for traad via `package.el`:

```
M-x package-install<RET>traad
```

Then you can use `traad-install-server` to install the Python server into a
default location::

```
M-x traad-install-server
```

The server is installed into a virtual environment using
[virtualenvwrapper.el](https://github.com/porterjamesj/virtualenvwrapper.el).
You can control this installation using **`traad-environment-name`** which
specifies the name of the virtual environment. By default it's "traad", and
there is often no need to change it. However, if you need to support multiple
installation of traad, you might need to modify this variable.

*For most users the default value should be fine. You really only need to
manipulate these variables if you're installing traad for more than one version
of Python.*

For more information on installation, see
[the Installation page](https://github.com/abingham/emacs-traad/wiki/Installation)
in [the wiki](https://github.com/abingham/emacs-traad/wiki).

For more information on using client, see
[the Usage page](https://github.com/abingham/emacs-traad/wiki/Usage) in
[the wiki](https://github.com/abingham/emacs-traad/wiki).

## Commands

### Refactoring Commands

| Command                         | Description                                                                                |
|---------------------------------|--------------------------------------------------------------------------------------------|
| `traad-rename`                  | Rename the object at the current location.                                                 |
| `traad-rename-module`           | Rename the currently opened module.                                                        |
| `traad-move`                    | Move the current object (DWIM).                                                            |
| `traad-move-global`             | Move the object at the current location to file `DEST'.                                    |
| `traad-move-module`             | Move the current module to file `DEST'.                                                    |
| `traad-extract-method`          | Extract the currently selected region to a new method.                                     |
| `traad-extract-variable`        | Extract the currently selected region to a new variable.                                   |
| `traad-add-argument`            | Add a new argument at `INDEX' in the signature at point.                                   |
| `traad-remove-argument`         | Remove the `INDEX'th argument from the signature at point.                                 |
| `traad-normalize-arguments`     | Normalize the arguments for the method at point.                                           |
| `traad-introduce-parameter`     | Introduce a parameter in a function.                                                       |
| `traad-display-doc`             | Display docstring for an object.                                                           |
| `traad-popup-doc`               | Display docstring for an object in a popup.                                                |
| `traad-display-calltip`         | Display calltip for an object.                                                             |
| `traad-popup-calltip`           | Display calltip for an object in a popup.                                                  |
| `traad-local-to-field`          | Turn a local variable into a field variable.                                               |
| `traad-encapsulate-field`       | Introduce getters and setters for a member and use them instead of direct references.      |
| `traad-use-function`            | Tries to find places this function can be used and inserts a call to the function instead. |
| `traad-inline`                  | Inline this object.                                                                        |
| `traad-thing-at`                | Get the type of the Python thing at point.                                                 |
| `traad-auto-import`             | Automatically add the necessary import for the current symbol.                             |
| `traad-organize-imports`        | Organize the import statements in `filename' according to pep8.                            |
| `traad-froms-to-imports`        | Convert 'from' imports to normal imports in `filename'.                                    |
| `traad-handle-long-imports`     | Transform long imports into 'from' imports in `filename'.                                  |
| `traad-expand-star-imports`     | Expand * import statements in `filename'.                                                  |
| `traad-relatives-to-absolutes`  | Convert relative imports to absolute in `filename'.                                        |
| `traad-imports-super-smackdown` | Apply all the import reformatting commands Traad provides.                                 |

### Other Commands

| Command                 | Description                                    |
|-------------------------|------------------------------------------------|
| `traad-install-server`  | Automatically install the Traad server         |
| `traad-display-history` | Display undo and redo history.                 |
| `traad-kill-all`        | Kill all traad servers and associated buffers. |
| `traad-undo`            | Undo the IDXth change from the history.        |
| `traad-undo-info`       | Get info on the I'th undo history.             |
| `traad-redo`            | Redo the IDXth change from the history.        |
| `traad-redo-info`       | Get info on the I'th redo history.             |


