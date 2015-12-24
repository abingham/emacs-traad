# emacs-traad

An Emacs client for [traad](https://github.com/abingham/traad), a client-server
approach to using the [rope](https://github.com/python-rope/rope) Python
refactory library. 

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
[python-environment](https://github.com/tkf/emacs-python-environment). You can
control this installation with two variables:

* **`traad-environment-root`** specifies the name of the virtual environment. By
  default it's "traad", and there is often no need to change it. However, if you
  need to support multiple installation of traad, you might need to modify this
  variable.

* **`traad-environment-virtualenv`** specifies the command used to create the
  virtual environment (e.g. virtualenv, pyvent, etc.). By default this is `nil`,
  in which case `python-environment-virtualenv` will be used (i.e. the
  lower-level python-environment setting).

*For most users the default values should be fine. You really only need to
manipulate these variables if you're installing traad for more than one version
of Python.*

For more information on installation, see
[the Installation page](https://github.com/abingham/emacs-traad/wiki/Installation)
in [the wiki](https://github.com/abingham/emacs-traad/wiki).

For more information on using client, see
[the Usage page](https://github.com/abingham/emacs-traad/wiki/Usage) in
[the wiki](https://github.com/abingham/emacs-traad/wiki).

## Python 3 warning

There is a
[known problem with emacs-request](https://github.com/tkf/emacs-request/pull/15)
that makes the traad emacs client misbehave when using Python3. You can fix this
by patching `request.el`.
