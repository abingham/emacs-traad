# emacs-traad

An Emacs client for [traad](https://github.com/abingham/traad), a client-server
approach to using the [rope](https://github.com/python-rope/rope) Python
refactory library. 

With this client and the `traad` server, you can do Python refactorings from
Emacs. This client includes support for installing traad for you, so you
probably don't even need to think about installing the server separately.

## Quickstart

You can install the Emacs client for traad via ``package.el``::

    M-x package-install<RET>traad

Then you can use ``traad-install-server`` to install the Python server
into a default location::

    M-x traad-install-server

For more information on this process, see `the wiki
<https://github.com/abingham/emacs-traad/wiki/installation>`_.

## Python 3 warning

There is a
[known problem with emacs-request](https://github.com/tkf/emacs-request/pull/15)
that makes the traad emacs client misbehave when using Python3. You can fix this
by patching `request.el`.
