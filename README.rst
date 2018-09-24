=========================
Sync Org files with Asana
=========================

    :Author: Trevor Murphy
    :Contact: trevor.m.murphy@gmail.com

.. contents::

This command line tool updates your Org mode agenda files to match your personal Asana workspace:

.. code:: sh
    :name: Update Org to match Asana

    $ todo-sync org /path/to/init/file.el asana $ASANA_PERSONAL_ACCESS_TOKEN

or updates your Asana workspace to match your Org agenda files:

.. code:: sh
    :name: Update Asana to match Org

    $ todo-sync asana $ASANA_PERSONAL_ACCESS_TOKEN org /path/to/init/file.el

Use the ``-n`` flag for a dry run.

Requirements
------------

Python 3.6
    Earlier versions of Python 3 are probably fine, I just haven’t tested them yet.  Python 3.7 is not currently supported due to a bug in the official Asana bindings library.

    - asana

    - pexpect >= 4.3

Emacs 25.1 or higher
    Probably not a hard requirement, I just haven’t tested with any earlier versions.

Org Mode 8.2.4 or higher
    Most modern Emacs distributions include Org Mode 9 or higher.

A personal Asana account
    This tool may work with other types of accounts, I just haven’t tested it yet.

Install
-------

Install Python Tools
~~~~~~~~~~~~~~~~~~~~

Just use `pip <https://pip.pypa.io/en/stable/>`_!

.. code:: sh

    $ pip install todo-sync

Install Emacs Tools
~~~~~~~~~~~~~~~~~~~

Emacs
^^^^^

Consult the official `installation instructions <https://www.gnu.org/software/emacs/download.html>`_ from the GNU Project.

Org
^^^

Your Emacs distribution likely includes a high enough version of Org.  You can check from within Emacs with ``M-x org-version RET``.  If you need a more recent version, consult the official `installation instructions <https://orgmode.org/>`_ from the Org project.

Todo-Sync’s Library
^^^^^^^^^^^^^^^^^^^

Install ``ts-org-interaction.el`` from this repo to your personal site lisp.

.. code:: sh

    $ cd /path/to/your/site/lisp
    $ curl -O https://raw.githubusercontent.com/tmurph/todo-sync/master/emacs/ts-org-interaction.el

(Optional) Create a Minimal Init File
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you don’t do this, you’ll have to point the tool at your main init file, which **just might** invoke complicated code that could hang the tool’s Emacs subprocess in difficult-to-debug ways.

Here’s a template that should get you going:

.. code:: common-lisp

    ;;; make Org mode available to Emacs
    (push "/path/to/org" load-path)

    ;;; make the downloaded ts-org-interaction library available
    (push "/path/to/your/site/lisp" load-path)

    ;;; set Org agenda file variables
    (defvar org-directory "/path/for/relative/filename/expansion")
    (defvar org-agenda-files '("relative/path/to/first/file"
                               "relative/path/to/second/file"
                               "/absolute/path/to/third/file"
                               "etc"))

Get your Asana Token
~~~~~~~~~~~~~~~~~~~~

Follow `this link <https://app.asana.com/-/account_api>`_ to your Account Settings dialog or click your picture in the Asana application and navigate to the “My Profile Settings” link, where you can find the “Apps” tab.  At the bottom of this tab there is a “Manage Developer Apps” link.

Follow the steps to “Create New Personal Access Token” and save it someplace secure where the command line tool can find it.

License
-------

Distributed under the terms of the `GPLv3 <https://www.gnu.org/licenses/gpl-3.0.en.html>`_ license.
