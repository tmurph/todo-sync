=========================
Sync Org files with Asana
=========================

    :Author: Trevor Murphy
    :Contact: trevor.m.murphy@gmail.com

This command line tool updates your Org mode agenda files to match your personal Asana workspace:

.. code:: sh
    :name: Update Org to match Asana

    $ todo-sync org /path/to/init/file.el asana $ASANA_PERSONAL_ACCESS_TOKEN

or updates your Asana workspace to match your Org agenda files:

.. code:: sh
    :name: Update Asana to match Org

    $ todo-sync asana $ASANA_PERSONAL_ACCESS_TOKEN org /path/to/init/file.el

Use the ``-n`` flag for a dry run.

.. _B69CDB2B-4AA7-4272-80F3-A78426E6B6E8:

Requirements
------------

Python 3.6
    Probably not a hard requirement, I just haven’t tested the tool with any earlier versions.

    - asana

    - pexpect

Emacs 25.1
    Also probably not a hard requirement, I just haven’t
    tested with any earlier versions.

    - A version of Org from `http://orgmode.org/ <http://orgmode.org/>`_

      - I say this because, at least on my distribution, the Org Mode bundled with Emacs does not include the function ``org-element-insert-before``, even though that function has been in the Org source code for three years.

      - ¯\\\_(ツ)\_/¯

    A personal Asana account
        This tool may work with other types of accounts, I just haven’t tested it yet.

.. _921294DE-9BE4-4B6C-BFC1-9A951E08FA1B:

Install
-------

Install Python Tools
~~~~~~~~~~~~~~~~~~~~

Just use `pip <https://pip.pypa.io/en/stable/>`_!

.. code:: sh

    $ pip install todo-sync

Install Emacs Tools
~~~~~~~~~~~~~~~~~~~

Org
^^^

Install Org from `http://orgmode.org/ <http://orgmode.org/>`_

Todo-Sync’s Library
^^^^^^^^^^^^^^^^^^^

Install ``ts-org-interaction.el`` from this repo to your personal site lisp.

.. code:: sh

    $ cd /path/to/your/site/lisp
    $ curl -O https://raw.githubusercontent.com/tmurph/todo-sync/master/emacs/ts-org-interaction.el

(Optional) Create a Minimal Init File
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you don’t do this, you’ll have to point the tool at your main init file, which *just might* invoke complicated code that could hang the tool’s Emacs subprocess in difficult-to-debug ways.

Here’s a template that should get you going:

.. code:: common-lisp

    ;;; we need the downloaded version of Org
    (push "/path/to/downloaded/org" load-path)

    ;;; we need the downloaded ts-org-interaction library
    (push "/path/to/your/site/lisp" load-path)

    ;;; and we need to set Org agenda files
    (defvar org-directory "/path/for/relative/filename/expansion")
    (defvar org-agenda-files '("relative/path/to/first/file"
                               "relative/path/to/second/file"
                               "/absolute/path/to/a/file"
                               "etc"))

Get your Asana Token
~~~~~~~~~~~~~~~~~~~~

Follow `this link <https://app.asana.com/-/account_api>`_ to your Account Settings dialog or click your picture in the Asana application and navigate to the “My Profile Settings” link, where you can find the “Apps” tab. At the bottom of this tab there is a “Manage Developer Apps” link.

Follow the steps to “Create New Personal Access Token” and save it someplace secure where the command line tool can find it.

.. _F840CA04-1A82-484C-B59F-738A6621EB0C:

License
-------

Distributed under the terms of the `GPLv3 <https://www.gnu.org/licenses/gpl-3.0.en.html>`_ license.
