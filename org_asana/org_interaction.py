if __name__ == "__main__" and __package__ is None:
    import os
    import sys
    sys.path.insert(0, os.path.abspath(".."))
__package__ = "org_asana"

import io
import pexpect
import pexpect.replwrap

from org_asana.node import Node
from org_asana.command import Command

class OrgNode(Node):
    CLASS_EXPORT_ATTRS_TEMPLATE = ('title', 'paragraph')

class OrgCommand(Command):
    DEFAULT_FETCH_FIELDS = ('id', 'title', 'paragraph', 'parent')

    def __init__(self, org_config_filename):
        command = 'emacs'
        args = ['-batch',
                '-l', org_config_filename,
                '-l', 'org-interaction.el',
                '--eval=(oi-repl)']
        spawn = pexpect.spawn(command, args, encoding='utf-8')
        self._repl = pexpect.replwrap.REPLWrapper(
            spawn, "Lisp expression: ", None)
        self._run_command('(oi-init)')

    def _run_command(self, command):
        return self._repl.run_command(command)

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        self._run_command('(oi-final)')
        self._repl.child.sendeof()

    def insert_child(self, parent_node, sibling_position, new_child_node):
        plist_string = elisp_string_from_dict(
            {n: getattr(new_child_node, n)
             for n in new_child_node.EXPORT_ATTRS})
        new_id = self._run_command(
            '(oi-insert-child "{}" {} \'{})'.format(
                parent_node.id, sibling_position, plist_string))
        new_child_node.id = new_id

    def delete(self, node_to_delete):
        self._run_command('(oi-delete "{}")'.format(node_to_delete.id))

    def update(self, node_to_update, model_node):
        parameters = {
            n: getattr(model_node, n) for n in model_node.EXPORT_ATTRS
            if getattr(model_node, n) != getattr(node_to_update, n, None)}
        plist_string = elisp_string_from_dict(parameters)
        self._run_command(
            '(oi-update "{}" \'{})'.format(
                node_to_update.id, plist_string))

    def move_to(self, child_node, sibling_position, parent_node):
        self._run_command(
            '(oi-move-to "{}" {} "{}")'.format(
                child_node.id, sibling_position, parent_node.id))

    def get_all_items(self, extra_field_list=None):
        field_list = self.DEFAULT_FETCH_FIELDS
        if extra_field_list:
            field_list = field_list + tuple(extra_field_list)
        result = self._run_command(
            '(oi-get-all-headlines \'{})'.format(
                elisp_string_from_list(field_list)))
        return eval(eval(result))

def elisp_string_from_value(value):
    "Make an Elisp string representation of a Python value."
    if value:
        result = '"' + str(value).replace('"', '\\"').replace('\n', '\\n') + '"'
    else:
        result = 'nil'
    return result

def elisp_string_from_key(key):
    "Make an Elisp string representation of a Python key."
    result = str(key).lower()
    if key in ['todo_keyword']:
        result = result.replace('_', '-')
    return result

def elisp_string_from_dict(d):
    "Make an Elisp plist string representation of a Python dictionary."
    if not d:
        result = "()"
    else:
        lst = list(d.items())
        buf = io.StringIO()
        k, v = lst[0]
        buf.write("(:")
        buf.write(elisp_string_from_key(k))
        buf.write(" ")
        buf.write(elisp_string_from_value(v))
        for k, v in lst[1:]:
            buf.write(" :")
            buf.write(elisp_string_from_key(k))
            buf.write(" ")
            buf.write(elisp_string_from_value(v))
        buf.write(")")
        result = buf.getvalue()
    return result

def elisp_string_from_list(lst):
    "Make an Elisp keyword list string representation of a Python list."
    if lst:
        buf = io.StringIO()
        buf.write("(:")
        buf.write(elisp_string_from_key(lst[0]))
        for k in lst[1:]:
            buf.write(" :")
            buf.write(elisp_string_from_key(k))
        buf.write(")")
        return buf.getvalue()
