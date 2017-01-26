if __name__ == "__main__" and __package__ is None:
    import os
    import sys
    sys.path.insert(0, os.path.abspath(".."))
__package__ = "org_asana"

import io
import subprocess

from org_asana.node import Node, Command

class OrgNode(Node):
    CLASS_EXPORT_ATTRS_TEMPLATE = ('title', 'paragraph')

class OrgCommand(Command):
    DEFAULT_FETCH_FIELDS = ('id', 'title', 'paragraph', 'parent')

    def _run(self, command, capture_output=False):
        cmd = ['emacsclient', '-s', self.servername, '-e']
        cmd.append(command)
        result = None
        if capture_output:
            result = subprocess.run(cmd, universal_newlines=True,
                                    stderr=subprocess.DEVNULL,
                                    stdout=subprocess.PIPE).stdout
        else:
            subprocess.run(cmd,
                           stderr=subprocess.DEVNULL,
                           stdout=subprocess.DEVNULL)
        return result

    def __init__(self):
        self.servername = "ORGINTERACTION"
        cmd = ['emacs', '-Q',
               '-l', '~/.emacs.d/min-init.el',
               '-l', 'org-interaction.el',
               '--daemon={}'.format(self.servername)]
        subprocess.run(cmd,
                       stderr=subprocess.DEVNULL,
                       stdout=subprocess.DEVNULL)
        self._run('(oi-init)')

    def insert_child(self, parent_node, sibling_position, new_child_node):
        plist_string = elisp_string_from_dict(
            {n: getattr(new_child_node, n)
             for n in new_child_node.EXPORT_ATTRS})
        new_id = self._run(
            '(oi-insert-child "{}" {} \'{})'.format(
                parent_node.id, sibling_position, plist_string),
            capture_output=True)
        new_child_node.id = new_id

    def delete(self, node_to_delete):
        self._run('(oi-delete "{}")'.format(node_to_delete.id))

    def update(self, node_to_update, model_node):
        plist_string = elisp_string_from_dict(
            {n: getattr(model_node, n) for n in model_node.EXPORT_ATTRS})
        self._run(
            '(oi-update "{}" \'{})'.format(
                node_to_update.id, plist_string))

    def move_to(self, child_node, sibling_position, parent_node):
        self._run(
            '(oi-move-to "{}" {} "{}")'.format(
                child_node.id, sibling_position, parent_node.id))

    def get_all_items(self, extra_field_list=None):
        field_list = self.DEFAULT_FETCH_FIELDS
        if extra_field_list:
            field_list = field_list + tuple(extra_field_list)
        result = self._run(
            '(oi-get-all-headlines \'{})'.format(
                elisp_string_from_list(field_list)),
            capture_output=True)
        return eval(eval(result))

def elisp_string_from_value(value):
    "Make an Elisp string representation of a Python value."
    if value:
        result = '"' + str(value).replace('"', '\\"').replace('\n', '\\n') + '"'
    else:
        result = 'nil'
    return result

def elisp_string_from_dict(d):
    "Make an Elisp plist string representation of a Python dictionary."
    if d:
        lst = list(d.items())
        buf = io.StringIO()
        k, v = lst[0]
        buf.write("(:" + str(k).lower() + " ")
        buf.write(elisp_string_from_value(v))
        for k, v in lst[1:]:
            buf.write(" :" + str(k).lower() + " ")
            buf.write(elisp_string_from_value(v))
        buf.write(")")
        return buf.getvalue()

def elisp_string_from_list(lst):
    "Make an Elisp keyword list string representation of a Python list."
    if lst:
        buf = io.StringIO()
        buf.write("(:" + str(lst[0]).lower())
        for k in lst[1:]:
            buf.write(" :" + str(k).lower())
        buf.write(")")
        return buf.getvalue()
