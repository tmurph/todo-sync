if __name__ == "__main__" and __package__ is None:
    import os
    import sys
    sys.path.insert(0, os.path.abspath(".."))
__package__ = "org_asana"

import io
import subprocess

from org_asana.node import Node, Command

class OrgNode(Node):
    EXTRA_ATTRS = ('title', 'paragraph')

class OrgCommand(Command):
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
             for n in new_child_node.EXTRA_ATTRS})
        new_id = self._run(
            '(oi-insert-child "{}" {} \'{})'.format(
                parent_node.id, sibling_position, plist_string),
            capture_output=True)
        new_child_node.id = new_id

    def delete(self, node_to_delete):
        self._run('(oi-delete "{}")'.format(node_to_delete.id))

    def update(self, node_to_update, model_node):
        plist_string = elisp_string_from_dict(
            {n: getattr(model_node, n) for n in model_node.EXTRA_ATTRS})
        self._run(
            '(oi-update "{}" \'{})'.format(
                node_to_update.id, plist_string))

    def move_to(self, child_node, sibling_position, parent_node):
        self._run(
            '(oi-move-to "{}" {} "{}")'.format(
                child_node.id, sibling_position, parent_node.id))

    def get_all_headlines(self):
        result = self._run('(oi-get-all-headlines)', capture_output=True)
        return eval(eval(result))

def elisp_string_from_dict(d):
    "Produce a string representation of an Elisp plist from D."
    buf = io.StringIO()
    space_needed = False
    buf.write("(")
    for k, v in d.items():
        if space_needed:
            buf.write(" ")
        buf.write(":" + str(k) + " ")
        if not v:
            buf.write("nil")
        elif isinstance(v, str):
            buf.write('"' + v + '"') # ugh, elisp cares about ' vs "
                                     # ... just pray no " in v
        elif isinstance(v, dict):
            buf.write(elisp_string_from_dict(v))
        space_needed = True
    buf.write(")")
    return buf.getvalue()
