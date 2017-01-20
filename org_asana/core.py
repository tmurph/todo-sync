if __name__ == "__main__" and __package__ is None:
    import os
    import sys
    sys.path.insert(0, os.path.abspath(".."))
__package__ = "org_asana"

import random
import copy
import io
import collections

import asana

from org_asana.node import Node, tree_from_dict_list
from org_asana.mapping import Mapping
from org_asana.asana_interaction import AsanaNode, AsanaCommand
from org_asana.org_interaction import OrgNode, OrgCommand

def prettify_tree(tree):
    buf = io.StringIO()
    if tree:
        buf.write("{!s}".format(tree))
        for c in tree.children:
            for line in prettify_tree(c).splitlines():
                buf.write("\n  ")
                buf.write(line)
    return buf.getvalue()

def ppt(tree):
    print(prettify_tree(tree))


def lcs2(X, Y, equal):
    """
    apply the greedy lcs/ses algorithm between X and Y sequence
    (should be any Python's sequence)
    equal is a function to compare X and Y which must return 0 if
    X and Y are different, 1 if they are identical
    return a list of matched pairs in tuplesthe greedy lcs/ses algorithm
    """
    N, M = len(X), len(Y)
    if not X or not Y :
        return []
    max = N + M
    v = [0 for i in range(2*max+1)]
    common = [[] for i in range(2*max+1)]
    for D in range(max+1):
        for k in range(-D, D+1, 2):
            if k == -D or k != D and v[k-1] < v[k+1]:
                x = v[k+1]
                common[k] = common[k+1][:]
            else:
                x = v[k-1] + 1
                common[k] = common[k-1][:]

            y = x - k
            while x < N and y < M and equal(X[x], Y[y]):
                common[k].append((x, y))
                x += 1 ; y += 1

            v[k] = x
            if x >= N and y >= M:
                return [ (X[x],Y[y]) for x,y in common[k] ]


def edit_script(s_tree, t_tree, mapping, s_script_class=Node):
    """Generate an edit script to go from S_TREE to T_TREE.

    As a side effect, modifies S_TREE.
    """
    edit_sequence = []
    for t_node in t_tree.breadth_first_order():
        s_node = mapping.source_from_target(t_node)
        t_parent = t_node.parent
        s_parent = mapping.source_from_target(t_parent)
        # insert
        if not s_node:
            t_node.in_order = True
            position = mapping.get_target_position(t_node)
            s_node = mapping.make_source_from_target(t_node)
            edit_sequence.append((s_script_class.insert_child,
                                  s_parent, position, s_node))
            mapping.add_mapped_nodes(s_node, t_node)
            s_parent.insert_child(position, s_node)
            s_node.in_order = True
        elif t_parent:
            s_node = mapping.source_from_target(t_node)
            s_parent = s_node.parent
            # update
            if not mapping.source_equals_target_p(s_node, t_node):
                model_s_node = mapping.make_source_from_target(t_node)
                edit_sequence.append((s_script_class.update,
                                      s_node, model_s_node))
                s_node.update(model_s_node)
            # move
            elif not mapping.mapped_nodes_p(s_parent, t_node.parent):
                t_node.in_order = True
                s_parent = mapping.source_from_target(t_node.parent)
                position = mapping.get_target_position(t_node)
                edit_sequence.append((s_script_class.move_to,
                                      s_node, position, s_parent))
                s_node.move_to(position, s_parent)
                s_node.in_order = True
        # align
        s_list, t_list = [], []
        for s_child in s_node.children:
            s_child.in_order = False
            t_child = mapping.target_from_source(s_child)
            if t_child and t_child.parent is t_node:
                s_list.append(s_child)
        for t_child in t_node.children:
            t_child.in_order = False
            s_child = mapping.source_from_target(t_child)
            if s_child and s_child.parent is s_node:
                t_list.append(t_child)
        s = lcs2(s_list, t_list, mapping.mapped_nodes_p)
        for s_child, t_child in s:
            s_child.in_order = t_child.in_order = True
        for s_child in s_list:
            t_child = mapping.target_from_source(s_child)
            if (t_child not in t_list) or ((s_child, t_child) in s):
                continue
            position = mapping.get_target_position(t_child)
            edit_sequence.append((s_script_class.move_to, s_child,
                                  position, s_node))
            s_child.move_to(position, s_node)
            s_child.in_order = t_child.in_order = True

    # delete
    for s_node in s_tree.post_order():
        if not mapping.target_from_source(s_node):
            edit_sequence.append((s_script_class.delete, s_node))
            s_node.delete()
    # results
    return edit_sequence

def main():
    asana_command = AsanaCommand(asana.Client.access_token(
        os.getenv('ASANA_PERSONAL_ACCESS_TOKEN')))
    asana_tree = tree_from_dict_list(
        AsanaNode, asana_command.get_all_tasks())

    org_command = OrgCommand()
    org_tree = tree_from_dict_list(
        OrgNode, org_command.get_all_headlines())

    def asana_equals_org_p(a_node, o_node):
        a_id = a_node.id
        o_id = getattr(o_node, 'asana_id', None)
        return a_id == o_id

    def make_asana_from_org(org_node):
        n = AsanaNode()
        keyword_map = {('name', 'title'),
                       ('notes', 'paragraph')}
        for a_key, o_key in keyword_map:
            setattr(n, a_key, getattr(org_node, o_key))
        return n

    a_tree, o_tree = copy.deepcopy(asana_tree), copy.deepcopy(org_tree)
    m = Mapping(a_tree, o_tree, asana_equals_org_p, make_asana_from_org)
    e = edit_script(a_tree, o_tree, m, asana_command)
