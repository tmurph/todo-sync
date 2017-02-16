import copy

import todo_sync.node as node
import todo_sync.helpers as helpers


def lcs2(X, Y, equal):
    """
    apply the greedy lcs/ses algorithm between X and Y sequence
    (should be any Python's sequence)
    equal is a function to compare X and Y which must return 0 if
    X and Y are different, 1 if they are identical
    return a list of matched pairs in tuplesthe greedy lcs/ses algorithm
    """
    N, M = len(X), len(Y)
    if not X or not Y:
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
                x += 1
                y += 1

            v[k] = x
            if x >= N and y >= M:
                return [(X[x], Y[y]) for x, y in common[k]]


def edit_script(s_tree, t_tree,
                s_maps_to_t_p, s_equals_t_p, make_s_from_t,
                no_delete=False):
    """Update S_TREE to match T_TREE.

Implements an algorithm as described in "Change detection in
hierarchically structured information" by S. Chawathe, A. Rajaraman,
H. Garcia-Molina and J. Widom ([CRGMW95])
    """
    if not isinstance(s_tree, node.RootNode):
        raise Exception("Source tree is not rooted.")
    if not isinstance(t_tree, node.RootNode):
        raise Exception("Target tree is not rooted.")

    # initialize mapping dictionaries
    s_from_t, t_from_s = {}, {}
    s_from_t[t_tree] = s_tree
    t_from_s[s_tree] = t_tree
    s_list = helpers.breadth_first_order(s_tree)[1:]
    t_list = helpers.breadth_first_order(t_tree)[1:]
    for s_node in s_list:
        for t_node in t_list:
            if s_maps_to_t_p(s_node, t_node):
                s_from_t[t_node] = s_node
                t_from_s[s_node] = t_node
                t_list.remove(t_node)
                break

    # define helper functions
    def mapped_nodes_p(s_node, t_node):
        return s_node is s_from_t.get(t_node)

    def s_left_sibling_id_from_t(target_node):
        target_parent = target_node.parent
        target_index = target_parent.children.index(target_node)
        target_ordered_nodes = [t_node for t_node
                                in target_parent.children[:target_index]
                                if t_node.in_order]
        result = None
        if target_ordered_nodes:
            if target_node is target_ordered_nodes[0]:
                result = None
            else:
                result = getattr(
                    s_from_t.get(target_ordered_nodes[-1]), 'id')
        return result

    for t_node in helpers.breadth_first_order(t_tree):
        s_node = s_from_t.get(t_node)
        t_parent = t_node.parent
        s_parent = s_from_t.get(t_parent)
        # insert
        if not s_node:
            t_node.in_order = True
            s_left_sibling_id = s_left_sibling_id_from_t(t_node)
            s_node = make_s_from_t(t_node)
            s_from_t[t_node] = s_node
            t_from_s[s_node] = t_node
            s_node.external_insert_as_child(s_left_sibling_id, s_parent)
            s_node.insert_as_child(s_left_sibling_id, s_parent)
            s_node.in_order = True
        elif t_parent:
            s_node = s_from_t.get(t_node)
            s_parent = s_node.parent
            # update
            if not s_equals_t_p(s_node, t_node):
                model_s_node = make_s_from_t(t_node)
                s_node.external_update(model_s_node)
                s_node.update(model_s_node)
            # move
            if not mapped_nodes_p(s_parent, t_node.parent):
                t_node.in_order = True
                s_parent = s_from_t.get(t_node.parent)
                s_left_sibling_id = s_left_sibling_id_from_t(t_node)
                s_node.external_move_to(s_left_sibling_id, s_parent)
                s_node.move_to(s_left_sibling_id, s_parent)
                s_node.in_order = True
        # align
        s_list, t_list = [], []
        for s_child in s_node.children:
            s_child.in_order = False
            t_child = t_from_s.get(s_child)
            if t_child and t_child.parent is t_node:
                s_list.append(s_child)
        for t_child in t_node.children:
            t_child.in_order = False
            s_child = s_from_t.get(t_child)
            if s_child and s_child.parent is s_node:
                t_list.append(t_child)
        s = lcs2(s_list, t_list, mapped_nodes_p)
        for s_child, t_child in s:
            s_child.in_order = t_child.in_order = True
        for s_child in s_list:
            t_child = t_from_s.get(s_child)
            if (t_child not in t_list) or ((s_child, t_child) in s):
                continue
            s_left_sibling_id = s_left_sibling_id_from_t(t_child)
            s_child.external_move_to(s_left_sibling_id, s_node)
            s_child.move_to(s_left_sibling_id, s_node)
            s_child.in_order = t_child.in_order = True

    # delete
    if not no_delete:
        s_list = helpers.breadth_first_order(s_tree)
        s_list.reverse()
        for s_node in s_list:
            if not t_from_s.get(s_node):
                s_node.external_delete()
                s_node.delete()
