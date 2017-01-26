import pytest

import todo_sync.node as node

from .helpers import assert_trees_equal_p


def node_from_dict(d=None):
    default_dict = {'id': None, 'parent': None}
    if d:
        default_dict.update(d)
    return node.Node.from_dict(default_dict)


def tree_from_dict_list(lst):
    result = node_from_dict(lst[0])
    node_cache = {result.id: result}
    for d in lst[1:]:
        node = node_from_dict(d)
        node_cache[node.id] = node
        parent_node = node_cache[d['parent']]
        parent_node.append_child(node)
    return result


@pytest.mark.parametrize(
    "parent_node, left_sibling_id, child_node, expected_node", [
        (node_from_dict({'id': 1, 'parent': None}),
         None,
         node_from_dict({'id': 2, 'parent': None}),
         tree_from_dict_list(
             [{'id': 1, 'parent': None},
              {'id': 2, 'parent': 1}])),
        (tree_from_dict_list(
            [{'id': 1, 'parent': None},
             {'id': 2, 'parent': 1}]),
         None,
         node_from_dict({'id': 3, 'parent': None}),
         tree_from_dict_list(
             [{'id': 1, 'parent': None},
              {'id': 3, 'parent': 1},
              {'id': 2, 'parent': 1}])),
        (tree_from_dict_list(
            [{'id': 1, 'parent': None},
             {'id': 2, 'parent': 1}]),
         2,
         node_from_dict({'id': 3, 'parent': None}),
         tree_from_dict_list(
             [{'id': 1, 'parent': None},
              {'id': 2, 'parent': 1},
              {'id': 3, 'parent': 1}]))])
def test_node_insert_as_child(parent_node, left_sibling_id, child_node,
                              expected_node):
    "Can a node insert itself as a child?"
    child_node.insert_as_child(left_sibling_id, parent_node)
    assert_trees_equal_p(parent_node, expected_node)


@pytest.mark.parametrize(
    "parent_node, child_pos_to_delete, expected_node", [
        (tree_from_dict_list([{'id': 1, 'parent': None},
                              {'id': 2, 'parent': 1}]),
         0,
         node_from_dict({'id': 1, 'parent': None})),
        (tree_from_dict_list([{'id': 1, 'parent': None},
                              {'id': 2, 'parent': 1},
                              {'id': 3, 'parent': 1}]),
         0,
         tree_from_dict_list([{'id': 1, 'parent': None},
                              {'id': 3, 'parent': 1}]))])
def test_node_delete(parent_node, child_pos_to_delete, expected_node):
    "Can a node delete itself?"
    parent_node.children[child_pos_to_delete].delete()
    assert_trees_equal_p(parent_node, expected_node)


@pytest.mark.parametrize("node_to_update, target_node, expected_node", [
    (node_from_dict({'id': 1, 'parent': None}),
     node_from_dict({'id': 2, 'parent': None, 'extra': "Hello world!"}),
     node_from_dict({'id': 1, 'parent': None, 'extra': "Hello world!"}))])
def test_node_update(node_to_update, target_node, expected_node):
    "Can a node update itself from another node?"
    node_to_update.update(target_node)
    assert_trees_equal_p(node_to_update, expected_node)


@pytest.mark.parametrize(
    "node_to_move, left_sibling_id, parent_node, expected_node", [
        (node_from_dict({'id': 1, 'parent': None}),
         None,
         node_from_dict({'id': 2, 'parent': None}),
         tree_from_dict_list(
             [{'id': 2, 'parent': None},
              {'id': 1, 'parent': 2}])),
        (node_from_dict({'id': 1, 'parent': None}),
         None,
         tree_from_dict_list(
             [{'id': 2, 'parent': None},
              {'id': 3, 'parent': 2}]),
         tree_from_dict_list(
             [{'id': 2, 'parent': None},
              {'id': 1, 'parent': 2},
              {'id': 3, 'parent': 2}])),
        (node_from_dict({'id': 1, 'parent': None}),
         3,
         tree_from_dict_list(
             [{'id': 2, 'parent': None},
              {'id': 3, 'parent': 2}]),
         tree_from_dict_list(
             [{'id': 2, 'parent': None},
              {'id': 3, 'parent': 2},
              {'id': 1, 'parent': 2}]))])
def test_node_move_to(node_to_move, left_sibling_id, parent_node,
                      expected_node):
    "Can a node reparent itself under another node?"
    node_to_move.move_to(left_sibling_id, parent_node)
    assert_trees_equal_p(parent_node, expected_node)
