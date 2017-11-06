import pytest

import todo_sync.node as node
import todo_sync.edit_script as edit_script

from .helpers import assert_trees_equal_p


class DummyNode(node.Node):

    def external_insert_as_child(self, left_sibling_id, parent_node):
        pass

    def external_update(self, other_node):
        pass

    def external_move_to(self, left_sibling_id, parent_node):
        pass

    def external_delete(self):
        pass


class DummyRootNode(node.RootNode):

    def external_insert_as_child(self, left_sibling_id, parent_node):
        pass

    def external_update(self, other_node):
        pass

    def external_move_to(self, left_sibling_id, parent_node):
        pass

    def external_delete(self):
        pass


def mock_tree(node_list=None):
    root = DummyRootNode()
    node_cache = {}
    if node_list:
        for n in node_list:
            node = DummyNode.from_dict(n)
            node_cache[node.id] = node
    for node_id, node in node_cache.items():
        parent_id = getattr(node, 'parent_id', None)
        if hasattr(node, 'parent_id'):
            del node.parent_id
        if parent_id:
            parent_node = node_cache[parent_id]
        else:
            parent_node = root
        parent_node.append_child(node)
    return root


def map_fn(b_node, a_node):
    return b_node.id == a_node.id


def eql_fn(b_node, a_node):
    return b_node.export_attrs == a_node.export_attrs


def make_fn(a_node):
    b_dict = {'id': None}
    b_dict.update(a_node.export_attrs)
    return DummyNode.from_dict(b_dict)


@pytest.mark.parametrize("tree_a", [
    mock_tree(),
    mock_tree([{'id': 'A'}]),
    mock_tree([{'id': 'A'}, {'id': 'B'}]),
    mock_tree([{'id': 'A'}, {'id': 'C', 'parent_id': 'A'}]),
    mock_tree([{'id': 'A'},
               {'id': 'B', 'parent_id': 'A'},
               {'id': 'C', 'parent_id': 'A'}])])
@pytest.mark.parametrize("tree_b", [
    mock_tree(),
    mock_tree([{'id': 'A'}]),
    mock_tree([{'id': 'A'}, {'id': 'B'}]),
    mock_tree([{'id': 'A'}, {'id': 'C', 'parent_id': 'A'}]),
    mock_tree([{'id': 'A'},
               {'id': 'B', 'parent_id': 'A'},
               {'id': 'C', 'parent_id': 'A'}])])
def test_conformity(tree_b, tree_a):
    edit_script.edit_script(tree_b, tree_a, map_fn, eql_fn, make_fn)
    assert_trees_equal_p(tree_b, tree_a)
