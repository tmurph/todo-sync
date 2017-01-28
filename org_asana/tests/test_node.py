import pytest
import pytest_mock

from org_asana.node import Node, RootNode, trees_equal_p

@pytest.mark.parametrize("parent_node, sib_position, child_node, "
                         "expected_node", [
                             (Node.from_dict({'id': 1, 'parent': None}),
                              0,
                              Node.from_dict({'id': 2, 'parent': None}),
                              Node.from_dict_list(
                                  [{'id': 1, 'parent': None},
                                   {'id': 2, 'parent': 1}])),
                             (Node.from_dict_list(
                                 [{'id': 1, 'parent': None},
                                  {'id': 2, 'parent': 1}]),
                              0,
                              Node.from_dict({'id': 3, 'parent': None}),
                              Node.from_dict_list(
                                  [{'id': 1, 'parent': None},
                                   {'id': 3, 'parent': 1},
                                   {'id': 2, 'parent': 1}])),
                             (Node.from_dict_list(
                                 [{'id': 1, 'parent': None},
                                  {'id': 2, 'parent': 1}]),
                              1,
                              Node.from_dict({'id': 3, 'parent': None}),
                              Node.from_dict_list(
                                  [{'id': 1, 'parent': None},
                                   {'id': 2, 'parent': 1},
                                   {'id': 3, 'parent': 1}]))])
def test_node_insert_child(parent_node, sib_position, child_node,
                           expected_node):
    "Can a node insert a child?"
    parent_node.insert_child(sib_position, child_node)
    assert trees_equal_p(parent_node, expected_node)

@pytest.mark.parametrize(
    "parent_node, child_pos_to_delete, expected_node", [
        (Node.from_dict_list([{'id': 1, 'parent': None},
                              {'id': 2, 'parent': 1}]),
         0,
         Node.from_dict({'id': 1, 'parent': None})),
        (Node.from_dict_list([{'id': 1, 'parent': None},
                              {'id': 2, 'parent': 1},
                              {'id': 3, 'parent': 1}]),
         0,
         Node.from_dict_list([{'id': 1, 'parent': None},
                              {'id': 3, 'parent': 1}]))])
def test_node_delete(parent_node, child_pos_to_delete, expected_node):
    "Can a node delete itself?"
    parent_node.children[child_pos_to_delete].delete()
    assert trees_equal_p(parent_node, expected_node)

@pytest.mark.parametrize("node_to_update, target_node, expected_node", [
    (Node.from_dict({'id': 1, 'parent': None}),
     Node.from_dict({'id': 2, 'parent': None, 'extra': "Hello world!"}),
     Node.from_dict({'id': 1, 'parent': None, 'extra': "Hello world!"}))])
def test_node_update(node_to_update, target_node, expected_node):
    "Can a node update itself from another node?"
    node_to_update.update(target_node)
    assert trees_equal_p(node_to_update, expected_node)

@pytest.mark.parametrize("node_to_move, sib_position, parent_node, "
                         "expected_node", [
                             (Node.from_dict({'id': 1, 'parent': None}),
                              0,
                              Node.from_dict({'id': 2, 'parent': None}),
                              Node.from_dict_list(
                                  [{'id': 2, 'parent': None},
                                   {'id': 1, 'parent': 2}])),
                             (Node.from_dict({'id': 1, 'parent': None}),
                              0,
                              Node.from_dict_list(
                                  [{'id': 2, 'parent': None},
                                   {'id': 3, 'parent': 2}]),
                              Node.from_dict_list(
                                  [{'id': 2, 'parent': None},
                                   {'id': 1, 'parent': 2},
                                   {'id': 3, 'parent': 2}])),
                             (Node.from_dict({'id': 1, 'parent': None}),
                              1,
                              Node.from_dict_list(
                                  [{'id': 2, 'parent': None},
                                   {'id': 3, 'parent': 2}]),
                              Node.from_dict_list(
                                  [{'id': 2, 'parent': None},
                                   {'id': 3, 'parent': 2},
                                   {'id': 1, 'parent': 2}]))])
def test_node_move_to(node_to_move, sib_position, parent_node,
                      expected_node):
    "Can a node reparent itself under another node?"
    node_to_move.move_to(sib_position, parent_node)
    assert trees_equal_p(parent_node, expected_node)

@pytest.mark.parametrize("root_node, expected_node", [
    (RootNode(),
     Node.from_dict({'id': None, 'parent': None, 'root': None})),
    (RootNode.from_dict_list(Node, [{'id': 1, 'parent': None}]),
     Node.from_dict_list([{'id': None, 'parent': None, 'root': None},
                          {'id': 1, 'parent': None}]))])
def test_root_node(root_node, expected_node):
    "Is a RootNode like a Node, but a little different?"
    assert trees_equal_p(root_node, expected_node)
