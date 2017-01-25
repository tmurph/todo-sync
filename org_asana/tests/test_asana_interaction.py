import pytest
import pytest_mock

from org_asana.node import Node, trees_equal_p
from org_asana.asana_interaction import AsanaNode

@pytest.mark.parametrize("asana_node, expected_node", [
    (AsanaNode.from_dict({'id': 1, 'parent': None}),
     Node.from_dict({'id': 1, 'parent': None,
                     'name': None, 'notes': None}))])
def test_asana_node(asana_node, expected_node):
    "Is an AsanaNode just a Node with extra attributes?"
    assert trees_equal_p(asana_node, expected_node)
