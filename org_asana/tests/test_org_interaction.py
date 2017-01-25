import pytest
import pytest_mock

from org_asana.node import Node, trees_equal_p
from org_asana.org_interaction import OrgNode

@pytest.mark.parametrize("org_node, expected_node", [
    (OrgNode.from_dict({'id': 1, 'parent': None}),
     Node.from_dict({'id': 1, 'parent': None,
                     'title': None, 'paragraph': None}))
])
def test_org_node(org_node, expected_node):
    "Is an OrgNode just a Node with extra attributes?"
    assert trees_equal_p(org_node, expected_node)
