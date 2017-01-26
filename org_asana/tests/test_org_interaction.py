import pytest
import pytest_mock

from org_asana.node import Node, trees_equal_p
from org_asana.org_interaction import OrgNode, OrgCommand

@pytest.fixture(autouse=True)
def patch_subprocess_run(mocker):
    mocker.patch('subprocess.run')

@pytest.fixture(autouse=True)
def patch_org_command_run(mocker):
    mocker.patch.object(OrgCommand, '_run', return_value='"{}"')

@pytest.fixture
def org_command():
    return OrgCommand()

@pytest.mark.parametrize("org_node, expected_node", [
    (OrgNode.from_dict({'id': 1, 'parent': None}),
     Node.from_dict({'id': 1, 'parent': None,
                     'title': None, 'paragraph': None}))
])
def test_org_node(org_node, expected_node):
    "Is an OrgNode just a Node with extra attributes?"
    assert trees_equal_p(org_node, expected_node)

def test_org_command_init(org_command):
    "Does OrgCommand.__init__ talk to Emacs correctly?"
    org_command._run.assert_called_with('(oi-init)')

@pytest.mark.parametrize("parent, pos, child, expected", [
    (OrgNode.from_dict({'id': '1', 'parent': None}),
     0,
     OrgNode(),
     '(oi-insert-child "1" 0 \'(:title nil :paragraph nil))'),
    (OrgNode(),
     1,
     OrgNode(),
     '(oi-insert-child "None" 1 \'(:title nil :paragraph nil))'),
    (OrgNode(),
     0,
     OrgNode.from_dict({'id': '1', 'parent': None,
                        'title': 'Hello!', 'paragraph': 'World!'}),
     '(oi-insert-child "None" 0 '
     '\'(:title "Hello!" :paragraph "World!"))'),
    (OrgNode(),
     0,
     OrgNode.from_dict({'id': '1', 'parent': None,
                        'paragraph': 'a "quoted" string'}),
     '(oi-insert-child "None" 0 \'(:title nil '
     ':paragraph "a \\"quoted\\" string"))'),
    (OrgNode(),
     0,
     OrgNode.from_dict({'id': '1', 'parent': None,
                        'paragraph': 'a string with a'
                        '\n'
                        'newline in it'}),
     '(oi-insert-child "None" 0 \'(:title nil '
     ':paragraph "a string with a'
     '\\n'
     'newline in it"))')
])
def test_org_command_insert_child(parent, pos, child, expected,
        org_command):
    "Does OrgCommand.insert_child talk to Emacs correctly?"
    org_command.insert_child(parent, pos, child)
    org_command._run.assert_called_with(expected, capture_output=True)

@pytest.mark.parametrize("node_to_delete, expected", [
    (OrgNode(), '(oi-delete "None")'),
    (OrgNode.from_dict({'id': '1', 'parent': None,}), '(oi-delete "1")')
])
def test_org_command_delete(node_to_delete, expected, org_command):
    "Does OrgCommand.delete talk to Emacs correctly?"
    org_command.delete(node_to_delete)
    org_command._run.assert_called_with(expected)

@pytest.mark.parametrize("to_update, model, expected", [
    (OrgNode.from_dict({'id': '1', 'parent': None,}),
     OrgNode(),
     '(oi-update "1" \'(:title nil :paragraph nil))'),
    (OrgNode(),
     OrgNode.from_dict({'id': '1', 'parent': None,
                        'title': 'Hello!', 'paragraph': 'World!'}),
     '(oi-update "None" \'(:title "Hello!" :paragraph "World!"))'),
    (OrgNode(),
     OrgNode.from_dict({'id': '1', 'parent': None,
                        'paragraph': 'a "quoted" string'}),
     '(oi-update "None" \'(:title nil '
     ':paragraph "a \\"quoted\\" string"))'),
    (OrgNode(),
     OrgNode.from_dict({'id': '1', 'parent': None,
                        'paragraph': 'a string with a'
                        '\n'
                        'newline in it'}),
     '(oi-update "None" \'(:title nil '
     ':paragraph "a string with a'
     '\\n'
     'newline in it"))')
])
def test_org_command_update(to_update, model, expected, org_command):
    "Does OrgCommand.update talk to Emacs correctly?"
    org_command.update(to_update, model)
    org_command._run.assert_called_with(expected)

@pytest.mark.parametrize("child, pos, parent, expected", [
    (OrgNode.from_dict({'id': '1', 'parent': None}), 0, OrgNode(),
     '(oi-move-to "1" 0 "None")'),
    (OrgNode(), 1, OrgNode(),
     '(oi-move-to "None" 1 "None")'),
    (OrgNode(), 0, OrgNode.from_dict({'id': '1', 'parent': None}),
     '(oi-move-to "None" 0 "1")')
])
def test_org_command_move_to(child, pos, parent, expected, org_command):
    "Does OrgCommand.move_to talk to Emacs correctly?"
    org_command.move_to(child, pos, parent)
    org_command._run.assert_called_with(expected)
