import pytest
import pytest_mock

from org_asana.node import Node, trees_equal_p
from org_asana.asana_interaction import AsanaNode, AsanaCommand

@pytest.fixture(autouse=True)
def patch_asana(mocker):
    mocker.patch('asana.resources.tasks')

@pytest.fixture
def asana_command():
    import asana
    return AsanaCommand(asana.resources.tasks(), 'DEFAULT')

@pytest.mark.parametrize("asana_node, expected_node", [
    (AsanaNode.from_dict({'id': 1, 'parent': None}),
     Node.from_dict({'id': 1, 'parent': None,
                     'name': None, 'notes': None}))])
def test_asana_node(asana_node, expected_node):
    "Is an AsanaNode just a Node with extra attributes?"
    assert trees_equal_p(asana_node, expected_node)

@pytest.mark.parametrize("parent, pos, child, expected", [
    (AsanaNode(), 0, AsanaNode(),
     {'assignee': 'me', 'name': None, 'notes': None,
      'workspace': 'DEFAULT'}),
    (AsanaNode.from_dict({'id': 1, 'parent': None}), 0, AsanaNode(),
     {'assignee': 'me', 'name': None, 'notes': None,
      'parent': 1}),
    (AsanaNode.from_dict({'id': 1, 'parent': None}),
     0,
     AsanaNode.from_dict({'id': 2, 'parent': None, 'custom': 'thing'}),
     {'assignee': 'me', 'name': None, 'notes': None,
      'parent': 1, 'custom': 'thing'})])
def test_asana_command_insert_child(parent, pos, child, expected,
                                    asana_command):
    "Does AsanaCommand.insert_child talk to Asana correctly?"
    asana_command.insert_child(parent, pos, child)
    asana_command._tasks.create.assert_called_with(params=expected)

@pytest.mark.parametrize("parent, pos, child, create_return", [
    (AsanaNode(), 0, AsanaNode(), {'id': 1})])
def test_asana_command_insert_child_result(parent, pos, child,
                                           create_return, asana_command,
                                           mocker):
    "Does AsanaCommand.insert_child listen to Asana correctly?"
    mocker.patch.object(asana_command._tasks, 'create',
                        return_value=create_return)
    asana_command.insert_child(parent, pos, child)
    assert child.id == create_return['id']

@pytest.mark.parametrize("node_to_delete", [
    (AsanaNode.from_dict({'id': 1, 'parent': None}))])
def test_asana_command_delete(node_to_delete, asana_command):
    "Does AsanaCommand.delete talk to Asana correctly?"
    asana_command.delete(node_to_delete)
    asana_command._tasks.delete.assert_called_with(node_to_delete.id)

@pytest.mark.parametrize("to_update, model, expected", [
    (AsanaNode(), AsanaNode(),
     {'name': None, 'notes': None}),
    (AsanaNode.from_dict({'id': 1, 'parent': None}), AsanaNode(),
     {'name': None, 'notes': None}),
    (AsanaNode(), AsanaNode.from_dict({'id': 1, 'parent': None,
                                       'name': "Hello",
                                       'notes': "World"}),
     {'name': "Hello", 'notes': "World"}),
    (AsanaNode(), AsanaNode.from_dict({'id': 1, 'parent': None,
                                       'name': "Hello",
                                       'notes': "World",
                                       'custom': "ID"}),
     {'name': "Hello", 'notes': "World", 'custom': "ID"}),
    (AsanaNode(), AsanaNode.from_dict({'id': 1, 'parent': None,
                                       'notes': "A string with a"
                                       "\n"
                                       "newline in it."}),
     {'name': None, 'notes': "A string with a\nnewline in it."})])
def test_asana_command_update(to_update, model, expected,
                              asana_command):
    "Does AsanaCommand.update talk to Asana correctly?"
    asana_command.update(to_update, model)
    asana_command._tasks.update.assert_called_with(to_update.id,
                                                   params=expected)

@pytest.mark.parametrize("child, pos, parent", [
    (AsanaNode(), 0, AsanaNode()),
    (AsanaNode(), 0, AsanaNode.from_dict({'id': 1, 'parent': None}))])
def test_asana_command_move_to(child, pos, parent, asana_command):
    "Does AsanaCommand.move_to talk to Asana correctly?"
    asana_command.move_to(child, pos, parent)
    asana_command._tasks.set_parent.assert_called_with(
        child.id, params={'parent': parent.id})

@pytest.mark.parametrize("extra_field_list, expected", [
    (None, ('id', 'name', 'notes', 'parent')),
    (['project'], ('id', 'name', 'notes', 'parent', 'project'))])
def test_asana_command_get_all_items(extra_field_list, expected,
                                     asana_command, mocker):
    "Does AsanaCommand.get_all_items talk to Asana correctly?"
    mocker.patch.object(asana_command._tasks, 'find_all',
                        return_value=[{'id': '1', 'parent': None}])
    asana_command.get_all_items(extra_field_list=extra_field_list)
    asana_command._tasks.find_all.assert_called_with(
        params={'assignee': 'me', 'workspace': 'DEFAULT'},
        fields=expected)
    asana_command._tasks.subtasks.assert_called_with(task='1',
                                                     fields=expected)

@pytest.mark.parametrize("find_all_return, subtasks_return_list, "
                         "expected", [
    ([], [[]], []),
    ([{'id': 1, 'parent': None}], [[]],
     [{'id': '1', 'parent': None}]),
    ([{'id': 1, 'parent': None}],
     [[{'id': 2, 'parent': {'id': 1}}], []],
     [{'id': '1', 'parent': None}, {'id': '2', 'parent': '1'}])

])
def test_asana_command_get_all_items_result(find_all_return,
                                            subtasks_return_list,
                                            expected,
                                            asana_command, mocker):
    "Does AsanaCommand.get_all_items listen to Asana correctly?"
    mocker.patch.object(asana_command._tasks, 'find_all',
                        return_value=find_all_return)
    mocker.patch.object(asana_command._tasks, 'subtasks',
                        side_effect=subtasks_return_list)
    result = asana_command.get_all_items()
    assert result == expected
