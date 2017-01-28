import pytest
import pytest_mock

from org_asana.node import Node
from org_asana.command import PrintCommand, VerbosePrintCommand

@pytest.fixture
def print_command():
    return PrintCommand()

@pytest.fixture
def verbose_command():
    return VerbosePrintCommand()

@pytest.mark.parametrize("parent_node, sib_pos, child_node, expected", [
    (Node(), 0, Node(),
     'insert_child: parent: None, pos: 0, child: None\n'),
    (Node.from_dict({'id': 'Parent', 'parent': 'ROOT'}),
     1,
     Node.from_dict({'id': 'Child', 'parent': 'Parent', 'custom': 'ID'}),
     'insert_child: parent: Parent, pos: 1, child: Child\n')])
def test_print_command_insert_child(parent_node, sib_pos, child_node,
                                    expected, print_command, capsys):
    "Does PrintCommand.insert_child print correctly?"
    print_command.insert_child(parent_node, sib_pos, child_node)
    out, err = capsys.readouterr()
    assert out == expected

@pytest.mark.parametrize("node_to_delete, expected", [
    (Node(), 'delete: None\n'),
    (Node.from_dict({'id': 'Delete Me', 'parent': None}),
     'delete: Delete Me\n')])
def test_print_command_delete(node_to_delete, expected, print_command,
                              capsys):
    "Does PrintCommand.delete print correctly?"
    print_command.delete(node_to_delete)
    out, err = capsys.readouterr()
    assert out == expected

@pytest.mark.parametrize("node_to_update, other_node, expected", [
    (Node(), Node(), 'update: to_update: None, model: None\n'),
    (Node.from_dict({'id': 'Update', 'parent': 'ROOT'}),
     Node.from_dict({'id': 'Model', 'parent': None, 'custom': 'ID'}),
     'update: to_update: Update, model: Model\n')])
def test_print_command_update(node_to_update, other_node, expected,
                              print_command, capsys):
    "Does PrintCommand.update print correctly?"
    print_command.update(node_to_update, other_node)
    out, err = capsys.readouterr()
    assert out == expected

@pytest.mark.parametrize("child_node, sib_pos, parent_node, expected", [
    (Node(), 0, Node(), 'move_to: child: None, pos: 0, parent: None\n'),
    (Node.from_dict({'id': 'Child', 'parent': None}),
     1,
     Node.from_dict({'id': 'Parent', 'parent': 'ROOT'}),
     'move_to: child: Child, pos: 1, parent: Parent\n')])
def test_print_command_move_to(child_node, sib_pos, parent_node,
                               expected, print_command, capsys):
    "Does PrintCommand.move_to print correctly?"
    print_command.move_to(child_node, sib_pos, parent_node)
    out, err = capsys.readouterr()
    assert out == expected

@pytest.mark.parametrize("parent_node, sib_pos, child_node, expected", [
    (Node(), 0, Node(),
     '\n'.join([
        'insert_child:',
        "    parent: {'id': None, 'parent': None}",
        '    pos: 0',
        "    child: {'id': None, 'parent': None}",
        ""])),
    (Node.from_dict({'id': 'Parent', 'parent': 'ROOT'}),
     1,
     Node.from_dict({'id': 'Child', 'parent': 'Parent', 'custom': 'ID'}),
     '\n'.join([
         'insert_child:',
         "    parent: {'id': 'Parent', 'parent': 'ROOT'}",
         "    pos: 1",
         "    child: {'id': 'Child', 'parent': 'Parent', 'custom': 'ID'}",
         ""]))])
def test_verbose_insert_child(parent_node, sib_pos, child_node,
                              expected, verbose_command, capsys):
    "Does VerbosePrintCommand.insert_child print correctly?"
    verbose_command.insert_child(parent_node, sib_pos, child_node)
    out, err = capsys.readouterr()
    assert out == expected

@pytest.mark.parametrize("node_to_delete, expected", [
    (Node(),
     '\n'.join([
         "delete:",
         "    node: {'id': None, 'parent': None}",
         ""])),
    (Node.from_dict({'id': 'Delete', 'parent': 'ROOT',
                     'custom': 'ID'}),
     '\n'.join([
         "delete:",
         "    node: {'id': 'Delete', 'parent': 'ROOT', 'custom': 'ID'}",
         ""]))])
def test_verbose_delete(node_to_delete, expected, verbose_command,
                        capsys):
    "Does VerbosePrintCommand.delete print correctly?"
    verbose_command.delete(node_to_delete)
    out, err = capsys.readouterr()
    assert out == expected

@pytest.mark.parametrize("node_to_update, other_node, expected", [
    (Node(), Node(),
     '\n'.join([
         'update:',
         "    to_update: {'id': None, 'parent': None}",
         "    model: {'id': None, 'parent': None}",
         ""])),
    (Node.from_dict({'id': 'Update', 'parent': 'ROOT'}),
     Node.from_dict({'id': 'Model', 'parent': None, 'custom': 'ID'}),
     '\n'.join([
         "update:",
         "    to_update: {'id': 'Update', 'parent': 'ROOT'}",
         "    model: {'id': 'Model', 'parent': None, 'custom': 'ID'}",
         ""]))])
def test_verbose_command_update(node_to_update, other_node, expected,
                                verbose_command, capsys):
    "Does VerbosePrintCommand.update print correctly?"
    verbose_command.update(node_to_update, other_node)
    out, err = capsys.readouterr()
    assert out == expected

@pytest.mark.parametrize("child_node, sib_pos, parent_node, expected", [
    (Node(), 0, Node(),
     '\n'.join([
         "move_to:",
         "    child: {'id': None, 'parent': None}",
         "    pos: 0",
         "    parent: {'id': None, 'parent': None}",
         ""])),
    (Node.from_dict({'id': 'Child', 'parent': None}),
     1,
     Node.from_dict({'id': 'Parent', 'parent': 'ROOT'}),
     '\n'.join([
         "move_to:",
         "    child: {'id': 'Child', 'parent': None}",
         "    pos: 1",
         "    parent: {'id': 'Parent', 'parent': 'ROOT'}",
         ""]))])
def test_verbose_command_move_to(child_node, sib_pos, parent_node,
                                 expected, verbose_command, capsys):
    "Does VerbosePrintCommand.move_to print correctly?"
    verbose_command.move_to(child_node, sib_pos, parent_node)
    out, err = capsys.readouterr()
    assert out == expected
