import pytest
import unittest.mock as mock

import todo_sync.backends.org as o

from .helpers import assert_trees_equal_p


MOCK_CREATED_HEADLINE_ID = 'NEW HEADLINE'
MOCK_EXTANT_HEADLINE_ID = 'HEADLINE'
MOCK_FILENAME_ID = 'FILENAME'


def mock_root_node():
    return o.RootNode()


def mock_filename_node(info_dict=None):
    default_dict = {'id': MOCK_FILENAME_ID}
    if info_dict:
        default_dict.update(info_dict)
    return o.FilenameNode.from_dict(default_dict, mock.Mock())


def mock_headline_node(info_dict=None):
    default_dict = {'id': MOCK_EXTANT_HEADLINE_ID}
    if info_dict:
        default_dict.update(info_dict)
    return o.HeadlineNode.from_dict(
        default_dict,
        mock.Mock(return_value='"{}"'.format(MOCK_CREATED_HEADLINE_ID)),
        mock.Mock())


def mock_source(get_all_headlines_return=None,
                get_all_filenames_return=None):
    if get_all_headlines_return is None:
        get_all_headlines_return = []
    if get_all_filenames_return is None:
        get_all_filenames_return = []
    repl_source_side_effect = ['"{}"'.format(get_all_headlines_return),
                               '"{}"'.format(get_all_filenames_return)]
    return o.Source(
        mock.Mock(side_effect=repl_source_side_effect),
        mock.Mock(return_value='"{}"'.format(MOCK_CREATED_HEADLINE_ID)),
        mock.Mock(),
        mock.Mock())


def mock_tree(root_node=None, filename_list=None, headline_list=None):
    root = root_node if root_node else mock_root_node()
    node_cache = {}
    if filename_list:
        for d in filename_list:
            node = mock_filename_node(d)
            node_cache[node.id] = node
    if headline_list:
        for d in headline_list:
            node = mock_headline_node(d)
            node_cache[node.id] = node
    for _, node in node_cache.items():
        parent_id = getattr(node, 'parent_id', None)
        if hasattr(node, 'parent_id'):
            del node.parent_id
        if parent_id:
            parent_node = node_cache[parent_id]
        else:
            parent_node = root
        parent_node.append_child(node)
    return root


@pytest.mark.parametrize(
    "parent_node, left_sibling_id, child_node, expected", [
        (mock_headline_node(),
         None,
         mock_headline_node(),
         ''.join(['(ts-insert-child "', MOCK_EXTANT_HEADLINE_ID, '" nil',
                  ' \'(:title "', o.DEFAULT_HEADLINE_TITLE, '"',
                  ' :todo-type "', o.DEFAULT_TODO_TYPE, '"))'])),
        (mock_headline_node({'id': '1'}),
         None,
         mock_headline_node(),
         ''.join(['(ts-insert-child "1" nil',
                  ' \'(:title "', o.DEFAULT_HEADLINE_TITLE, '"',
                  ' :todo-type "', o.DEFAULT_TODO_TYPE, '"))'])),
        (mock_headline_node(),
         "1",
         mock_headline_node(),
         ''.join(['(ts-insert-child "', MOCK_EXTANT_HEADLINE_ID, '" "1"',
                  ' \'(:title "', o.DEFAULT_HEADLINE_TITLE, '"',
                  ' :todo-type "', o.DEFAULT_TODO_TYPE, '"))'])),
        (mock_headline_node(),
         None,
         mock_headline_node({'title': 'Hello!', 'paragraph': 'World!'}),
         ''.join(['(ts-insert-child "', MOCK_EXTANT_HEADLINE_ID, '" nil',
                  ' \'(:title "Hello!"',
                  ' :todo-type "', o.DEFAULT_TODO_TYPE, '"',
                  ' :paragraph "World!"))'])),
        (mock_headline_node(),
         None,
         mock_headline_node({'paragraph': 'a "quoted" string'}),
         ''.join(['(ts-insert-child "', MOCK_EXTANT_HEADLINE_ID, '" nil',
                  ' \'(:title "', o.DEFAULT_HEADLINE_TITLE, '"',
                  ' :todo-type "', o.DEFAULT_TODO_TYPE, '"',
                  ' :paragraph "a \\"quoted\\" string"))'])),
        (mock_headline_node(),
         None,
         mock_headline_node({'paragraph': 'a string with a\nnewline in it'}),
         ''.join(['(ts-insert-child "', MOCK_EXTANT_HEADLINE_ID, '" nil',
                  ' \'(:title "', o.DEFAULT_HEADLINE_TITLE, '"',
                  ' :todo-type "', o.DEFAULT_TODO_TYPE, '"',
                  ' :paragraph "a string with a\\nnewline in it"))'])),
        (mock_headline_node(),
         None,
         mock_headline_node({'todo_type': 'DONE'}),
         ''.join(['(ts-insert-child "', MOCK_EXTANT_HEADLINE_ID, '" nil',
                  ' \'(:title "', o.DEFAULT_HEADLINE_TITLE, '"',
                  ' :todo-type "DONE"))'])),
        (mock_headline_node(),
         None,
         mock_headline_node({'custom_id': 'A'}),
         ''.join(['(ts-insert-child "', MOCK_EXTANT_HEADLINE_ID, '" nil',
                  ' \'(:title "', o.DEFAULT_HEADLINE_TITLE, '"',
                  ' :todo-type "', o.DEFAULT_TODO_TYPE, '"',
                  ' :custom_id "A"))'])),
        (mock_headline_node(),
         None,
         mock_headline_node({'tags': {"morning"}}),
         ''.join(['(ts-insert-child "', MOCK_EXTANT_HEADLINE_ID, '" nil',
                  ' \'(:title "', o.DEFAULT_HEADLINE_TITLE, '"',
                  ' :todo-type "', o.DEFAULT_TODO_TYPE, '"',
                  ' :tags ("morning")))'])),
        (mock_filename_node(),
         None,
         mock_headline_node(),
         ''.join(['(ts-insert-child-into-file "', MOCK_FILENAME_ID, '"',
                  ' nil',
                  ' \'(:title "', o.DEFAULT_HEADLINE_TITLE, '"',
                  ' :todo-type "', o.DEFAULT_TODO_TYPE, '"))'])),
        (mock_root_node(),
         None,
         mock_headline_node(),
         ''.join(['(ts-insert-child-into-file nil nil',
                  ' \'(:title "', o.DEFAULT_HEADLINE_TITLE, '"',
                  ' :todo-type "', o.DEFAULT_TODO_TYPE, '"))']))])
def test_headline_node_insert_as_child(
        parent_node, left_sibling_id, child_node, expected):
    "Does HeadlineNode.external_insert_as_child talk to Emacs correctly?"
    child_node.external_insert_as_child(left_sibling_id, parent_node)
    child_node._repl_make_headline_command.assert_called_with(expected)


@pytest.mark.parametrize(
    "parent_node, left_sibling_id, child_node, expected", [
        (mock_root_node(),
         None,
         mock_filename_node(),
         ''.join(['(ts-insert-file "', MOCK_FILENAME_ID, '" nil \'())'])),
        (mock_root_node(),
         "1",
         mock_filename_node({'project_id': 12345}),
         ''.join(['(ts-insert-file "', MOCK_FILENAME_ID, '" "1"',
                  ' \'(:project_id "12345"))']))])
def test_filename_node_insert_as_child(
        parent_node, left_sibling_id, child_node, expected):
    "Does FilenameNode.external_insert_as_child talk to Emacs correctly?"
    child_node.external_insert_as_child(left_sibling_id, parent_node)
    child_node._repl_to_source_command.assert_called_with(expected)


def test_headline_node_insert_as_child_result():
    "Does HeadlineNode.external_insert_as_child update the child correctly?"
    parent_node = mock_headline_node()
    left_sibling_id = "1"
    child_node = mock_headline_node()
    child_node.external_insert_as_child(left_sibling_id, parent_node)
    assert child_node.id == MOCK_CREATED_HEADLINE_ID


@pytest.mark.parametrize("current_node, new_node, expected", [
    (mock_headline_node({'id': '1'}),
     mock_headline_node(),
     '(ts-update "1" \'())'),
    (mock_headline_node(),
     mock_headline_node({'title': 'Hello!', 'paragraph': 'World!'}),
     ''.join(['(ts-update "', MOCK_EXTANT_HEADLINE_ID, '"',
              ' \'(:title "Hello!" :paragraph "World!"))'])),
    (mock_headline_node(),
     mock_headline_node({'todo_type': 'DONE'}),
     ''.join(['(ts-update "', MOCK_EXTANT_HEADLINE_ID, '"',
              ' \'(:todo-type "DONE"))'])),
    (mock_headline_node(),
     mock_headline_node({'custom_id': 'ID'}),
     ''.join(['(ts-update "', MOCK_EXTANT_HEADLINE_ID, '"',
              ' \'(:custom_id "ID"))'])),
    (mock_headline_node(),
     mock_headline_node({'paragraph': 'a "quoted" string'}),
     ''.join(['(ts-update "', MOCK_EXTANT_HEADLINE_ID, '"',
              ' \'(:paragraph "a \\"quoted\\" string"))'])),
    (mock_headline_node(),
     mock_headline_node({'paragraph': 'a string with a\nnewline in it'}),
     ''.join(['(ts-update "', MOCK_EXTANT_HEADLINE_ID, '"',
              ' \'(:paragraph "a string with a\\nnewline in it"))'])),
    (mock_headline_node({'todo_type': 'DONE',
                         'closed': '2017-01-01T10:30:00.000Z'}),
     mock_headline_node({'todo_type': 'DONE',
                         'closed': '2017-01-31T18:30:00.000Z',
                         'custom': 'ID'}),
     ''.join(['(ts-update "', MOCK_EXTANT_HEADLINE_ID, '"',
              ' \'(:custom "ID"))'])),
    (mock_headline_node(),
     mock_headline_node({'tags': {"morning"}}),
     ''.join(['(ts-update "', MOCK_EXTANT_HEADLINE_ID, '"',
              ' \'(:tags ("morning")))'])),
    (mock_headline_node(),
     mock_headline_node({'tags': {"morning", "@home"}}),
     ''.join(['(ts-update "', MOCK_EXTANT_HEADLINE_ID, '"',
              ' \'(:tags ("@home" "morning")))'])),
    (mock_filename_node({'id': 'project.org'}),
     mock_filename_node(),
     '(ts-update-file "project.org" \'())'),
    (mock_filename_node(),
     mock_filename_node({'project_id': 1234}),
     ''.join(['(ts-update-file "', MOCK_FILENAME_ID, '"',
              ' \'(:project_id "1234"))']))])
def test_headline_node_update(current_node, new_node, expected):
    "Does HeadlineNode.external_update talk to Emacs correctly?"
    current_node.external_update(new_node)
    current_node._repl_to_source_command.assert_called_with(expected)


@pytest.mark.parametrize("child_node, pos, parent_node, expected", [
    (mock_headline_node(),
     None,
     mock_headline_node({'id': '1'}),
     ''.join(['(ts-move-to "', MOCK_EXTANT_HEADLINE_ID, '" nil "1")'])),
    (mock_headline_node(),
     "1",
     mock_headline_node(),
     ''.join(['(ts-move-to "', MOCK_EXTANT_HEADLINE_ID, '"',
              ' "1"',
              ' "', MOCK_EXTANT_HEADLINE_ID, '")'])),
    (mock_headline_node(),
     None,
     mock_filename_node(),
     ''.join(['(ts-move-to-file "', MOCK_EXTANT_HEADLINE_ID, '"',
              ' nil',
              ' "', MOCK_FILENAME_ID, '")'])),
    (mock_headline_node(),
     None,
     mock_root_node(),
     ''.join(['(ts-move-to-file "', MOCK_EXTANT_HEADLINE_ID, '"',
              ' nil nil)']))])
def test_headline_node_move_to(child_node, pos, parent_node, expected):
    "Does HeadlineNode.external_move_to talk to Emacs correctly?"
    child_node.external_move_to(pos, parent_node)
    child_node._repl_to_source_command.assert_called_with(expected)


def test_filename_node_move_to():
    "Does FilenameNode.external_move_to do nothing?"
    parent_node = mock_root_node()
    left_sibling_id = "1"
    child_node = mock_filename_node()
    child_node.external_move_to(left_sibling_id, parent_node)
    child_node._repl_to_source_command.assert_not_called()


@pytest.mark.parametrize("node_to_delete, expected", [
    (mock_headline_node({'id': 1}), '(ts-delete "1")'),
    (mock_filename_node({'id': 'file'}), '(ts-delete-file "file")')])
def test_headline_node_delete(node_to_delete, expected):
    "Does HeadlineNode.external_delete talk to Emacs correctly?"
    node_to_delete.external_delete()
    node_to_delete._repl_to_source_command.assert_called_with(expected)


@pytest.mark.parametrize("source_class", [o.Source, o.DryRunSource])
def test_source_from_repl(source_class):
    "Can we make an Org Source from an Emacs REPL?"
    mock_repl = mock.Mock(['run_command'])
    mock_child = mock.Mock(['sendeof'])
    mock_repl.attach_mock(mock_child, 'child')
    assert source_class.from_emacs_repl(mock_repl)


@pytest.mark.parametrize("org_source, expected", [
    (mock_source(get_all_headlines_return=[]),
     mock_root_node()),
    (mock_source(get_all_headlines_return=[
        {'id': '1', 'parent_id': None}]),
     mock_tree(headline_list=[{'id': '1'}])),
    (mock_source(
        get_all_headlines_return=[{'id': '1', 'parent_id': None},
                                  {'id': '2', 'parent_id': '1'}]),
     mock_tree(headline_list=[{'id': '1'},
                              {'id': '2', 'parent_id': '1'}])),
    # Headlines at the top level of a file should pick up their filename
    # as a parent.  Can we pull stuff that way?
    (mock_source(
        get_all_headlines_return=[
            {'id': '1', 'parent_id': None, 'filename': 'inbox'}],
        get_all_filenames_return=[{'id': 'inbox'}]),
     mock_tree(filename_list=[{'id': 'inbox'}],
               headline_list=[{'id': '1', 'parent_id': 'inbox'}])),
    # Tags come back as a list, but I want them as a set.
    (mock_source(get_all_headlines_return=[
        {'id': '1', 'parent_id': None, 'tags': None}]),
     mock_tree(headline_list=[{'id': '1', 'tags': set()}])),
    (mock_source(get_all_headlines_return=[
        {'id': '1', 'parent_id': None, 'tags': ["morning"]}]),
     mock_tree(headline_list=[{'id': '1', 'tags': {"morning"}}])),
    (mock_source(get_all_headlines_return=[
        {'id': '1', 'parent_id': None, 'tags': ["morning", "evening"]}]),
     mock_tree(headline_list=[{'id': '1',
                               'tags': {"morning", "evening"}}]))])
def test_source_get_all_items_result(org_source, expected):
    "Does Source.get_all_items listen to Emacs correctly?"
    result = org_source.get_all_items()
    assert_trees_equal_p(result, expected)


def test_source_context():
    "Does an Org Source, as a context manager, talk to Emacs correctly?"
    org_source = mock_source()
    with org_source:
        org_source._repl_get_source_command.assert_called_with('(ts-init)')
    org_source._repl_to_source_command.assert_called_with('(ts-final)')
    org_source._repl_sendeof.assert_called()
