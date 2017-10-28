import pytest
import unittest.mock as mock
import copy

import asana

import todo_sync.node as node
import todo_sync.backends.asana as a
import todo_sync.helpers as lib

from .helpers import assert_trees_equal_p

MOCK_WORKSPACE_ID = 'DEFAULT'
MOCK_CREATED_PROJECT_ID = 'NEW PROJECT'
MOCK_EXTANT_PROJECT_ID = 'PROJECT'
MOCK_CREATED_TAG_ID = 'NEW TAG'
MOCK_CREATED_TASK_ID = 'NEW TASK'
MOCK_EXTANT_TASK_ID = 'TASK'


def mock_root_node():
    return node.RootNode()


def mock_project_node(info_dict=None):
    default_dict = {'id': MOCK_EXTANT_PROJECT_ID, 'parent': None}
    if info_dict:
        default_dict.update(info_dict)
    return a.ProjectNode.from_dict(
        default_dict,
        MOCK_WORKSPACE_ID,
        mock.Mock(return_value={'id': MOCK_CREATED_PROJECT_ID}),
        mock.Mock(),
        mock.Mock())


def mock_task_node(info_dict=None, tag_dict=None):
    default_dict = {'id': MOCK_EXTANT_TASK_ID, 'parent': None}
    if info_dict:
        default_dict.update(info_dict)
    if tag_dict is None:
        tag_dict = {}
    return a.TaskNode.from_dict(
        default_dict,
        MOCK_WORKSPACE_ID,
        mock.Mock(return_value={'id': MOCK_CREATED_PROJECT_ID}),
        mock.Mock(),
        mock.Mock(),
        a.TagNameLookup(
            MOCK_WORKSPACE_ID,
            mock.Mock(return_value={'id': MOCK_CREATED_TAG_ID}),
            tag_dict),
        mock.Mock(return_value={'id': MOCK_CREATED_TASK_ID}),
        mock.Mock(),
        mock.Mock(),
        mock.Mock(),
        mock.Mock(),
        mock.Mock(),
        mock.Mock(),
        mock.Mock())


def mock_projtask_node(info_dict=None):
    default_dict = {'project_id': MOCK_EXTANT_PROJECT_ID}
    if info_dict:
        default_dict.update(info_dict)
    return mock_task_node(default_dict)


def default_side_effect_fn(*args, **kwargs):
    return mock.DEFAULT


def mock_source(find_by_workspace_return=None,
                find_by_workspace_side_effect=None,
                project_tasks_return=None, project_tasks_side_effect=None,
                tag_find_by_workspace_return=None, find_all_return=None,
                subtasks_return=None, subtasks_side_effect=None):
    if find_by_workspace_return is None:
        find_by_workspace_return = []
    if find_by_workspace_side_effect is None:
        find_by_workspace_side_effect = default_side_effect_fn
    if project_tasks_return is None:
        project_tasks_return = []
    if project_tasks_side_effect is None:
        project_tasks_side_effect = default_side_effect_fn
    if tag_find_by_workspace_return is None:
        tag_find_by_workspace_return = []
    if find_all_return is None:
        find_all_return = []
    if subtasks_return is None:
        subtasks_return = []
    if subtasks_side_effect is None:
        subtasks_side_effect = default_side_effect_fn
    return a.Source(
        MOCK_WORKSPACE_ID,
        mock.Mock(return_value=find_by_workspace_return,
                  side_effect=find_by_workspace_side_effect),
        mock.Mock(return_value=project_tasks_return,
                  side_effect=project_tasks_side_effect),
        mock.Mock(return_value={'id': MOCK_CREATED_PROJECT_ID}),
        mock.Mock(),
        mock.Mock(),
        mock.Mock(return_value=tag_find_by_workspace_return),
        mock.Mock(return_value={'id': MOCK_CREATED_TAG_ID}),
        mock.Mock(return_value=find_all_return),
        mock.Mock(return_value=subtasks_return,
                  side_effect=subtasks_side_effect),
        mock.Mock(return_value={'id': MOCK_CREATED_TASK_ID}),
        mock.Mock(),
        mock.Mock(),
        mock.Mock(),
        mock.Mock(),
        mock.Mock(),
        mock.Mock(),
        mock.Mock())


def mock_tree(root_node=None, project_list=None, task_list=None):
    root = root_node if root_node else mock_root_node()
    node_cache = {}
    if project_list:
        for d in project_list:
            node = mock_project_node(d)
            node_cache[node.id] = node
    if task_list:
        for d in task_list:
            node = mock_task_node(d)
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


def test_task_node_insert_as_child_under_task_make_a_project():
    "Does TaskNode.external_insert_as_child create a project?"
    child_node = mock_task_node()
    left_sibling_id = None
    parent_node = mock_task_node()
    child_node.external_insert_as_child(left_sibling_id, parent_node)
    parent_node._project_create_in_workspace.assert_called_with(
        MOCK_WORKSPACE_ID, params={'name': parent_node.name,
                                   'archived': parent_node.completed})


def test_task_node_insert_as_child_under_task_set_project_id():
    "Does TaskNode.external_insert_as_child set the project id?"
    child_node = mock_task_node()
    left_sibling_id = None
    parent_node = mock_task_node()
    child_node.external_insert_as_child(left_sibling_id, parent_node)
    assert parent_node.project_id == MOCK_CREATED_PROJECT_ID


def test_task_node_insert_as_child_under_task_unset_parent():
    "Does TaskNode.external_insert_as_child unparent other subtasks?"
    child_node = mock_task_node()
    left_sibling_id = None
    parent_node = mock_tree(mock_task_node(),
                             task_list=[{'id': 'A'}, {'id': 'B'}])
    child_node.external_insert_as_child(left_sibling_id, parent_node)
    for c_node in parent_node.children:
        c_node._task_set_parent.assert_called_with(
            c_node.id, params={'parent': None})


def test_task_node_insert_as_child_under_task_add_subtasks():
    "Does TaskNode.external_insert_as_child add subtasks to the project in order?"
    child_node = mock_task_node()
    left_sibling_id = None
    parent_node = mock_tree(mock_task_node(),
                             task_list=[{'id': 'A'}, {'id': 'B'}])
    child_node.external_insert_as_child(left_sibling_id, parent_node)
    task_add_project_params = {'project': MOCK_CREATED_PROJECT_ID,
                               'insert_after': None}
    for c_node in parent_node.children:
        c_node._task_add_project.assert_called_with(
            c_node.id, params=task_add_project_params)
        task_add_project_params['insert_after'] = c_node.id


@pytest.mark.parametrize("child_node, expected_task_create", [
    (mock_task_node(),
     {'assignee': 'me', 'workspace': MOCK_WORKSPACE_ID,
      'name': a.DEFAULT_TASK_NAME, 'completed': False}),
    (mock_task_node({'id': 'ignored'}),
     {'assignee': 'me', 'workspace': MOCK_WORKSPACE_ID,
      'name': a.DEFAULT_TASK_NAME, 'completed': False}),
    (mock_task_node({'name': 'a task'}),
     {'assignee': 'me', 'workspace': MOCK_WORKSPACE_ID,
      'name': 'a task', 'completed': False}),
    (mock_task_node({'custom': 'ID'}),
     {'assignee': 'me', 'workspace': MOCK_WORKSPACE_ID,
      'name': a.DEFAULT_TASK_NAME, 'completed': False, 'custom': 'ID'})])
def test_task_node_insert_as_child_under_task_make_a_task(
        child_node, expected_task_create):
    "Does TaskNode.external_insert_as_child create a task?"
    left_sibling_id = None
    parent_node = mock_task_node()
    child_node.external_insert_as_child(left_sibling_id, parent_node)
    child_node._task_create.assert_called_with(
        params=expected_task_create)


@pytest.mark.parametrize("child_node, expected_task_create", [
    (mock_task_node({'tags': set()}, tag_dict={}),
     {'assignee': 'me', 'workspace': MOCK_WORKSPACE_ID,
      'name': a.DEFAULT_TASK_NAME, 'completed': False, 'tags': []}),
    (mock_task_node({'tags': {'morning', 'evening'}},
                    tag_dict={'morning': 1, 'evening': 2}),
     {'assignee': 'me', 'workspace': MOCK_WORKSPACE_ID,
      'name': a.DEFAULT_TASK_NAME, 'completed': False, 'tags': [1, 2]})])
def test_task_node_insert_as_child_under_task_make_a_tagged_task(
        child_node, expected_task_create):
    "Does TaskNode.external_insert_as_child create a task with tags?"
    left_sibling_id = None
    parent_node = mock_task_node()
    child_node.external_insert_as_child(left_sibling_id, parent_node)
    child_node._task_create.assert_called_with(
        params=expected_task_create)


def test_task_node_insert_as_child_under_task_make_a_tag():
    "Does TaskNode.external_insert_as_child create new tags for tasks?"
    child_node = mock_task_node({'tags': {'morning'}},
                                tag_dict={})
    tag_name_lookup = child_node._tag_name_lookup
    left_sibling_id = None
    parent_node = mock_task_node()
    child_node.external_insert_as_child(left_sibling_id, parent_node)
    tag_name_lookup._tag_create_in_workspace.assert_called_with(
        MOCK_WORKSPACE_ID, params={'name': 'morning'})


def test_task_node_insert_as_child_under_task_store_a_tag():
    "Does TaskNode.external_insert_as_child store new tags?"
    child_node = mock_task_node({'tags': {'morning'}},
                                tag_dict={})
    tag_name_lookup = child_node._tag_name_lookup
    left_sibling_id = None
    parent_node = mock_task_node()
    child_node.external_insert_as_child(left_sibling_id, parent_node)
    assert 'morning' in tag_name_lookup


def test_task_node_insert_as_child_under_task_set_task_id():
    "Does TaskNode.external_insert_as_child set the task id?"
    child_node = mock_task_node()
    left_sibling_id = None
    parent_node = mock_task_node()
    child_node.external_insert_as_child(left_sibling_id, parent_node)
    assert child_node.id == MOCK_CREATED_TASK_ID


@pytest.mark.parametrize("left_sibling_id, parent_node", [
    (None, mock_task_node()),
    (None, mock_tree(mock_task_node(),
                     task_list=[{'id': 'A'}, {'id': 'B'}])),
    ('A', mock_tree(mock_task_node(),
                     task_list=[{'id': 'A'}, {'id': 'B'}])),
    ('B', mock_tree(mock_task_node(),
                     task_list=[{'id': 'A'}, {'id': 'B'}]))])
def test_task_node_insert_as_child_under_task_add_project(
        left_sibling_id, parent_node):
    "Does TaskNode.external_insert_as_child add the task to the project?"
    child_node = mock_task_node()
    child_node.external_insert_as_child(left_sibling_id, parent_node)
    child_node._task_add_project.assert_called_with(
        child_node.id, params={'project': MOCK_CREATED_PROJECT_ID,
                               'insert_after': left_sibling_id})


def test_task_node_insert_as_child_under_projectified_task_no_project():
    child_node = mock_task_node()
    left_sibling_id = None
    parent_node = mock_projtask_node()
    child_node.external_insert_as_child(left_sibling_id, parent_node)
    parent_node._project_create_in_workspace.assert_not_called()


def test_task_node_insert_as_child_under_projectified_task_no_reparent():
    child_node = mock_task_node()
    left_sibling_id = None
    parent_node = mock_tree(mock_projtask_node(),
                            task_list=[{'id': 'A'}, {'id': 'B'}])
    child_node.external_insert_as_child(left_sibling_id, parent_node)
    for c_node in parent_node.children:
        if c_node.id != child_node.id:
            c_node._task_set_parent.assert_not_called()


def test_task_node_insert_as_child_under_projectified_task_no_reproject():
    child_node = mock_task_node()
    left_sibling_id = None
    parent_node = mock_tree(mock_projtask_node(),
                            task_list=[{'id': 'A'}, {'id': 'B'}])
    child_node.external_insert_as_child(left_sibling_id, parent_node)
    for c_node in parent_node.children:
        if c_node.id != child_node.id:
            c_node._task_add_project.assert_not_called()


@pytest.mark.parametrize(
    "parent_node, left_sibling_id, child_node, expected_task_create", [
        (mock_tree(mock_projtask_node()),
         None,
         mock_task_node(),
         {'assignee': 'me', 'workspace': MOCK_WORKSPACE_ID,
          'name': a.DEFAULT_TASK_NAME, 'completed': False}),
        (mock_tree(mock_projtask_node()),
         None,
         mock_task_node({'name': 'a task'}),
         {'assignee': 'me', 'workspace': MOCK_WORKSPACE_ID,
          'name': 'a task', 'completed': False}),
        (mock_tree(mock_projtask_node()),
         None,
         mock_task_node({'custom': 'ID'}),
         {'assignee': 'me', 'workspace': MOCK_WORKSPACE_ID,
          'name': a.DEFAULT_TASK_NAME, 'completed': False, 'custom': 'ID'})])
def test_task_node_insert_as_child_under_projectified_task_create_task(
        parent_node, left_sibling_id, child_node, expected_task_create):
    child_node.external_insert_as_child(left_sibling_id, parent_node)
    child_node._task_create.assert_called_with(
        params=expected_task_create)


def test_task_node_insert_as_child_under_projectified_task_set_task_id():
    "Does TaskNode.external_insert_as_child set the task id?"
    child_node = mock_task_node()
    left_sibling_id = None
    parent_node = mock_projtask_node()
    child_node.external_insert_as_child(left_sibling_id, parent_node)
    assert child_node.id == MOCK_CREATED_TASK_ID


@pytest.mark.parametrize("left_sibling_id, parent_node", [
    (None, mock_projtask_node()),
    (None, mock_tree(mock_projtask_node(),
                     task_list=[{'id': 'A'}, {'id': 'B'}])),
    ('A', mock_tree(mock_projtask_node(),
                     task_list=[{'id': 'A'}, {'id': 'B'}])),
    ('B', mock_tree(mock_projtask_node(),
                     task_list=[{'id': 'A'}, {'id': 'B'}]))])
def test_task_node_insert_as_child_under_projectified_task_add_project(
        left_sibling_id, parent_node):
    "Does TaskNode.external_insert_as_child add the task to the project?"
    child_node = mock_task_node()
    child_node.external_insert_as_child(left_sibling_id, parent_node)
    child_node._task_add_project.assert_called_with(
        child_node.id, params={'project': MOCK_EXTANT_PROJECT_ID,
                               'insert_after': left_sibling_id})


@pytest.mark.parametrize(
    "parent_node, left_sibling_id, child_node, expected_task_create,"
    " expected_task_reparent", [
        (mock_tree(mock_project_node({'id': 3})),
         None,
         mock_task_node(),
         {'assignee': 'me', 'workspace': MOCK_WORKSPACE_ID,
          'name': a.DEFAULT_TASK_NAME, 'completed': False},
         {'project': 3, 'insert_after': None}),
        (mock_tree(mock_project_node({'id': 0, 'name': 'a project'}),
                   task_list=[{'id': 'A'}]),
         None,
         mock_task_node(),
         {'assignee': 'me', 'workspace': MOCK_WORKSPACE_ID,
          'name': a.DEFAULT_TASK_NAME, 'completed': False},
         {'project': 0, 'insert_after': None}),
        (mock_tree(mock_project_node({'id': 0}),
                   task_list=[{'id': 'A'}, {'id': 'B'}]),
         'A',
         mock_task_node(),
         {'assignee': 'me', 'workspace': MOCK_WORKSPACE_ID,
          'name': a.DEFAULT_TASK_NAME, 'completed': False},
         {'project': 0, 'insert_after': 'A'}),
        (mock_tree(mock_project_node({'id': 0}),
                   task_list=[{'id': 'A'}, {'id': 'B', 'parent': 'A'}]),
         'A',
         mock_task_node(),
         {'assignee': 'me', 'workspace': MOCK_WORKSPACE_ID,
          'name': a.DEFAULT_TASK_NAME, 'completed': False},
         {'project': 0, 'insert_after': 'A'}),
        (mock_tree(mock_project_node({'id': 0})),
         None,
         mock_task_node({'name': 'a task', 'custom': 'ID'}),
         {'assignee': 'me', 'workspace': MOCK_WORKSPACE_ID,
          'name': 'a task', 'completed': False, 'custom': 'ID'},
         {'project': 0, 'insert_after': None})])
def test_task_node_insert_as_child_under_project_nodes(
        parent_node, left_sibling_id, child_node, expected_task_create,
        expected_task_reparent):
    "Does TaskNode.external_insert_as_child talk to Asana correctly?"
    child_node.external_insert_as_child(left_sibling_id, parent_node)
    parent_node._create_in_workspace.assert_not_called()
    child_node._task_create.assert_called_with(
        params=expected_task_create)
    child_node._task_add_project.assert_called_with(
        child_node.id, params=expected_task_reparent)


@pytest.mark.parametrize(
    "parent_node, pos, child_node, expected_project_create", [
        (mock_root_node(),
         0,
         mock_project_node(),
         {'name': a.DEFAULT_PROJECT_NAME}),
        (mock_root_node(),
         1,
         mock_project_node({'name': 'a project'}),
         {'name': 'a project'})])
def test_project_node_insert_as_child(
        parent_node, pos, child_node, expected_project_create):
    "Does ProjectNode.external_insert_as_child talk to Asana correctly?"
    child_node.external_insert_as_child(pos, parent_node)
    child_node._create_in_workspace.assert_called_with(
        MOCK_WORKSPACE_ID, params=expected_project_create)


@pytest.mark.parametrize("parent_node, pos, child_node", [
    (mock_task_node({'project_id': 1}), 0, mock_task_node()),
    (mock_project_node(), 0, mock_task_node())])
def test_task_node_insert_as_child_under_projecty_node_result(
        parent_node, pos, child_node):
    "Does TaskNode.external_insert_as_child update the child correctly?"
    child_node.external_insert_as_child(pos, parent_node)
    assert child_node.id == MOCK_CREATED_TASK_ID


def test_project_node_insert_as_child_result():
    "Does ProjectNode.external_insert_as_child update the child correctly?"
    parent_node = mock_root_node()
    pos = 0
    child_node = mock_project_node()
    child_node.external_insert_as_child(pos, parent_node)
    assert child_node.id == MOCK_CREATED_PROJECT_ID


@pytest.mark.parametrize("current_node, new_node, expected", [
    (mock_task_node(),
     mock_task_node({'name': "Hello", 'notes': "World"}),
     {'name': "Hello", 'notes': "World"}),
    (mock_task_node({'id': 1, 'name': MOCK_WORKSPACE_ID}),
     mock_task_node({'id': None, 'name': 'Update'}),
     {'name': 'Update'}),
    (mock_task_node(),
     mock_task_node({'completed': True}),
     {'completed': True}),
    (mock_task_node({'completed': True}),
     mock_task_node(),
     {'completed': False}),
    (mock_task_node(),
     mock_task_node({'custom': 'ID'}),
     {'custom': 'ID'}),
    (mock_task_node(),
     mock_task_node({'notes': "A string with a\nnewline in it."}),
     {'notes': "A string with a\nnewline in it."})])
def test_task_node_update(current_node, new_node, expected):
    "Does TaskNode.external_update talk to Asana correctly?"
    current_node.external_update(new_node)
    current_node._task_update.assert_called_with(current_node.id,
                                                 params=expected)


@pytest.mark.parametrize(
    "current_node, new_node, expected_add, expected_remove", [
        (mock_task_node(tag_dict={'morning': 1}),
         mock_task_node({'tags': {'morning'}}),
         [1], []),
        (mock_task_node({'tags': {'evening'}},
                        tag_dict={'evening': 2}),
         mock_task_node({'tags': set()}),
         [], [2]),
        (mock_task_node({'tags': {'morning', '@home'}},
                        tag_dict={'morning': 1, '@home': 2,
                                  'evening': 3, '@work': 4}),
         mock_task_node({'tags': {'evening', '@work'}}),
         [3, 4], [1, 2])])
def test_task_node_update_tags(current_node, new_node,
                               expected_add, expected_remove):
    "Does TaskNode.external_update handle tags correctly?"
    current_node.external_update(new_node)
    add_calls = [mock.call(current_node.id, params={'tag': t})
                 for t in expected_add]
    current_node._task_add_tag.assert_has_calls(add_calls, any_order=True)
    remove_calls = [mock.call(current_node.id, params={'tag': t})
                    for t in expected_remove]
    current_node._task_remove_tag.assert_has_calls(
        remove_calls, any_order=True)
    current_node._task_update.assert_not_called()


def test_task_node_update_tags_noop():
    "Does TaskNode.external_update handle tags correctly?"
    current_node = mock_task_node({'tags': {'project'}})
    new_node = mock_task_node({'tags': {'project'}})
    current_node.external_update(new_node)
    current_node._task_add_tag.assert_not_called()
    current_node._task_remove_tag.assert_not_called()


@pytest.mark.parametrize(
    "current_node, new_node, expected_task_update, expected_project_update", [
        (mock_task_node({'project_id': '1'}),
         mock_task_node({'name': 'New Name'}),
         {'name': 'New Name'},
         {'name': 'New Name'}),
        (mock_task_node({'project_id': '1'}),
         mock_task_node({'completed': True}),
         {'completed': True},
         {'archived': True})])
def test_projectified_task_node_update(
        current_node, new_node, expected_task_update,
        expected_project_update):
    "Does TaskNode.external_update talk to Asana correctly?"
    current_node.external_update(new_node)
    current_node._task_update.assert_called_with(
        current_node.id, params=expected_task_update)
    current_node._project_update.assert_called_with(
        current_node.project_id, params=expected_project_update)


@pytest.mark.parametrize("current_node, new_node, expected", [
    (mock_project_node(),
     mock_project_node({'name': 'new name'}),
     {'name': 'new name'})])
def test_project_node_update(current_node, new_node, expected):
    "Does ProjectNode.external_update talk to Asana correctly?"
    current_node.external_update(new_node)
    current_node._update.assert_called_with(current_node.id,
                                            params=expected)


@pytest.mark.parametrize(
    "child_node, left_sibling_id, parent_node, expected_project_create,"
    " expected_task_reparent", [
        (mock_task_node(),
         None,
         mock_tree(mock_task_node()),
         {'name': a.DEFAULT_TASK_NAME, 'archived': False},
         {'project': MOCK_CREATED_PROJECT_ID, 'insert_after': None}),
        (mock_task_node(),
         None,
         mock_tree(mock_task_node({'name': 'a project'}),
                   task_list=[{'id': 'A'}]),
         {'name': 'a project', 'archived': False},
         {'project': MOCK_CREATED_PROJECT_ID, 'insert_after': None}),
        (mock_task_node(),
         'A',
         mock_tree(mock_task_node(),
                   task_list=[{'id': 'A'}, {'id': 'B'}]),
         {'name': a.DEFAULT_TASK_NAME, 'archived': False},
         {'project': MOCK_CREATED_PROJECT_ID, 'insert_after': 'A'}),
        (mock_task_node({'name': 'a task', 'custom': 'ID'}),
         None,
         mock_tree(mock_task_node()),
         {'name': a.DEFAULT_TASK_NAME, 'archived': False},
         {'project': MOCK_CREATED_PROJECT_ID, 'insert_after': None}),
        (lib.breadth_first_order(
            mock_tree(mock_task_node(), task_list=[{'id': 'A'}]))[-1],
         None,
         mock_tree(mock_task_node(),
                   task_list=[{'id': 'B'}, {'id': 'C'}]),
         {'name': a.DEFAULT_TASK_NAME, 'archived': False},
         {'project': MOCK_CREATED_PROJECT_ID, 'insert_after': None})])
def test_task_node_move_to_task(
        child_node, left_sibling_id, parent_node, expected_project_create,
        expected_task_reparent):
    "Does TaskNode.external_move_to talk to Asana correctly?"
    old_parent_node = child_node.parent
    child_node.external_move_to(left_sibling_id, parent_node)
    parent_node._project_create_in_workspace.assert_called_with(
        MOCK_WORKSPACE_ID, params=expected_project_create)
    new_left_sibling_id = None
    for c_node in parent_node.children:
        expected_task_reparent['insert_after'] = new_left_sibling_id
        c_node._task_set_parent.assert_called_with(
            c_node.id, params={'parent': None})
        c_node._task_add_project.assert_called_with(
            c_node.id, params=expected_task_reparent)
        new_left_sibling_id = c_node.id
    expected_task_reparent['insert_after'] = left_sibling_id
    if old_parent_node:
        if old_parent_node.project_id:
            child_node._task_remove_project.assert_called_with(
                child_node.id, params={
                    'project': old_parent_node.project_id})
        else:
            child_node._task_set_parent.assert_called_with(
                child_node.id, params={'parent': None})
    child_node._task_add_project.assert_called_with(
        child_node.id, params=expected_task_reparent)


@pytest.mark.parametrize(
    "child_node, left_sibling_id, parent_node, expected_task_reparent", [
        (mock_task_node(),
         None,
         mock_tree(mock_task_node({'project_id': 3})),
         {'project': 3, 'insert_after': None}),
        (mock_task_node(),
         None,
         mock_tree(
             mock_task_node({'project_id': 0, 'name': 'a project'}),
             task_list=[{'id': 'A'}]),
         {'project': 0, 'insert_after': None}),
        (mock_task_node(),
         'A',
         mock_tree(mock_task_node({'project_id': 0}),
                   task_list=[{'id': 'A'}, {'id': 'B'}]),
         {'project': 0, 'insert_after': 'A'}),
        (mock_task_node({'name': 'a task', 'custom': 'ID'}),
         None,
         mock_tree(mock_task_node({'project_id': 0})),
         {'project': 0, 'insert_after': None}),
        (lib.breadth_first_order(
            mock_tree(mock_projtask_node(), task_list=[{'id': 'A'}]))[-1],
         None,
         mock_task_node({'project_id': 0}),
         {'project': 0, 'insert_after': None})])
def test_task_node_move_to_projectified_task_node(
        child_node, left_sibling_id, parent_node, expected_task_reparent):
    "Does TaskNode.external_move_to talk to Asana correctly?"
    old_parent_node = child_node.parent
    child_node.external_move_to(left_sibling_id, parent_node)
    parent_node._project_create_in_workspace.assert_not_called()
    if old_parent_node:
        if old_parent_node.project_id:
            child_node._task_remove_project.assert_called_with(
                child_node.id, params={
                    'project': old_parent_node.project_id})
        else:
            child_node._task_set_parent.assert_called_with(
                child_node.id, params={'parent': None})
    child_node._task_add_project.assert_called_with(
        child_node.id, params=expected_task_reparent)


@pytest.mark.parametrize(
    "child_node, left_sibling_id, parent_node, expected_task_reparent", [
        (mock_task_node(),
         None,
         mock_tree(mock_project_node({'id': 3})),
         {'project': 3, 'insert_after': None}),
        (mock_task_node(),
         None,
         mock_tree(
             mock_project_node({'id': 0, 'name': 'a project'}),
             task_list=[{'id': 'A'}]),
         {'project': 0, 'insert_after': None}),
        (mock_task_node(),
         'A',
         mock_tree(mock_project_node({'id': 0}),
                   task_list=[{'id': 'A'}, {'id': 'B'}]),
         {'project': 0, 'insert_after': 'A'}),
        (mock_task_node({'name': 'a task', 'custom': 'ID'}),
         None,
         mock_tree(mock_project_node({'id': 0})),
         {'project': 0, 'insert_after': None})])
def test_task_node_move_to_project_node(
        child_node, left_sibling_id, parent_node, expected_task_reparent):
    "Does TaskNode.external_move_to talk to Asana correctly?"
    child_node.external_move_to(left_sibling_id, parent_node)
    parent_node._create_in_workspace.assert_not_called()
    child_node._task_add_project.assert_called_with(
        child_node.id, params=expected_task_reparent)


def test_task_node_delete():
    "Does TaskNode.external_delete talk to Asana correctly?"
    node = mock_task_node({'id': 1})
    node.external_delete()
    node._task_delete.assert_called_with(node.id)


def test_projectified_task_node_delete():
    "Does TaskNode.external_delete talk to Asana correctly?"
    node = mock_task_node({'id': 1, 'project_id': 1})
    node.external_delete()
    node._task_delete.assert_called_with(node.id)
    node._project_delete.assert_called_with(node.project_id)


def test_project_node_delete():
    "Does ProjectNode.external_delete talk to Asana correctly?"
    node = mock_project_node({'id': 1})
    node.external_delete()
    node._delete.assert_called_with(node.id)


@pytest.mark.parametrize("source_class", [a.Source, a.DryRunSource])
def test_source_from_client(source_class):
    "Can we make an Asana Source from an Asana Client?"
    mock_client = mock.Mock(asana.Client)
    users_kwargs = {'me.return_value':
                    {'workspaces': [{'id': MOCK_WORKSPACE_ID}]}}
    mock_users = mock.Mock(asana.resources.users.Users, **users_kwargs)
    mock_client.attach_mock(mock_users, 'users')
    mock_projects = mock.Mock(asana.resources.projects.Projects)
    mock_client.attach_mock(mock_projects, 'projects')
    mock_tasks = mock.Mock(asana.resources.tasks.Tasks)
    mock_client.attach_mock(mock_tasks, 'tasks')
    mock_tags = mock.Mock(asana.resources.tags.Tags)
    mock_client.attach_mock(mock_tags, 'tags')
    assert source_class.from_client(mock_client)


def subtask_side_effect_fn(*tasks):
    def subtasks(task, fields):
        return [copy.copy(t) for t in tasks if t['parent']['id'] == task]
    return subtasks


def project_tasks_side_effect_fn(*tasks):
    def project_tasks(task, fields):
        return [copy.copy(t) for t in tasks
                if t['projects'][0]['id'] == task]
    return project_tasks


def find_by_workspace_side_effect_fn(*projects):
    def filter_projects(w_id, params=None):
        if params is None:
            params = {'archived': False}
        return [copy.copy(p) for p in projects
                if p['archived'] == params['archived']]
    return filter_projects


@pytest.mark.parametrize("asana_source, expected", [
    (mock_source(),
     mock_root_node()),
    (mock_source(find_by_workspace_return=[{'id': 1}]),
     mock_tree(project_list=[{'id': 1}])),
    (mock_source(find_all_return=[{'id': 1, 'parent': None}]),
     mock_tree(task_list=[{'id': 1}])),
    (mock_source(find_all_return=[{'id': 1, 'parent': None},
                                  {'id': 2, 'parent': None}]),
     mock_tree(task_list=[{'id': 1}, {'id': 2}])),
    # but on the other hand, Asana API returns projects and subtasks in
    # forward order, so we must handle those correctly, too
    (mock_source(find_by_workspace_return=[{'id': 1}, {'id': 2}]),
     mock_tree(project_list=[{'id': 1}, {'id': 2}])),
    (mock_source(
            find_all_return=[{'id': 1, 'parent': None},
                             {'id': 2, 'parent': {'id': 1}},
                             {'id': 3, 'parent': {'id': 1}}],
            subtasks_side_effect=subtask_side_effect_fn(
                {'id': 2, 'parent': {'id': 1}},
                {'id': 3, 'parent': {'id': 1}})),
     mock_tree(task_list=[{'id': 1},
                          {'id': 2, 'parent_id': 1},
                          {'id': 3, 'parent_id': 1}])),
    # Tasks at the top level pick up their project as a parent.
    # WARNING!  We're implicitly assuming Project IDs and Task IDs never
    # clash.  Maybe this is always valid?  It looks okay to me.
    (mock_source(
        find_by_workspace_return=[{'id': 1}],
        find_all_return=[{'id': 2, 'parent': None,
                          'projects': [{'id': 1}]}],
        project_tasks_return=[{'id': 2, 'parent': None,
                               'projects': [{'id': 1}]}]),
     mock_tree(project_list=[{'id': 1}],
               task_list=[{'id': 2, 'parent_id': 1}])),
    # Let's put everything together.
    (mock_source(
        find_by_workspace_return=[
            {'id': 1, 'name': 'A project'}],
        find_all_return=[{'id': 2, 'parent': None,
                          'projects': [{'id': 1}]},
                         {'id': 3, 'parent': {'id': 2},
                          'projects': [{'id': 1}]},
                         {'id': 4, 'parent': {'id': 2},
                          'projects': [{'id': 1}]}],
        project_tasks_side_effect=project_tasks_side_effect_fn(
            {'id': 2, 'parent': None, 'projects': [{'id': 1}]}),
        subtasks_side_effect=subtask_side_effect_fn(
            {'id': 3, 'parent': {'id': 2}, 'projects': [{'id': 1}]},
            {'id': 4, 'parent': {'id': 2}, 'projects': [{'id': 1}]})),
     mock_tree(project_list=[{'id': 1, 'name': 'A project'}],
               task_list=[{'id': 2, 'parent_id': 1},
                          {'id': 3, 'parent_id': 2},
                          {'id': 4, 'parent_id': 2}])),
    # Part of the plan involves treating a task and project with the
    # same name as really the same object.  Can we pull it that way?
    (mock_source(
            find_by_workspace_return=[
                {'id': 1, 'name': 'A task "project"'}],
            find_all_return=[
                {'id': 2, 'parent': None, 'name': 'A task "project"',
                 'projects': []}]),
     mock_tree(task_list=[{'id': 2, 'name': 'A task "project"',
                           'parent_id': None, 'project_id': 1}])),
    # We also need to rejigger all the parent pointers on tasks when
    # their project is really a task.  Can we pull that way?
    (mock_source(
        find_by_workspace_return=[
            {'id': 1, 'name': 'A task "project"'}],
        find_all_return=[
            {'id': 2, 'parent': None, 'name': 'A task "project"',
             'projects': []},
            {'id': 3, 'parent': None, 'name': '1st under Task',
             'projects': [{'id': 1, 'name': 'A task "project"'}]},
            {'id': 4, 'parent': None, 'name': '2nd under Task',
             'projects': [{'id': 1, 'name': 'A task "project"'}]},
            {'id': 5, 'parent': None, 'name': '3rd under Task',
             'projects': [{'id': 1, 'name': 'A task "project"'}]},
            {'id': 6, 'parent': None, 'name': 'After task',
             'projects': []}],
        project_tasks_return=[
            {'id': 3, 'parent': None, 'name': '1st under Task',
             'projects': [{'id': 1, 'name': 'A task "project"'}]},
            {'id': 4, 'parent': None, 'name': '2nd under Task',
             'projects': [{'id': 1, 'name': 'A task "project"'}]},
            {'id': 5, 'parent': None, 'name': '3rd under Task',
             'projects': [{'id': 1, 'name': 'A task "project"'}]}]),
     mock_tree(task_list=[{'id': 2, 'name': 'A task "project"',
                           'project_id': 1},
                          {'id': 3, 'name': '1st under Task',
                           'parent_id': 2},
                          {'id': 4, 'name': '2nd under Task',
                           'parent_id': 2},
                          {'id': 5, 'name': '3rd under Task',
                           'parent_id': 2},
                          {'id': 6, 'name': 'After task'}])),
    # What about nested tasks / projects?
    (mock_source(
        find_by_workspace_return=[
            {'id': 1, 'name': 'Contains a task "project"'},
            {'id': 2, 'name': 'A task "project"'}],
        find_all_return=[
            {'id': 3, 'parent': None, 'name': 'A task "project"',
             'projects': [{'id': 1,
                           'name': 'Contains a task "project"'}]}],
        project_tasks_side_effect=project_tasks_side_effect_fn(
            {'id': 3, 'parent': None, 'name': 'A task "project"',
             'projects': [{'id': 1,
                           'name': 'Contains a task "project"'}]})),
     mock_tree(project_list=[{'id': 1,
                              'name': 'Contains a task "project"'}],
               task_list=[{'id': 3, 'parent_id': 1, 'project_id': 2,
                           'name': 'A task "project"'}])),
    (mock_source(
        find_by_workspace_return=[
            {'id': 1, 'name': 'My Inbox'},
            {'id': 2, 'name': 'a subtask of Project 3'},
            {'id': 3, 'name': 'a task in My Inbox'}],
        project_tasks_side_effect=project_tasks_side_effect_fn(
            {'id': 4, 'parent': None, 'name': 'a subtask of Project 3',
             'projects': [{'id': 3}]},
            {'id': 5, 'parent': None, 'name': 'a task in My Inbox',
             'projects': [{'id': 1}]})),
     mock_tree(project_list=[{'id': 1, 'name': 'My Inbox'}],
               task_list=[{'id': 5, 'parent_id': 1, 'project_id': 3,
                           'name': 'a task in My Inbox'},
                          {'id': 4, 'parent_id': 5, 'project_id': 2,
                           'name': 'a subtask of Project 3'}])),
    # We can merge projects and tasks with the same name, now let's
    # merge the archived / completed status with AND.
    (mock_source(
        find_by_workspace_side_effect=find_by_workspace_side_effect_fn(
            {'id': 1, 'name': 'A task "project"', 'archived': False}),
        find_all_return=[
            {'id': 2, 'parent': None, 'name': 'A task "project"',
             'projects': [], 'completed': True}]),
     mock_tree(task_list=[{'id': 2, 'parent_id': None, 'project_id': 1,
                           'name': 'A task "project"',
                           'completed': False}])),
    (mock_source(
        find_by_workspace_side_effect=find_by_workspace_side_effect_fn(
            {'id': 1, 'name': 'A task "project"', 'archived': True}),
        find_all_return=[
            {'id': 2, 'parent': None, 'name': 'A task "project"',
             'projects': [], 'completed': True}]),
     mock_tree(task_list=[{'id': 2, 'parent_id': None, 'project_id': 1,
                           'name': 'A task "project"',
                           'completed': True}])),
    # Tags come back as a list of dicts, but I want them as a set.
    (mock_source(
        find_all_return=[{'id': 1, 'parent': None, 'tags': [{'id': 1}]}],
        tag_find_by_workspace_return=[{'id': 1, 'name': 'morning'}]),
     mock_tree(task_list=[{'id': 1, 'tags': {'morning'}}]))
])
def test_source_get_all_items_result(asana_source, expected):
    "Does Source.get_all_items listen to Asana correctly?"
    result = asana_source.get_all_items()
    assert_trees_equal_p(result, expected)


@pytest.mark.parametrize("asana_source, expected_fields", [
    (mock_source(),
     ('id', 'name', 'notes', 'parent', 'completed', 'completed_at',
      'due_on', 'due_at', 'projects', 'tags'))])
def test_source_get_all_items(asana_source, expected_fields):
    "Does Source.get_all_items talk to Asana correctly?"
    asana_source.get_all_items()
    asana_source._project_find_by_workspace.assert_called_with(
        asana_source._w_id, params={'archived': False})
    asana_source._task_find_all.assert_called_with(
        params={'assignee': 'me', 'workspace': asana_source._w_id},
        fields=expected_fields)
    # asana_source._task_subtasks.assert_called_with(
    #     task=???,
    #     fields=expected_fields)
