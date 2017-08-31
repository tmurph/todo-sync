import pytest
import unittest.mock as mock

import todo_sync.backends.asana as a
import todo_sync.backends.org as o
import todo_sync.mappers.org_to_asana as o2a

from .helpers import assert_trees_equal_p
from .test_org import mock_headline_node, mock_filename_node
from .test_org import mock_source as mock_org_source
from .test_asana import mock_task_node, mock_project_node
from .test_asana import mock_source as mock_asana_source

MOCK_NAME_TITLE = MOCK_TITLE_NAME = 'REQUIRED'
MOCK_TASK_ID = 1
MOCK_PROJECT_ID = 2


def mapped_mock_headline_node(info_dict=None):
    d = {'title': MOCK_TITLE_NAME}
    if info_dict:
        d.update(info_dict)
    return mock_headline_node(d)


def mapped_mock_task_node(info_dict=None):
    d = {'id': MOCK_TASK_ID, 'name': MOCK_TITLE_NAME}
    if info_dict:
        d.update(info_dict)
    return mock_task_node(d)


def mapped_mock_filename_node(info_dict=None):
    d = {}
    if info_dict:
        d.update(info_dict)
    return mock_filename_node(d)


def mapped_mock_project_node(info_dict=None):
    d = {'id': MOCK_PROJECT_ID, 'name': MOCK_TITLE_NAME}
    if info_dict:
        d.update(info_dict)
    return mock_project_node(d)


def test_o2a_behind_source_creation(mocker):
    "Can we get an Org Source?"
    mocker.patch('pexpect.spawn')
    mocker.patch('pexpect.replwrap.REPLWrapper')
    s = o2a.behind_source(None, dry_run=True)
    assert isinstance(s, o.Source)


def test_o2a_ahead_source_creation(mocker):
    "Can we get an Asana Source?"
    mocker.patch('asana.Client')
    s = o2a.ahead_source(None)
    assert isinstance(s, a.Source)


@pytest.fixture
def ahead_source():
    with mock.patch('asana.Client'):
        mocked_source = mock.Mock(a.Source, wraps=mock_asana_source())
        with mock.patch.object(a.Source, 'from_client',
                               return_value=mocked_source):
            yield o2a.ahead_source(None)


@pytest.fixture
def behind_source():
    with mock.patch('pexpect.spawn'):
        with mock.patch('pexpect.replwrap.REPLWrapper'):
            mocked_source = mock.Mock(o.Source, wraps=mock_org_source())
            with mock.patch.object(o.Source, 'from_emacs_repl',
                                   return_value=mocked_source):
                yield o2a.behind_source(None)


def test_o2a_behind_get_tree(behind_source):
    "Does the Org Source have a special get_tree method?"
    behind_source.get_tree()
    behind_source.get_all_items.assert_called_with(
        ['asana_id', 'asana_project_id'])


def test_o2a_ahead_get_tree(ahead_source):
    "Does the Asana Source have a special get_tree method?"
    ahead_source.get_tree()
    ahead_source.get_all_items.assert_called_with()


@pytest.mark.parametrize("a_node, expected_node", [
    (mapped_mock_task_node(),
     mapped_mock_headline_node({'asana_id': str(MOCK_TASK_ID)})),
    (mapped_mock_task_node({'notes': 'my stuff'}),
     mapped_mock_headline_node({'asana_id': str(MOCK_TASK_ID),
                                'paragraph': 'my stuff'})),
    (mapped_mock_task_node({'completed': True}),
     mapped_mock_headline_node({'asana_id': str(MOCK_TASK_ID),
                                'todo_type': 'DONE'})),
    (mapped_mock_task_node({'completed': False}),
     mapped_mock_headline_node({'asana_id': str(MOCK_TASK_ID),
                                'todo_type': 'TODO'})),
    (mapped_mock_task_node({'due_on': '2016-11-19'}),
     mapped_mock_headline_node({'asana_id': str(MOCK_TASK_ID),
                                'deadline': '2016-11-19'})),
    (mapped_mock_task_node({'due_at': '2016-11-20T03:34:00.000Z'}),
     mapped_mock_headline_node({'asana_id': str(MOCK_TASK_ID),
                                'deadline': '2016-11-20T03:34:00.000Z'})),
    (mapped_mock_task_node({'project_id': 261881552216474}),
     mapped_mock_headline_node({'asana_id': str(MOCK_TASK_ID),
                                'asana_project_id': '261881552216474'})),
    (mapped_mock_project_node({'name': 'My Inbox'}),
     mapped_mock_filename_node({'id': 'My Inbox',
                                'asana_project_id': str(MOCK_PROJECT_ID)})),
    (mapped_mock_task_node({'tags': {'@work'}}),
     mapped_mock_headline_node({'asana_id': str(MOCK_TASK_ID),
                                'tags': {'@work'}})),
    (mapped_mock_task_node({'tags': set()}),
     mapped_mock_headline_node({'asana_id': str(MOCK_TASK_ID),
                                'tags': set()})),
    (mapped_mock_task_node({'notes': ''}),
     mapped_mock_headline_node({'asana_id': str(MOCK_TASK_ID),
                                'paragraph': ''}))])
def test_o2a_behind_make_fn(a_node, expected_node, behind_source):
    "Does the Org Source have a special make_fn method?"
    o_node = behind_source.make_fn(a_node)
    assert_trees_equal_p(o_node, expected_node, ['id'])


@pytest.mark.parametrize("o_node, a_node, expected", [
    (mapped_mock_headline_node(), mapped_mock_task_node(), True),
    (mapped_mock_headline_node({'title': 'On the other hand ...'}),
     mapped_mock_task_node({'name': 'On the one hand ...'}),
     False),
    (mapped_mock_headline_node({'title': 'On the other hand ...',
                                'asana_id': str(MOCK_TASK_ID)}),
     mapped_mock_task_node({'name': 'On the one hand ...'}),
     True),
    (mapped_mock_filename_node({'id': 'My Inbox'}),
     mapped_mock_project_node({'name': 'My Inbox'}),
     True),
    (mapped_mock_filename_node({'id': 'On the other hand ...'}),
     mapped_mock_project_node({'name': 'On the one hand ...'}),
     False),
    (mapped_mock_filename_node({'id': 'On the other hand ...',
                                'asana_project_id': str(MOCK_PROJECT_ID)}),
     mapped_mock_project_node({'name': 'On the one hand ...'}),
     True),
    (mapped_mock_headline_node, mapped_mock_project_node(), False),
    (mapped_mock_headline_node({'title': 'A "project" headline'}),
     mapped_mock_project_node({'name': 'A "project" headline'}),
     False),
    (mapped_mock_filename_node(), mapped_mock_task_node(), False),
    (mapped_mock_filename_node({'id': 'My Inbox'}),
     mapped_mock_task_node({'name': 'My Inbox'}),
     False)])
def test_o2a_map_fn(o_node, a_node, expected):
    "Can we map from Org nodes to Asana nodes?"
    assert o2a.map_fn(o_node, a_node) == expected


@pytest.mark.parametrize("o_node, a_node, expected", [
    (mapped_mock_headline_node({'asana_id': str(MOCK_TASK_ID)}),
     mapped_mock_task_node(), True),
    (mapped_mock_headline_node({'asana_id': str(MOCK_TASK_ID),
                                'title': 'On the other hand ...'}),
     mapped_mock_task_node({'name': 'On the one hand ...'}),
     False),
    (mapped_mock_filename_node(), mapped_mock_project_node(), False),
    (mapped_mock_filename_node({'id': 'My Inbox'}),
     mapped_mock_project_node({'name': 'My Inbox'}),
     False),
    (mapped_mock_filename_node({'id': 'My Inbox',
                                'asana_project_id': str(MOCK_PROJECT_ID)}),
     mapped_mock_project_node({'name': 'My Inbox'}),
     True),
    (mapped_mock_headline_node({'asana_id': str(MOCK_TASK_ID),
                                'tags': {'@home'}}),
     mapped_mock_task_node({'tags': {'@work'}}),
     False)])
def test_o2a_eql_fn(o_node, a_node, expected):
    "Can we compare Org nodes to Asana nodes?"
    assert o2a.eql_fn(o_node, a_node) == expected
