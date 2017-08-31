import pytest
import unittest.mock as mock

import todo_sync.backends.asana as a
import todo_sync.backends.org as o
import todo_sync.mappers.asana_to_org as a2o

from .helpers import assert_trees_equal_p
from .test_org import mock_headline_node, mock_filename_node
from .test_org import mock_source as mock_org_source
from .test_asana import mock_task_node, mock_project_node
from .test_asana import mock_source as mock_asana_source

MOCK_NAME_TITLE = MOCK_TITLE_NAME = 'REQUIRED'


def mapped_mock_headline_node(info_dict=None):
    d = {'title': MOCK_TITLE_NAME}
    if info_dict:
        d.update(info_dict)
    return mock_headline_node(d)


def mapped_mock_task_node(info_dict=None):
    d = {'name': MOCK_TITLE_NAME}
    if info_dict:
        d.update(info_dict)
    return mock_task_node(d)


def mapped_mock_filename_node(info_dict=None):
    d = {}
    if info_dict:
        d.update(info_dict)
    return mock_filename_node(d)


def mapped_mock_project_node(info_dict=None):
    d = {'name': MOCK_TITLE_NAME}
    if info_dict:
        d.update(info_dict)
    return mock_project_node(d)


def test_a2o_behind_source_creation(mocker):
    "Can we get an Asana Source?"
    mocker.patch('asana.Client')
    s = a2o.behind_source(None, dry_run=True)
    assert isinstance(s, a.Source)


def test_a2o_ahead_source_creation(mocker):
    "Can we get an Org Source?"
    mocker.patch('pexpect.spawn')
    mocker.patch('pexpect.replwrap.REPLWrapper')
    s = a2o.ahead_source(None)
    assert isinstance(s, o.Source)


@pytest.fixture
def behind_source():
    with mock.patch('asana.Client'):
        mocked_source = mock.Mock(a.Source, wraps=mock_asana_source())
        with mock.patch.object(a.Source, 'from_client',
                               return_value=mocked_source):
            yield a2o.behind_source(None)


@pytest.fixture
def ahead_source():
    with mock.patch('pexpect.spawn'):
        with mock.patch('pexpect.replwrap.REPLWrapper'):
            mocked_source = mock.Mock(o.Source, wraps=mock_org_source())
            with mock.patch.object(o.Source, 'from_emacs_repl',
                                   return_value=mocked_source):
                yield a2o.ahead_source(None)


def test_a2o_behind_get_tree(behind_source):
    "Does the Asana Source have a special get_tree method?"
    behind_source.get_tree()
    behind_source.get_all_items.assert_called_with()


def test_a2o_ahead_get_tree(ahead_source):
    "Does the Org Source have a special get_tree method?"
    ahead_source.get_tree()
    ahead_source.get_all_items.assert_called_with(
        ['asana_id', 'asana_project_id'])


@pytest.mark.parametrize("o_node, expected_node", [
    (mapped_mock_headline_node(), mapped_mock_task_node()),
    (mapped_mock_headline_node({'paragraph': 'my stuff'}),
     mapped_mock_task_node({'notes': 'my stuff'})),
    (mapped_mock_headline_node({'todo_type': 'DONE'}),
     mapped_mock_task_node({'completed': True})),
    (mapped_mock_headline_node({'todo_type': 'TODO'}),
     mapped_mock_task_node({'completed': False})),
    (mapped_mock_headline_node({'deadline': '2016-11-19'}),
     mapped_mock_task_node({'due_on': '2016-11-19'})),
    (mapped_mock_headline_node({'deadline': '2016-11-20T03:34:00.000Z'}),
     mapped_mock_task_node({'due_at': '2016-11-20T03:34:00.000Z'})),
    (mapped_mock_filename_node({'id': 'My Inbox'}),
     mapped_mock_project_node({'name': 'My Inbox'})),
    (mapped_mock_headline_node({'tags': {'morning'}}),
     mapped_mock_task_node({'tags': {'morning'}})),
    (mapped_mock_headline_node({'tags': set()}),
     mapped_mock_task_node({'tags': set()}))])
def test_a2o_behind_make_fn(o_node, expected_node, behind_source):
    "Does the Asana Source have a special make_fn method?"
    a_node = a2o.make_fn(behind_source, o_node)
    assert_trees_equal_p(a_node, expected_node)


@pytest.mark.parametrize("a_node, o_node, expected", [
    (mapped_mock_task_node(),
     mapped_mock_headline_node(),
     True),
    (mapped_mock_task_node({'name': 'On the one hand ...'}),
     mapped_mock_headline_node({'title': 'On the other hand ...'}),
     False),
    (mapped_mock_task_node({'name': 'On the one hand ...',
                            'id': 12345}),
     mapped_mock_headline_node({'title': 'On the other hand ...',
                                'asana_id': '12345'}),
     True),
    (mapped_mock_project_node({'name': 'My Inbox'}),
     mapped_mock_filename_node({'id': 'My Inbox'}),
     True),
    (mapped_mock_project_node({'name': 'On the one hand ...'}),
     mapped_mock_filename_node({'id': 'On the other hand ...'}),
     False),
    (mapped_mock_project_node({'name': 'On the one hand ...',
                               'id': 12345}),
     mapped_mock_filename_node({'id': 'On the other hand ...',
                                'asana_project_id': '12345'}),
     True),
    (mapped_mock_project_node(),
     mapped_mock_headline_node(),
     False),
    (mapped_mock_project_node({'name': 'On the one hand ...'}),
     mapped_mock_headline_node({'title': 'On the other hand ...'}),
     False),
    (mapped_mock_project_node({'name': 'On the one hand ...',
                               'id': 12345}),
     mapped_mock_headline_node({'title': 'On the other hand ...',
                                'asana_project_id': '12345'}),
     False)])
def test_a2o_map_fn(a_node, o_node, expected):
    "Can we map from Asana nodes to Org nodes?"
    assert a2o.map_fn(a_node, o_node) == expected


@pytest.mark.parametrize("a_node, o_node, expected", [
    (mapped_mock_task_node(),
     mapped_mock_headline_node(),
     True),
    (mapped_mock_task_node({'name': 'On the one hand ...'}),
     mapped_mock_headline_node({'title': 'On the other hand ...'}),
     False),
    (mapped_mock_project_node(),
     mapped_mock_filename_node(),
     False),
    (mapped_mock_project_node({'name': 'My Inbox'}),
     mapped_mock_filename_node({'id': 'My Inbox'}),
     True),
    (mapped_mock_task_node(),
     mapped_mock_filename_node(),
     False),
    (mapped_mock_project_node(),
     mapped_mock_headline_node(),
     False),
    (mapped_mock_project_node({'name': 'On the one hand ...'}),
     mapped_mock_headline_node({'title': 'On the other hand ...'}),
     False),
    (mapped_mock_task_node({'tags': {'@home'}}),
     mapped_mock_headline_node({'tags': {'@work'}}),
     False)])
def test_a2o_eql_fn(a_node, o_node, expected):
    "Can we compare Asana nodes to Org nodes?"
    assert a2o.eql_fn(a_node, o_node) == expected
