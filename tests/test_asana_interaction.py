import pytest

if __name__ == '__main__' and __package__ is None:
    import os
    import sys
    sys.path.insert(0, os.path.abspath('..'))
__package__ = 'org_asana.tests'

from org_asana.asana_interaction import dfs_task_list

class MockAsanaUser():
    def me(self):
        return {'workspaces': [{'id': 0}]}

class MockAsanaTasks():
    def __init__(self, task_dicts):
        self._tasks = task_dicts

    def find_all(self, params, **options):
        return [t for t in self._tasks
                if not t.get('parent')]

    def subtasks(self, task, **options):
        return [t for t in self._tasks
                if t.get('parent') and t['parent']['id'] == task]

class MockAsanaClient():
    def __init__(self, mock_user, mock_tasks):
        self.users = mock_user
        self.tasks = mock_tasks

def mock_asana_client(task_dicts):
    user = MockAsanaUser()
    tasks = MockAsanaTasks(task_dicts)
    client = MockAsanaClient(user, tasks)
    return client


@pytest.mark.parametrize("asana_client, expected_list", [
    (mock_asana_client([]), [])    # nino
    , (mock_asana_client(          # in-n-out
        [{'id': 0, 'parent': None}]),
       [{'id': 0, 'parent': None}])
    , (mock_asana_client(          # DFS
        [{'id': 0, 'parent': None},
         {'id': 1, 'parent': None},
         {'id': 2, 'parent': {'id': 0}}]),
       [{'id': 0, 'parent': None},
        {'id': 2, 'parent': {'id': 0}},
        {'id': 1, 'parent': None}])
])
def test_dfs_task_list(asana_client, expected_list):
    "Can we pull tasks from Asana into a DFS list?"
    assert dfs_task_list(asana_client) == expected_list
