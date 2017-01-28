if __name__ == "__main__" and __package__ is None:
    import os
    import sys
    sys.path.insert(0, os.path.abspath(".."))
__package__ = "org_asana"

import asana

from org_asana.node import Node
from org_asana.command import Command

class AsanaNode(Node):
    CLASS_EXPORT_ATTRS_TEMPLATE = ('name', 'notes', 'completed')

    def __init__(self):
        super().__init__()
        self.completed = False

class AsanaCommand(Command):
    DEFAULT_FETCH_FIELDS = ('id', 'name', 'notes', 'parent',
                            'completed', 'completed_at')

    def __init__(self, asana_tasks, default_workspace_id):
        self._tasks = asana_tasks
        self._w_id = default_workspace_id

    @classmethod
    def from_access_token(cls, token):
        asana_client = asana.Client.access_token(token)
        w_id = asana_client.users.me()['workspaces'][0]['id']
        c = cls(asana_client.tasks, w_id)
        return c

    def insert_child(self, parent_node, sibling_position, new_child_node):
        # doh, Asana API won't let me use sibling position
        parameters = {'assignee': 'me'}
        if parent_node.id:
            parameters.update({'parent': parent_node.id})
        else:
            parameters.update({'workspace': self._w_id})
        parameters.update({n: getattr(new_child_node, n)
                           for n in new_child_node.EXPORT_ATTRS})
        created_task = self._tasks.create(params=parameters)
        # this side effect is important
        new_child_node.id = created_task['id']

    def delete(self, node_to_delete):
        self._tasks.delete(node_to_delete.id)

    def update(self, node_to_update, model_node):
        parameters = {
            n: getattr(model_node, n)
            for n in model_node.EXPORT_ATTRS
            if getattr(model_node, n) != getattr(node_to_update, n, None)}
        self._tasks.update(node_to_update.id, params=parameters)

    def move_to(self, child_node, sibling_position, parent_node):
        self._tasks.set_parent(
            child_node.id, params={'parent': parent_node.id})

    def get_all_items(self, extra_field_list=None):
        field_list = self.DEFAULT_FETCH_FIELDS
        if extra_field_list:
            field_list = field_list + tuple(extra_field_list)
        result, stack = [], []
        all_tasks = list(self._tasks.find_all(
            params={'assignee': 'me', 'workspace': self._w_id},
            fields=field_list))
        task_list = [t for t in all_tasks if not t.get('parent')]
        task_list.reverse()
        stack.extend(task_list)
        if stack:
            task = stack.pop()
        else:
            task = None
        while task:
            parent_dict = task.pop('parent')
            if parent_dict:
                task['parent'] = parent_dict['id']
            else:
                task['parent'] = None
            for k, v in task.items():
                if v is None or v is True or v is False:
                    continue
                else:
                    task[k] = str(v)
            result.append(task)
            task_list = list(self._tasks.subtasks(
                task=task['id'],
                fields=field_list))
            task_list.reverse()
            stack.extend(task_list)
            if stack:
                task = stack.pop()
            else:
                task = None
        return result
