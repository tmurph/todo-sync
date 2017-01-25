if __name__ == "__main__" and __package__ is None:
    import os
    import sys
    sys.path.insert(0, os.path.abspath(".."))
__package__ = "org_asana"

from org_asana.node import Node, Command

class AsanaNode(Node):
    CLASS_EXPORT_ATTRS_TEMPLATE = ('name', 'notes')

class AsanaCommand(Command):

    def __init__(self, asana_client):
        self._client = asana_client
        self._w_id = asana_client.users.me()['workspaces'][0]['id']

    def insert_child(self, parent_node, sibling_position, new_child_node):
        # doh, Asana API won't let me use sibling position
        parameters = {'assignee': 'me'}
        if parent_node.id:
            parameters.update({'parent': parent_node.id})
        else:
            paremeters.update({'workspace': self._w_id})
        parameters.update({n: getattr(new_child_node, n)
                           for n in new_child_node.EXTRA_ATTRS})
        created_task = self._client.tasks.create(params=parameters)
        # this side effect is important
        new_child_node.id = created_task['id']

    def delete(self, node_to_delete):
        self._client.tasks.delete(node_to_delete.id)

    def update(self, node_to_update, model_node):
        self._client.tasks.update(
            node_to_update.id, params={n: getattr(model_node, n)
                                       for n in model_node.EXTRA_ATTRS})

    def move_to(self, child_node, sibling_position, parent_node):
        self._client.tasks.set_parent(
            child_node.id, params={'parent': parent_node.id})

    def get_all_tasks(self, with_fields=None):
        field_list = with_fields or ["id", "name", "notes", "parent"]
        result, stack = [], []
        task_list = list(self._client.tasks.find_all(
            params={'assignee': 'me', 'workspace': self._w_id},
            fields=field_list))
        task_list.reverse()
        stack.extend(task_list)
        if stack:
            task = stack.pop()
        else:
            task = None
        while task:
            result.append(task)
            task_list = list(self._client.tasks.subtasks(
                task=task['id'],
                fields=field_list))
            task_list.reverse()
            stack.extend(task_list)
            if stack:
                task = stack.pop()
            else:
                task = None
        return result
