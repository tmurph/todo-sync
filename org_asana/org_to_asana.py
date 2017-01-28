if __name__ == "__main__" and __package__ is None:
    import os
    import sys
    sys.path.insert(0, os.path.abspath(".."))
__package__ = "org_asana"

from org_asana.org_interaction import OrgNode, OrgCommand
from org_asana.asana_interaction import AsanaNode, AsanaCommand
from org_asana.node import RootNode

source_command = OrgCommand

def target_command(access_token):
    return AsanaCommand.from_access_token(access_token)

def s_tree(org_command):
    return RootNode.from_dict_list(
        OrgNode, org_command.get_all_items(['asana_id']))

def t_tree(asana_command):
    return RootNode.from_dict_list(
        AsanaNode, asana_command.get_all_items())

def map_fn(o_node, a_node):
    o_node_asana_id = getattr(o_node, 'asana_id', None)
    if o_node_asana_id is None:
        result = getattr(o_node, 'title') == getattr(a_node, 'name')
    else:
        result = o_node_asana_id == getattr(a_node, 'id')
    return result

def eql_fn(o_node, a_node):
    completed_from_keyword = {'TODO': False, 'DONE': True}
    and_tests = [
        (getattr(a_node, 'name') == getattr(o_node, 'title')),
        (getattr(a_node, 'notes') == getattr(o_node, 'paragraph')),
        (getattr(a_node, 'completed')
         == completed_from_keyword[getattr(o_node, 'todo_keyword')]),
        (getattr(a_node, 'id') == getattr(o_node, 'asana_id', None))]
    return all(and_tests)

def make_fn(a_node):
    org_dict = {
        'id': None,
        'parent': None,
        'title': a_node.name,
        'paragraph': a_node.notes,
        'todo_keyword': 'DONE' if a_node.completed else 'TODO',
        'asana_id': a_node.id,
        'closed': a_node.completed_at}
    o_node = OrgNode.from_dict(org_dict)
    return o_node
