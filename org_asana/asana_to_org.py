if __name__ == "__main__" and __package__ is None:
    import os
    import sys
    sys.path.insert(0, os.path.abspath(".."))
__package__ = "org_asana"

from org_asana.asana_interaction import AsanaNode, AsanaCommand
from org_asana.org_interaction import OrgNode, OrgCommand
from org_asana.node import RootNode

def source_command(access_token):
    return AsanaCommand.from_access_token(access_token)

target_command = OrgCommand

def s_tree(asana_command):
    return RootNode.from_dict_list(
        AsanaNode, asana_command.get_all_items())

def t_tree(org_command):
    return RootNode.from_dict_list(
        OrgNode, org_command.get_all_items(['asana_id']))

def map_fn(a_node, o_node):
    o_node_asana_id = getattr(o_node, 'asana_id', None)
    if o_node_asana_id is None:
        result = getattr(o_node, 'title') == getattr(a_node, 'name')
    else:
        result = o_node_asana_id == getattr(a_node, 'id')
    return result

def eql_fn(a_node, o_node):
    completed_from_keyword = {'TODO': False, 'DONE': True}
    tests = [
        (getattr(a_node, 'name') == getattr(o_node, 'title')),
        (getattr(a_node, 'notes') == getattr(o_node, 'paragraph')),
        (getattr(a_node, 'completed')
         == completed_from_keyword[getattr(o_node, 'todo_keyword')])]
    return all(tests)

def make_fn(o_node):
    completed_from_keyword = {'TODO': False, 'DONE': True}
    asana_dict = {
        'id': None,
        'parent': None,
        'name': o_node.title,
        'notes': o_node.paragraph,
        'completed': completed_from_keyword[o_node.todo_keyword]}
    a_node = AsanaNode.from_dict(asana_dict)
    return a_node
