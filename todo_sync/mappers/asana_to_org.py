import re

import asana
import pexpect
import pexpect.replwrap

import todo_sync.backends.asana as a
import todo_sync.backends.org as o
import todo_sync.helpers as h

COMPLETED_FROM_TYPE = {'TODO': False, 'DONE': True}


def make_fn(asana_source, o_node):
    asana_dict = {'id': None, 'parent': None}
    a_node = None

    if isinstance(o_node, o.HeadlineNode):
        asana_dict['name'] = o_node.title
        if getattr(o_node, 'paragraph', None):
            asana_dict['notes'] = o_node.paragraph
        if getattr(o_node, 'todo_type', None):
            asana_dict['completed'] = COMPLETED_FROM_TYPE.get(
                o_node.todo_type)
        if getattr(o_node, 'deadline', None):
            if re.match("[0-9]{4}-[0-9]{2}-[0-9]{2}$", o_node.deadline):
                asana_dict['due_on'] = o_node.deadline
            else:
                asana_dict['due_at'] = o_node.deadline
        if hasattr(o_node, 'tags'):
            asana_dict['tags'] = o_node.tags
        a_node = asana_source.make_task_node(asana_dict)

    elif isinstance(o_node, o.FilenameNode):
        asana_dict['name'] = h.basename_no_ext(o_node.id)
        a_node = asana_source.make_project_node(asana_dict)

    return a_node


def behind_source(access_token, verbose=False, dry_run=False):
    client = asana.Client.access_token(access_token)

    if dry_run:
        a.DryRunSource.make_fn = make_fn
        source = a.DryRunSource.from_client(client, verbose)
    else:
        a.Source.make_fn = make_fn
        source = a.Source.from_client(client, verbose)
    source.get_tree = source.get_all_items

    return source


def ahead_source(org_config_filename, verbose=False):
    command = 'emacs'
    args = ['-batch',
            '-l', org_config_filename,
            '-l', 'ts-org-interaction.el',
            '--eval=(ts-repl)']
    spawn = pexpect.spawn(command, args, encoding='utf-8')
    emacs_repl_wrapper = pexpect.replwrap.REPLWrapper(
        spawn, "Lisp expression: ", None)

    source = o.Source.from_emacs_repl(emacs_repl_wrapper, verbose)
    source.get_tree = lambda: source.get_all_items(
        ['asana_id', 'asana_project_id'])

    return source


def map_fn(a_node, o_node):
    result = False
    if isinstance(a_node, a.TaskNode):
        if isinstance(o_node, o.HeadlineNode):
            o_node_asana_id = h.safe_int(
                getattr(o_node, 'asana_id', None))
            if o_node_asana_id is None:
                result = o_node.title == a_node.name
            else:
                result = o_node_asana_id == a_node.id
    elif isinstance(a_node, a.ProjectNode):
        if isinstance(o_node, o.FilenameNode):
            o_node_project_id = h.safe_int(
                getattr(o_node, 'asana_project_id', None))
            if o_node_project_id is None:
                result = h.basename_no_ext(o_node.id) == a_node.name
            else:
                result = o_node_project_id == a_node.id
    return result


def eql_fn(a_node, o_node):
    result = False
    if isinstance(a_node, a.TaskNode):
        if isinstance(o_node, o.HeadlineNode):
            tests = [
                (a_node.name == o_node.title),
                (getattr(a_node, 'notes', None)
                 == getattr(o_node, 'paragraph', None)),
                (a_node.completed
                 == COMPLETED_FROM_TYPE[o_node.todo_type]),
                (getattr(o_node, 'deadline', None)
                 == (getattr(a_node, 'due_at', None)
                     or (getattr(a_node, 'due_on', None)))),
                (getattr(a_node, 'tags', None)
                 == getattr(o_node, 'tags', None))]
            result = all(tests)
    elif isinstance(a_node, a.ProjectNode):
        if isinstance(o_node, o.FilenameNode):
            result = (a_node.name == h.basename_no_ext(o_node.id))
    return result
