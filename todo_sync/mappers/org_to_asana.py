import asana
import pexpect
import pexpect.replwrap

import todo_sync.backends.org as o
import todo_sync.backends.asana as a
import todo_sync.helpers as lib


def make_fn(org_source, a_node):
    org_dict = {'id': None, 'parent': None}
    o_node = None

    if isinstance(a_node, a.TaskNode):
        org_dict['title'] = a_node.name
        org_dict['todo_type'] = 'DONE' if a_node.completed else 'TODO'
        org_dict['asana_id'] = str(a_node.id)
        if getattr(a_node, 'completed_at', None):
            org_dict['closed'] = a_node.completed_at
        if hasattr(a_node, 'notes'):
            org_dict['paragraph'] = a_node.notes
        deadline_info = (getattr(a_node, 'due_at', None)
                         or getattr(a_node, 'due_on', None))
        if deadline_info:
            org_dict['deadline'] = deadline_info
        if getattr(a_node, 'project_id', None):
            org_dict['asana_project_id'] = str(a_node.project_id)
        if hasattr(a_node, 'tags'):
            org_dict['tags'] = a_node.tags
        o_node = org_source.make_headline_node(org_dict)

    elif isinstance(a_node, a.ProjectNode):
        org_dict['id'] = a_node.name
        org_dict['asana_project_id'] = str(a_node.id)
        o_node = org_source.make_filename_node(org_dict)

    return o_node


def behind_source(org_config_filename, verbose=False, dry_run=False):
    command = 'emacs'
    args = ['-batch',
            '-l', org_config_filename,
            '-l', 'ts-org-interaction.el',
            '--eval=(ts-repl)']
    spawn = pexpect.spawn(command, args, encoding='utf-8')
    emacs_repl_wrapper = pexpect.replwrap.REPLWrapper(
        spawn, "Lisp expression: ", None)

    if dry_run:
        o.DryRunSource.make_fn = make_fn
        source = o.DryRunSource.from_emacs_repl(emacs_repl_wrapper,
                                                verbose)
    else:
        o.Source.make_fn = make_fn
        source = o.Source.from_emacs_repl(emacs_repl_wrapper, verbose)

    source.get_tree = lambda: source.get_all_items(
        ['asana_id', 'asana_project_id'])

    return source


def ahead_source(access_token, verbose=False):
    client = asana.Client.access_token(access_token)

    source = a.Source.from_client(client, verbose)
    source.get_tree = source.get_all_items

    return source


def map_fn(o_node, a_node):
    result = False
    if isinstance(o_node, o.HeadlineNode):
        if isinstance(a_node, a.TaskNode):
            o_node_asana_id = lib.safe_int(
                getattr(o_node, 'asana_id', None))
            if o_node_asana_id is None:
                result = o_node.title == a_node.name
            else:
                result = o_node_asana_id == a_node.id
    elif isinstance(o_node, o.FilenameNode):
        if isinstance(a_node, a.ProjectNode):
            o_node_project_id = lib.safe_int(
                getattr(o_node, 'asana_project_id', None))
            if o_node_project_id is None:
                result = lib.basename_no_ext(o_node.id) == a_node.name
            else:
                result = o_node_project_id == a_node.id
    return result


def eql_fn(o_node, a_node):
    result = False
    if isinstance(o_node, o.HeadlineNode):
        if isinstance(a_node, a.TaskNode):
            completed_from_type = {'TODO': False, 'DONE': True}
            tests = [
                (a_node.name == o_node.title),
                (getattr(a_node, 'notes', None)
                 == getattr(o_node, 'paragraph', None)),
                (a_node.completed
                 == completed_from_type[o_node.todo_type]),
                (a_node.id
                 == lib.safe_int(getattr(o_node, 'asana_id', None))),
                (a_node.project_id
                 == lib.safe_int(getattr(o_node, 'asana_project_id', None))),
                (getattr(o_node, 'deadline', None)
                 == (getattr(a_node, 'due_at', None)
                     or (getattr(a_node, 'due_on', None)))),
                (getattr(a_node, 'tags', None)
                 == (getattr(o_node, 'tags', None)))]
            result = all(tests)
    elif isinstance(o_node, o.FilenameNode):
        if isinstance(a_node, a.ProjectNode):
            tests = [
                (a_node.name
                 == lib.basename_no_ext(o_node.id)),
                (a_node.id
                 == lib.safe_int(getattr(o_node, 'asana_project_id', None)))]
            result = all(tests)
    return result
