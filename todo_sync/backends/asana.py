import collections

import todo_sync.node as node
import todo_sync.source as source
import todo_sync.helpers as lib


DEFAULT_PROJECT_NAME = 'Default Project from Todo-Sync'
DEFAULT_TASK_NAME = 'Default Task from Todo-Sync'


class RootNode(node.RootNode):

    def external_remove_child(self, child_node):
        pass


class ProjectNode(node.Node):

    def __init__(self, workspace_id, create_in_workspace_fn, update_fn,
                 delete_fn):
        super().__init__()
        self._w_id = workspace_id
        self._create_in_workspace = create_in_workspace_fn
        self._update = update_fn
        self._delete = delete_fn
        self.name = DEFAULT_PROJECT_NAME

    def project_params_for_child(self, left_sibling_id):
        parameters = {'project': self.id,
                      'insert_after': left_sibling_id}
        return parameters

    def external_insert_as_child(self, left_sibling_id, parent_node):
        # no use for left_sibling_id or parent_node
        created_project = self._create_in_workspace(
            self._w_id, params={'name': self.name})
        self.id = created_project['id']

    def external_update(self, other_node):
        parameters = {k: v for k, v in other_node.export_attrs.items()
                      if getattr(self, k, None) != v}
        self._update(self.id, params=parameters)

    def external_remove_child(self, child_node):
        child_node._task_remove_project(
            child_node.id, params={'project': self.id})

    def external_move_to(self, left_sibling_id, parent_node):
        pass                    # don't try to reorder projects

    def external_delete(self):
        self._delete(self.id)


class TaskNode(node.Node):

    def __init__(self, workspace_id, project_create_in_workspace_fn,
                 project_update_fn, project_delete_fn,
                 tag_name_lookup, task_create_fn,
                 task_update_fn, task_set_parent_fn, task_add_project_fn,
                 task_remove_project_fn, task_add_tag_fn,
                 task_remove_tag_fn, task_delete_fn):
        super().__init__()
        self._w_id = workspace_id
        self._project_create_in_workspace = project_create_in_workspace_fn
        self._project_update = project_update_fn
        self._project_delete = project_delete_fn
        self._tag_name_lookup = tag_name_lookup
        self._task_create = task_create_fn
        self._task_update = task_update_fn
        self._task_set_parent = task_set_parent_fn
        self._task_add_project = task_add_project_fn
        self._task_remove_project = task_remove_project_fn
        self._task_add_tag = task_add_tag_fn
        self._task_remove_tag = task_remove_tag_fn
        self._task_delete = task_delete_fn
        self.name = DEFAULT_TASK_NAME
        self.completed = False
        self._project_id = None

    @property
    def project_id(self):
        return self._project_id

    @project_id.setter
    def project_id(self, project_id):
        self._project_id = project_id

    def projectify_task(self):
        "Promote the current task to a new Project"
        new_project = self._project_create_in_workspace(
            self._w_id, params={'name': self.name,
                                'archived': self.completed})
        self.project_id = new_project['id']
        # we just promoted a task to a project, so now we have to
        # reparent all the extant subtasks
        left_sibling_id = None
        for c_node in self.children:
            c_node._task_set_parent(
                c_node.id, params={'parent': None})
            c_node._task_add_project(
                c_node.id,
                params={'project': self.project_id,
                        'insert_after': left_sibling_id})
            left_sibling_id = c_node.id

    def project_params_for_child(self, left_sibling_id):
        if self.project_id is None:
            self.projectify_task()
        parameters = {'project': self.project_id,
                      'insert_after': left_sibling_id}
        return parameters

    def external_insert_as_child(self, left_sibling_id, parent_node):
        create_params = {'assignee': 'me', 'workspace': self._w_id}
        create_params.update(self.export_attrs)
        if 'tags' in create_params:
            old_tags = create_params.pop('tags')
            create_params['tags'] = sorted(
                list(self._tag_name_lookup[name] for name in old_tags))
        created_task = self._task_create(params=create_params)
        self.id = created_task['id']  # very important side effect
        self.external_move_to(left_sibling_id, parent_node)

    def external_update(self, other_node):
        parameters = {k: v for k, v in other_node.export_attrs.items()
                      if getattr(self, k, None) != v}
        if 'tags' in parameters:
            other_tags = parameters.pop('tags')
            self_tags = getattr(self, 'tags', set())
            add_tags = other_tags.difference(self_tags)
            for name in add_tags:
                self._task_add_tag(
                    self.id, params={'tag': self._tag_name_lookup[name]})
            remove_tags = self_tags.difference(other_tags)
            for name in remove_tags:
                self._task_remove_tag(
                    self.id, params={'tag': self._tag_name_lookup[name]})
        if parameters:
            self._task_update(self.id, params=parameters)
        if self.project_id:
            project_parameters = {}
            if 'name' in parameters:
                project_parameters['name'] = parameters['name']
            if 'completed' in parameters:
                project_parameters['archived'] = parameters['completed']
            if project_parameters:
                self._project_update(
                    self.project_id, params=project_parameters)

    def external_remove_child(self, child_node):
        if self.project_id:
            child_node._task_remove_project(
                child_node.id, params={'project': self.project_id})
        else:
            child_node._task_set_parent(
                child_node.id, params={'parent': None})

    def external_move_to(self, left_sibling_id, parent_node):
        if self.parent and self.parent is not parent_node:
            self.parent.external_remove_child(self)
        project_params = parent_node.project_params_for_child(
            left_sibling_id)
        self._task_add_project(self.id, params=project_params)

    def external_delete(self):
        self._task_delete(self.id)
        if self.project_id:
            self._project_delete(self.project_id)


class TagNameLookup(collections.Mapping):

    def __init__(self, workspace_id, tag_create_in_workspace_fn,
                 *args, **kwargs):
        self._store = dict(*args, **kwargs)
        self._w_id = workspace_id
        self._tag_create_in_workspace = tag_create_in_workspace_fn

    def __getitem__(self, name):
        if name in self._store:
            value = self._store[name]
        else:
            value = self._tag_create_in_workspace(
                self._w_id, params={'name': name})['id']
            self._store[name] = value
        return value

    def __len__(self):
        return len(self._store)

    def __iter__(self):
        return iter(self._store)


class Source(source.Source):
    DEFAULT_FETCH_FIELDS = ('id', 'name', 'notes', 'parent',
                            'completed', 'completed_at',
                            'due_on', 'due_at', 'projects', 'tags')

    def __init__(self, default_workspace_id,
                 project_find_by_workspace_fn,
                 project_tasks_fn,
                 project_create_in_workspace_fn,
                 project_update_fn,
                 project_delete_fn,
                 tag_find_by_workspace_fn,
                 tag_create_in_workspace_fn,
                 task_find_all_fn,
                 task_subtasks_fn,
                 task_create_fn,
                 task_update_fn,
                 task_set_parent_fn,
                 task_add_project_fn,
                 task_remove_project_fn,
                 task_add_tag_fn,
                 task_remove_tag_fn,
                 task_delete_fn,
                 verbose=False):
        self._w_id = default_workspace_id
        self._project_find_by_workspace = project_find_by_workspace_fn
        if verbose:
            self._project_find_by_workspace = lib.make_wrapped_fn(
                'Project Find By Workspace:',
                self._project_find_by_workspace)
        self._project_tasks = project_tasks_fn
        if verbose:
            self._project_tasks = lib.make_wrapped_fn(
                'Project Tasks:', self._project_tasks)
        self._project_create_in_workspace = project_create_in_workspace_fn
        if verbose:
            self._project_create_in_workspace = lib.make_wrapped_fn(
                'Project Create In Workspace:',
                self._project_create_in_workspace)
        self._project_update = project_update_fn
        if verbose:
            self._project_update = lib.make_wrapped_fn(
                'Project Update:', self._project_update)
        self._project_delete = project_delete_fn
        if verbose:
            self._project_delete = lib.make_wrapped_fn(
                'Project Delete:', self._project_delete)
        self._tag_find_by_workspace = tag_find_by_workspace_fn
        if verbose:
            self._tag_find_by_workspace = lib.make_wrapped_fn(
                'Tag Find by Workspace:', self._tag_find_by_workspace)
        self._tag_create_in_workspace = tag_create_in_workspace_fn
        if verbose:
            self._tag_create_in_workspace = lib.make_wrapped_fn(
                'Tag Create In Workspace:', self._tag_create_in_workspace)
        self._task_find_all = task_find_all_fn
        if verbose:
            self._task_find_all = lib.make_wrapped_fn(
                'Task Find All:', self._task_find_all)
        self._task_subtasks = task_subtasks_fn
        if verbose:
            self._task_subtasks = lib.make_wrapped_fn(
                'Task Subtasks:', self._task_subtasks)
        self._task_create = task_create_fn
        if verbose:
            self._task_create = lib.make_wrapped_fn(
                'Task Create:', self._task_create)
        self._task_update = task_update_fn
        if verbose:
            self._task_update = lib.make_wrapped_fn(
                'Task Update:', self._task_update)
        self._task_set_parent = task_set_parent_fn
        if verbose:
            self._task_set_parent = lib.make_wrapped_fn(
                'Task Set Parent:', self._task_set_parent)
        self._task_add_project = task_add_project_fn
        if verbose:
            self._task_add_project = lib.make_wrapped_fn(
                'Task Add Project:', self._task_add_project)
        self._task_remove_project = task_remove_project_fn
        if verbose:
            self._task_remove_project = lib.make_wrapped_fn(
                'Task Remove Project:', self._task_remove_project)
        self._task_add_tag = task_add_tag_fn
        if verbose:
            self._task_add_tag = lib.make_wrapped_fn(
                'Task Add Tag:', self._task_add_tag)
        self._task_remove_tag = task_remove_tag_fn
        if verbose:
            self._task_remove_tag = lib.make_wrapped_fn(
                'Task Remove Tag:', self._task_remove_tag)
        self._task_delete = task_delete_fn
        if verbose:
            self._task_delete = lib.make_wrapped_fn(
                'Task Delete:', self._task_delete)
        self._verbose = verbose
        self._tag_name_lookup = TagNameLookup(
            self._w_id, self._tag_create_in_workspace)

    @classmethod
    def from_client(cls, asana_client, verbose=False):
        c = cls(asana_client.users.me()['workspaces'][0]['id'],
                asana_client.projects.find_by_workspace,
                asana_client.projects.tasks,
                asana_client.projects.create_in_workspace,
                asana_client.projects.update,
                asana_client.projects.delete,
                asana_client.tags.find_by_workspace,
                asana_client.tags.create_in_workspace,
                asana_client.tasks.find_all,
                asana_client.tasks.subtasks,
                asana_client.tasks.create,
                asana_client.tasks.update,
                asana_client.tasks.set_parent,
                asana_client.tasks.add_project,
                asana_client.tasks.remove_project,
                asana_client.tasks.add_tag,
                asana_client.tasks.remove_tag,
                asana_client.tasks.delete,
                verbose)
        return c

    def make_root_node(self):
        return RootNode()

    def make_project_node(self, info_dict=None):
        return ProjectNode.from_dict(
            info_dict, self._w_id, self._project_create_in_workspace,
            self._project_update, self._project_delete)

    def make_task_node(self, info_dict=None):
        return TaskNode.from_dict(
            info_dict, self._w_id, self._project_create_in_workspace,
            self._project_update, self._project_delete,
            self._tag_name_lookup, self._task_create,
            self._task_update, self._task_set_parent,
            self._task_add_project, self._task_remove_project,
            self._task_add_tag, self._task_remove_tag, self._task_delete)

    def get_all_items(self, extra_field_list=None):
        field_list = self.DEFAULT_FETCH_FIELDS
        if extra_field_list:
            field_list = field_list + tuple(extra_field_list)
        node_cache = collections.OrderedDict()
        task_stack, name_cache, tag_cache = [], {}, {}

        # first get all the tags, so we can reference them later.
        all_tags = self._tag_find_by_workspace(self._w_id)
        for tag in all_tags:
            tag_id = tag['id']
            tag_name = tag['name']
            tag_cache[tag_id] = tag_name
        self._tag_name_lookup = TagNameLookup(
            self._w_id, self._tag_create_in_workspace,
            {v: k for k, v in tag_cache.items()})

        # now the tasks, via depth-first-search

        # Asana tasks.find_all is weird ... it won't reliably find all
        # subtasks.  I think there's an issue if the user creates a
        # subtask through the web UI, not sure.  And if you change the
        # task order in a Project, find_all doesn't see that.  So
        # frustrating.  At any rate, just skip subtasks and project
        # tasks entirely at this point.  Hella inefficient, I know, but
        # we're going to be recursing through all subtasks ANYWAY
        # because there is a shadow of a doubt.
        all_tasks = self._task_find_all(
            params={'assignee': 'me', 'workspace': self._w_id},
            fields=field_list)
        task_stack = [t for t in all_tasks
                      if not t.get('parent') and not t.get('projects')]

        # We can also pick up Project tasks at this point.  They always
        # come back in order, so just extend the stack.  And no worries,
        # they never include subtasks.
        all_projects = self._project_find_by_workspace(self._w_id)
        for project in all_projects:
            project_tasks = self._project_tasks(
                project['id'], fields=field_list)
            task_stack.extend(list(project_tasks))

        # gotta reverse the stack to get true DFS iteration
        task_stack.reverse()
        if task_stack:
            task = task_stack.pop()
        else:
            task = None
        while task:

            # surgery on the parent property
            parent_dict = task.pop('parent', None)
            project_list = task.pop('projects', None)
            if parent_dict:
                task['parent_id'] = parent_dict['id']
            elif project_list:
                task['parent_id'] = project_list[0]['id']
            else:
                task['parent_id'] = None

            # process any tags
            if 'tags' in task:
                tag_list = task.pop('tags')
                task['tags'] = set(tag_cache[i] for i in
                                   (t['id'] for t in tag_list))

            # record the node
            node = self.make_task_node(task)
            node_cache[node.id] = node
            name_cache[node.name] = node.id

            # push onto the stack, gotta reverse it to get true DFS
            task_list = list(self._task_subtasks(
                task=task['id'],
                fields=field_list))
            task_list.reverse()
            task_stack.extend(task_list)

            # iterative step
            if task_stack:
                task = task_stack.pop()
            else:
                task = None

        # then the projects ... which may be "cousin" projects that are
        # standing for tasks, in which case set the project_id and
        # change any pointers that are out there
        all_projects = self._project_find_by_workspace(self._w_id)
        for d in all_projects:
            if d.get('name') and name_cache.get(d['name']):
                task_node = node_cache[name_cache[d['name']]]
                task_node.project_id = d['id']
                for _, n in node_cache.items():
                    if n.parent_id == task_node.project_id:
                        n.parent_id = task_node.id
            else:
                d['parent_id'] = None
                node = self.make_project_node(d)
                node_cache[node.id] = node
        # if a project is really a task, mark the task incomplete if the
        # project is still unarchived
        open_projects = self._project_find_by_workspace(
            self._w_id, params={'archived': False})
        for d in open_projects:
            if d.get('name') and name_cache.get(d['name']):
                task_node = node_cache[name_cache[d['name']]]
                task_node.completed = False

        # stitch everything together
        root_node = self.make_root_node()

        for _, node in node_cache.items():
            parent_id = getattr(node, 'parent_id', None)
            if hasattr(node, 'parent_id'):
                del node.parent_id
            if parent_id:
                parent_node = node_cache[parent_id]
            else:
                parent_node = root_node
            parent_node.append_child(node)

        if self._verbose:
            lib.ppt(root_node)
        return root_node


class DryRunSource(Source):

    @classmethod
    def from_client(cls, asana_client, verbose=False):
        c = cls(asana_client.users.me()['workspaces'][0]['id'],
                asana_client.projects.find_by_workspace,
                asana_client.projects.tasks,
                lib.make_counting_fn(lambda i: {
                    'id': 'NEW PROJECT {}'.format(i)}),
                lib.noop,
                lib.noop,
                asana_client.tags.find_by_workspace,
                lib.make_counting_fn(lambda i: {'id': i}),
                asana_client.tasks.find_all,
                asana_client.tasks.subtasks,
                lib.make_counting_fn(lambda i: {
                    'id': 'NEW TASK {}'.format(i)}),
                lib.noop,
                lib.noop,
                lib.noop,
                lib.noop,
                lib.noop,
                lib.noop,
                lib.noop,
                verbose)
        return c
