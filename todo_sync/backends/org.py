import io

import todo_sync.node as node
import todo_sync.source as source
import todo_sync.helpers as lib


DEFAULT_HEADLINE_TITLE = 'Default Headline from Todo-Sync'
DEFAULT_TODO_TYPE = 'TODO'


def elisp_string_from_value(value):
    "Make an Elisp string representation of a Python value."
    if type(value) == str:
        result = value.replace('"', '\\"').replace('\n', '\\n')
        result = '"' + result + '"'
    elif type(value) == int:
        result = '"' + str(value) + '"'
    elif type(value) == set:
        result = ' '.join(sorted(elisp_string_from_value(v) for v in value))
        result = '(' + result + ')'
    else:
        result = 'nil'
    return result


def elisp_string_from_key(key):
    "Make an Elisp string representation of a Python key."
    result = str(key).lower()
    if key in ['parent_id', 'todo_type']:
        result = result.replace('_', '-')
    return result


def elisp_string_from_dict(d):
    "Make an Elisp plist string representation of a Python dictionary."
    if not d:
        result = "()"
    else:
        lst = list(d.items())
        buf = io.StringIO()
        k, v = lst[0]
        buf.write("(:")
        buf.write(elisp_string_from_key(k))
        buf.write(" ")
        buf.write(elisp_string_from_value(v))
        for k, v in lst[1:]:
            buf.write(" :")
            buf.write(elisp_string_from_key(k))
            buf.write(" ")
            buf.write(elisp_string_from_value(v))
        buf.write(")")
        result = buf.getvalue()
    return result


def elisp_string_from_list(lst):
    "Make an Elisp keyword list string representation of a Python list."
    if lst:
        buf = io.StringIO()
        buf.write("(:")
        buf.write(elisp_string_from_key(lst[0]))
        for k in lst[1:]:
            buf.write(" :")
            buf.write(elisp_string_from_key(k))
        buf.write(")")
        return buf.getvalue()


def elisp_string_from_id(node_id):
    if node_id:
        result = '"{}"'.format(node_id)
    else:
        result = 'nil'
    return result


class RootNode(node.RootNode):

    def as_parent_insert_child_command(self, left_sibling_id,
                                       plist_string):
        export_left_id = elisp_string_from_id(left_sibling_id)
        return '(ts-insert-child-into-file nil {} \'{})'.format(
            export_left_id, plist_string)

    def as_parent_move_child_command(self, child_id, left_sibling_id):
        export_child_id = elisp_string_from_id(child_id)
        export_left_id = elisp_string_from_id(left_sibling_id)
        return '(ts-move-to-file {} {} nil)'.format(
            export_child_id, export_left_id)


class FilenameNode(node.Node):

    def __init__(self, repl_to_source_command):
        super().__init__()
        self._repl_to_source_command = repl_to_source_command

    def as_parent_insert_child_command(self, left_sibling_id,
                                       plist_string):
        export_left_id = elisp_string_from_id(left_sibling_id)
        return '(ts-insert-child-into-file "{}" {} \'{})'.format(
            self.id, export_left_id, plist_string)

    def external_insert_as_child(self, left_sibling_id, parent_node):
        plist_string = elisp_string_from_dict(self.export_attrs)
        export_left_id = elisp_string_from_id(left_sibling_id)
        self._repl_to_source_command(
            '(ts-insert-file "{}" {} \'{})'.format(
                self.id, export_left_id, plist_string))

    def external_update(self, other_node):
        parameters = {k: v for k, v in other_node.export_attrs.items()
                      if getattr(self, k, None) != v}
        plist_string = elisp_string_from_dict(parameters)
        self._repl_to_source_command(
            '(ts-update-file "{}" \'{})'.format(self.id, plist_string))

    def as_parent_move_child_command(self, child_id, left_sibling_id):
        export_child_id = elisp_string_from_id(child_id)
        export_left_id = elisp_string_from_id(left_sibling_id)
        return '(ts-move-to-file {} {} "{}")'.format(
            export_child_id, export_left_id, self.id)

    def external_move_to(self, left_sibling_id, parent_node):
        pass        # don't try to reorder files

    def external_delete(self):
        self._repl_to_source_command(
            '(ts-delete-file "{}")'.format(self.id))


class HeadlineNode(node.Node):

    def __init__(self, repl_make_headline_command,
                 repl_to_source_command):
        super().__init__()
        self._repl_make_headline_command = repl_make_headline_command
        self._repl_to_source_command = repl_to_source_command
        self.title = DEFAULT_HEADLINE_TITLE
        self.todo_type = DEFAULT_TODO_TYPE

    def as_parent_insert_child_command(self, left_sibling_id,
                                       plist_string):
        export_left_id = elisp_string_from_id(left_sibling_id)
        return '(ts-insert-child "{}" {} \'{})'.format(
            self.id, export_left_id, plist_string)

    def external_insert_as_child(self, left_sibling_id, parent_node):
        plist_string = elisp_string_from_dict(self.export_attrs)
        command = parent_node.as_parent_insert_child_command(
            left_sibling_id, plist_string)
        new_id = self._repl_make_headline_command(command)
        self.id = eval(new_id)

    def external_update(self, other_node):
        parameters = {k: v for k, v in other_node.export_attrs.items()
                      if getattr(self, k, None) != v}
        dont_update_closed_tests = [self.todo_type == 'DONE',
                                    other_node.todo_type == 'DONE',
                                    parameters.get('closed', None)]
        if all(dont_update_closed_tests):
            parameters.pop('closed')
        plist_string = elisp_string_from_dict(parameters)
        self._repl_to_source_command(
            '(ts-update "{}" \'{})'.format(self.id, plist_string))

    def as_parent_move_child_command(self, child_id, left_sibling_id):
        export_child_id = elisp_string_from_id(child_id)
        export_left_id = elisp_string_from_id(left_sibling_id)
        return '(ts-move-to {} {} "{}")'.format(
            export_child_id, export_left_id, self.id)

    def external_move_to(self, left_sibling_id, parent_node):
        command = parent_node.as_parent_move_child_command(
            self.id, left_sibling_id)
        self._repl_to_source_command(command)

    def external_delete(self):
        self._repl_to_source_command('(ts-delete "{}")'.format(self.id))


class Source(source.Source):
    DEFAULT_FETCH_FIELDS = ('id', 'title', 'paragraph', 'parent_id',
                            'todo_type', 'closed', 'deadline',
                            'filename', 'tags')

    def __init__(self, repl_get_source_command,
                 repl_make_headline_command, repl_to_source_command,
                 repl_sendeof, verbose=False):
        self._repl_get_source_command = repl_get_source_command
        if verbose:
            self._repl_get_source_command = lib.make_wrapped_fn(
                'Get From Source:', self._repl_get_source_command)
        self._repl_make_headline_command = repl_make_headline_command
        if verbose:
            self._repl_make_headline_command = lib.make_wrapped_fn(
                'Make Headline:', self._repl_make_headline_command)
        self._repl_to_source_command = repl_to_source_command
        if verbose:
            self._repl_to_source_command = lib.make_wrapped_fn(
                'Send To Source:', self._repl_to_source_command)
        self._repl_sendeof = repl_sendeof
        self._verbose = verbose

    def __enter__(self):
        self._repl_get_source_command('(ts-init)')
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        self._repl_to_source_command('(ts-final)')
        self._repl_sendeof()

    @classmethod
    def from_emacs_repl(cls, emacs_repl, verbose=False):
        c = cls(emacs_repl.run_command,
                emacs_repl.run_command,
                emacs_repl.run_command,
                emacs_repl.child.sendeof,
                verbose)
        return c

    def make_root_node(self):
        return RootNode()

    def make_headline_node(self, info_dict):
        return HeadlineNode.from_dict(
            info_dict,
            self._repl_make_headline_command,
            self._repl_to_source_command)

    def make_filename_node(self, info_dict):
        return FilenameNode.from_dict(info_dict,
                                      self._repl_to_source_command)

    def get_all_items(self, extra_field_list=None):
        field_list = self.DEFAULT_FETCH_FIELDS
        if extra_field_list:
            field_list = field_list + tuple(extra_field_list)
        node_cache = {}

        # first process the headlines
        all_headlines = self._repl_get_source_command(
            '(ts-get-all-headlines \'{})'.format(
                elisp_string_from_list(field_list)))
        all_headlines = eval(eval(all_headlines))
        for h in all_headlines:
            filename = h.pop('filename', None)
            if h['parent_id'] is None:
                h['parent_id'] = filename
            if 'tags' in h:
                if h['tags'] is None:
                    h['tags'] = set()
                else:
                    h['tags'] = set(h['tags'])
            node = self.make_headline_node(h)
            node_cache[node.id] = node

        # then the filenames
        all_filenames = self._repl_get_source_command(
            '(ts-get-all-filenames \'{})'.format(
                elisp_string_from_list(field_list)))
        all_filenames = eval(eval(all_filenames))
        for f in all_filenames:
            node = self.make_filename_node(f)
            node_cache[node.id] = node

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
    def from_emacs_repl(cls, emacs_repl, verbose=False):
        c = cls(
            emacs_repl.run_command,
            lib.make_counting_fn(lambda i: '"NEW HEADLINE"'.format(i)),
            lib.noop,
            emacs_repl.child.sendeof,
            verbose)
        return c
