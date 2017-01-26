import todo_sync.node as node


class Source():
    DEFAULT_FETCH_FIELDS = tuple()

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        pass

    def get_all_items(self, extra_field_list):
        "Fetch all items from the source."
        raise NotImplementedError

    def make_root_node(self):
        return node.RootNode()
