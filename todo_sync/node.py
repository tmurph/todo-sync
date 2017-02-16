class Node():

    def __init__(self):
        self._id = None
        self._in_order = False
        self._children = []
        self._parent = None

    def __str__(self):
        d = {'id': self.id}
        d.update(self.export_attrs)
        return str(d)

    @classmethod
    def from_dict(cls, info_dict, *args):
        if 'id' not in info_dict.keys():
            raise Exception('All Nodes must have an ID.'
                            '\n\n'
                            "{'id': None} is reserved for Root nodes.")
        n = cls(*args)
        for k, v in info_dict.items():
            setattr(n, k, v)
        return n

    @property
    def id(self):
        return self._id

    @id.setter
    def id(self, new_id):
        self._id = new_id

    @property
    def children(self):
        return self._children

    @children.setter
    def children(self, new_children):
        self._children = new_children

    @property
    def parent(self):
        return self._parent

    @parent.setter
    def parent(self, new_parent):
        self._parent = new_parent

    @property
    def in_order(self):
        return self._in_order

    @in_order.setter
    def in_order(self, in_order):
        self._in_order = in_order

    @property
    def export_attrs(self):
        return {k: v for k, v in vars(self).items()
                if not k.startswith("_")}

    def insert_as_child(self, left_sibling_id, parent_node):
        child_ids = [None] + [n.id for n in parent_node.children]
        insert_index = child_ids.index(left_sibling_id)
        parent_node.children.insert(insert_index, self)
        self.parent = parent_node

    def update(self, other_node):
        for k, v in other_node.export_attrs.items():
            setattr(self, k, v)

    def move_to(self, left_sibling_id, parent_node):
        if self.parent:
            self.parent.children.remove(self)
        self.insert_as_child(left_sibling_id, parent_node)

    def delete(self):
        if self.children:
            raise Exception("Cannot delete a parent node!")
        else:
            self.parent.children.remove(self)

    def external_insert_as_child(self, left_sibling_id, parent_node):
        raise NotImplementedError

    def external_update(self, other_node):
        raise NotImplementedError

    def external_move_to(self, left_sibling_id, parent_node):
        raise NotImplementedError

    def external_delete(self):
        raise NotImplementedError

    def append_child(self, child_node):
        self.children.append(child_node)
        child_node.parent = self

    def next_sibling(self):
        result = None
        try:
            i = self.parent.children.index(self)
            result = self.parent.children[i + 1]
        finally:
            return result


class RootNode(Node):
    "Just a placeholder to indicate the root of a tree."
