if __name__ == "__main__" and __package__ is None:
    import os
    import sys
    sys.path.insert(0, os.path.abspath(".."))
__package__ = "org_asana"

import collections

class Node():
    CLASS_EXPORT_ATTRS_TEMPLATE = tuple()

    def __init__(self):
        self.id = None
        self.EXPORT_ATTRS = list(type(self).CLASS_EXPORT_ATTRS_TEMPLATE)
        for a in self.EXPORT_ATTRS:
            setattr(self, a, None)
        self.in_order = False
        self.children = []
        self.parent = None

    def __str__(self):
        d = {'id': self.id, 'parent': self.parent}
        for a in self.EXPORT_ATTRS:
            d[a] = getattr(self, a, None)
        return str(d)

    @classmethod
    def from_dict(cls, info_dict):
        if not info_dict.get('id'):
            raise Exception('All Nodes must have an ID.')
        if not 'parent' in info_dict.keys():
            raise Exception('All Nodes must have a "parent" '
                            '({"parent": None} is fine).')
        n = cls()
        for k, v in info_dict.items():
            setattr(n, k, v)
            if k not in ['id', 'parent']:
                n.EXPORT_ATTRS.append(k)
        return n

    @classmethod
    def from_dict_list(cls, info_dict_list):
        node_cache = {}
        n = cls.from_dict(info_dict_list[0])
        node_cache[n.id] = n
        for d in info_dict_list[1:]:
            node_cache[d['id']] = cls.from_dict(d)
        for d in info_dict_list[1:]:
            node = node_cache[d['id']]
            if d.get('parent'):
                parent_node = node_cache[d['parent']]
            else:
                parent_node = n
            parent_node.append_child(node)
        return n

    def insert_child(self, sibling_position, child_node):
        self.children.insert(sibling_position, child_node)
        child_node.parent = self

    def append_child(self, child_node):
        self.children.append(child_node)
        child_node.parent = self

    def delete(self):
        if self.children:
            raise Exception("Cannot delete a parent node!")
        else:
            self.parent.children.remove(self)

    def update(self, other_node):
        for a in other_node.EXPORT_ATTRS:
            setattr(self, a, getattr(other_node, a, None))
            self.EXPORT_ATTRS.append(a)

    def move_to(self, sibling_position, parent_node):
        self.parent.children.remove(self)
        parent_node.insert_child(sibling_position, self)

    def next_sibling(self):
        result = None
        try:
            i = self.parent.children.index(self)
            result = self.parent.children[i + 1]
        finally:
            return result

class RootNode(Node):
    CLASS_EXPORT_ATTRS_TEMPLATE = ('root',)

    def __init__(self):
        super().__init__()
        self.id = 'ROOT'

    @classmethod
    def from_dict_list(cls, node_cls, info_dict_list):
        n = cls()
        node_cache = {}
        for d in info_dict_list:
            node_cache[d['id']] = node_cls.from_dict(d)
        for d in info_dict_list:
            node = node_cache[d['id']]
            if d.get('parent'):
                parent_node = node_cache[d['parent']]
            else:
                parent_node = n
            parent_node.append_child(node)
        return n

class Command():
    DEFAULT_FETCH_FIELDS = tuple()

    def insert_child(self, parent_node, sibling_position, new_child_node):
        "Generate an external call to insert a child node."
        raise NotImplementedError

    def delete(self, node_to_delete):
        "Generate an external call to delete a node."
        raise NotImplementedError

    def update(self, node_to_update, other_node):
        "Generate an external call to update a node's values."
        raise NotImplementedError

    def move_to(self, child_node, sibling_position, parent_node):
        "Generate an external call to reparent a node."
        raise NotImplementedError

    def get_all_items(self, extra_field_list):
        "Fetch all items from the source."
        raise NotImplementedError

def trees_equal_p(t_left, t_right):
    result = False
    keys_equal_p = t_left.__dict__.keys() == t_right.__dict__.keys()
    if keys_equal_p:
        values_equal_p = (getattr(t_left, k) == getattr(t_right, k)
                          for k in t_left.__dict__.keys()
                          if k not in ['children', 'parent'])
        children_length_p = len(t_left.children) == len(t_right.children)
        if all(values_equal_p) and children_length_p:
            children_equal_p = (trees_equal_p(c_left, c_right)
                                for c_left, c_right in
                                zip(t_left.children, t_right.children))
            if all(children_equal_p):
                result = True
    return result

def breadth_first_order(self):
    result, queue = [], collections.deque()
    result.append(self)
    if self.children:
        node = self.children[0]
        while node:
            result.append(node)
            if node.children:
                queue.append(node)
            node = node.next_sibling()
            if not node and queue:
                node = queue.popleft().children[0]
    return result
