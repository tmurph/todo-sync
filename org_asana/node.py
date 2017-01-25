if __name__ == "__main__" and __package__ is None:
    import os
    import sys
    sys.path.insert(0, os.path.abspath(".."))
__package__ = "org_asana"

import collections

class Node():
    EXTRA_ATTRS = set()

    def __init__(self):
        self.id = None
        for a in type(self).EXTRA_ATTRS:
            setattr(self, a, None)
        self.in_order = False
        self.children = []
        self.parent = None

    def __str__(self):
        d = {}
        d['id'] = self.id
        for a in type(self).EXTRA_ATTRS:
            d[a] = getattr(self, a, None)
        return str(d)

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

    def post_order(self):
        lst = self.breadth_first_order()
        lst.reverse()
        return lst

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
        for a in type(self).EXTRA_ATTRS:
            setattr(self, a, getattr(other_node, a, None))

    def move_to(self, sibling_position, parent_node):
        parent_node.insert_child(sibling_position, self)

    def next_sibling(self):
        result = None
        try:
            i = self.parent.children.index(self)
            result = self.parent.children[i + 1]
        finally:
            return result

class RootNode(Node):
    EXTRA_ATTRS = ('root')

class Command():

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

