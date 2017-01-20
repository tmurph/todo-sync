if __name__ == "__main__" and __package__ is None:
    import os
    import sys
    sys.path.insert(0, os.path.abspath(".."))
__package__ = "org_asana"

class Mapping():

    def __init__(self, source_tree, target_tree,
                 s_equals_t_p, make_s_from_t):
        self._s_from_t_dict = {}
        self._equals_s_t = s_equals_t_p
        self._make_s_from_t = make_s_from_t
        if not hasattr(source_tree, 'root'):
            raise Exception("Source tree is not rooted.")
        if not hasattr(target_tree, 'root'):
            raise Exception("Target tree is not rooted.")
        s_list = source_tree.breadth_first_order()
        t_list = target_tree.breadth_first_order()
        self._s_from_t_dict[target_tree] = source_tree
        s_list.remove(source_tree)
        t_list.remove(target_tree)
        for s_node in s_list:
            for t_node in t_list:
                if self._equals_s_t(s_node, t_node):
                    self._s_from_t_dict[t_node] = s_node
                    t_list.remove(t_node)
                    break

    def source_equals_target_p(source_node, target_node):
        # So ... here's an overkill solution.  I'm lucky because
        # there's only one way to get to this function right now,
        # and that's after we've checked that the target_node
        # has a parent and found the target_node's match.
        # So I'm pretty sure there's no way to test root nodes vs
        # non-root nodes.  But whew.  This feels weird.
        #
        # s_root_p = hasattr(source_node, 'root')
        # t_root_p = hasattr(target_node, 'root')
        # if s_root_p or t_root_p:
        #     result = s_root_p and t_root_p
        # else:
        #     result = self._equals_s_t(source_node, target_node)
        # return result
        return self._equals_s_t(source_node, target_node)

    def make_source_from_target(self, target_node):
        return self._make_s_from_t(target_node)

    def target_from_source(self, source_node):
        result = None
        if hasattr(source_node, 'root'):
            for t_node in self._s_from_t_dict.keys():
                if hasattr(t_node, 'root'):
                    result = t_node
        else:
            for t_node in self._s_from_t_dict.keys():
                if (not hasattr(t_node, 'root') and
                    self._equals_s_t(source_node, t_node)):
                    result = t_node
        return result

    def source_from_target(self, target_node):
        return self._s_from_t_dict.get(target_node)

    def add_mapped_nodes(self, source_node, target_node):
        self._s_from_t_dict[target_node] = source_node

    def mapped_nodes_p(self, source_node, target_node):
        return (target_node, source_node) in self._s_from_t_dict.items()

    def get_target_position(self, target_node):
        target_parent = target_node.parent
        target_index = target_parent.children.index(target_node)
        target_ordered_nodes = [t_node for t_node
                                in target_parent.children[:target_index]
                                if t_node.in_order]
        result = 0
        if target_ordered_nodes:
            if target_node is target_ordered_nodes[0]:
                result = 0
            else:
                source_node = self.source_from_target(target_ordered_nodes[-1])
                source_parent = source_node.parent
                source_ordered_nodes = [s_node
                                        for s_node in source_parent.children
                                        if s_node.in_order]
                source_ordered_index = source_ordered_nodes.index(source_node)
                result = source_ordered_index + 1
        return result
