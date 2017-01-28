if __name__ == "__main__" and __package__ is None:
    import os
    import sys
    sys.path.insert(0, os.path.abspath(".."))
__package__ = "org_asana"

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

class PrintCommand(Command):
    def insert_child(self, parent_node, sibling_position, new_child_node):
        print('insert_child: parent: {}, pos: {}, child: {}'.format(
            parent_node.id, sibling_position, new_child_node.id))

    def delete(self, node_to_delete):
        print('delete: {}'.format(node_to_delete.id))

    def update(self, node_to_update, other_node):
        print('update: to_update: {}, model: {}'.format(
            node_to_update.id, other_node.id))

    def move_to(self, child_node, sibling_position, parent_node):
        print('move_to: child: {}, pos: {}, parent: {}'.format(
            child_node.id, sibling_position, parent_node.id))

class VerbosePrintCommand(Command):
    def insert_child(self, parent_node, sibling_position, new_child_node):
        strings = ['insert_child:',
                   '    parent: {}'.format(parent_node),
                   '    pos: {}'.format(sibling_position),
                   '    child: {}'.format(new_child_node)]
        print(*strings, sep='\n')

    def delete(self, node_to_delete):
        strings = ['delete:', '    node: {}'.format(node_to_delete)]
        print(*strings, sep='\n')

    def update(self, node_to_update, other_node):
        strings = ['update:',
                   '    to_update: {}'.format(node_to_update),
                   '    model: {}'.format(other_node)]
        print(*strings, sep='\n')

    def move_to(self, child_node, sibling_position, parent_node):
        strings = ['move_to:',
                   '    child: {}'.format(child_node),
                   '    pos: {}'.format(sibling_position),
                   '    parent: {}'.format(parent_node)]
        print(*strings, sep='\n')
