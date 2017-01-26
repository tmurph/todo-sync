def assert_trees_equal_p(left, right, ignored_keys_list=None):
    if ignored_keys_list is None:
        ignored_keys_list = []
    left_keys = left.export_attrs.keys()
    right_keys = right.export_attrs.keys()
    if len(left_keys) != len(right_keys):
        raise AssertionError(
            '\n'.join([
                'Mismatched keys.',
                '',
                'Left keys:',
                '  {}'.format(left_keys),
                '',
                'Right keys:',
                '  {}'.format(right_keys)]))
    for key in left_keys:
        if key not in ignored_keys_list:
            left_val = getattr(left, key)
            right_val = getattr(right, key)
            if left_val != right_val:
                raise AssertionError(
                    '\n'.join([
                        '\'{}\' values compare unequal.'.format(key),
                        '',
                        'Left \'{}\':'.format(key),
                        '  {!r}'.format(left_val),
                        '',
                        'Right \'{}\':'.format(key),
                        '  {!r}'.format(right_val)]))
    left_children = left.children
    right_children = right.children
    if len(left_children) != len(right_children):
        raise AssertionError(
            '\n'.join([
                'Mismatched children.',
                '',
                'Left children:',
                '  {}'.format([n.id for n in left_children]),
                '',
                'Right children:',
                '  {}'.format([n.id for n in right_children])]))
    for c_left, c_right in zip(left_children, right_children):
        assert_trees_equal_p(c_left, c_right, ignored_keys_list)
        # # This works fine in practice, but I can't make it work with
        # # Pytest
        # try:
        #     assert_trees_equal_p(c_left, c_right)
        # except AssertionError as err:
        #     raise AssertionError(
        #         '\n'.join([
        #             'Children compare unequal.',
        #             '',
        #             'Left child: {}'.format(c_left.id),
        #             'Right child: {}'.format(c_right.id)])) from err
