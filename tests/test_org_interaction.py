import pytest

if __name__ == '__main__' and __package__ is None:
    import os
    import sys
    sys.path.insert(0, os.path.abspath('..'))
__package__ = 'org_asana.tests'

from org_asana.org_interaction import parse_headline_list

@pytest.mark.parametrize("org_string, expected_list", [
    ("()", [])                    # nino
    , ('((:id "A" :parent nil))', # in-n-out
       [{'id': "A", 'parent': None}])
    , ('((:id "A" :parent nil)' # more complicated
       ' (:id "B" :parent (:id "A"))'
       ' (:id "C" :parent nil))',
       [{'id': "A", 'parent': None},
        {'id': "B", 'parent': {'id': "A"}},
        {'id': "C", 'parent': None}])
])
def test_parse_headline_list(org_string, expected_list):
    "Can we parse Org tasks into a list?"
    assert parse_headline_list(org_string) == expected_list
