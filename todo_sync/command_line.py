import sys
import argparse
import importlib

from .edit_script import edit_script


def main(argv=None):
    if argv is None:
        argv = sys.argv

    supported_backends = ['org', 'asana']

    description = ('Manipulate one todo list to match another.')
    epilog = "".join(["(Currently supports '",
                      "', '".join(supported_backends[:-1]),
                      "' and '",        # no oxford comma
                      supported_backends[-1],
                      "' as backends.)"])
    parser = argparse.ArgumentParser(description=description,
                                     epilog=epilog)
    parser.add_argument('behind', metavar='behind',
                        choices=supported_backends,
                        help='the backend that will be manipulated')
    parser.add_argument('behind_config',
                        help='arg for configuring the "behind" backend')
    parser.add_argument('ahead', metavar='ahead',
                        choices=supported_backends,
                        help='the backend that will be used as a model')
    parser.add_argument('ahead_config',
                        help='arg for configuring the "ahead" backend')
    parser.add_argument('-n', '--dry-run', action='store_true',
                        help='don\'t manipulate the "behind" backend,'
                        ' just print the computed actions to stdout')
    parser.add_argument('-v', '--verbose', action='store_true',
                        help='print commands as they happen')
    parser.add_argument('--no-delete', action='store_true',
                        help='skip Delete commands')

    args = parser.parse_args(argv[1:])

    module_name = (__package__
                   + '.mappers.{}_to_{}'.format(args.behind, args.ahead))
    module = importlib.import_module(module_name)

    with module.behind_source(
            args.behind_config, args.verbose, args.dry_run) as b_source:
        with module.ahead_source(
                args.ahead_config, args.verbose) as a_source:

            edit_script(b_source.get_tree(), a_source.get_tree(),
                        module.map_fn, module.eql_fn, b_source.make_fn,
                        args.no_delete)

    return 0


if __name__ == '__main__':
    sys.exit(main())
