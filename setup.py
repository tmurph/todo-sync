import setuptools as tools


def readme():
    with open('README.rst') as f:
        return f.read()


tools.setup(
    name='todo_sync',
    version='0.3.1',
    description='Synchronize todo lists, supports Emacs Org Mode and Asana.',
    long_description=readme(),
    url='https://github.com/tmurph/todo-sync',
    author='Trevor Murphy',
    author_email='trevor.m.murphy@gmail.com',
    license='GPL3',
    packages=tools.find_packages(exclude=['tests']),
    zip_safe=False,
    install_requires=[
        'asana', 'pexpect>=4.3'
    ],
    python_requires='>=3, <3.7',
    classifiers=[
        'Development Status :: 3 - Alpha',
        'License :: OSI Approved :: GNU General Public License v3 or later (GPLv3+)',
        'Programming Language :: Python :: 3.6',
        'Topic :: Utilities',
    ],
    include_package_data=True,
    setup_requires=['pytest-runner'],
    tests_require=['pytest'],
    entry_points={
        'console_scripts': ['todo-sync=todo_sync.command_line:main']
    },
)
