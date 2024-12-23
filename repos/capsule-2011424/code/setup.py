#!/usr/bin/env python
# -*- coding: utf-8 -*-


try:
    from setuptools import setup, find_packages
except ImportError:
    from distutils.core import setup

with open('README.md') as readme_file:
    readme = readme_file.read()

with open('HISTORY.rst') as history_file:
    history = history_file.read()

requirements = [
    # TODO: put package requirements here
    'scikit-fuzzy','numpy'
]

test_requirements = [
    # TODO: put package test requirements here
]


setup(
    name='bordertgraank',
    version='1.0',
    description="A Python implementation of the Border Temporal GRAdual rANKing algorithm.",
    long_description=readme + '\n\n' + history,
    author="Dickson Owuor",
    author_email='owuordickson@ieee.org',
    url='https://github.com/owuordickson/border-tgraank',
    packages=find_packages(),
    #package_dir={'tgraank':'tgraank'},
    include_package_data=True,
    install_requires=requirements,
    license="MIT",
    zip_safe=False,
    keywords='border-tgraank',
    classifiers=[
        'Development Status :: 1 - Production/Stable',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: Massachusetts Institute of Technology (MIT) License',
        'Natural Language :: English',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.3',
        'Programming Language :: Python :: 3.4',
        'Programming Language :: Python :: 3.5',
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: 3.7',
    ],
    test_suite='tests',
    tests_require=test_requirements
)