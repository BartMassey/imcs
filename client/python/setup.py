from setuptools import setup, find_packages
from os import path

here = path.abspath(path.dirname(__file__))

setup(
    name='IMCS_Client',
    version='1.0beta',
    packages=find_packages(exclude=['contrib', 'docs', 'tests*']),
    url='https://github.com/mikelane/IMCS_Client',
    license='MIT',
    author='Michael Lane',
    author_email='mikelane@gmail.com',
    description='A client for the Internet Minichess Server',

    # https://packaging.python.org/distributing/#classifiers
    classifiers=[
        # How mature is this project? Common values are
        #   3 - Alpha
        #   4 - Beta
        #   5 - Production/Stable
        'Development Status :: 4 - Beta',

        # Indicate who your project is intended for
        'Intended Audience :: Education',
        'Topic :: Education :: Artificial Intelligence',
        'Topic :: Education :: Combinatorial Games',

        # License should match License above
        'License :: OSI Approved :: MIT License',

        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: 2.7'
    ],

    keywords='networking, chess, client, PSU',
    install_requires=[
        'configparser'
    ],
    setup_requires=[
        'configparser'
    ]
)
