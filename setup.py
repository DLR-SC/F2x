import setuptools
from distutils import core

core.setup(
    name='F2x',
    version='0.1.dev0',

    description='Template-based Fortran wrapper.',
    author='Michael Meinel',
    author_email='michael.meinel@dlr.de',
    url='http://www.dlr.de/sc',
    
    packages=setuptools.find_packages('src'),
    package_dir={ '': 'src' },
    
    install_requires = [
        'plyplus',
        'jinja2',
    ],
    
    entry_points = {
        'console_scripts': [
            'F2x=F2x.main:main',
        ],
    }
)
