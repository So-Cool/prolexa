#! /usr/bin/env python

import re
from setuptools import find_packages, setup
from setuptools.command.install import install
from setuptools.command.develop import develop

import prolexa

def setup_models():
    import prolexa.setup_models as setup_models_
    setup_models_.get_wordnet_nltk()
    setup_models_.get_pos_flair()

class PostInstallCommand(install):
    """Post-installation for installation mode."""
    def run(self):
        install.run(self)
        setup_models()

class PostDevelopCommand(develop):
    """Post-installation for development mode."""
    def run(self):
        develop.run(self)
        setup_models()

def dependencies_from_file(file_path):
    required = []
    with open(file_path) as f:
        for l in f.readlines():
            l_c = l.strip()
            # get not empty lines and ones that do not start with python
            # comment "#" (preceded by any number of white spaces)
            if l_c and not l_c.startswith('#'):
                required.append(l_c)
    return required

def get_dependency_version(dependency, list_of_dependencies):
    matched_dependencies = []

    reformatted_dependency = dependency.lower().strip()
    for dep in list_of_dependencies:
        dependency_version = re.split('~=|==|!=|<=|>=|<|>|===', dep)
        if dependency_version[0].lower().strip() == reformatted_dependency:
            matched_dependencies.append(dep)

    if not matched_dependencies:
        raise NameError(('{} dependency could not be found in the list of '
                         'dependencies.').format(dependency))

    return matched_dependencies

DISTNAME = 'prolexa'
VERSION = prolexa.__version__
DESCRIPTION = 'Prolexa Plus'
with open('README.md') as f:
    LONG_DESCRIPTION = f.read()
MAINTAINER = 'Kacper Sokol'
MAINTAINER_EMAIL = 'k.sokol@bristol.ac.uk'
URL = 'https://github.com/so-cool/{}'.format(DISTNAME)
DOWNLOAD_URL = 'https://github.com/so-cool/{}'.format(DISTNAME)
LICENSE = 'new BSD'
PACKAGES = find_packages(exclude=['*.tests', '*.tests.*', 'tests.*', 'tests'])
INSTALL_REQUIRES = dependencies_from_file('requirements.txt')
# Python 3.5 and up but not commited to Python 4 support yet
PYTHON_REQUIRES = '~=3.5'
INCLUDE_PACKAGE_DATA = True
#ZIP_SAFE = False

def setup_package():
    metadata = dict(name=DISTNAME,
                    maintainer=MAINTAINER,
                    maintainer_email=MAINTAINER_EMAIL,
                    description=DESCRIPTION,
                    license=LICENSE,
                    url=URL,
                    download_url=DOWNLOAD_URL,
                    version=VERSION,
                    install_requires=INSTALL_REQUIRES,
                    long_description=LONG_DESCRIPTION,
                    python_requires=PYTHON_REQUIRES,
                    #zip_safe=ZIP_SAFE,
                    packages=PACKAGES,
                    # include_package_data=INCLUDE_PACKAGE_DATA,
                    package_data={DISTNAME: ['prolog/*.pl']},
                    cmdclass={'install': PostInstallCommand,
                              'develop': PostDevelopCommand},
                    entry_points = {
                        'console_scripts': [
                            'prolexa-setup-models=prolexa.command_line:setup_models',
                            'prolexa-plus=prolexa.command_line:prolexa_plus']
                    }
                    )

    setup(**metadata)

if __name__ == "__main__":
    setup_package()
