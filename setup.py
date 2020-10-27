import os
from setuptools import setup, find_packages


def read(fname):
    return open(os.path.join(os.path.dirname(__file__), fname)).read()


setup(
    name="aitools",
    version="0.0.1",
    author="Daniele Trebbi",
    author_email="dRain88@gmail.com",
    description=("Various symbolic AI tools"),
    license="MIT",
    keywords="logic unification",
    url="https://github.com/OneManEquipe/aitools",
    packages=find_packages(exclude=['tests']),
    long_description=read('README.md'),
    classifiers=[
        "Development Status :: 1 - Planning",
        "Topic :: Scientific/Engineering :: Artificial Intelligence",
        "License :: OSI Approved :: MIT License",
    ],
    install_requires=[
        "janus==0.6.1"
    ]
)
