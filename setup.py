from setuptools import setup, find_packages
from uxsdcxx import __version__

setup(
    name="uxsdcxx",
    version=__version__,
    author="duck2",
    author_email="duck2@protonmail.com",
    description="generate PugiXML reader/writer from XSD schema",
    license="Apache-2.0",
    install_requires=["xmlschema==1.0.11", "automata-lib==3.1.0.post1"],
    packages=["uxsdcxx"],
    scripts=["uxsdcxx.py", "uxsdcap.py"],
    url="https://github.com/duck2/uxsdcxx",
    project_urls={
        "Source Code": "https://github.com/duck2/uxsdcxx",
        "Bug Tracker": "https://github.com/duck2/uxsdcxx/issues",
    },
)
