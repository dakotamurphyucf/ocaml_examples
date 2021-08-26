from distutils.core import setup

NAME = "python_ocaml"

setup(
    name=NAME,
    version="0.0.1",
    description="A small example package",
    packages=[NAME],
    package_data={NAME: ["pywrap.so"]},
)
