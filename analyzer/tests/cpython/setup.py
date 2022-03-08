from setuptools import setup, Extension

basicmodule = Extension('basic', sources=['basicmodule.c'])

setup(name="basic", description="Cbox c module", ext_modules=[basicmodule])
