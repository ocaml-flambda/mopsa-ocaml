from setuptools import setup, Extension

module = Extension('counter', sources=['counter.c'])

setup(name="counter", ext_modules=[module])
