from setuptools import setup, find_packages
import os
from pathlib import Path
import shutil

if os.path.exists('install/lib') and not os.path.exists('install/isl'):
  shutil.move('install/lib', 'install/isl')

if not os.path.exists('install/isl/isl.py'):
  shutil.copy('interface/isl.py', 'install/isl/isl.py')
if not os.path.exists('install/isl/__init__.py'):
  shutil.copy('interface/__init__.py', 'install/isl/__init__.py')

libfiles = [os.path.join('install/isl', f) for f in os.listdir('install/isl') if f.startswith('libisl') and not f.endswith('.a') and not f.endswith('.py') and not f.endswith('.la')]
setup(name='isl',
      version='0.1.0',
      packages=['isl'],
      package_dir={'': 'install'},
      data_files=[('lib', libfiles)],
      )
