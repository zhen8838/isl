from pathlib import Path
from setuptools import setup, find_packages, Extension
from setuptools.command.build_ext import build_ext
from setuptools.command.install_lib import install_lib

class BuildSharedLibs(build_ext):
  def run(self):
    for ext in self.extensions:
      if ext.name == 'isl':
        self.build(ext)

  def build(self, ext: Extension):
    self.announce("Auto Generating")
    self.spawn(["./autogen.sh"])
    self.announce("Configuring")
    bin_dir = Path(self.build_temp).absolute()
    self.distribution.bin_dir = bin_dir
    self.spawn(["./configure", f"--prefix={bin_dir}",
               "--with-clang=no", "--with-int=imath"])
    self.announce("Compiling")
    self.spawn(["make", "install", "-j"])
    self.distribution.run_command("install_lib")


class InstallSharedLibs(install_lib):
  def run(self):
    self.skip_build = True
    lib_dir = Path(self.distribution.bin_dir) / 'lib'
    data_files = [str(file.absolute()) for file in lib_dir.iterdir()
                  if file.name.startswith('libisl') and 
                    sum(map(lambda suffix: suffix in file.name, [".dll", ".so", ".dylib"])) > 0 and
                    not '.py' in file.name]
    self.announce(f"Installing Files {' '.join(data_files)}")
    self.distribution.data_files = [('lib', data_files)]
    self.distribution.run_command("install_data")
    super().run()

setup(py_modules=['isl'],
    package_dir={'': 'interface'},
    ext_modules=[Extension('isl', [])],
    cmdclass={
        "build_ext": BuildSharedLibs,
        'install_lib': InstallSharedLibs,
    })
