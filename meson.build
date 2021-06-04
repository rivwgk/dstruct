project(
  'dstruct',
  'fortran',
  version: '0.1.0',
  default_options: [
    'buildtype=debugoptimized',
    'default_library=both',
  ],
)

testdrive_prj = subproject(
  'test-drive',
  version: '>=0.3',
)
testdrive_dep = testdrive_prj.get_variable('testdrive_dep')

sources = []
subdir('src')

dstruct_lib = library(
  meson.project_name(),
  sources: sources,
  dependencies: testdrive_dep,
)
dstruct_inc = dstruct_lib.private_dir_include()
dstruct_dep = declare_dependency(
  link_with: dstruct_lib,
  include_directories: dstruct_inc,
)

dstruct_lic = files(
  'LICENSE',
)

subdir('test')
