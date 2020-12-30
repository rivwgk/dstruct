project(
  'dstruct',
  'fortran',
  version: '0.1.0',
)

sources = []
subdir('src')

dstruct_lib = library(
  meson.project_name(),
  sources: sources,
)

dstruct_dep = declare_dependency(
  link_with: dstruct_lib,
  include_directories: dstruct_lib.private_dir_include(),
)
