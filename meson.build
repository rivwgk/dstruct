project(
  'dstruct',
  'fortran',
  version: '0.1.0',
)

sources = []
mapping_sources = []
subdir('src')

add_global_arguments('-pie', language: 'fortran')

dstruct_lib = library(
  meson.project_name(),
  sources: sources,
)

map_test = executable(
  'mapping_test',
  sources: mapping_sources,
  link_with: dstruct_lib,
)
test(
  'testing default operations of mapping',
  map_test,
)
