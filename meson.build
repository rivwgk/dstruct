project(
  'dstruct',
  'fortran',
  version: '0.1.0',
)

sources = []
avltree_sources = []
dynarray_sources = []
subdir('src')

dstruct_lib = library(
  meson.project_name(),
  sources: sources,
)

avltree_test = executable(
  'mapping_test',
  sources: avltree_sources,
  link_with: dstruct_lib,
)
test(
  'testing default operations of the avl tree',
  avltree_test,
)

dynarray_test = executable(
  'dynamic_array_test',
  sources: dynarray_sources,
  link_with: dstruct_lib,
)
test(
  'testing default operations of the dynamic array',
  dynarray_test,
)
