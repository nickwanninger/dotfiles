def FlagsForFile( filename, **kwargs ):
  return {
    'flags': [ '-std=c99', '-x', 'c++', '-Wall', '-Wextra', '-Werror', '-Wno-unused-parameter', '-Wno-unused-variable', '-I./include' ],
  }
