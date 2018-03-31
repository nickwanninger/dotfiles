def FlagsForFile( filename, **kwargs ):
  return {
    'flags': [ '-std=c99', '-x', 'c++', '-Wall', '-Wextra', '-Werror' ],
  }
