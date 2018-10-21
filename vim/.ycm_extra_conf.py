def FlagsForFile( filename, **kwargs ):
  return {
    'flags': [ '-x', 'c++', '-Wall', '-I./include', '-Wno-unused-variable' ],
  }
