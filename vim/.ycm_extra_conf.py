import os

def FlagsForFile( filename, **kwargs ):
    name, ext = os.path.splitext(filename)
    lang = "c++"
    lib = "-std=c++11"
    if ext == ".c":
        lang = "c"
        lib = "-std=c99"
    return {
           'flags': [ '-x', lang, lib, '-Wall', '-Wpedantic', '-I./include', '-Wno-unused-variable', '-fms-extensions' ]
            }
