import os

def FlagsForFile( filename, **kwargs ):
    name, ext = os.path.splitext(filename)
    lang = "c++"
    std = "-std=c++17"
    if ext == ".c":
        lang = "c"
        std = "-std=c99"
    return {
           'flags': [ '-x', lang, std, '-Wall', '-I./include', '-I./Include',
               '-I/usr/local/opt/llvm/include', '-Wunused-variable', '-fms-extensions', '-Wno-gnu-anonymous-struct' ]
            }
