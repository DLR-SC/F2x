import argparse

from F2x.lib import find_library_path

_LANGUAGES = {
    "fortran": {
        "include" : ("c_interface_module.mod", '-I"{1}"'),
        "lib": ("libF2x.so", '"{0}"'),
        "libpath" : ("libF2x.so", '-L"{1}"'),
    },
    
    "python" : {
    },
    
    "dotnet" : {
        "include" : ("F2x.Glue.dll", '-r:"{0}"'),
    },
}

def main():
    parser = argparse.ArgumentParser()
    
    lang_group = parser.add_mutually_exclusive_group(required=True)
    lang_group.add_argument("--fortran", "-F", dest="lang", action="store_const", const="fortran", help="Use FORTRAN as traget language.")
    lang_group.add_argument("--python", "-P", dest="lang", action="store_const", const="python", help="Use Python as target language.")
    lang_group.add_argument("--dotnet", "-N", dest="lang", action="store_const", const="dotnet", help="Use Microsoft .Net (C#) as target language.")
    lang_arg = lang_group.add_argument("--lang", dest="lang", help="Select other language as target.")
    
    output_group = parser.add_argument_group("Mode", description="Select what to output.")
    output_group.add_argument("--include-path", "-I", dest="output", action="append_const", const="include", help="Output flags to include path.")
    output_group.add_argument("--compile-flags", "-c", dest="output", action="append_const", const="include", help="Output compile flags.")
    output_group.add_argument("--library-path", "-l", dest="output", action="append_const", const="libpath", help="Output library path.")
    output_group.add_argument("--library", "-L", dest="output", action="append_const", const="lib", help="Output library.")
    output_group.add_argument("--linker-flags", "-x", dest="output", action="append_const", const="libpath,lib", help="Output flags to link.")
    
    args = parser.parse_args()
    if args.lang not in _LANGUAGES:
        raise argparse.ArgumentError(lang_arg, "Unsupported value {0}. (Select one of {1})".format(args.lang, ", ".join(_LANGUAGES.keys())))
    
    output_config = set()
    for output_name in args.output:
        output_config = output_config.union(set(output_name.split(",")))
    
    lang_config = _LANGUAGES[args.lang]
    lang_dir = find_library_path(args.lang)
    options = []
    for output_name in output_config:
        item, output_text = lang_config[output_name]
        options.append(output_text.format(find_library_path(args.lang, item), lang_dir))
    
    print(" ".join(options))

if __name__ == '__main__':
    main()
