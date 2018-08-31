# Using F2x to Wrap Fortran Code

In contrast to `README.md`, this file reflects the actual status of this project and how to use it for wrapping Fortran code to Python.


## Installation

**F2x** currently depends on Python and `setuptools` in order to install it. If you have these pre-requirements in place, you can go ahead and install **F2x** by cloning the repository and running the setup script:

	$ git clone https://github.com/led02/F2x
	$ cd F2x
	$ python setup.py install

This should install all dependencies and a command line tool to wrap your Fortran code.

**F2x** brings additional helper libraries that are directly or indirectly used by the generated wrapper. These should be built automatically when `setup.py install` is run. However, you can still invoke the build process manually using:

    $ python setup.py build_glue


## What it does

There is a command line tool called `F2x` that handles the wrapping of your Fortran code. It first does some pre-processing on the source (based on regular expressions). This later helps the parser to read in the code as Fortran's grammar is a real beast! It then parses the source into an abstract syntax tree (AST). After applying some small transformations (mainly mapping long tree selectors like `function_subprogram function_stmt name` to
`function name`) the AST is rendered by some templates.

This generates wrapper modules for the specified output languages which in turn can be used by your code. In general, the generated interfaces reflect the original Fortran module as close as possible:

* Derived types and their fields have the same names but field types are suitable for the target language.
* Subroutines and functions keep their names. Arguments are converted to match the types of the target language.

Take the following `example.f90`:

    MODULE test
    
        TYPE :: Foo
           INTEGER :: bar
           CHARACTER(32) :: baz
        END TYPE
    
    CONTAINS
    
        FUNCTION INIT_FOO(bar, baz)
            INTEGER :: bar
            CHARACTER(32) :: baz
            TYPE(Foo), POINTER :: INIT_FOO
            
            ALLOCATE(INIT_FOO)
            INIT_FOO%bar = bar
            INIT_FOO%baz = baz
        END FUNCTION
    
    END

When this is wrapped e.g. to Python, you can easily do the following:

    import test_glue as test
    
    foo = test.INIT_FOO(42, "spam")
    print(foo.bar)
    foo.bar = 23
    print(foo.bar, foo.baz)
    
For language specific details, please refer to the additinal documentation for the supported languages.


## Command line (output of `F2x -h`)

    usage: F2x [-h] [-G GRAMMAR] [-C CONFIG_SUFFIX] [-P] [-F] [-e ENCODING] -t
               TEMPLATE [-l LOGFILE] [-v] [-q]
               [SOURCE [SOURCE ...]]
    
    F2x - A versatile FORTRAN wrapper.
    
    positional arguments:
      SOURCE
    
    optional arguments:
      -h, --help            show this help message and exit
      -l LOGFILE, --logfile LOGFILE
                            Write detailed log to LOGFILE.
      -v, --verbose         Increase verbosity
      -q, --quiet           Decrease verbosity
    
    Parsing FORTRAN input:
      -G GRAMMAR, --grammar GRAMMAR
                            Use specified grammar. Use '@'-prefix for bundled
                            grammars. (Default: @fortran.g)
      -C CONFIG_SUFFIX, --config-suffix CONFIG_SUFFIX
                            Suffix for per-source configuration file. (Default:
                            -wrap)
      -P, --output-pre      Write pre-processed source.
      -F, --configure       Create/update configuration file.
      -e ENCODING, --encoding ENCODING
                            Use the specified encoding for reading/writing source
                            files.
    
    Code generation:
      -t TEMPLATE, --template TEMPLATE
                            Generate wrapping code for each template given. Uset
                            '@'-prefix for bundled templates.


## Basic usage

To wrap a Fortran file, you need to tell `F2x` at least, which templates should be rendered (i.e. which wrappers to generate). Usally, you need some kind of Fortran or C glue code. This is generated using the `bindc/_glue.f90.t` template which is part of **F2x**. This produces a Fortran glue module that will be compiled with a C compatible ABI. A second
template accesses these modules from another language. There are already a `ctypes/_glue.py.t` template that generates a Python wrapper.

So if you want to wrap a file called `foo.f90` to Python using the bundled templates, you should run:

	$ F2x -t @bindc/_glue.f90.t -t @ctypes/_glue.py.t foo.f90

The generated files will be named just like the input file but without the original extension and with the name of the template appended (again, without the `.t`). So `foo.f90` will produce a Fortran glue module called `foo_glue.f90` and a Python module called `foo_glue.py`.

Note that **F2x** currently does not care about compiling your Fortran (wrapper) code to a library. However, there is a second tool called `F2x-lib` that provides helpful features for building/ running the generated wrapper. Alternatively you can add the `c_interface_module.f90`
to your library which is currently recommended. You will find this module in the package `F2x.library.fortran`.


## Wrapper configuration

Only in rare cases, **F2x** is able to generate useful wrappers out of the box. You most likely need to create a wrapper configuration file. To fine tune the performance, a per-source configuration file is evaluated. The file should be named just like the source file itself but with an appended `-wrap`. (This suffix can be changed using the `-C` flag.)

This configuration file is in the well known INI format, where sections are indicated by brackets like:

	[section name]

Key-value-pairs are represented as an assignment:

	key = value

One more importent format is a key followed by multiple value lines all prefixed with a tab:

	key =
		value 1
		value 2
		value 3

There following sections are of particular interest:


### `[parser]` section

This section contains parameters to configure the parsing process.

You can specify the `encoding` used for your source file. This is the same as the `-e` flag and defaults to `utf-8` .

This section contains also a multi-value parameter `ignore` that lists all the line numbers the parser should not attempt to parse. You can get a pre-populated list of lines with parse-errors if you specify the `-F` flag on the command line. Update your wrapper configuration with the output of the run and interate until the parser accepts the input. *[Note that this feature might lead to the program crashing. However, it mostly still produces useful output.]*


### `[export]` section

This section lists all Fortran functions and subroutines that should be exported by the C-like wrapper and thus are usable by other languages. The keys are the names as found in the original Fortran code. The keys are case-insensitive as are Fortran names. The values are case-sensitive as are C names.

To avoid name clashes, it is good practice to add a prefix (e.g. the module name) to the C names. An example could be:

	[export]
	func1 = foo_func1
	proc1 = foo_proc1

Note that you do not need to differentiate between functions and subroutines. You also do not need to specify their arguments.


### `[replace]` section

In this section, the keys are line numbers and the values contain the lines that should be used instead of the original line. These substitutions are only valid for the F2x parser and do not modify the actual code.

If you want to see the data that is actually used by F2x, add the `-P` flag to output the preprocessed source file.

There are still some more settings which are template/language specific and are explained in the respective documentation.


## Using the wrapper

When the wrapper was generated, most languages (like Fortran, C#) require an additional compilation step. To support this, **F2x** provides the `F2x-lib` tool. This tool automatically generates apporpriate flags for usage with the different compilers and different stages.


### Command line (i.e. output of `F2x-lib -h`)

    usage: F2x-lib [-h] (--fortran | --python | --dotnet | --lang LANG)
                   [--include-path] [--compile-flags] [--library-path] [--library]
                   [--linker-flags]
    
    optional arguments:
      -h, --help           show this help message and exit
      --fortran, -F        Use FORTRAN as traget language.
      --python, -P         Use Python as target language.
      --lang LANG          Select other language as target.
    
    Mode:
      Select what to output.
    
      --include-path, -I   Output flags to include path.
      --compile-flags, -c  Output compile flags.
      --library-path, -l   Output library path.
      --library, -L        Output library.
      --linker-flags, -x   Output flags to link.

Assume you have a Fortran module `foo.f90` wrapped using `_glue.f90.t` and `_glue.cs.t`. You can build your Fortran wrapper libary with:

    $ gfortran -fPIC -shared -o libfoo.so foo.f90 foo_glue.f90 $(F2x-lib -FIx)


## Known problems

* Callback functions are not yet supported.
* If you compile your ignore lists, be sure not to include empty blocks. Instead ignore the whole block including start and the respective `END` instruction. As the wrapper does not do *any* sematic analysis, you can ignore almost the whole execution part of your functions and subroutines.
* `-v` and `-q` are cumulative, i.e. you can specify them several times to increase/decrease verbosity.
