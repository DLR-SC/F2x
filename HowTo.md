Using F2x to Wrap Fortran Code
==============================

In contrast to `README.md`, this file reflects the actual status of this project and how to use it for wrapping Fortran
code to Python.


Installation
------------

**F2x** currently depends on a custom fork of `plyplus` that can be found at https://github.com/led02/plyplus. You have
to install it before you can go ahead and install **F2x**. You will also need `setuptools` in order to install it. If
you have these pre-requirements in place, you can go ahead and install **F2x** by cloning the repository and running the
setup script:

	$ git clone https://github.com/led02/F2x
	$ cd F2x
	$ setup.py install

This should install all dependencies and a command line tool to wrap your Fortran code.


What it does
------------

There is a command line tool called `F2x` that handles the wrapping of your Fortran code. It first does some
pre-processing on the source (based on regular expressions). This later helps the parser to read in the code as
Fortran's grammar is a real beast! It then parses the source into an abstract syntax tree (AST). After applying some
small transformations (mainly mapping long tree selectors like `function_subprogram function_stmt name` to
`function name`) the AST is rendered by some templates.


Command line (output of `F2x -h`)
---------------------------------

	usage: main.py [-h] [-G GRAMMAR] [-C CONFIG_SUFFIX] [-P] [-F] [-e ENCODING] -t
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


Basic usage
-----------

To wrap a Fortran file, you need to tell `F2x` at least, which templates should be rendered (i.e. which wrappers to
generate). Usally, you need some kind of Fortran or C glue code. This can be done using the `_bindc.f90.t` template
which is part of **F2x**. This produces a Fortran glue module that will be compiled with a C compatible ABI. A second
templates accesses these modules from another language. There is already a `_ctypes.py.t` template that generates a
Python wrapper using `ctypes`.

So if you want to wrap a file called `foo.f90` to Python using the bundled templates, you should run:

	$ F2x -t @_bindc.f90.t -t @_ctypes.py.t foo.f90

The generated files will be named just like the input file but without the original extension and with the name of
the template appended (again, without the `.t`). So `foo.f90` will produce a Fortran glue module called `foo_bindc.f90`
and a Python module called `foo_ctypes.py`.

Note that **F2x** currently does not care about compiling your Fortran (wrapper) code to a library.


Wrapper configuration
---------------------

Only in rare cases, **F2x** is able to generate useful wrappers out of the box. You most likely need to create a
wrapper configuration file. To fine tune the performance, a per-source configuration file is evaluated. The file should
be named just like the source file itself but with an appended `-wrap`. (This suffix can be changed using the `-C`
flag.)

This configuration file is in the well known INI format, where sections are indicated by brackets like:

	[section name]

Key-value-pairs are represented as an assignment:

	key = value

One more importent format is a key followed by multiple value lines all prefixed with a tab:

	key =
		value 1
		value 2
		value 3

There are mainly two sections of interest:


### `[parser]` section

This section contains parameters to configure the parsing process.

You can specify the `encoding` used by your source file. (This is the same as the `-e` flag.)

This section contains also a multi-value parameter `ignore` that lists all the line numbers the parser should not
attempt to parse. You can get a pre-populated list of lines with parse-errors if you specify the `-F` flag on the
command line. Update your wrapper configuration with the output of the run an interate until the parser accepts
the input.


### `[export]` section

This section lists all Fortran functions and subroutines that should be exported by the C-like wrapper and thus are
usable by other languages. The keys are the names as found in the original Fortran code. The keys are case-insensitive
as are Fortran names. The values are case-sensitive as are C names.

To avoid name clashes, it is good practice to add a prefix (e.g. the module name) to the C names. An example could be:

	[export]
	func1 = foo_func1
	proc1 = foo_proc1

Note that you do not need to differentiate between functions and subroutines. You also do not need to specify their
arguments.


General Hints
-------------

* Callback functions are not yet supported.
* Specifing variables only one at a time (i.e. no multiple variables declarations after the same type line).
* If you compile your ignore lists, be sure not to include empty blocks. Instead ignore the whole block including start
  and the respective `END` instruction. As the wrapper does not do *any* sematic analysis, you can ignore almost the
  whole execution part of your functions and subroutines.
* `-v` and `-q` are cumulative, i.e. you can specify them several times to increase/decrease verbosity.


Required Adaptation
-------------------

Currently, (scalar) arrays are not handled automatically. For every routine / funcion handling with arrays, you need to
manually apply the following changes:

* Only for functions returning arrays: Change the `FUNCTION` to be a `SUBROUTINE`. This also means that you need to add
  another dummy variable that holds the result to the argument list with `INTENT(INOUT)`.
* Adapt dummy array arguments, i.e. remove the `VALUE` option and append the correct dimension to the variable
  definitions.
* Only for functions returning arrays: Make sure to assign the result to the result variable.

### Example

`foo.f90` contains the following function that should be wrapped:

	FUNCTION SCALAR_MULTIPLY(SCALAR, VECTOR)
		REAL(KIND=LF) :: SCALAR_MULTIPLY(3)
		REAL(KIND=LF), INTENT(IN) :: SCALAR
		REAL(KIND=LF), INTENT(IN) :: VECTOR(3)
		
		...
		
		SCALAR_MULTIPLY = ...
	END FUNCTION


`foo_bindc.f90` would contain something like:

	FUNCTION FOO_SCALAR_MULTIPLY(SCALAR, VECTOR) BIND(C, NAME="FOO_SCALAR_MULTIPLY")
		REAL(KIND=LF) :: FOO_SCALAR_MULTIPLY
		REAL(KIND=LF), VALUE, INTENT(IN) :: SCALAR
		REAL(KIND=LF), VALUE, INTENT(IN) :: VECTOR
		
		FOO_SCALAR_MULTIPLY = SCALAR_MULTIPLY(SCALAR, VECTOR)
	END FUNCTION

After the first step, it is converted to a subroutine:

	SUBROUTINE FOO_SCALAR_MULTIPLY(SCALAR, VECTOR, SCALAR_MULTIPLY_VALUE) BIND(C, NAME="FOO_SCALAR_MULTIPLY") ! Subroutine, output dummy arg
		REAL(KIND=LF), VALUE, INTENT(IN) :: SCALAR
		REAL(KIND=LF), VALUE, INTENT(IN) :: VECTOR
		REAL(KIND=LF), INTENT(INOUT) :: SCALAR_MULTIPLY_VALUE(3)           ! Output dummy arg is an INOUT array
		
		FOO_SCALAR_MULTIPLY = SCALAR_MULTIPLY(SCALAR, VECTOR)
	END SUBROUTINE

In the next step, the input arrays are adapted (here, only `VECTOR`):

	SUBROUTINE FOO_SCALAR_MULTIPLY(SCALAR, VECTOR, SCALAR_MULTIPLY_VALUE) BIND(C, NAME="FOO_SCALAR_MULTIPLY")
		REAL(KIND=LF), VALUE, INTENT(IN) :: SCALAR
		REAL(KIND=LF), INTENT(IN) :: VECTOR(3)                             ! Dimension for input arg, no VALUE
		REAL(KIND=LF), INTENT(INOUT) :: SCALAR_MULTIPLY_VALUE(3)
		
		FOO_SCALAR_MULTIPLY = SCALAR_MULTIPLY(SCALAR, VECTOR)
	END SUBROUTINE

Finally, we need to return the result using the output arg:

	SUBROUTINE FOO_SCALAR_MULTIPLY(SCALAR, VECTOR, SCALAR_MULTIPLY_VALUE) BIND(C, NAME="FOO_SCALAR_MULTIPLY")
		REAL(KIND=LF), VALUE, INTENT(IN) :: SCALAR
		REAL(KIND=LF), INTENT(IN) :: VECTOR(3)
		REAL(KIND=LF), INTENT(INOUT) :: SCALAR_MULTIPLY_VALUE(3)
		
		SCALAR_MULTIPLY_VALUE(:) = SCALAR_MULTIPLY(SCALAR, VECTOR)         ! Assign result to output dummy arg
	END SUBROUTINE

Now, Fortran should be able to compile the interface code.
