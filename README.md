# OCaml compiler project

## Hierarchy
* ARM/     arm source example and compilation with libmincaml
* asml/    asml examples
* doc/     all the documentation, start with index.hml
* mincaml/ MinCaml examples
* ocaml/   MinCaml parser in OCaml, if you do the project in OCaml
* scripts/ put your test scripts and symbolic links there, and add this
         directory to your path
* tests/   put your tests there
* tools/   asml intepreter (linux binary)

## Requirements
* ocaml >= 4.01
* ocamlbuild
* ocamlyacc
* make

## Compile the compiler
* move the current working directory to `ocaml`
* to compile the compiler:
```
make
```
which will generate a binary file as `mincamlc` in the same directory

## Launch the compiler
* With the current working directory still being at `ocaml`, to compile `mincaml` program, run
```
./mincamlc <mincaml_filenames>
```
* Some command-line options:
    * -o <output_file> : 
    * -h : display help
    * -help : display help
    * --help : display help
    * -v : show `mincamlc` version
    * -t : only perform type checking
    * -p : only parse `mincaml` programs into Syntax Tree
    * -asml : output intermediate `asml` code
    * -d : debug mode, output syntax tree, k-normalized form, closure-converted form, asml form

## Test
### File structure
* Test scripts are located in `scripts/` directory
* Test cases are located in `tests/` directory
    * Test cases are divided into `syntax/`, `typechecking/`, etc (_to be added_) subdirectories which indicate which phase of the compiler these tests are dedicate for
    * For *syntax* and *type checking* tests:
        * In each directory representing compiler phase, test cases are further divided into `invalid/` and `valid/`, indicating whether these test `mincaml` programs should be rejected by the compiler or not
    * For _other_ tests (_which is very likely to be structured differently_):
        * _to be added_

### Launch tests
The following scripts are meant to be run at project root directory.

#### Syntax tests
```
./scripts/mincaml-test-parser.sh
```

Each test runner runs through all test cases, and yields *OK* if the compiler runs the test case successfully, *KO* if the compiler rejects (crashes on) the test case. If an *OK* is yielded for a _invalid_ test case, or *KO* is yielded for a _valid_ test case, the test runner will print out the expected output as well.

At the end of execution, the test runner also prints out the number of failed tests.

#### Type checking tests
```
./scripts/mincaml-test-typechecking.sh
```

Type checking test runner works in the same manner as with syntax test runner

#### Executing ML file
```
./compile_and_run.sh
```

Taking a mincaml file as parameter, this script first transforms the program into ARM assembly code, then compiles it into a binary using qemu. The executable file is then executed. (Interesting features would be to make sure that the result obtained is the same than with the OCaml interpreter.

#### Other tests to be added
_The mechanism is likely to be totally different from the 2 previous runners._
