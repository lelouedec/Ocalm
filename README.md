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
    * -h : display help
    * -help : display help
    * --help : display help
    * -v : show `mincamlc` version
    * -t : only perform type checking
    * -p : only parse `mincaml` programs into Syntax Tree
    * -opt : include code optimizations in frontend steps - constant folding, inline expansion, unnecessary definitions elimination
    * -asml : output intermediate `asml` code
    * -s : debug mode, output closure-converted form and asml form
    * -threshold : maximum function size allowed for inlining

## Test
### File structure
* Test scripts are located in `scripts/` directory
* Test cases are located in `tests/` directory
    * Test cases are divided into `syntax/`, `typechecking/`, `asml/`, and `gen-code` subdirectories which indicate which phase of the compiler these tests are dedicate for
    * For *syntax* and *type checking* tests:
        * In each directory representing compiler phase, test cases are further divided into `invalid/` and `valid/`, indicating whether these test `mincaml` programs should be rejected by the compiler or not
    * For frontend steps after type checking, test cases are stored in `asml/` subdirectory. All these tests are valid `mincaml` programs.
    * For backend (assembly code generation): _to be added_

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

#### Virtual code generation tests
```
./scripts/mincaml-test-virtual-codegen.sh
```

The test runner runs through all test cases, compile each of them, generate _asml_ virtual code, then execute _asml_ code using asml tool at `tools/asml`. Execution result is compared with expected result, which is yielded by running the test case using `Ocaml` interpreter. The test runner yields *KO* if any of the aforementioned steps fails. Otherwise, it yields *OK* (test passes).

##### To include optimizations
```
./scripts/mincaml-test-virtual-codegen.sh -opt
```

#### Assembly code generation tests
_to be added_

#### Executing ML file
```
./compile_and_run.sh
```

Taking a mincaml file as parameter, this script first transforms the program into ARM assembly code, then compiles it into a binary using qemu. The executable file is then executed. (Interesting features would be to make sure that the result obtained is the same than with the OCaml interpreter.

#### Other tests to be added
_The mechanism is likely to be totally different from the 2 previous runners._
