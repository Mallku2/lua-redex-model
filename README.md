This is the mechanization of the semantics presented in "Decoding Lua: Formal 
Semantics for the Developer and the Semanticist".

1_ Installation of DrRacket

To install Racket's IDE DrRacket, simply download the installer that corresponds 
to your system from https://racket-lang.org/download/. The present version of the 
mechanization has been tested on several versions of DrRacket, up to 6.8.

2_ Structure of the mechanization

The modules' distribution mimics the structure of the semantics presented on
the accompanying paper. Its main components are:
	_ grammar.rkt: grammar of the language, together with the definition
of the evaluation contexts.
	_ executionEnvironment.rkt: where bindings to the services available to every
program are defined. The execution environment itself is defined as an evaluation
context. The user program to be executed must be placed in the evaluation contexts'
hole. A meta-function to ease this tasks is provided (plugIntoExecutionEnvironment).
The way in which such meta-function can be used to run a user program under the
execution environment, can be seen in the file Tests/executionEnvironmentTests.rkt.
It requires some knowledge of PLT Redex, but in short, it is done by calling:

(apply-reduction-relation* 
	full-progs-rel 
		(term (plugIntoExecutionEnvironment user_program)))

It applies full-progs-rel, the relation that describes the semantics of any
program, to the program that results from putting user_program into the execution 
environment.

	_ Desugar directory: the implementation of our desugaring function. 

	_ Meta-functions directory:
		_ delta.rkt: the implementation of the delta interpretation
function.
		_ errorMessagesMetafunctions.rkt: implementation of the meta-function
#errmessage (mentioned in the paper)

		_ grammarMetaFunctions.rkt: a couple of predicates over grammar's symbols
(that ease the definition of several rules from the model) and some minor tasks related with 
the manipulation of phrases from that grammar (they are mostly implementation related).

		_ objStoreMetafunctions.rkt: meta-function for the manipulation of the 
objects' store.

		_ substitution.rkt: our implementation of a substitution function, that 
TODO suites our needs (in particular, we don't need it to be capture-avoiding).
		
		_ tablesMetafunctions.rkt: several meta-functions that manipulate tables.
This is the module where the meta-function addkeys (mentioned in the paper) is defined.
		
		_ valStoreMetafunctions.rkt: meta-function for the manipulation of the 
values' store.	
	
	_ Relations directory: each module corresponds to one relation from the ones
composing our model.

	_ Tests directory: each meta-function and relation has its own test suite. They
are placed in this directory.
 
		_ LuaTests: contains versions of the files from Lua 5.2's test suite
that can be executed in our model. For each .lua file, a .rkt file is provided which
implements a single test that compiles the corresponding .lua file and executes it
in our model.

3_ Tests

The module Tests/runAllTests.rkt, when executed, calls every test suite  
specifically for the mechanization (that is, it doesn't call the Lua 5.2's compliance 
test suite). You should be able to open the file within DrRacket, execute it and
see how all the tests passed. This test suite exploits the modular
structure of the semantics, by testing each relation and meta-function in isolation,
while also doing some "integration tests" (running some complete programs and testing
snippets of code plugged into the execution environment). 
	To run the conformance tests, against a particular module M.lua from
Lua 5.2's test suite, go to the directory Tests/LuaTests and open the file M.rkt.
This module will provide a simple test suite, named lua-M-test-suite. By executing
it, like a regular procedure (that is, (lua-M-test-suite)), the single test that
it contains will compile all the content of the file M.lua, plug the result
into the execution environment, execute the resulting program and test
whether the execution was successful or not. However, we don’t recommend to
test all the files in one shot. In the present version we don’t model garbage
collection, and executing a very large program will result in an increasing amount 
of garbage that slows down the performance. It’s better to execute the test by phases. 
To do that, look at the content of the file M.lua, choose some snippet of code that you 
want to test, and comment the remaining portions of the file. Proceeding in this way, 
you should be able to test every line of code, as indicated in the file M.lua (not 
every line can be compiled and executed, thought; comments with respect to that are 
included in each file).

4_ Now, where to go?

If you want to test by your own some Lua code, and you are not familiar with PLT 
Redex's syntax, nor with Lisp dialects' syntax, reading the file grammar.rkt won't be 
very useful. But don't worry. We can still make use of the mechanized Lua's compiler,
to execute programs using the familiar Lua's syntax: open the file 
"executionEnvironment.rkt" with DrRacket, execute it and, in the interaction window, type:

(traces
    full-progs-rel
    (term
     (plugIntoExecutionEnvironment
      (((~ENV \[ "load" \]) ("your_lua_code")) ()))))

Where "your_lua_code" should be replaced with a string representing the Lua program
that you want to execute. ((~ENV \[ "load" \]) ("your_lua_code")) is just the
parenthesized version of Lua's grammar, with some minor differences like ~ENV instead
of _ENV (because underscores have special meaning within PLT Redex syntax), a way of 
using global variables without Lua's syntax sugar and escaped brackets
(same reason as with _). The program that we are trying to execute is just:

load("your_lua_code")()

So hit enter and let the marvelous syntactic machine of PLT Redex to do the rest:
it will begin by populating the execution environment with all the services that
we are actually mechanized, load included, and the it will proceed by executing
the previous code. 
	A point worth to mention is that when we are executing a program
in our mechanization, we are also checking at each step a pair of properties of
the model: namely, that every step results in constructions from the corresponding
syntactic category and it also test against every semantic rule defined in the
relation devoted for complete programs. We want a deterministic semantics, so
it should occur that at each step just one rule can by applied. This, of course,
is a cpu and memory-bounded process.
	The result of the previous program will appears somewhat cryptic at the beginning:
it will consist of a syntactic representation of what the program had left in
the value and object stores, together with the skip statement (;), if there were no
problems with the execution. 
	Remember that a Lua program works by making successive changes to a state.
They are truly state modifiers. That's why we obtain as a result just that: the final
state of the memory. If you want to actually observe the result of some computation,
you should save it in a variable and look at the store for the content of the variable
(we do have an emulation of the print statement, but its output is consumed internally
by the "traces" function).
