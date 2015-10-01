# Introduction

This is Liz, an interpreter for a C++ dialect inspired by the book
"Elements of Programming" authored by Alexander Stepanov and Paul McJones.
Liz is designed to provide direct support for axiomatic programming
and structured generic programming.

Liz is a research compiler for studying Axiomatic Programming.  It
is being developed at Texas A&M University by the research group of
Dr. Gabriel Dos Reis.  

The source code is in the src/ subdirectory, with the following
overview:

  * src/Token.H and src/Token.C define the lexical analysizer.

  * src/Ast.H and src/Ast.C define the abstract syntax tree
    data structures used by the parser.

  * src/Parser.H and src/Parser.C define the parser for the language
    accepted by Liz.  It is recursive descent, exploiting parser
    combinators to reduce clutter.

  * src/Elaborator.H defines the bulk of datatypes needed for
    type checking and translating Liz programs into an internal
    representation suitable for interpretation.

  * src/Type.C contains implementions of Liz types representations.

  * src/expressions.C contains implementations of the intermediate
    representation.

  * src/elaboration.C contains implementations of the type checker
    and intermediate code generator.

  * src/Evaluator.H and src/Evaluator.C define the Liz evaluator.

  * src/intrinsics.C implement Liz builtin operations.

  * src/utility.H and src/utility.C provides some utility functions.



## The Concepts branch

This branch, named the Concepts branch, is used to investigate axiomatic programming for network programming. This compiler extension is called Arbiter and introduces some new dependencies. It is maintained by Michael Lopez, a student of Dr. Gabriel Dos Reis as of 2015.

Any bugs may be reported via the [trac system](http://liz.axiomatics.org/trac). Just remember to select the *michael-lopez* version when filing your bug report.  I would love to here your feedback and comments!




--------------------------------------------------------------------------------



# Building the concept's branch of Liz

In this section we build the Liz programming language. It has the following dependencies:

  * [The GNU Multiple Precision Arithmetic Library](https://gmplib.org/) - A C library for performing multiple precision arithmetic.

  * [LPSolve](http://sourceforge.net/projects/lpsolve/) - A mixed integer linear programming solver written in C.

  * [C++ Rest SDK](https://casablanca.codeplex.com/) - A C++ library for client-server communication.

While experimental support is available to Windows and Mac users, the test suite has only been tested on Ubuntu 14.04 environments.



## Building Liz

Liz is a system for axiomatic programming (see [this paper](http://www.axiomatics.org/~gdr/liz/cicm-2012.pdf) for more). Liz is an open source project whose source may be obtained from [here](http://liz.axiomatics.org/trac). Arbiter is a Liz compiler extension and library suite that allows us to write Software Defined Network applications in Liz. It is maintained under the `concepts` branch. Before installing Liz, we must first install `Liz`'s dependencies.



### Installing GMP

Your distribution's repository version of GMP should suffice. If not, acquire the GMP source code, enter the source code directory and enter these commands:

    mkdir build
    cd build
    ../configure
    make
    make check      // Ensure that there were no build errors
    make install

GMP can be removed later with ```make uninstall``` from within the ```build``` directory.



### Installing the C++ Rest SDK

Instructions for installing the C++ Rest SDK can be found [here](https://casablanca.codeplex.com/wikipage?title=Setup%20and%20Build%20on%20Linux&referringTitle=Documentation).



### Installing LPsolve

The LPSolve libraries are hosted at SourceForge [here](http://sourceforge.net/projects/lpsolve/files/lpsolve/). Download the most recent development version. When this README was written, we used the file ```lp_solve_5.5.2.0_dev_ux64.tar.gz```. Assuming the link still works, you command line junkies can ```wget``` it.

    wget 'http://sourceforge.net/projects/lpsolve/files/lpsolve/5.5.2.0/lp_solve_5.5.2.0_dev_ux64.tar.gz'

Choose a destination for your LPSolve libs and headers. I like the ```/home/$USER/opt/lpsolve``` directory because ```opt``` is so rad. You can choose your own directory as it will be supplied to Liz explicitly later on. Just replace my folder with your in the follwing commands.

    mkdir /home/$USER/opt/lpsolve/lib
    mkdir /home/$USER/opt/lpsolve/include/lpsolve
    tar -xvf lp_solve_5.5.2.0_dev_ux64.tar.gz
    mv lib* /home/$USER/opt/lpsolve/lib
    mv *.h /home/$USER/opt/lpsolve/include/lpsolve

This completes the installation of lpsolve.



### Building Liz

If you're reading this, you probably already have the Liz source code. If not, enter the directory in wish you would like the repository to reside and download the Liz source code.

    svn co svn://axiomatics/liz liz

Navigate to the concepts branch and make a ```build``` directory.

    cd liz/branches/concepts
    mkdir build
    cd build

Configure the Liz build by supplying it with the location of your LPSolve library. On my machine, the libraries reside in ```~/opt/lib/lpsolve```. Then make Liz.

    ../configure --with-lpsolve=/usr/lib
    make
    make install

Then run Liz!

    liz

That's it.

--------------------------------------------------------------------------------



# Building the Arbiter test suite

The Arbiter test suite is the benchmark used in the paper [TBD](???). To build and run it, we will need the following:

* [Vagrant](https://www.vagrantup.com/) - A tool for automating the configuration of development environments.

* [Virtual Box](https://www.virtualbox.org/) - A virtualization tool used by Vagrant.



## Setting up Vagrant

## Running the test suite