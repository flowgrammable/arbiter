# Arbiter

This repository contains the Liz programming language source code and the Arbiter example used in the paper (TBD). This document contains a description of the project, a description of the Liz programming language, and a guide to get Liz up and running.

## File structure

  * `README.md` - Project description. You're reading it!

  * `liz_arbiter_snapshot.tar.gz` - A snapshot of Liz with syntax and language feature support frozen on the (TDB) submission.

  * `compiled_apps/` - Pre-complied Arbiter applications used in testing. The Arbiter project will continue to undergo bug fixes and optimizations. We provide these pre-compiled applications to recreate the experiments for your convenience. The `safe_bridge` application exists in the Liz snapshot in the folder Arbiter.

## Liz Snapshot

The Liz snapshot `liz_arbiter_snapshot.tar.gz` contains the source code to Liz and a folder named arbiter_examples. The Liz source code is a snapshot of the language as described in the paper (TDB). While the language syntax and feature set will remain frozen, this snapshot will be updated when bugs are discovered and as more compiler intrinsics are transformed into libraries.


## Update policy

While the language syntax and feature set will remain frozen, this snapshot will be updated when bugs are discovered and as more compiler intrinsics are transformed into libraries. New tests and features (excluding changes in syntax and semantics), will be added as work continues on Arbiter.

Please submit comments, complaints, and bugs to mlopez -at- cse.tamu.edu. I'll push bug fixes into the snapshot as soon as I can. Cheers.

--------------------------------------------------------------------------------



# The Liz programming language

This is Liz, an interpreter for a C++ dialect inspired by the book
"Elements of Programming" authored by Alexander Stepanov and Paul McJones.
Liz is designed to provide direct support for axiomatic programming
and structured generic programming.

Liz is an open source research compiler for studying Axiomatic Programming.  It
is being developed at Texas A&M University by the research group of
Dr. Gabriel Dos Reis. The Liz source code may be viewed and bugs may be submitted via the [trac system](http://liz.axiomatics.org/trac).

--------------------------------------------------------------------------------



# Installing Liz

The snapshot of Liz for this project originates from the *concepts* branch of Liz. If you submit a bug report for this project via trac, remember to select the *michael-lopez* branch. This branch has the following dependencies.

  * [GCC G++](https://gcc.gnu.org/) version 4.7 or higher - This snapshot requires a C++ compiler that supports the C++11 standard. Newer versions of Liz require the C++14 standard.

  * [The GNU Multiple Precision Arithmetic Library](https://gmplib.org/) - A C library for performing multiple precision arithmetic.

  * [LPSolve](http://sourceforge.net/projects/lpsolve/) - A mixed integer linear programming solver written in C.

  * [C++ Rest SDK](https://casablanca.codeplex.com/) - A C++ library for client-server communication.

While experimental support is available to Windows and Mac users, the test suite has only been tested on Ubuntu 14.04 environments.

## Installing GMP

It is likely that your operating system's pre-compiled GMP library suffices. If not, obtain the newest GMP library source code and build it. The following commands should build GMP correctly. First enter the source code directory. Then enter the following commands.

    mkdir build
    cd build
    ../configure
    make
    make check
    make install

GMP may be removed later with `make uninstall` from the `build` directory.

## Installing the C++ Rest SDK

Instructions for installing the C++ Rest SDK can be found [here](https://casablanca.codeplex.com/wikipage?title=Setup%20and%20Build%20on%20Linux&referringTitle=Documentation). Please follow the instructions precisely. We have encountered several strange bugs in cases where the precise versions of dependencies were not respected.

## Installing LPsolve

The LPSolve libraries are hosted at SourceForge [here](http://sourceforge.net/projects/lpsolve/files/lpsolve/). Download the most recent development version. When this README was written, we used the file ```lp_solve_5.5.2.0_dev_ux64.tar.gz```. Assuming the link still works, you command line junkies can ```wget``` it.

    wget 'http://sourceforge.net/projects/lpsolve/files/lpsolve/5.5.2.0/lp_solve_5.5.2.0_dev_ux64.tar.gz'

Choose a destination for your LPSolve libs and headers. I like the ```/home/$USER/opt/lpsolve``` directory because ```opt``` is so rad. You can choose your own directory as it will be supplied to Liz explicitly later on. Just replace my folder with your in the following commands.

    mkdir /home/$USER/opt/lpsolve/lib
    mkdir /home/$USER/opt/lpsolve/include/lpsolve
    tar -xvf lp_solve_5.5.2.0_dev_ux64.tar.gz
    mv lib* /home/$USER/opt/lpsolve/lib
    mv *.h /home/$USER/opt/lpsolve/include/lpsolve

This completes the installation of lpsolve.

## Building Liz

Untar the Liz snapshot and enter the untarred directory. Enter the following commands.

    mkdir build
    cd build
    ../configure --with-lpsolve=/usr/lib
    make
    make install

Then run Liz!

    liz

That's it.

## Compiling the Arbiter example

The Liz source directory contains an Arbiter code example. This contains an Ethernet Bridge application. After installation, it can be compiled via:

    liz --compile main.liz

This command will attempt
