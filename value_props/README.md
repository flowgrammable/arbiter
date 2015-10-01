# Value properties in the Liz programming language

This project was created for (TBD - SAC 2016). It contains a snapshot of the Liz programming langauage with an Arbiter SDN library interface and a time complexity of overload resolution.

## Quick start - Building Liz

The snapshot `./liz_snapshot` of Liz for this project originates from the *concepts* branch of Liz. If you submit a bug report for this project via trac, remember to select the *michael-lopez* branch. This branch has the following dependencies.

  * [GCC G++](https://gcc.gnu.org/) version 4.7 or higher - This snapshot requires a C++ compiler that supports the C++11 standard. Newer versions of Liz require the C++14 standard.

  * [The GNU Multiple Precision Arithmetic Library](https://gmplib.org/) - A C library for performing multiple precision arithmetic.

  * [LPSolve](http://sourceforge.net/projects/lpsolve/) - A mixed integer linear programming solver written in C.

While experimental support is available to Windows and Mac users, the test suite has only been tested on Ubuntu 14.04 environments.

### Installing GMP

It is likely that your operating system's pre-compiled GMP library suffices. If not, obtain the newest GMP library source code and build it. The following commands should build GMP correctly. First enter the source code directory. Then enter the following commands.

    mkdir build
    cd build
    ../configure
    make
    make check
    make install

GMP may be removed later with `make uninstall` from the `build` directory.

### Installing LPsolve

The LPSolve libraries are hosted at SourceForge [here](http://sourceforge.net/projects/lpsolve/files/lpsolve/). Download the most recent development version. When this README was written, we used the file ```lp_solve_5.5.2.0_dev_ux64.tar.gz```. Assuming the link still works, you command line junkies can ```wget``` it.

    wget 'http://sourceforge.net/projects/lpsolve/files/lpsolve/5.5.2.0/lp_solve_5.5.2.0_dev_ux64.tar.gz'

Choose a destination for your LPSolve libs and headers. I like the ```/home/$USER/opt/lpsolve``` directory because ```opt``` is so rad. You can choose your own directory as it will be supplied to Liz explicitly later on. Just replace my folder with your in the following commands.

    mkdir /home/$USER/opt/lpsolve/lib
    mkdir /home/$USER/opt/lpsolve/include/lpsolve
    tar -xvf lp_solve_5.5.2.0_dev_ux64.tar.gz
    mv lib* /home/$USER/opt/lpsolve/lib
    mv *.h /home/$USER/opt/lpsolve/include/lpsolve

This completes the installation of lpsolve.

### Building Liz

Untar the Liz snapshot and enter the untarred directory. Enter the following commands.

    mkdir build
    cd build
    ../configure --with-lpsolve=/usr/lib
    make
    make install

Then run Liz!

    liz

That's it. For more information on Liz, see the `README.md` inside of the liz snapshot.



## Time Complexity of Overload Resolution

We provide a worst-case time complexity analysis of the overload resolution algorithm. The time complexity is contained in `./time_complexity_report`. Build it with `make`.