# VICAR
VICAR, which stands for Video Image Communication And Retrieval, is a general purpose image processing software system that has been developed since 1966 to digitally process multi-dimensional imaging data.

We are pleased to announce that the VICAR Open Source Core version 4.0 is now available.
Please visit the VICAR Open Source release homepage:

[http://www-mipl.jpl.nasa.gov/vicar_open.html](http://www-mipl.jpl.nasa.gov/vicar_open.html).

This release includes:

- Pre-built binaries for 32- and 64-bit Linux(Red Hat 7.3) and Mac OS X 64-bit. 
- A Docker Centos7 image that runs the 64-bit Linux binaries (Java 1.8).
- A Docker Centos7 image preloaded with VOS 64-bit Linux(Red Hat 7.3) using Java 1.8.
- 6 new application programs: 
  avgpix 
  comprs 
  crosshair 
  medval
  nimsr2iof 
  sdsems
- 19 anomalies, 72 bug fixes/improvements. For a full list, [click here](vos/docsource/vicar/VOS4-Release-Notes.pdf).

For a full list of programs being released [click here](vos/docsource/vicar/VICAR_OS_contents_v4.0.pdf).

## AFIDS-POMM
AFIDS-POMM is a set of workstation tools supporting the automation of planetary orbital mosaicking and mapping requirements. In a future VICAR open source release, the AFIDS-POMM source code is intended to be merged and maintained as part of the VICAR open source repo.

In the meantime, you can download the AFIDS-POMM docker releases from: https://github.com/NASA-AMMOS/AFIDS-POMM

## VICAR Discussion Forum

We have set up a [VICAR Open Source Google group](https://groups.google.com/forum/#!forum/vicar-open-source/), where you can find notifications of new releases, bug reports, and general discussion. You can join the group [here](https://groups.google.com/forum/#!forum/vicar-open-source/join). 

## Obtaining VICAR

The VICAR source code can be obtained via github.com using the links below. In addition to the VICAR source code an externals package containing 3rd party software is required. 

Note that for externals you'll find a tarball that contains all the platforms, as
well as separate ones for each platform. Externals are 3rd party packages that are required to run VICAR. See section 2 of the [Building VICAR document](vos/docsource/vicar/VICAR_build_4.0.pdf) for more information. You need only the one that
applies to your machine type.

Pre-built VICAR binaries are available at https://github.com/NASA-AMMOS/VICAR/releases.

#### Tarballs

* Main VICAR source code:  [Click to download](https://github.com/NASA-AMMOS/VICAR/tarball/master)
* Linux 32-bit externals:  [Click to download](http://www-mipl.jpl.nasa.gov/vicar_os/v4.0/vicar_open_ext_x86-linux_4.0.tar.gz)
* Linux 64-bit externals:  [Click to download](http://www-mipl.jpl.nasa.gov/vicar_os/v4.0/vicar_open_ext_x86-64-linx_4.0.tar.gz)
* Mac OS X externals:  [Click to download](http://www-mipl.jpl.nasa.gov/vicar_os/v4.0/vicar_open_ext_mac64-osx_4.0.tar.gz)
* Solaris externals:  [Click to download](http://www-mipl.jpl.nasa.gov/vicar_os/v4.0/vicar_open_ext_sun-solr_4.0.tar.gz)
* All externals (don't get unless you know you need this): [Click to download](http://www-mipl.jpl.nasa.gov/vicar_os/v4.0/vicar_open_ext_4.0.tar.gz)

## Using VICAR

* The VICAR installation guide can be found [here](vos/docsource/vicar/VICAR_build_4.0.pdf).
* The VICAR quick-start guide can be found [here](vos/docsource/vicar/VICAR_guide_4.0.pdf).


Questions:  vicar_help@jpl.nasa.gov
