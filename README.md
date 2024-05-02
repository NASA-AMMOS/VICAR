# VICAR
[VICAR, which stands for Video Image Communication And Retrieval,](https://www.hou.usra.edu/meetings/planetdata2015/pdf/7059.pdf) is a general purpose image processing software system that has been developed since 1966 to digitally process multi-dimensional imaging data.

The VICAR core contains a large number of multimission application programs and utilities.  The general VICAR web page is [https://www-mipl.jpl.nasa.gov/vicar.html](https://www-mipl.jpl.nasa.gov/vicar.html).

Also included in the VICAR Open Source delivery are the following major subsystems:

## VISOR
VISOR, VICAR In-Situ Operations for Robotics, formerly known as the “Mars Suite”, is a set of VICAR programs used for operational and PDS
archive image processing for all recent Mars surface missions at NASA Jet Propulsion Laboratory (JPL). VISOR is included as part of VICAR starting with Release 5.  The VISOR release announcement is here: [6th Planetary Data Workshop(2023), abstract #7076](https://www.hou.usra.edu/meetings/planetdata2023/pdf/7076.pdf).

## AFIDS-POMM
[AFIDS-POMM, or Automatic Fusion of Image Data System - Planetary Orbital Mosaicking and Mapping,](https://www.hou.usra.edu/meetings/lpsc2023/pdf/1261.pdf) is a set of workstation tools supporting the automation of planetary orbital mosaicking and mapping requirements. In a future VICAR open source release, the AFIDS-POMM source code is intended to be merged and maintained as part of the VICAR open source repo.  Currently, only a few of the applications are available.

In the meantime, you can download the AFIDS-POMM docker releases from: https://github.com/NASA-AMMOS/AFIDS-POMM

## Labelocity
Labelocity is a toolset for generating PDS4 labels.  It is included as part of the VICAR Open Source Release but is also available separately at [https://github.com/NASA-AMMOS/labelocity](https://github.com/NASA-AMMOS/labelocity), where you can find documentation for it.  Labelocity is also discussed in [6th Planetary Data Workshop(2023), abstract #7071](https://www.hou.usra.edu/meetings/planetdata2023/pdf/7071.pdf).

# What's New in Release 5

This release includes:

- Pre-built binaries for 64-bit Linux (Red Hat 8.7) and Mac OS X 64-bit. 
- All of VISOR (in $MARSLIB), 109 application programs
- 88 new application programs: 

| executables |  |  |  |
| - | - | - | - |
| accck | asc2tcl | astroref | bowtie |
| calfit | compject | comptab2 | comptab |
| concompi | concomp | cutquad | demfix |
| deriv | detrend | detstat | difnear |
| ephref | f2comp | featherw | fft2005 |
| fitg | fitsout | gengrid2 | gengrid |
| genpc | getpoint | getzval | grid3pt |
| gridavg | gridck | gridcomp | gt2tcl |
| gtcomp | gtcopy | gtcrop | gtdel |
| gtgenup | gtoffset | gtp | gtpwarp |
| gtrot | gtsize | gtstat | gtwarp |
| gtwarpxd | gtwrpsub | ibis2asc | ibis2img |
| ibis2rpc | ibisclst | ibislsq2 | ibislsql |
| ibisnlsq | ibisx | icat | idgen |
| ifthen | ilist | imcorner | imgsum |
| immerge | imsplit | medfill | moore |
| mos_l2_dem | mvecall | nacma | norangle |
| obj2gltf | obj2plane | picmtch4 | plab2tcl |
| polygeov | rastovec | rowop2 | rpc2grid |
| rpc2ibis | rpcscal | rpcwarpm | rpcwarp |
| sdsems | surffit | tcl2file | tcl2tcl |
| testtrnscol2 | trnscol2 | vextract2 | watermask |

- 20 anomalies, 11 new capabilities, 222 bug fixes/improvements. For release notes, [click here](vos/docsource/vicar/VOS5-Release-Notes.pdf).

For a full list of programs being released [click here](vos/docsource/vicar/VICAR_OS_contents_v5.0.pdf).

# Obtaining VICAR

VICAR can be obtained in two ways: pre-built binaries via tarball download (which includes source), or source code via github.com.

In addition to the VICAR source code an externals package containing 3rd party software is required.  The externals come in the form of a tarball that contains all the platforms, as well as separate tarballs for each platform.  Externals are 3rd party packages that are required to run VICAR. See section 2 of the [Building VICAR document](vos/docsource/vicar/VICAR_build_5.0.pdf) for more information. You need only the one that
applies to your machine type.

For VISOR, you will also need the calibration directory for the mission(s) you will be working with.  Sample data for use with the VISOR User Guide is also available.

### Source Code

* GitHub: [https://github.com/NASA-AMMOS/VICAR](https://github.com/NASA-AMMOS/VICAR)
* Main VICAR source code:  [Click to download](https://github.com/NASA-AMMOS/VICAR/tarball/master)  

**NOTE:** Due to the size of some included files, GitHub users will need to [install Git LFS](https://docs.github.com/en/repositories/working-with-files/managing-large-files/installing-git-large-file-storage) before [cloning](https://docs.github.com/en/get-started/using-git/getting-changes-from-a-remote-repository#cloning-a-repository) and/or [pulling](https://docs.github.com/en/get-started/using-git/getting-changes-from-a-remote-repository#pulling-changes-from-a-remote-repository) the repository.

### Pre-built Binaries

* Pre-built VICAR binaries are available for Linux (64-bit) and MacOS (x86, 64 bit) [here](https://github.com/NASA-AMMOS/VICAR/releases).

### Externals

* Linux 64-bit externals:  [Click to download](https://github.com/NASA-AMMOS/VICAR/releases/download/5.0/vicar_open_ext_x86-64-linx_5.0.tar.gz)
* Mac OS X externals:  [Click to download](https://github.com/NASA-AMMOS/VICAR/releases/download/5.0/vicar_open_ext_mac64-osx_5.0.tar.gz)

### VISOR Calibration files

* [Mars 2020, Perseverence and Ingenuity - Part 1](https://github.com/NASA-AMMOS/VICAR/releases/download/5.0/visor_calibration_20230608_m20.tar.gzaa)
* [Mars 2020, Perseverence and Ingenuity - Part 2](https://github.com/NASA-AMMOS/VICAR/releases/download/5.0/visor_calibration_20230608_m20.tar.gzab)
* [Mars Science Laboratory (MSL), Curiosity](https://github.com/NASA-AMMOS/VICAR/releases/download/5.0/visor_calibration_20230608_msl.tar.gz)
* [InSight lander](https://github.com/NASA-AMMOS/VICAR/releases/download/5.0/visor_calibration_20230608_nsyt.tar.gz)
* [Mars Exploration Rover (MER), Spirit and Opportunity](https://github.com/NASA-AMMOS/VICAR/releases/download/5.0/visor_calibration_20230608_mer.tar.gz)
* [Phoenix lander](https://github.com/NASA-AMMOS/VICAR/releases/download/5.0/visor_calibration_20230608_phx.tar.gz)
* [MSAM (Mastcam Stereo Analysis and Mosaics), a PDART task for MSL-Mastcam data](https://github.com/NASA-AMMOS/VICAR/releases/download/5.0/visor_calibration_20230608_msam.tar.gz)

### VISOR Sample Data

* [VISOR Sample Data for User Guide](https://github.com/NASA-AMMOS/VICAR/releases/download/5.0/visor_sample_data_20230623.tar.gz)

# Getting Started and Documentation

The [VICAR/VISOR Wiki](https://github.com/NASA-AMMOS/VICAR/wiki/VICAR-VISOR-Notes) is the most up-to-date resource for late-breaking changes and other application notes. [Up-to-date Labelocity guidance](https://github.com/NASA-AMMOS/VICAR/wiki/Labelocity-Notes) is also provided.

For any use of VICAR (including VISOR and AFIDS), we recommend the Quick-Start Guide:

* [VICAR Quick-Start Guide](vos/docsource/vicar/VICAR_guide_5.0.pdf).

The Installation Guide is useful for prebuilt binaries and critical if you want to attempt to build VICAR:

* [VICAR installation guide](vos/docsource/vicar/VICAR_build_5.0.pdf).

If you want to use the VISOR Mars programs, first review the VICAR Quick-Start Guide and then the VISOR User Guide:

* [VISOR User Guide](vos/docsource/vicar/VISORUserGuide_v1_0.pdf)

Other less commonly referenced documents:
* [VICAR File Format](https://www-mipl.jpl.nasa.gov/external/VICAR_file_fmt.pdf)
* [VICAR Porting Guide](https://www-mipl.jpl.nasa.gov/portguide/portguide.html)
* [Building and Delivering VICAR Applications](https://www-mipl.jpl.nasa.gov/buildapps/)
* [VRDI User's Reference Guide (mostly obsolete)](https://www-mipl.jpl.nasa.gov/vrdi/vrdi.html)

# VICAR Discussion Forums

We've set up online groups to help users collaborate and interact on VICAR-related subjects, where you can find notifications of new releases, view bug reports and participate in general discussions:
* The [VICAR Open Source Google group](https://groups.google.com/forum/#!forum/vicar-open-source/) can be joined [here](https://groups.google.com/forum/#!forum/vicar-open-source/join).
* A **#vicar** channel is available in the [OpenPlanetary Slack workspace](http://openplanetary.slack.com/) ([join here](https://www.openplanetary.org/join))

Questions: [vicar_help@jpl.nasa.gov](mailto:vicar_help@jpl.nasa.gov)

# Modifying VICAR/VISOR

One of the prime purposes of Open Source is to solicit contributions from the community, and we welcome such contributions.  However, at the current time, this git repo is read-only.  At some point in the future, we hope to make this easier, but for now, send any changes to [vicar_help@jpl.nasa.gov](mailto:vicar_help@jpl.nasa.gov) and we can discuss them.  Since this software is used as part of surface operations for these missions, we must carefully vet any changes, which is the primary reason for this.  Getting more contributions from the community will help make the case for improved governance here.
