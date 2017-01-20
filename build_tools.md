---
layout: default
title: Compiler Build Environment`
---

<h1><center>{{title}}</center></h1>

The following tools are needed to build the compiler:

- Delphi 2007 (or later) or the latest version of Lazarus.
- Resource Hacker (free at http://www.angusj.com/resourcehacker/) if using Delphi.  Resource Hacker must be installed on the PATH.

The primary development environment for the compiler is Delphi 2007 under Windows.  Lazarus could also be used but it is a bit more cumbersome, however it is important because it may eventually provide a path for producing compilers to run on OSX and Linux.

## Other Delphi Versions

Later versions of Delphi may also be used for development however no feature introduced after Delphi 2007 (e.g. generics) should be used that introduces incompatibility with either Delphi 2007 or the latest version of Lazarus.  The continuous integration system ensures that the sources can be compiled with Delphi 2007 as well as the most recent version of Lazarus.  

The reason for developing with Delphi 2007 is the exponentially soaring cost of Embarcadero Delphi licenses.  Many people still have older Delphi licenses that could be used for compiler development and we want to minimize barriers for potential assistance from the open source community.  Presently Embarcadero is offering their Delphi Starter Edition for free on their website.  It is perfectly adequate for development of the compiler but it is unknown how long it will continue to be free.  We note that Turbo Delphi (a feature reduced version of Delphi 2006) was also free but it was withdrawn after a few years.  

The compiler makes use of features introduced in Delphi 2006 so earlier versions of Delphi cannot be used.  In theory Delphi 2006/Turbo Delphi should be usable (indeed much of the early work on the compiler was done with Turbo Delphi), however there was an elusive bug that intermittently caused problems and attempts to work around it were unsuccessful.  This bug was not fixed until Delphi 2007 and no patch was ever released for the 2006 versions.

## .dproj and .dpr modifications

Later versions of Delphi will make modifications to the .dproj files that are incompatible with Delphi 2007.  These modified .dproj files must not be checked back into GitHub as that will break the build.  Similarly the IDE sometimes modifies the .dpr files and removes $IFDEFs needed for Lazarus compatibility.  Make sure any modified .dpr files have the necessary $IFDEFs (a comment in each .dpr file shows how to fix this).  Normally uploading changes to .dproj and .dpr files to GitHub will not be necessary unless additional source files were added to the project.  We can help at this end by producing new Delphi 2007 compatible .dproj and .dpr files if that is necessary.





