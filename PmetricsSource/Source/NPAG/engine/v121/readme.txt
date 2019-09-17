
Both of the below f90 versions of dvode are downloaded from
http://www.radford.edu/~thompson/vodef90web/vodef90source/misc.html
(1) dvodeoriginal.f90 is a straightforward translation of the f77 code to f90
(2) dvode_f90_m is supposed to be threadsafe (for OpenMP)

NO! dvode_f90_m_KEN.f90 is the threadsafe version, "f90_m" is just
  converted to f90. KEN is also downloaded from the same site.

note: The original f77 dvode can also be downloaded from above link

note: vodetot.f contains the modules for vode.f and vodext.f (these are the 
   fortran 77 programs) as implemented in NPAG_v120

note: Sample programs for the three packages
http://www.radford.edu/~thompson/vodef90web/problems/demosnodislin/overview.html

Quick f90 Code Check:

> gfortran example.f dvodeoriginal.f90
should produce a runnable a.out, albeit w/some warnings re: assign statements

note: example.f and example.f90 ar almost identical programs!

> gfortran -c -fopenmp dvode_f90_m.f90
should produce dvode_f90_m.mod and dvode_f90_m.o, the module for linking
into the examples.

> gfortran -fopenmp example1.f90 dvode_f90_m.f90
will produce the a.out and example1.mod

> ./a.out 
will produce example1.dat, the ASCII results of the run; which should compare
to the 

Development Notes::

1) RUSER and IUSER replace RPAR and IPAR in the _m version; but  w/added "stuff"
     -- read the code comments carefully and be sure to put optional values into
        increments 23 and higher !!!

2) dvodeoriginal_f.zip contains example.f and dvodeoriginal.f
