This  directory  contains  packages of the simple components project.
See the documentation:

   http://www.dmitry-kazakov.de/ada/components.htm
   
The  package  implementations   provided   here   use   GCC  intrinsic
operations  to atomically load and store 64-bit objects,  such  as the
blackboard references and Stream_Element_Offset. This is the case, for
example, for GNAT Pro 7.3.1

The following table lists cases when particular a implementation shall
be selected:
                    +-------------------------------------------------
                    | 64-bit pragma Atomic supported
                    |     +-------------------------------------------
                    |     | Stream_Element_Offset bits
--------------------|     |     +-------------------------------------
../ada              | yes | any |
../gcc              | no  | 32  |
../gcc-long-offsets | no  | 64  |