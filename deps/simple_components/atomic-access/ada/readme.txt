This  directory  contains  packages of the simple components project.
See the documentation:

   http://www.dmitry-kazakov.de/ada/components.htm
   
The package implementations provided  here use Ada's pragma Atomic for
all cases.

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