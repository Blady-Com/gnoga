with Ada.Text_IO;

procedure Blocks is
   package Test is
      procedure My_Test;
   end Test;

   package body Test is
      procedure My_Test is
         use Ada.Text_IO;
         --  A use clause removes the need to fully qualify the location
         --  of package members.
      begin
         Put_Line ("Hello from Blocks.My_Test.Test");
      end My_Test;
   end Test;
begin
   declare
      X : Integer := 10;
      --  Declarations in Ada of variables/objects are Name followed by :
      --  followed by the Type. Optionally as here an initial value is set
      --  using the assignment operator :=
   begin
      declare
         X : Integer := 11;
         --  Note that since this X is in an inner block it hides the X
         --  X of the outer block.
      begin
         Ada.Text_IO.Put_Line (Integer'Image (X));
         --  Types in Ada have attributes that can be access using the type
         --  name ' attribute. In this case it has an attribute that can be
         --  used to return a string representation of a numeric type called
         --  Image. Variables and Objects also have some attributes as well.
      end;

      Ada.Text_IO.Put_Line (Integer'Image (X));
   end;

   Test.My_Test;
end Blocks;
