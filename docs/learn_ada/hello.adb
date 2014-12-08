with Ada.Text_IO;
--  "with" loads the package (in this case a standard library) called
--  Ada.Text_IO and its parent package Ada. Note that there is no way
-- to "with" in all sub-packages as in some languages.

procedure Hello is
begin
   Ada.Text_IO.Put_Line ("Hello World");
   --  Note that the procedure Put_Line is called from the Text_IO package
   --  and to do so required fully qualifying where the procedure was located.
end Hello;
