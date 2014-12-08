with Ada.Text_IO;

package body Talk2 is
   Say_Count : Natural := 0;

   procedure Say (S : in String) is
      use Ada.Text_IO;
   begin
      Say_Count  := Say_Count + 1;
      Talk_Count := Talk_Count + 1;

      Put_Line (Natural'Image (Talk_Count) & " : " & S);
      Put_Line ("Say was called" & Natural'Image (Say_Count) & " times.");
      --  In Ada '&' is used to concatinate strings
   end Say;
end Talk2;
