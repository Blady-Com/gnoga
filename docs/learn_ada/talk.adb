with Ada.Text_IO;

package body Talk is
   procedure Say (S : in String) is
      use Ada.Text_IO;
   begin
      Put_Line (S);
   end Say;
end Talk;
