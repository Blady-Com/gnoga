with Gnoga;
with Parsers.String_Source;
with Parsers.Generic_Source.Keywords;
with Parsers.Generic_Source.Get_Blank;
with Strings_Edit.Integers;

package body Logo.Engine is

   package Primitives is new Parsers.String_Source.Code.Keywords (Primitive);
   procedure Get_Blank is new Parsers.String_Source.Code.Get_Blank;

   ------------
   -- Decode --
   ------------

   function Decode (Line : String) return Instruction is
      Copy   : aliased String := Line;
      Code   : Parsers.String_Source.Source (Copy'Access);
      Got_It : Boolean;
   begin
      return Instr : Instruction do
         Get_Blank (Code, Got_It);
         Primitives.Get (Code, Instr.Command, Got_It);
         if not Got_It then
            Instr.Command := nop;
         else
            Get_Blank (Code, Got_It);
            declare
               Pointer : Integer := Parsers.String_Source.Get_Pointer (Code);
            begin
               Gnoga.Log (Pointer'Img);
               Strings_Edit.Integers.Get (Line, Pointer, Instr.Arg1);
            end;
         end if;
      end return;
   end Decode;

   ------------
   -- Action --
   ------------

   procedure Action
     (Instr : Instruction;
      View  : Logo.View.Default_View_Access)
   is
      ST : Duration;
   begin
      Gnoga.Log (Instr.Command'Img & Instr.Arg1'Img);
      case Instr.Command is
         when av =>
            View.Turtle.Move_Rel (Instr.Arg1, 10.0, 10.0, ST);
         when re =>
            View.Turtle.Move_Rel (-Instr.Arg1, -10.0, -10.0, ST);
         when td =>
            View.Turtle.Rotate_Rel (Instr.Arg1, 10.0, 10.0, ST);
         when tg =>
            View.Turtle.Rotate_Rel (-Instr.Arg1, -10.0, -10.0, ST);
         when others =>
            View.Console.Put_Line ("Error");
      end case;
   end Action;

end Logo.Engine;
