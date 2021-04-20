-------------------------------------------------------------------------------
-- NAME (body)                  : logo-engine.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : Logo engine unit.
-- NOTES                        : Ada 2012, GNOGA 1.4 beta
--
-- COPYRIGHT                    : (c) Pascal Pignard 2018
-- LICENCE                      : CeCILL V2 (http://www.cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Gnoga;
with Parsers.String_Source;
with Strings_Edit.Integers;

with logo_messages.logo_Strings;

with UXStrings.Conversions;

package body Logo.Engine is

   ------------
   -- Decode --
   ------------

   function Decode
     (Line       : String;
      Primitives : Logo.Parser.Primitive_Tables.Dictionary)
      return Instruction
   is
      Copy   : aliased Standard.String := To_UTF_8 (Line);
      Code   : Parsers.String_Source.Source (Copy'Access);
      Got_It : Boolean;
   begin
      return Instr : Instruction do
         Logo.Parser.Get_Blank (Code, Got_It);
         Logo.Parser.Get_Token (Code, Primitives, Instr.Command, Got_It);
         if not Got_It then
            Instr.Command := Logo.Parser.nop;
         else
            Logo.Parser.Get_Blank (Code, Got_It);
            if Got_It then
               declare
                  Pointer : Integer := Parsers.String_Source.Get_Pointer (Code);
               begin
                  Strings_Edit.Integers.Get (Copy, Pointer, Instr.Arg1);
               end;
            end if;
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
      use all type Parser.Primitive;
      function Image is new UXStrings.Conversions.Scalar_Image (Logo.Parser.Primitive);
   begin
      Gnoga.Log (Image (Instr.Command) & Gnoga.Image (Instr.Arg1, Prefix => ' '));
      case Instr.Command is
         when forward =>
            View.Turtle.Move_Rel (Instr.Arg1, 10.0, 10.0, ST);
         when back =>
            View.Turtle.Move_Rel (-Instr.Arg1, -10.0, -10.0, ST);
         when right =>
            View.Turtle.Rotate_Rel (Instr.Arg1, 10.0, 10.0, ST);
         when left =>
            View.Turtle.Rotate_Rel (-Instr.Arg1, -10.0, -10.0, ST);
         when help =>
            View.Console.Put_Line (logo_messages.logo_Strings.Format_CPYR (View.Locale));
            View.Console.Put_Line (logo_messages.logo_Strings.Format_HETX (View.Locale));
         when others =>
            View.Console.Put_Line (logo_messages.logo_Strings.Format_SERR (View.Locale));
      end case;
   end Action;

end Logo.Engine;
