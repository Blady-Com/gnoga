with UXStrings;         use UXStrings;
with UXStrings.Text_IO; use UXStrings.Text_IO;

procedure Test_UXStrings_Text_IO is

   S1 : UXString;

begin
   -- Change the default to LF and UTF-8
   Ending (Current_Output, LF);
   Line_Mark (LF);
   Scheme (Current_Output, UTF_8);
   Ending (Current_Input, LF);
   Scheme (Current_Input, UTF_8);
   loop
      Put ("-->");
      Get_Line (S1);
      Put_Line (S1);
      exit when S1 = "exit";
   end loop;
   Put_Line ("<-->");
end Test_UXStrings_Text_IO;
