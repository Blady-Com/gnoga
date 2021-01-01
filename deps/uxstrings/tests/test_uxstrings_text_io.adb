with UXStrings;         use UXStrings;
with UXStrings.Text_IO; use UXStrings.Text_IO;

procedure Test_UXStrings_Text_IO is

   procedure Write is
      F : File_Type;
   begin
      Create (F,Out_File, "test.txt");
      Put_Line (F,"Test");
      Put_Line (F,"une soirée passée à étudier la physique ω=Δθ/Δt...");
      Put_Line (F,"une soirée passée à étudier les mathématiques ℕ⊂𝕂...");
      Put (F,"Test");
      Close (F);
      Put_Line ("File witten.");
   end;

   procedure Read is
      F : File_Type;
   begin
      Open (F, In_File, "test.txt");
      while not End_Of_File(F) loop
         Put_Line (Get_Line (F));
      end loop;
      Close (F);
      Put_Line ("File read.");
   end;

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
      if S1 = "write" then
         Write;
      end if;
      if S1 = "read" then
         Read;
      end if;
   end loop;
   Put_Line ("<-->");
end Test_UXStrings_Text_IO;
