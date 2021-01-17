with UXStrings;         use UXStrings;
with UXStrings.Text_IO; use UXStrings.Text_IO;
with UXStrings.Conversions;

procedure Test_UXStrings_Text_IO is
   function Image is new UXStrings.Conversions.Scalar_Image (Encoding_Scheme);
   function Value is new UXStrings.Conversions.Scalar_Value (Encoding_Scheme);

   procedure Write (Encoding : Encoding_Scheme) is
      F : File_Type;
   begin
      Create (F,Out_File, "test_" & Image (Encoding) & ".txt", Encoding);
      Put_BOM (F);
      Put_Line (F,"Test_" & Image (Encoding));
      Put_Line (F,"une soirée passée à étudier la physique ω=Δθ/Δt...");
      Put_Line (F,"une soirée passée à étudier les mathématiques ℕ⊂𝕂...");
      Put (F,"Test_End");
      Close (F);
      Put_Line ("File witten.");
   end;

   procedure Read (Encoding : Encoding_Scheme) is
      F : File_Type;
   begin
      Open (F, In_File, "test_" & Image(Encoding) & ".txt", Encoding);
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
      if S1.Index( "write") = S1.First then
         Write (if S1.index ("utf_") > 0 then Value(S1.Slice (6, S1.Length)) else Latin_1);
      end if;
      if S1.Index( "read")= S1.First then
         Read(if S1.index ("utf_") > 0 then Value(S1.Slice (6, S1.Length)) else Latin_1);
      end if;
   end loop;
   Put_Line ("<-->");
end Test_UXStrings_Text_IO;
