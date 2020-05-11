with UXStrings;   use UXStrings;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_UXStrings is

   procedure Send (Msg : UTF_8_Character_Array) is null;
   function Receive return UTF_8_Character_Array is ("");

   S1, S2, S3 : UXString;
   C          : Character;
   WC         : Wide_Character;
   WWC        : Wide_Wide_Character;
   F          : Boolean;

begin
   S1 := From_Latin_1 ("blah blah");
   S2 := From_BMP ("une soirée passée à étudier la physique ω=Δθ/Δt...");
   S3 := From_Unicode ("une soirée passée à étudier les mathématiques ℕ⊂𝕂...");
   Send (To_UTF_8 (S1) & To_UTF_8 (S3));
   S2  := "Received: " & From_UTF8 (Receive);
   C   := S1 (3);
   WC  := S1 (2);
   WWC := S1 (1);
   Put_Line (Character'pos (C)'img & Wide_Character'pos (WC)'img & Wide_Wide_Character'pos (WWC)'img);
   for I in S3 loop
      C   := S3 (I);
      WC  := S3 (I);
      WWC := S3 (I);
      F   := WWC = 'é';
      if F then
         S3 (I) := Character'('e');
      end if;
      Put_Line (Character'pos (C)'img & Wide_Character'pos (WC)'img & Wide_Wide_Character'pos (WWC)'img & F'img);
   end loop;
   for CC : Wide_Wide_Character of S2 loop
      WWC := CC;
      F   := CC = 'é';
      Put_Line (Wide_Wide_Character'pos (WWC)'img & F'img);
   end loop;
   S1 (3) := WWC;
   S1 (2) := WC;
   S1 (1) := C;
end Test_UXStrings;
