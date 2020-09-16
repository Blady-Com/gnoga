with UXStrings;   use UXStrings;
with Ada.Text_IO; use Ada.Text_IO;
with Strings_Edit.Integers; use Strings_Edit.Integers;

procedure Test_UXStrings is

   procedure Send (Msg : UTF_8_Character_Array) is null;
   function Receive return UTF_8_Character_Array is ("");

   S1, S2, S3 : UXString;
   C          : Character;
   WC         : Wide_Character;
   WWC        : Wide_Wide_Character;
   F          : Boolean;

begin
   S1 := From_Latin_1 ("était blah blah");
   S2 := From_BMP ("une soirée passée à étudier la physique ω=Δθ/Δt...");
   S3 := From_Unicode ("une soirée passée à étudier les mathématiques ℕ⊂𝕂...");
   Send (To_UTF_8 (S1) & To_UTF_8 (S3));
   S2  := "Received: " & From_UTF8 (Receive);
   S3 := S1 & "Sent ok";
   S1 := "était blah blah";
   S2 := "une soirée passée à étudier la physique ω=Δθ/Δt...";
   S3 := "une soirée passée à étudier les mathématiques ℕ⊂𝕂...";
   C   := S1 (6);
   WC  := S1 (7);
   WWC := S1 (1);
   Put_Line (Image(Character'pos (C), 16) & ',' & Image(Wide_Character'pos (WC), 16)  & ','& Image(Wide_Wide_Character'pos (WWC), 16));
   for I in S3 loop
      F   := S3 (I) = Wide_Wide_Character'('é');
      if F then
         S2 (I) := Character'('e');
      end if;
      WWC := S3 (I);
      Put_Line (I'Image & ':' & Image(Wide_Wide_Character'pos (WWC), 16) & ',' & F'Image);
   end loop;
   for CC : Wide_Wide_Character of S2 loop
      WWC := CC;
      F   := CC = 'é';
      Put_Line (Image(Wide_Wide_Character'pos (WWC), 16)  & ','& F'img);
   end loop;
--     S1 (3) := WWC; -- discriminant check failed
--     S1 (2) := WC; -- discriminant check failed
   S1 (1) := C;
   if S1 /= "test" then
      S1 := Null_UXString;
      S2 := 2 * 'z';
      S3 := 4 * "po";
   end if;
   S1.Append ("roro");
   S2.Append ('R');
end Test_UXStrings;
