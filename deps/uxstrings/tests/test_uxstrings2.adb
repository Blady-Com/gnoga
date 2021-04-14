with UXStrings;         use UXStrings;
with UXStrings.Text_IO; use UXStrings.Text_IO;
with UXStrings.Conversions;
with UXStrings.Hash;
with UXStrings.Formatting;

procedure Test_UXStrings2 is

   function Image is new UXStrings.Conversions.Scalar_Image (Boolean);
   function Image is new UXStrings.Conversions.Integer_Image (Integer);
   function Value is new UXStrings.Conversions.Integer_Value (Integer);

   function Format is new UXStrings.Formatting.Integer_Format (Natural);
   use all type UXStrings.Formatting.Alignment;

   procedure Send (Msg : UTF_8_Character_Array) is
   begin
      for Code of Msg loop
         Put (Image (Character'pos (Code), 16));
      end loop;
      New_Line;
   end Send;
   function Receive return UTF_8_Character_Array is (To_UTF_8 ("donnees"));

   S1, S2, S3 : UXString;
   C          : Character;
   WC         : Wide_Character;
   WWC        : Wide_Wide_Character;
   F          : Boolean;
   D          : constant array (Positive range <>) of Natural := (16#0075#, 16#003E#, 16#30E3#, 16#03A3#);
   --     Data : constant BMP_Character_array := (for I in D'Range => BMP_Character'val (D(I)));

begin
   -- Change the default to LF and UTF-8
   Ending (Current_Output, LF_Ending);
   Line_Mark (LF_Ending);
   Scheme (Current_Output, ASCII_7);

   S1 := From_ASCII ("etait blah blah");
   S2 := From_ASCII ("une soiree passee a etudier la physique...");
   S3 := From_ASCII ("une soiree passee a etudier les mathematiques...");
   Put_Line (S1 & Line_Mark & S2 & Line_Mark & S3);
   Put_Line (Image (S1.Is_ASCII) & ' ' & Image (S2.Is_ASCII) & ' ' & Image (S3.Is_ASCII));
   Send (To_UTF_8 (S1) & To_UTF_8 (S3));
   S2 := "Received: " & From_UTF_8 (Receive);
   S3 := S1 & " - Sent ok";
   Put_Line (S1 & Line_Mark & S2 & Line_Mark & S3);
   S1 := 4 * '.';
   --     S2 := From_BMP (Data);
   --     S3 := 4*'.';
   --     for I in Data'Range loop
   --        S3(I) := Data (I); -- discriminant check failed
   --     end loop;
   S1 := From_ASCII ('r');
   Put_Line (S1 & Line_Mark & S2 & Line_Mark & S3);
   S1 := "etait blah blah";
   S2 := "une soiree passee à etudier la physique...";
   S3 := "une soiree passee à etudier les mathematiques...";
   Put_Line (S1 & Line_Mark & S2 & Line_Mark & S3);
   Put_Line (Image (S1.Is_ASCII) & ' ' & Image (S2.Is_ASCII) & ' ' & Image (S3.Is_ASCII));
   Put_Line
     (Image (Index (S1, "ee")) & Image (Index (S2, "ee"), prefix => ' ') & Image (index (S3, "ee", 10), prefix => ' '));
   C   := S1.Get_ASCII (6);
   WC  := S1.Get_BMP (7);
   WWC := S1 (1);
   Put_Line
     (Image (Character'pos (C), 16) & ',' & Image (Wide_Character'pos (WC), 16) & ',' &
      Image (Wide_Wide_Character'pos (WWC), 16));
   for I in S3 loop
      F := S3.Get_ASCII (I) = 'e';
--        if F then
--           Replace_Latin_1 (S2 ,I, 'e');
--        end if;
      WWC := S3 (I);
      Put_Line (Image (I) & ':' & Image (Wide_Wide_Character'pos (WWC), 16) & ',' & Image (F));
   end loop;
   for CC of S2 loop
      WWC := CC;
      F   := CC = 'e';
      Put_Line (Image (Wide_Wide_Character'pos (WWC), 16) & ',' & Image (F));
   end loop;
--     Replace_Unicode (S1 ,3, WWC);
--     S1.Replace_BMP (2, WC);
--     S1.Replace_Latin_1 (1, C);
--     Put_Line (S1);
   if S1 /= "test" then
      S1 := Null_UXString;
      S2 := 2 * 'z';
      S3 := 4 * "po";
   end if;
   S3 := "Riri";
   S2 := "Loulou";
   S1 := " et Fifi";
   S2.Append (S1);
   S1.Prepend (S3);
   Put_Line (S1 & Line_Mark & S2 & Line_Mark & S3 & Line_Mark & Image (S3.Is_ASCII));
   Put_Line (Image (Integer (UXStrings.Hash (S1)), 16));
   Put_Line (Image (Value ("  + 73")));
   C := S1.To_Latin_1 (3);
   Put_Line (Image (Character'pos (C), 16));
   C := S1.Get_Latin_1 (3); -- same result but avoid all string conversion
   Put_Line (Image (Character'pos (C), 16));
   Put_Line (Format (5, 2, True, 10, Center, '@'));
   Put_Line ("--end--");
end Test_UXStrings2;
