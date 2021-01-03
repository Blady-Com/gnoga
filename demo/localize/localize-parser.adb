-------------------------------------------------------------------------------
-- NAME (body)                  : localize-parser.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : Localization files parser unit.
-- NOTES                        : Ada 2012, GNOGA 2.1 alpha
--
-- COPYRIGHT                    : (c) Pascal Pignard 2021
-- LICENCE                      : CeCILL V2 (http://www.cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Direct_IO;
with Ada.Characters.Conversions;
with Ada.Characters.Latin_1;
with Ada.Characters.Wide_Latin_1;
with Ada.Characters.Wide_Wide_Latin_1;
with Gnoga;
with UXStrings.Formatting;

package body Localize.Parser is

   use Ada.Characters.Conversions;
   use type Content_Maps.Cursor;

   procedure Parse_Strings_File (File_Name : String; Content : out Property_List) is
      package Strings_IO is new Ada.Direct_IO (Wide_Character);
      package File_Text is new Ada.Containers.Vectors (Positive, Wide_Wide_Character);
      use type File_Text.Cursor;

      type State_Type is (None, In_Comment, In_Key, In_Value, Equal, Semi_Colon);

      Raw_File : Strings_IO.File_Type;
      Text     : File_Text.Vector;
      C        : Wide_Character;
      I        : File_Text.Cursor;
      State    : State_Type := None;
      Comment  : String;
      Key      : String;
      Value    : String;

      procedure Append (C : Unicode_Character) is
      begin
         case State is
            when In_Comment =>
               Append (Comment, C);
            when In_Key =>
               Append (Key, C);
            when In_Value =>
               Append (Value, C);
            when others =>
               null;
         end case;
      end Append;

   begin
      Strings_IO.Open (Raw_File, Strings_IO.In_File, File_Name.To_Latin_1);
      while not Strings_IO.End_Of_File (Raw_File) loop
         Strings_IO.Read (Raw_File, C);
         Text.Append (To_Wide_Wide_Character (C));
      end loop;
      Strings_IO.Close (Raw_File);

      Content.Clear;
      I := Text.First;
      while I /= File_Text.No_Element loop
         if Text (I) = '\' then
            File_Text.Next (I);
            case Text (I) is
               when 't' =>
                  Append (Ada.Characters.Wide_Wide_Latin_1.HT);
               when 'n' =>
                  Append (Ada.Characters.Wide_Wide_Latin_1.LF);
               when Ada.Characters.Wide_Wide_Latin_1.LF =>
                  null;
               when '"' | '\' =>
                  Append (Text (I));
               when others =>
                  Append ('\');
                  Append (Text (I));
            end case;
         elsif Text (I) = '/' and State = None then
            File_Text.Next (I);
            if Text (I) = '*' then
               State := In_Comment;
            end if;
         elsif Text (I) /= '*' and State = In_Comment then
            Append (Comment, Text (I));
         elsif Text (I) = '*' and State = In_Comment then
            File_Text.Next (I);
            if Text (I) = '/' then
               State := None;
            else
               Append (Comment, '*');
               Append (Comment, Text (I));
            end if;
         elsif Text (I) = '"' and State = None then
            State := In_Key;
         elsif Text (I) /= '"' and State = In_Key then
            Append (Key, Text (I));
         elsif Text (I) = '"' and State = In_Key then
            State := Equal;
         elsif Text (I) = '"' and State = Equal then
            State := In_Value;
         elsif Text (I) /= '"' and State = In_Value then
            Append (Value, Text (I));
         elsif Text (I) = '"' and State = In_Value then
            State := Semi_Colon;
         elsif Text (I) = ';' and State = Semi_Colon then
            Content.Insert (Key, (Comment, Value, False));
            Comment := Null_UXString;
            Key     := Null_UXString;
            Value   := Null_UXString;
            State   := None;
         end if;
         File_Text.Next (I);
      end loop;
   end Parse_Strings_File;

   procedure Write_Strings_File (File_Name : String; Content : Property_List) is
      package Strings_IO is new Ada.Direct_IO (Wide_Character);

      BOM_LE : constant Wide_Character := Wide_Character'Val (16#FEFF#);

      Raw_File : Strings_IO.File_Type;

      procedure Escaped_Write (Str : String; Multi_Comment : Boolean := False) is
      begin
         for I in 1 .. Length (Str) loop
            if Element (Str, I) = Ada.Characters.Wide_Wide_Latin_1.HT then
               Strings_IO.Write (Raw_File, '\');
               Strings_IO.Write (Raw_File, 't');
            elsif Element (Str, I) = Ada.Characters.Wide_Wide_Latin_1.LF then
               if Multi_Comment then
                  Strings_IO.Write (Raw_File, Ada.Characters.Wide_Latin_1.LF);
               else
                  Strings_IO.Write (Raw_File, '\');
                  Strings_IO.Write (Raw_File, 'n');
               end if;
            elsif Element (Str, I) in '"' | '\' then
               Strings_IO.Write (Raw_File, '\');
               Strings_IO.Write (Raw_File, To_Wide_Character (Element (Str, I)));
            else
               Strings_IO.Write (Raw_File, To_Wide_Character (Element (Str, I)));
            end if;
         end loop;
      end Escaped_Write;

   begin
      Strings_IO.Create (Raw_File, Strings_IO.Out_File, File_Name.To_Latin_1);
      Strings_IO.Write (Raw_File, BOM_LE);
      for C in Content.Iterate loop
         if Content_Maps.Element (C).Comment /= Null_UXString then
            Strings_IO.Write (Raw_File, '/');
            Strings_IO.Write (Raw_File, '*');
            Escaped_Write (Content_Maps.Element (C).Comment, True);
            Strings_IO.Write (Raw_File, '*');
            Strings_IO.Write (Raw_File, '/');
            Strings_IO.Write (Raw_File, Ada.Characters.Wide_Latin_1.LF);
         end if;
         Strings_IO.Write (Raw_File, '"');
         Escaped_Write (Content_Maps.Key (C));
         Strings_IO.Write (Raw_File, '"');
         Strings_IO.Write (Raw_File, ' ');
         Strings_IO.Write (Raw_File, '=');
         Strings_IO.Write (Raw_File, ' ');
         Strings_IO.Write (Raw_File, '"');
         Escaped_Write (Content_Maps.Element (C).Text);
         Strings_IO.Write (Raw_File, '"');
         Strings_IO.Write (Raw_File, ';');
         Strings_IO.Write (Raw_File, Ada.Characters.Wide_Latin_1.LF);
      end loop;
      Strings_IO.Close (Raw_File);
   end Write_Strings_File;

   procedure Parse_Properties_File (File_Name : String; Content : out Property_List) is
      package Strings_IO is new Ada.Direct_IO (Character);
      package File_Text is new Ada.Containers.Vectors (Positive, Wide_Wide_Character);
      use type File_Text.Cursor;

      type State_Type is (None, In_Comment, In_Key, In_Value, Equal);

      Raw_File : Strings_IO.File_Type;
      Text     : File_Text.Vector;
      C        : Character;
      I        : File_Text.Cursor;
      State    : State_Type := None;
      Comment  : String;
      Key      : String;
      Value    : String;
      Hex4     : String     := "0000";

      procedure Append (C : Wide_Wide_Character) is
      begin
         case State is
            when In_Comment =>
               Append (Comment, C);
            when In_Key =>
               Append (Key, C);
            when In_Value =>
               Append (Value, C);
            when None =>
               State := In_Key;
               Append (Key, C);
            when Equal =>
               State := In_Value;
               Append (Value, C);
         end case;
      end Append;

   begin
      Strings_IO.Open (Raw_File, Strings_IO.In_File, File_Name.To_Latin_1);
      while not Strings_IO.End_Of_File (Raw_File) loop
         Strings_IO.Read (Raw_File, C);
         Text.Append (To_Wide_Wide_Character (C));
      end loop;
      Strings_IO.Close (Raw_File);

      Content.Clear;
      I := Text.First;
      while I /= File_Text.No_Element loop
         if Text (I) = '\' then
            File_Text.Next (I);
            case Text (I) is
               when 'u' =>
                  for J in Hex4 loop
                     File_Text.Next (I);
                     exit when I = File_Text.No_Element;
                     Hex4.Replace_Unicode (J, Text (I));
                  end loop;
                  Append (Wide_Wide_Character'Val (Gnoga.Value (Hex4, 16)));
               when ' ' | ':' | '=' | '#' | '!' | '\' =>
                  Append (Text (I));
               when 't' =>
                  Append (Ada.Characters.Wide_Wide_Latin_1.HT);
               when 'n' =>
                  Append (Ada.Characters.Wide_Wide_Latin_1.LF);
               when Ada.Characters.Wide_Wide_Latin_1.LF =>
                  null;
               when others =>
                  Append ('\');
                  Append (Text (I));
            end case;
         elsif Text (I) = Ada.Characters.Wide_Wide_Latin_1.LF and State = In_Comment then
            State := None;
         elsif State = In_Comment then
            Append (Comment, Text (I));
         elsif Text (I) = Ada.Characters.Wide_Wide_Latin_1.LF and State in In_Value | Equal | In_Key then
            Content.Include (Key, (Comment, Value, False));
            Comment := Null_UXString;
            Key     := Null_UXString;
            Value   := Null_UXString;
            State   := None;
         elsif Text (I) in '#' | '!' and State = None then
            State := In_Comment;
            if Comment /= Null_UXString then
               Append (Ada.Characters.Wide_Wide_Latin_1.LF);
            end if;
         elsif Text (I) in '=' | ':' and State = In_Key then
            State := Equal;
         elsif Text (I) not in Ada.Characters.Wide_Wide_Latin_1.HT | Ada.Characters.Wide_Wide_Latin_1.LF |
               Ada.Characters.Wide_Wide_Latin_1.FF | ' ' and
           State in In_Key
         then
            Append (Key, (Text (I)));
         elsif Text (I) not in Ada.Characters.Wide_Wide_Latin_1.HT | Ada.Characters.Wide_Wide_Latin_1.LF |
               Ada.Characters.Wide_Wide_Latin_1.FF | ' ' and
           State in None
         then
            State := In_Key;
            Append (Key, Text (I));
         elsif Text (I) not in Ada.Characters.Wide_Wide_Latin_1.HT | Ada.Characters.Wide_Wide_Latin_1.FF and
           State in In_Value
         then
            Append (Value, Text (I));
         elsif Text (I) not in Ada.Characters.Wide_Wide_Latin_1.HT | Ada.Characters.Wide_Wide_Latin_1.FF | ' ' and
           State in Equal
         then
            State := In_Value;
            Append (Value, Text (I));
         end if;
         File_Text.Next (I);
      end loop;
   end Parse_Properties_File;

   procedure Write_Properties_File (File_Name : String; Content : Property_List) is
      package Strings_IO is new Ada.Direct_IO (Character);

      Raw_File : Strings_IO.File_Type;

      type Escape_Space_Type is (No, Start, Full);

      procedure Escaped_Write (Str : String; Escape_Space : Escape_Space_Type; Multi_Comment : Boolean := False) is
         Space : Boolean := Escape_Space = Start;
         use UXStrings.Formatting;
         use all type UXStrings.Formatting.Alignment;
         function Format is new Integer_Format (Natural);
         function Format_U16
           (Item    :    Natural; Base : in Number_Base := 16; PutPlus : in Boolean := False; Field : in Natural := 4;
            Justify : in Alignment := Right; Fill : in Character := '0') return UXString renames
           Format;
      begin
         for I in 1 .. Length (Str) loop
            if Element (Str, I) = Ada.Characters.Wide_Wide_Latin_1.HT then
               Strings_IO.Write (Raw_File, '\');
               Strings_IO.Write (Raw_File, 't');
               Space := False;
            elsif Element (Str, I) = Ada.Characters.Wide_Wide_Latin_1.LF then
               if Multi_Comment then
                  Strings_IO.Write (Raw_File, Ada.Characters.Latin_1.LF);
                  Strings_IO.Write (Raw_File, '#');
               else
                  Strings_IO.Write (Raw_File, '\');
                  Strings_IO.Write (Raw_File, 'n');
               end if;
               Space := False;
            elsif Element (Str, I) in '=' | ':' | '#' | '!' | '\' and Escape_Space /= No then
               Strings_IO.Write (Raw_File, '\');
               Strings_IO.Write (Raw_File, To_Character (Element (Str, I)));
               Space := False;
            elsif Element (Str, I) = ' ' and (Space or Escape_Space = Full) then
               Strings_IO.Write (Raw_File, '\');
               Strings_IO.Write (Raw_File, ' ');
               Space := False;
            elsif Element (Str, I) < Ada.Characters.Wide_Wide_Latin_1.DEL then
               Strings_IO.Write (Raw_File, To_Character (Element (Str, I)));
               Space := False;
            else
               Strings_IO.Write (Raw_File, '\');
               Strings_IO.Write (Raw_File, 'u');
               declare
                  Hex4 : constant String := Format_U16 (Wide_Character'Pos (To_Wide_Character (Element (Str, I))));
               begin
                  for C of Hex4 loop
                     Strings_IO.Write (Raw_File, To_Character (C));
                  end loop;
               end;
               Space := False;
            end if;
         end loop;
      end Escaped_Write;

   begin
      Strings_IO.Create (Raw_File, Strings_IO.Out_File, File_Name.To_Latin_1);
      for C in Content.Iterate loop
         if Content_Maps.Element (C).Comment /= Null_UXString then
            Strings_IO.Write (Raw_File, '#');
            Escaped_Write (Content_Maps.Element (C).Comment, No, True);
            Strings_IO.Write (Raw_File, Ada.Characters.Latin_1.LF);
         end if;
         Escaped_Write (Content_Maps.Key (C), Full);
         Strings_IO.Write (Raw_File, '=');
         Escaped_Write (Content_Maps.Element (C).Text, Start);
         Strings_IO.Write (Raw_File, Ada.Characters.Latin_1.LF);
      end loop;
      Strings_IO.Close (Raw_File);
   end Write_Properties_File;

   procedure Read (Properties : out Property_List; File_Name : String) is
   begin
      if Tail (File_Name, 8, ' ') = ".strings" then
         Parse_Strings_File (File_Name, Properties);
      end if;
      if Tail (File_Name, 11, ' ') = ".properties" then
         Parse_Properties_File (File_Name, Properties);
      end if;
   end Read;

   procedure Write (Properties : Property_List; File_Name : String) is
   begin
      if Tail (File_Name, 8, ' ') = ".strings" then
         Write_Strings_File (File_Name, Properties);
      end if;
      if Tail (File_Name, 11, ' ') = ".properties" then
         Write_Properties_File (File_Name, Properties);
      end if;
   end Write;

   function Keys (Properties : Property_List) return Key_List is
   begin
      return Result_Key_List : Key_List do
         for Index in Properties.Iterate loop
            Result_Key_List.Append (Content_Maps.Key (Index));
         end loop;
      end return;
   end Keys;

   function Contains (Properties : Property_List; Key : String) return Boolean is
     (Content_Maps.Contains (Content_Maps.Map (Properties), Key));

   procedure Insert (Properties : in out Property_List; Key : String) is
   begin
      Properties.Insert (Key, (Null_UXString, Null_UXString, False));
   end Insert;

   procedure Delete (Properties : in out Property_List; Key : String) is
   begin
      if Properties.Find (Key) /= Content_Maps.No_Element then
         Properties.Delete (Key);
      end if;
   end Delete;

   procedure Rename (Properties : in out Property_List; From, To : String) is
   begin
      Properties.Insert (To, Properties.Element (From));
      Properties.Delete (From);
   end Rename;

   function Text (Properties : Property_List; Key : String) return String is
   begin
      if Properties.Find (Key) = Content_Maps.No_Element then
         return "";
      else
         return Properties.Element (Key).Text;
      end if;
   end Text;

   procedure Text (Properties : in out Property_List; Key : String; Value : String) is
      Cursor  : constant Content_Maps.Cursor := Properties.Find (Key);
      Element : Property_Type;
   begin
      if Cursor /= Content_Maps.No_Element then
         Element      := Content_Maps.Element (Cursor);
         Element.Text := Value;
         if Element.Text /= Content_Maps.Element (Cursor).Text then
            Element.Modified := True;
            Properties.Replace_Element (Cursor, Element);
         end if;
      end if;
   end Text;

   function Comment (Properties : Property_List; Key : String) return String is
   begin
      if Properties.Find (Key) = Content_Maps.No_Element then
         return Null_UXString;
      else
         return Properties.Element (Key).Comment;
      end if;
   end Comment;

   procedure Comment (Properties : in out Property_List; Key : String; Value : String) is
      Cursor  : constant Content_Maps.Cursor := Properties.Find (Key);
      Element : Property_Type;
   begin
      if Cursor /= Content_Maps.No_Element then
         Element         := Content_Maps.Element (Cursor);
         Element.Comment := Value;
         if Element.Comment /= Content_Maps.Element (Cursor).Comment then
            Element.Modified := True;
            Properties.Replace_Element (Cursor, Element);
         end if;
      end if;
   end Comment;

   function Modified (Properties : Property_List; Key : String) return Boolean is
   begin
      if Properties.Find (Key) = Content_Maps.No_Element then
         return False;
      else
         return Properties.Element (Key).Modified;
      end if;
   end Modified;

   procedure Reset_Modified_Indicators (Properties : in out Property_List) is
   begin
      for Element of Properties loop
         Element.Modified := False;
      end loop;
   end Reset_Modified_Indicators;

   function Selected_Keys (Master, Locale : Property_List; Pattern : String) return Key_List is
      package Keys_Sorting is new Lists.Generic_Sorting;
   begin
      if Pattern /= "" then
         return Result_Key_List : Key_List do
            for Cursor in Master.Iterate loop
               if
                 (Index (Content_Maps.Key (Cursor), Pattern) > 0 or
                  Index (Content_Maps.Element (Cursor).Text, Pattern) > 0 or
                  Index (Content_Maps.Element (Cursor).Comment, Pattern) > 0) and
                 not Result_Key_List.Contains (Content_Maps.Key (Cursor))
               then
                  Result_Key_List.Append (Content_Maps.Key (Cursor));
               end if;
            end loop;
            for Cursor in Locale.Iterate loop
               if
                 (Index (Content_Maps.Key (Cursor), Pattern) > 0 or
                  Index (Content_Maps.Element (Cursor).Text, Pattern) > 0 or
                  Index (Content_Maps.Element (Cursor).Comment, Pattern) > 0) and
                 not Result_Key_List.Contains (Content_Maps.Key (Cursor))
               then
                  Result_Key_List.Append (Content_Maps.Key (Cursor));
               end if;
            end loop;
         end return;
      else
         return Result_Key_List : Key_List := Keys (Master) do
            for Cursor in Locale.Iterate loop
               if not Result_Key_List.Contains (Content_Maps.Key (Cursor)) then
                  Result_Key_List.Append (Content_Maps.Key (Cursor));
               end if;
            end loop;
            Keys_Sorting.Sort (Result_Key_List);
         end return;
      end if;
   end Selected_Keys;

end Localize.Parser;
