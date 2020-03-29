-------------------------------------------------------------------------------
-- NAME (body)                  : localize-parser.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : Localization files parser unit.
-- NOTES                        : Ada 2012, GNOGA 1.6 alpha
--
-- COPYRIGHT                    : (c) Pascal Pignard 2020
-- LICENCE                      : CeCILL V2 (http://www.cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Direct_IO;
with Ada.Integer_Text_IO;
with Ada.Characters.Conversions;
with Ada.Characters.Latin_1;
with Ada.Characters.Wide_Latin_1;
with Ada.Strings.Wide_Unbounded;
with Ada.Strings.Fixed;

package body Localize.Parser is

   use Ada.Strings.Wide_Unbounded;
   use Ada.Characters.Conversions;

   Substitution_Character : constant Character := '?';

   type Content_Type is record
      Comment  : Unbounded_Wide_String;
      Text     : Unbounded_Wide_String;
      Modified : Boolean;
   end record;

   package Content_Maps is new Ada.Containers.Ordered_Maps
     (Unbounded_Wide_String, Content_Type);
   subtype Content_Map_Type is Content_Maps.Map;
   use type Content_Maps.Cursor;

   Master_Content, Locale_Content : Content_Map_Type;

   procedure Parse_Strings_File
     (File_Name : String; Content : out Content_Map_Type)
   is
      package Strings_IO is new Ada.Direct_IO (Wide_Character);
      package File_Text is new Ada.Containers.Vectors (Positive,
         Wide_Character);
      use type File_Text.Cursor;

      type State_Type is
        (None, In_Comment, In_Key, In_Value, Equal, Semi_Colon);

      Raw_File : Strings_IO.File_Type;
      Text     : File_Text.Vector;
      C        : Wide_Character;
      I        : File_Text.Cursor;
      State    : State_Type := None;
      Comment  : Unbounded_Wide_String;
      Key      : Unbounded_Wide_String;
      Value    : Unbounded_Wide_String;

   begin
      Strings_IO.Open (Raw_File, Strings_IO.In_File, File_Name);
      while not Strings_IO.End_Of_File (Raw_File) loop
         Strings_IO.Read (Raw_File, C);
         Text.Append (C);
      end loop;
      Strings_IO.Close (Raw_File);

      Content.Clear;
      I := Text.First;
      while I /= File_Text.No_Element loop
         if Text (I) = '\' then
            File_Text.Next (I);
            case State is
               when In_Comment =>
                  Append (Comment, '\');
                  Append (Comment, Text (I));
               when In_Key =>
                  Append (Key, '\');
                  Append (Key, Text (I));
               when In_Value =>
                  Append (Value, '\');
                  Append (Value, Text (I));
               when others =>
                  null;
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
            Comment := Null_Unbounded_Wide_String;
            Key     := Null_Unbounded_Wide_String;
            Value   := Null_Unbounded_Wide_String;
            State   := None;
         end if;
         File_Text.Next (I);
      end loop;
   end Parse_Strings_File;

   procedure Write_Strings_File
     (File_Name : String; Content : Content_Map_Type)
   is
      package Strings_IO is new Ada.Direct_IO (Wide_Character);

      BOM_LE : constant Wide_Character := Wide_Character'Val (16#FEFF#);

      Raw_File : Strings_IO.File_Type;

   begin
      Strings_IO.Create (Raw_File, Strings_IO.Out_File, File_Name);
      Strings_IO.Write (Raw_File, BOM_LE);
      for C in Content.Iterate loop
         if Content_Maps.Element (C).Comment /= Null_Unbounded_Wide_String then
            Strings_IO.Write (Raw_File, '/');
            Strings_IO.Write (Raw_File, '*');
            for I in 1 .. Length (Content_Maps.Element (C).Comment) loop
               Strings_IO.Write
                 (Raw_File, Element (Content_Maps.Element (C).Comment, I));
            end loop;
            Strings_IO.Write (Raw_File, '*');
            Strings_IO.Write (Raw_File, '/');
            Strings_IO.Write (Raw_File, Ada.Characters.Wide_Latin_1.LF);
         end if;
         Strings_IO.Write (Raw_File, '"');
         for I in 1 .. Length (Content_Maps.Key (C)) loop
            Strings_IO.Write (Raw_File, Element (Content_Maps.Key (C), I));
         end loop;
         Strings_IO.Write (Raw_File, '"');
         Strings_IO.Write (Raw_File, ' ');
         Strings_IO.Write (Raw_File, '=');
         Strings_IO.Write (Raw_File, ' ');
         Strings_IO.Write (Raw_File, '"');
         for I in 1 .. Length (Content_Maps.Element (C).Text) loop
            Strings_IO.Write
              (Raw_File, Element (Content_Maps.Element (C).Text, I));
         end loop;
         Strings_IO.Write (Raw_File, '"');
         Strings_IO.Write (Raw_File, ';');
         Strings_IO.Write (Raw_File, Ada.Characters.Wide_Latin_1.LF);
      end loop;
      Strings_IO.Close (Raw_File);
   end Write_Strings_File;

   procedure Parse_Properties_File
     (File_Name : String; Content : out Content_Map_Type)
   is
      package Strings_IO is new Ada.Direct_IO (Character);
      package File_Text is new Ada.Containers.Vectors (Positive, Character);
      use type File_Text.Cursor;

      type State_Type is (None, In_Comment, In_Key, In_Value, Equal);

      Raw_File : Strings_IO.File_Type;
      Text     : File_Text.Vector;
      C        : Character;
      I        : File_Text.Cursor;
      State    : State_Type := None;
      Comment  : Unbounded_Wide_String;
      Key      : Unbounded_Wide_String;
      Value    : Unbounded_Wide_String;
      Hex4     : String     := "0000";

      procedure Append (C : Wide_Character) is
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
      Strings_IO.Open (Raw_File, Strings_IO.In_File, File_Name);
      while not Strings_IO.End_Of_File (Raw_File) loop
         Strings_IO.Read (Raw_File, C);
         Text.Append (C);
      end loop;
      Strings_IO.Close (Raw_File);

      Content.Clear;
      I := Text.First;
      while I /= File_Text.No_Element loop
         if Text (I) = '\' then
            File_Text.Next (I);
            case Text (I) is
               when 'u' =>
                  for C of Hex4 loop
                     File_Text.Next (I);
                     exit when I = File_Text.No_Element;
                     C := Text (I);
                  end loop;
                  Append
                    (Wide_Character'Val (Natural'Value ("16#" & Hex4 & '#')));
               when ' ' | ':' | '=' | '#' | '!' =>
                  Append (To_Wide_Character (Text (I)));
               when 't' =>
                  Append (Ada.Characters.Wide_Latin_1.HT);
               when 'n' =>
                  Append (Ada.Characters.Wide_Latin_1.LF);
               when Ada.Characters.Latin_1.LF =>
                  null;
               when others =>
                  Append ('\');
                  Append (To_Wide_Character (Text (I)));
            end case;
         elsif Text (I) = Ada.Characters.Latin_1.LF and State = In_Comment then
            State := None;
         elsif State = In_Comment then
            Append (Comment, To_Wide_Character (Text (I)));
         elsif Text (I) = Ada.Characters.Latin_1.LF and
           State in In_Value | Equal | In_Key
         then
            Content.Insert (Key, (Comment, Value, False));
            Comment := Null_Unbounded_Wide_String;
            Key     := Null_Unbounded_Wide_String;
            Value   := Null_Unbounded_Wide_String;
            State   := None;
         elsif Text (I) in '#' | '!' and State = None then
            State := In_Comment;
         elsif Text (I) in '=' | ':' and State = In_Key then
            State := Equal;
         elsif Text (I) not in Ada.Characters.Latin_1.HT |
               Ada.Characters.Latin_1.FF                 | ' ' and
           State in In_Key
         then
            Append (Key, To_Wide_Character (Text (I)));
         elsif Text (I) not in Ada.Characters.Latin_1.HT |
               Ada.Characters.Latin_1.FF                 | ' ' and
           State in None
         then
            State := In_Key;
            Append (Key, To_Wide_Character (Text (I)));
         elsif Text (I) not in Ada.Characters.Latin_1.HT |
               Ada.Characters.Latin_1.FF and
           State in In_Value
         then
            Append (Value, To_Wide_Character (Text (I)));
         elsif Text (I) not in Ada.Characters.Latin_1.HT |
               Ada.Characters.Latin_1.FF                 | ' ' and
           State in Equal
         then
            State := In_Value;
            Append (Value, To_Wide_Character (Text (I)));
         end if;
         File_Text.Next (I);
      end loop;
   end Parse_Properties_File;

   procedure Write_Properties_File
     (File_Name : String; Content : Content_Map_Type)
   is
      package Strings_IO is new Ada.Direct_IO (Character);

      Raw_File : Strings_IO.File_Type;

      type Escape_Space_Type is (No, Start, Full);

      procedure Escaped_Write
        (Str : Unbounded_Wide_String; Escape_Space : Escape_Space_Type)
      is
         Space : Boolean := Escape_Space = Start;
      begin
         for I in 1 .. Length (Str) loop
            if Element (Str, I) = Ada.Characters.Wide_Latin_1.HT then
               Strings_IO.Write (Raw_File, '\');
               Strings_IO.Write (Raw_File, 't');
               Space := False;
            elsif Element (Str, I) = Ada.Characters.Wide_Latin_1.LF then
               Strings_IO.Write (Raw_File, '\');
               Strings_IO.Write (Raw_File, 'n');
               Space := False;
            elsif Element (Str, I) in '=' | ':' | '#' | '!' and
              Escape_Space /= No
            then
               Strings_IO.Write (Raw_File, '\');
               Strings_IO.Write (Raw_File, To_Character (Element (Str, I)));
               Space := False;
            elsif Element (Str, I) = ' ' and (Space or Escape_Space = Full)
            then
               Strings_IO.Write (Raw_File, '\');
               Strings_IO.Write (Raw_File, ' ');
            elsif Element (Str, I) < Ada.Characters.Wide_Latin_1.DEL then
               Strings_IO.Write (Raw_File, To_Character (Element (Str, I)));
               Space := False;
            else
               Strings_IO.Write (Raw_File, '\');
               Strings_IO.Write (Raw_File, 'u');
               declare
                  Hex_Full : String
                    (1 ..
                         Ada.Integer_Text_IO.Default_Width); --  Format 16#...#
                  Hex_Digit_Index : Natural :=
                    Hex_Full'Last - 1; --  Last hex digit
                  Hex4 : String := "0000";
               begin
                  Ada.Integer_Text_IO.Put
                    (Hex_Full, Wide_Character'Pos (Element (Str, I)), 16);
                  while Hex_Full (Hex_Digit_Index) /= '#' loop
                     Hex4 (Hex4'Last + Hex_Digit_Index - Hex_Full'Last + 1) :=
                       Hex_Full (Hex_Digit_Index);
                     Hex_Digit_Index := Hex_Digit_Index - 1;
                  end loop;
                  for C of Hex4 loop
                     Strings_IO.Write (Raw_File, C);
                  end loop;
               end;
               Space := False;
            end if;
         end loop;
      end Escaped_Write;

   begin
      Strings_IO.Create (Raw_File, Strings_IO.Out_File, File_Name);
      for C in Content.Iterate loop
         if Content_Maps.Element (C).Comment /= Null_Unbounded_Wide_String then
            Strings_IO.Write (Raw_File, '#');
            Escaped_Write (Content_Maps.Element (C).Comment, No);
            Strings_IO.Write (Raw_File, Ada.Characters.Latin_1.LF);
         end if;
         Escaped_Write (Content_Maps.Key (C), Full);
         Strings_IO.Write (Raw_File, '=');
         Escaped_Write (Content_Maps.Element (C).Text, Start);
         Strings_IO.Write (Raw_File, Ada.Characters.Latin_1.LF);
      end loop;
      Strings_IO.Close (Raw_File);
   end Write_Properties_File;

   procedure Parse_Master (File_Name : String) is
   begin
      if Ada.Strings.Fixed.Tail (File_Name, 8, ' ') = ".strings" then
         Parse_Strings_File (File_Name, Master_Content);
      end if;
      if Ada.Strings.Fixed.Tail (File_Name, 11, ' ') = ".properties" then
         Parse_Properties_File (File_Name, Master_Content);
      end if;
   exception
      when others =>
         Master_Content.Clear;
   end Parse_Master;

   function Master_Keys return List_Type is
   begin
      return Keys : List_Type do
         for Index in Master_Content.Iterate loop
            Keys.Append
              (To_String
                 (To_Wide_String (Content_Maps.Key (Index)),
                  Substitution_Character));
         end loop;
      end return;
   end Master_Keys;

   function Master_Contains (Key : String) return Boolean is
     (Master_Content.Contains
        (To_Unbounded_Wide_String (To_Wide_String (Key))));

   function Master_Text (Key : String) return String is
      UWS_Key : constant Unbounded_Wide_String :=
        To_Unbounded_Wide_String (To_Wide_String (Key));
   begin
      if Master_Content.Find (UWS_Key) = Content_Maps.No_Element then
         return "";
      else
         return To_String
             (To_Wide_String
                (Master_Content.Element
                   (To_Unbounded_Wide_String (To_Wide_String (Key)))
                   .Text),
              Substitution_Character);
      end if;
   end Master_Text;

   procedure Master_Text (Key : String; Value : String) is
      UWS_Key : constant Unbounded_Wide_String :=
        To_Unbounded_Wide_String (To_Wide_String (Key));
      Cursor  : constant Content_Maps.Cursor := Master_Content.Find (UWS_Key);
      Element : Content_Type                 := Content_Maps.Element (Cursor);
   begin
      Element.Text := To_Unbounded_Wide_String (To_Wide_String (Value));
      Master_Content.Replace_Element (Cursor, Element);
   end Master_Text;

   function Master_Comment (Key : String) return String is
      UWS_Key : constant Unbounded_Wide_String :=
        To_Unbounded_Wide_String (To_Wide_String (Key));
   begin
      if Master_Content.Find (UWS_Key) = Content_Maps.No_Element then
         return "";
      else
         return To_String
             (To_Wide_String
                (Master_Content.Element
                   (To_Unbounded_Wide_String (To_Wide_String (Key)))
                   .Comment),
              Substitution_Character);
      end if;
   end Master_Comment;

   procedure Master_Comment (Key : String; Value : String) is
      UWS_Key : constant Unbounded_Wide_String :=
        To_Unbounded_Wide_String (To_Wide_String (Key));
      Cursor  : constant Content_Maps.Cursor := Master_Content.Find (UWS_Key);
      Element : Content_Type                 := Content_Maps.Element (Cursor);
   begin
      Element.Comment := To_Unbounded_Wide_String (To_Wide_String (Value));
      Master_Content.Replace_Element (Cursor, Element);
   end Master_Comment;

   procedure Parse_Locale (File_Name : String) is
   begin
      if Ada.Strings.Fixed.Tail (File_Name, 8, ' ') = ".strings" then
         Parse_Strings_File (File_Name, Locale_Content);
      end if;
      if Ada.Strings.Fixed.Tail (File_Name, 11, ' ') = ".properties" then
         Parse_Properties_File (File_Name, Locale_Content);
      end if;
   exception
      when others =>
         Locale_Content.Clear;
   end Parse_Locale;

   procedure Write_Locale (File_Name : String) is
   begin
      if Ada.Strings.Fixed.Tail (File_Name, 8, ' ') = ".strings" then
         Write_Strings_File (File_Name, Locale_Content);
      end if;
      if Ada.Strings.Fixed.Tail (File_Name, 11, ' ') = ".properties" then
         Write_Properties_File (File_Name, Locale_Content);
      end if;
   end Write_Locale;

   function Locale_Keys return List_Type is
   begin
      return Key_List : List_Type do
         for Index in Locale_Content.Iterate loop
            Key_List.Append
              (To_String
                 (To_Wide_String (Content_Maps.Key (Index)),
                  Substitution_Character));
         end loop;
      end return;
   end Locale_Keys;

   function Locale_Contains (Key : String) return Boolean is
     (Locale_Content.Contains
        (To_Unbounded_Wide_String (To_Wide_String (Key))));

   procedure Insert_Locale (Key : String) is
   begin
      Locale_Content.Include
        (To_Unbounded_Wide_String (To_Wide_String (Key)),
         (Null_Unbounded_Wide_String, Null_Unbounded_Wide_String, False));
   end Insert_Locale;

   procedure Delete_Locale (Key : String) is
      UWS_Key : constant Unbounded_Wide_String :=
        To_Unbounded_Wide_String (To_Wide_String (Key));
   begin
      if Locale_Content.Find (UWS_Key) /= Content_Maps.No_Element then
         Locale_Content.Delete
           (To_Unbounded_Wide_String (To_Wide_String (Key)));
      end if;
   end Delete_Locale;

   procedure Rename_Locale (From, To : String) is
   begin
      Locale_Content.Include
        (To_Unbounded_Wide_String (To_Wide_String (To)),
         Locale_Content.Element
           (To_Unbounded_Wide_String (To_Wide_String (From))));
      Locale_Content.Delete (To_Unbounded_Wide_String (To_Wide_String (From)));
   end Rename_Locale;

   function Locale_Text (Key : String) return String is
      UWS_Key : constant Unbounded_Wide_String :=
        To_Unbounded_Wide_String (To_Wide_String (Key));
   begin
      if Locale_Content.Find (UWS_Key) = Content_Maps.No_Element then
         return "";
      else
         return To_String
             (To_Wide_String
                (Locale_Content.Element
                   (To_Unbounded_Wide_String (To_Wide_String (Key)))
                   .Text),
              Substitution_Character);
      end if;
   end Locale_Text;

   procedure Locale_Text (Key : String; Value : String) is
      UWS_Key : constant Unbounded_Wide_String :=
        To_Unbounded_Wide_String (To_Wide_String (Key));
      Cursor  : constant Content_Maps.Cursor := Locale_Content.Find (UWS_Key);
      Element : Content_Type;
   begin
      if Cursor /= Content_Maps.No_Element then
         Element      := Content_Maps.Element (Cursor);
         Element.Text := To_Unbounded_Wide_String (To_Wide_String (Value));
         if Element.Text /= Content_Maps.Element (Cursor).Text then
            Element.Modified := True;
            Locale_Content.Replace_Element (Cursor, Element);
         end if;
      end if;
   end Locale_Text;

   function Locale_Comment (Key : String) return String is
      UWS_Key : constant Unbounded_Wide_String :=
        To_Unbounded_Wide_String (To_Wide_String (Key));
   begin
      if Locale_Content.Find (UWS_Key) = Content_Maps.No_Element then
         return "";
      else
         return To_String
             (To_Wide_String (Locale_Content.Element (UWS_Key).Comment),
              Substitution_Character);
      end if;
   end Locale_Comment;

   procedure Locale_Comment (Key : String; Value : String) is
      UWS_Key : constant Unbounded_Wide_String :=
        To_Unbounded_Wide_String (To_Wide_String (Key));
      Cursor  : constant Content_Maps.Cursor := Locale_Content.Find (UWS_Key);
      Element : Content_Type;
   begin
      if Cursor /= Content_Maps.No_Element then
         Element         := Content_Maps.Element (Cursor);
         Element.Comment := To_Unbounded_Wide_String (To_Wide_String (Value));
         if Element.Comment /= Content_Maps.Element (Cursor).Comment then
            Element.Modified := True;
            Locale_Content.Replace_Element (Cursor, Element);
         end if;
      end if;
   end Locale_Comment;

   function Locale_Modified (Key : String) return Boolean is
      UWS_Key : constant Unbounded_Wide_String :=
        To_Unbounded_Wide_String (To_Wide_String (Key));
   begin
      if Locale_Content.Find (UWS_Key) = Content_Maps.No_Element then
         return False;
      else
         return Locale_Content.Element
             (To_Unbounded_Wide_String (To_Wide_String (Key)))
             .Modified;
      end if;
   end Locale_Modified;

   procedure Reset_Locale_Modified_Indicators is
   begin
      for Element of Locale_Content loop
         Element.Modified := False;
      end loop;
   end Reset_Locale_Modified_Indicators;

   function Selected_Keys (Pattern : String) return List_Type is
      package Keys_Sorting is new Lists.Generic_Sorting;
   begin
      if Pattern /= "" then
         return Keys : List_Type do
            for Cursor in Master_Content.Iterate loop
               if
                 (Index (Content_Maps.Key (Cursor), To_Wide_String (Pattern)) >
                  0 or
                  Index
                      (Content_Maps.Element (Cursor).Text,
                       To_Wide_String (Pattern)) >
                    0 or
                  Index
                      (Content_Maps.Element (Cursor).Comment,
                       To_Wide_String (Pattern)) >
                    0) and
                 not Keys.Contains
                   (To_String
                      (To_Wide_String (Content_Maps.Key (Cursor)),
                       Substitution_Character))
               then
                  Keys.Append
                    (To_String
                       (To_Wide_String (Content_Maps.Key (Cursor)),
                        Substitution_Character));
               end if;
            end loop;
            for Cursor in Locale_Content.Iterate loop
               if
                 (Index (Content_Maps.Key (Cursor), To_Wide_String (Pattern)) >
                  0 or
                  Index
                      (Content_Maps.Element (Cursor).Text,
                       To_Wide_String (Pattern)) >
                    0 or
                  Index
                      (Content_Maps.Element (Cursor).Comment,
                       To_Wide_String (Pattern)) >
                    0) and
                 not Keys.Contains
                   (To_String
                      (To_Wide_String (Content_Maps.Key (Cursor)),
                       Substitution_Character))
               then
                  Keys.Append
                    (To_String
                       (To_Wide_String (Content_Maps.Key (Cursor)),
                        Substitution_Character));
               end if;
            end loop;
         end return;
      else
         return Keys : List_Type := Master_Keys do
            for Cursor in Locale_Content.Iterate loop
               if not Keys.Contains
                   (To_String
                      (To_Wide_String (Content_Maps.Key (Cursor)),
                       Substitution_Character))
               then
                  Keys.Append
                    (To_String
                       (To_Wide_String (Content_Maps.Key (Cursor)),
                        Substitution_Character));
               end if;
            end loop;
            Keys_Sorting.Sort (Keys);
         end return;
      end if;
   end Selected_Keys;

end Localize.Parser;
