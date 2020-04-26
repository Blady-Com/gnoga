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

with Ada.Containers.Vectors;
with Ada.Direct_IO;
with Ada.Integer_Text_IO;
with Ada.Characters.Conversions;
with Ada.Characters.Latin_1;
with Ada.Characters.Wide_Latin_1;
with Ada.Strings.Fixed;

package body Localize.Parser is

   use Ada.Strings.Wide_Unbounded;
   use Ada.Characters.Conversions;
   use type Content_Maps.Cursor;

   Substitution_Character : constant Character := '?';

   procedure Parse_Strings_File
     (File_Name : String; Content : out Property_List)
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

      procedure Append (C : Wide_Character) is
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
               when 't' =>
                  Append (Ada.Characters.Wide_Latin_1.HT);
               when 'n' =>
                  Append (Ada.Characters.Wide_Latin_1.LF);
               when Ada.Characters.Wide_Latin_1.LF =>
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

   procedure Write_Strings_File (File_Name : String; Content : Property_List)
   is
      package Strings_IO is new Ada.Direct_IO (Wide_Character);

      BOM_LE : constant Wide_Character := Wide_Character'Val (16#FEFF#);

      Raw_File : Strings_IO.File_Type;

      procedure Escaped_Write (Str : Unbounded_Wide_String) is
      begin
         for I in 1 .. Length (Str) loop
            if Element (Str, I) = Ada.Characters.Wide_Latin_1.HT then
               Strings_IO.Write (Raw_File, '\');
               Strings_IO.Write (Raw_File, 't');
            elsif Element (Str, I) = Ada.Characters.Wide_Latin_1.LF then
               Strings_IO.Write (Raw_File, '\');
               Strings_IO.Write (Raw_File, 'n');
            elsif Element (Str, I) in '"' | '\' then
               Strings_IO.Write (Raw_File, '\');
               Strings_IO.Write (Raw_File, Element (Str, I));
            else
               Strings_IO.Write (Raw_File, Element (Str, I));
            end if;
         end loop;
      end Escaped_Write;

   begin
      Strings_IO.Create (Raw_File, Strings_IO.Out_File, File_Name);
      Strings_IO.Write (Raw_File, BOM_LE);
      for C in Content.Iterate loop
         if Content_Maps.Element (C).Comment /= Null_Unbounded_Wide_String then
            Strings_IO.Write (Raw_File, '/');
            Strings_IO.Write (Raw_File, '*');
            Escaped_Write (Content_Maps.Element (C).Comment);
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

   procedure Parse_Properties_File
     (File_Name : String; Content : out Property_List)
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
               when ' ' | ':' | '=' | '#' | '!' | '\' =>
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
     (File_Name : String; Content : Property_List)
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
            elsif Element (Str, I) in '=' | ':' | '#' | '!' | '\' and
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

   procedure Read (Properties : out Property_List; File_Name : String) is
   begin
      if Ada.Strings.Fixed.Tail (File_Name, 8, ' ') = ".strings" then
         Parse_Strings_File (File_Name, Properties);
      end if;
      if Ada.Strings.Fixed.Tail (File_Name, 11, ' ') = ".properties" then
         Parse_Properties_File (File_Name, Properties);
      end if;
   exception
      when others =>
         Properties.Clear;
   end Read;

   procedure Write (Properties : Property_List; File_Name : String) is
   begin
      if Ada.Strings.Fixed.Tail (File_Name, 8, ' ') = ".strings" then
         Write_Strings_File (File_Name, Properties);
      end if;
      if Ada.Strings.Fixed.Tail (File_Name, 11, ' ') = ".properties" then
         Write_Properties_File (File_Name, Properties);
      end if;
   end Write;

   function Keys (Properties : Property_List) return Key_List is
   begin
      return Result_Key_List : Key_List do
         for Index in Properties.Iterate loop
            Result_Key_List.Append
              (To_String
                 (To_Wide_String (Content_Maps.Key (Index)),
                  Substitution_Character));
         end loop;
      end return;
   end Keys;

   function Contains
     (Properties : Property_List; Key : String) return Boolean is
     (Properties.Contains (To_Unbounded_Wide_String (To_Wide_String (Key))));

   procedure Insert (Properties : in out Property_List; Key : String) is
   begin
      Properties.Include
        (To_Unbounded_Wide_String (To_Wide_String (Key)),
         (Null_Unbounded_Wide_String, Null_Unbounded_Wide_String, False));
   end Insert;

   procedure Delete (Properties : in out Property_List; Key : String) is
      UWS_Key : constant Unbounded_Wide_String :=
        To_Unbounded_Wide_String (To_Wide_String (Key));
   begin
      if Properties.Find (UWS_Key) /= Content_Maps.No_Element then
         Properties.Delete (To_Unbounded_Wide_String (To_Wide_String (Key)));
      end if;
   end Delete;

   procedure Rename (Properties : in out Property_List; From, To : String) is
   begin
      Properties.Include
        (To_Unbounded_Wide_String (To_Wide_String (To)),
         Properties.Element
           (To_Unbounded_Wide_String (To_Wide_String (From))));
      Properties.Delete (To_Unbounded_Wide_String (To_Wide_String (From)));
   end Rename;

   function Text (Properties : Property_List; Key : String) return String is
      UWS_Key : constant Unbounded_Wide_String :=
        To_Unbounded_Wide_String (To_Wide_String (Key));
   begin
      if Properties.Find (UWS_Key) = Content_Maps.No_Element then
         return "";
      else
         return To_String
             (To_Wide_String
                (Properties.Element
                   (To_Unbounded_Wide_String (To_Wide_String (Key)))
                   .Text),
              Substitution_Character);
      end if;
   end Text;

   procedure Text
     (Properties : in out Property_List; Key : String; Value : String)
   is
      UWS_Key : constant Unbounded_Wide_String :=
        To_Unbounded_Wide_String (To_Wide_String (Key));
      Cursor  : constant Content_Maps.Cursor := Properties.Find (UWS_Key);
      Element : Property_Type;
   begin
      if Cursor /= Content_Maps.No_Element then
         Element      := Content_Maps.Element (Cursor);
         Element.Text := To_Unbounded_Wide_String (To_Wide_String (Value));
         if Element.Text /= Content_Maps.Element (Cursor).Text then
            Element.Modified := True;
            Properties.Replace_Element (Cursor, Element);
         end if;
      end if;
   end Text;

   function Comment (Properties : Property_List; Key : String) return String is
      UWS_Key : constant Unbounded_Wide_String :=
        To_Unbounded_Wide_String (To_Wide_String (Key));
   begin
      if Properties.Find (UWS_Key) = Content_Maps.No_Element then
         return "";
      else
         return To_String
             (To_Wide_String (Properties.Element (UWS_Key).Comment),
              Substitution_Character);
      end if;
   end Comment;

   procedure Comment
     (Properties : in out Property_List; Key : String; Value : String)
   is
      UWS_Key : constant Unbounded_Wide_String :=
        To_Unbounded_Wide_String (To_Wide_String (Key));
      Cursor  : constant Content_Maps.Cursor := Properties.Find (UWS_Key);
      Element : Property_Type;
   begin
      if Cursor /= Content_Maps.No_Element then
         Element         := Content_Maps.Element (Cursor);
         Element.Comment := To_Unbounded_Wide_String (To_Wide_String (Value));
         if Element.Comment /= Content_Maps.Element (Cursor).Comment then
            Element.Modified := True;
            Properties.Replace_Element (Cursor, Element);
         end if;
      end if;
   end Comment;

   function Modified (Properties : Property_List; Key : String) return Boolean
   is
      UWS_Key : constant Unbounded_Wide_String :=
        To_Unbounded_Wide_String (To_Wide_String (Key));
   begin
      if Properties.Find (UWS_Key) = Content_Maps.No_Element then
         return False;
      else
         return Properties.Element
             (To_Unbounded_Wide_String (To_Wide_String (Key)))
             .Modified;
      end if;
   end Modified;

   procedure Reset_Modified_Indicators (Properties : in out Property_List) is
   begin
      for Element of Properties loop
         Element.Modified := False;
      end loop;
   end Reset_Modified_Indicators;

   function Selected_Keys
     (Master, Locale : Property_List; Pattern : String) return Key_List
   is
      package Keys_Sorting is new Lists.Generic_Sorting;
   begin
      if Pattern /= "" then
         return Result_Key_List : Key_List do
            for Cursor in Master.Iterate loop
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
                 not Result_Key_List.Contains
                   (To_String
                      (To_Wide_String (Content_Maps.Key (Cursor)),
                       Substitution_Character))
               then
                  Result_Key_List.Append
                    (To_String
                       (To_Wide_String (Content_Maps.Key (Cursor)),
                        Substitution_Character));
               end if;
            end loop;
            for Cursor in Locale.Iterate loop
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
                 not Result_Key_List.Contains
                   (To_String
                      (To_Wide_String (Content_Maps.Key (Cursor)),
                       Substitution_Character))
               then
                  Result_Key_List.Append
                    (To_String
                       (To_Wide_String (Content_Maps.Key (Cursor)),
                        Substitution_Character));
               end if;
            end loop;
         end return;
      else
         return Result_Key_List : Key_List := Keys (Master) do
            for Cursor in Locale.Iterate loop
               if not Result_Key_List.Contains
                   (To_String
                      (To_Wide_String (Content_Maps.Key (Cursor)),
                       Substitution_Character))
               then
                  Result_Key_List.Append
                    (To_String
                       (To_Wide_String (Content_Maps.Key (Cursor)),
                        Substitution_Character));
               end if;
            end loop;
            Keys_Sorting.Sort (Result_Key_List);
         end return;
      end if;
   end Selected_Keys;

end Localize.Parser;
