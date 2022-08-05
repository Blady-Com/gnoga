--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Parser_JSON                            Luebeck            --
--  Test program                                   Autumn, 2019       --
--                                                                    --
--                                Last revision :  10:13 29 Nov 2020  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of  the  GNU  Library  General  Public  --
--  License  as  published  by  the Free Software Foundation; either  --
--  version 2 of the License, or (at your option) any later version.  --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--                                                                    --
--  This  library is distributed in the hope that it will be useful,  --
--  but WITHOUT ANY WARRANTY; without even the implied  warranty  of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  Library General Public License for more details.                  --
--                                                                    --
--  You  should  have  received  a  copy  of the GNU Library General  --
--  Public  License  along  with  this library; if not, write to the  --
--  Free Software Foundation, Inc.,  675  Mass  Ave,  Cambridge,  MA  --
--  02139, USA.                                                       --
--____________________________________________________________________--

with Ada.Calendar;                        use Ada.Calendar;
with Ada.Characters.Latin_1;              use Ada.Characters.Latin_1;
with Ada.Exceptions;                      use Ada.Exceptions;
with Ada.Text_IO;                         use Ada.Text_IO;
with Strings_Edit.Streams;                use Strings_Edit.Streams;

with Parsers.JSON.Multiline_Source;
with Parsers.JSON.String_Source;
with Parsers.Multiline_Source.Stream_IO;
with Parsers.Multiline_Source.Text_IO;
with Parsers.String_Source;
with Stack_Storage;
with Strings_Edit.Floats;
with Strings_Edit.Streams;
-- with GNAT.Exception_Traces;

procedure Test_JSON is
   use Parsers.JSON;
   CRLF  : constant String := Character'Val (13) & Character'Val (10);
   Arena : aliased Stack_Storage.Pool (200, 512);
begin
-- GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
   declare
      use Parsers.JSON.String_Source;
      use Parsers.String_Source;
      Text : aliased String :=
             "{"                                           & CRLF &
             "  ""args"": {},"                             & CRLF &
             "  ""headers"": {"                            & CRLF &
             "    ""Accept"": ""application/json"","       & CRLF &
             "    ""Host"": ""172.20.21.11:8090"","        & CRLF &
             "    ""User-Agent"": ""curl/7.65.0"""         & CRLF &
             "  },"                                        & CRLF &
             "  ""origin"": ""172.32.1.64"","              & CRLF &
             "  ""url"": ""http://172.20.21.11:8090/get""" & CRLF &
             "}" & CRLF;
      Input : aliased Source (Text'Access);
      Data  : constant JSON_Value := Parse (Input'Access, Arena'Access);
   begin
      Put_Line ("Parsed:" & Image (Data));
   end;
   declare
      use Parsers.JSON.String_Source;
      use Parsers.String_Source;
      Text : aliased String :=
             "{""empty"": [" & CRLF &
                "]"          &
             "}"             & CRLF;
      Input : aliased Source (Text'Access);
      Data  : constant JSON_Value := Parse (Input'Access, Arena'Access);
   begin
      Put_Line ("Parsed:" & Image (Data));
   end;
   declare
      use Parsers.JSON.String_Source;
      use Parsers.String_Source;
      Text : aliased String :=
             "{""empty"": {" & CRLF &
                "}"          &
             "}"             & CRLF;
      Input : aliased Source (Text'Access);
      Data  : constant JSON_Value := Parse (Input'Access, Arena'Access);
   begin
      Put_Line ("Parsed:" & Image (Data));
   end;
   declare
      use Parsers.JSON.String_Source;
      use Parsers.String_Source;
      Text  : aliased String := "123";
      Input : aliased Source (Text'Access);
      Data  : constant JSON_Value := Parse (Input'Access, Arena'Access);
   begin
      Put_Line ("Parsed:" & Image (Data));
   end;
   declare
      use Parsers.JSON.String_Source;
      use Parsers.String_Source;
      Text : aliased String :=
             "{"                         &
                """id"": 1,"             &
                """name"": ""Foo"","     &
                """price"": 123,"        &
                """tags"": ["            &
                   """Bar"","            &
                   """Eek"""             &
                   "],"                  &
                """stock"": {"           &
                   """warehouse"": 300," &
                   """retail"": 20"      &
                   "}"                   &
             "}";
      Input : aliased Source (Text'Access);
      Data  : constant JSON_Value := Parse (Input'Access, Arena'Access);
   begin
      Put_Line ("Parsed:" & Image (Data));
   end;
   Stack_Storage.Deallocate_All (Arena);
   declare
      use Parsers.JSON.String_Source;
      use Parsers.String_Source;
      Text  : aliased String := "{ ""face"": ""\uD83D\uDE02"" }";
      Input : aliased Source (Text'Access);
      Data  : constant JSON_Value := Parse (Input'Access, Arena'Access);
   begin
      Put_Line ("Parsed:" & Image (Data, True));
   end;
   Stack_Storage.Deallocate_All (Arena);
   declare
      use Parsers.JSON.Multiline_Source;
      use Parsers.Multiline_Source.Stream_IO;
      use Strings_Edit.Streams;
      Text  : constant String :=
              "{"                                          & CRLF &
              "  ""first name"": ""John"","                & CRLF &
              "  ""last name"": ""Smith"","                & CRLF &
              "  ""age"": 25,"                             & CRLF &
              "  ""address"": {"                           & CRLF &
              "    ""street address"": ""21 2nd Street""," & CRLF &
              "    ""city"": ""New York"","                & CRLF &
              "    ""state"": ""NY"","                     & CRLF &
              "    ""postal code"": ""10021"""             & CRLF &
              "  },"                                       & CRLF &
              "  ""phone numbers"": ["                     & CRLF &
              "    {"                                      & CRLF &
              "      ""type"": ""home"","                  & CRLF &
              "      ""number"": ""212 555-1234"""         & CRLF &
              "    },"                                     & CRLF &
              "    {"                                      & CRLF &
              "      ""type"": ""fax"","                   & CRLF &
              "      ""number"": ""646 555-4567"""         & CRLF &
              "    }"                                      & CRLF &
              "  ],"                                       & CRLF &
              "  ""sex"": {"                               & CRLF &
              "    ""type"": ""male"""                     & CRLF &
              "  }"                                        & CRLF &
              "}";
      Content : aliased String_Stream (500);
   begin
      Set (Content, Text); -- Note, content is set before Input declared
      declare
         Input : aliased Source (Content'Access);
      begin
         declare
            Data : constant JSON_Value :=
                            Parse (Input'Access, Arena'Access);
         begin
            Put_Line ("Parsed:" & Image (Data, True));
         end;
      end;
   end;
   Stack_Storage.Deallocate_All (Arena);
   declare
      use Parsers.JSON.Multiline_Source;
      use Parsers.Multiline_Source.Text_IO;
      File : aliased File_Type;
   begin
      Create (File, Out_File, "test.json");
      Put_Line (File, "[");
      Put_Line (File, "  {");
      Put_Line (File, "     ""precision"": ""zip"",");
      Put_Line (File, "     ""Latitude"":  37.7668,");
      Put_Line (File, "     ""Longitude"": -122.3959,");
      Put_Line (File, "     ""Address"":   """",");
      Put_Line (File, "     ""City"":      ""SAN FRANCISCO"",");
      Put_Line (File, "     ""State"":     ""CA"",");
      Put_Line (File, "     ""Zip"":       ""94107"",");
      Put_Line (File, "     ""Country"":   ""US""");
      Put_Line (File, "  },");
      Put_Line (File, "  {");
      Put_Line (File, "     ""precision"": ""zip"",");
      Put_Line (File, "     ""Latitude"":  37.371991,");
      Put_Line (File, "     ""Longitude"": -122.026020,");
      Put_Line (File, "     ""Address"":   ""C:\\Program Files"",");
      Put_Line (File, "     ""City"":      ""SUNNYVALE"",");
      Put_Line (File, "     ""State"":     ""CA"",");
      Put_Line (File, "     ""Zip"":       ""94085"",");
      Put_Line (File, "     ""Country"":   ""US""");
      Put_Line (File, "  }");
      Put_Line (File, "]");
      Close (File);
      Open (File, In_File, "test.json");
      declare
         Input : aliased Source (File'Access);
         Data  : constant JSON_Value :=
                          Parse (Input'Access, Arena'Access);
      begin
         Put_Line ("Parsed:" & Image (Data, True));
      end;
      Close (File);
   end;
--     declare
--        use Parsers.JSON.Multiline_Source;
--        use Parsers.Multiline_Source.Text_IO;
--        use Strings_Edit.Floats;
--        File  : aliased File_Type;
--        Start : Time;
--     begin
--        Open (File, In_File, "c:/temp/1.json");
--        Start := Clock;
--        declare
--           Input : aliased Source (File'Access);
--           Data  : constant JSON_Value :=
--                            Parse (Input'Access, Arena'Access);
--        begin
--           Put_Line
--           (  "Parsed: "
--           &  Image (Float (Clock - Start), AbsSmall => -2)
--           &  "s"
--           );
--        end;
--        Close (File);
--     end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_JSON;
