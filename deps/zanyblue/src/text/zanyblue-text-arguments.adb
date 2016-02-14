--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, Michael Rohan <mrohan@zanyblue.com>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--    * Redistributions of source code must retain the above copyright
--      notice, this list of conditions and the following disclaimer.
--
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--
--    * Neither the name of ZanyBlue nor the names of its contributors may
--      be used to endorse or promote products derived from this software
--      without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
--  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

with Ada.Strings.Wide_Fixed;
with ZanyBlue.Text.Buffer;

package body ZanyBlue.Text.Arguments is

   Missing_Start : constant Wide_Character := '⁅';   --  U+2045
   Missing_End   : constant Wide_Character := '⁆';   --  U+2046

   type Format_Map_Type is
      record
         Format_Name   : Constant_String_Access;
         Category_Name : Constant_String_Access;
      end record;

   type Format_Map_List is array (Positive range <>) of Format_Map_Type;

   Format_Mappings : constant Format_Map_List := (
      (Any_Format_Name'Access,       Any_Category_Name'Access),
      (Boolean_Format_Name'Access,   Boolean_Category_Name'Access),
      (Character_Format_Name'Access, Character_Category_Name'Access),
      (Date_Format_Name'Access,      Date_Category_Name'Access),
      (Datetime_Format_Name'Access,  Datetime_Category_Name'Access),
      (Duration_Format_Name'Access,  Duration_Category_Name'Access),
      (Enum_Format_Name'Access,      Enumeration_Category_Name'Access),
      (Exception_Format_Name'Access, Exception_Category_Name'Access),
      (Fixed_Format_Name'Access,     Fixed_Category_Name'Access),
      (Float_Format_Name'Access,     Float_Category_Name'Access),
      (Integer_Format_Name'Access,   Integer_Category_Name'Access),
      (Modular_Format_Name'Access,   Modular_Category_Name'Access),
      (Number_Format_Name'Access,    Number_Category_Name'Access),
      (Real_Format_Name'Access,      Real_Category_Name'Access),
      (String_Format_Name'Access,    String_Category_Name'Access),
      (Time_Format_Name'Access,      Time_Category_Name'Access));

   function Type_Name_Search (Type_Name   : Wide_String;
                              Return_Name : Boolean) return Wide_String;
   --  Search the type name mapping table for a matching prefix name and
   --  return either the name matched or the associated category.

   ------------
   -- Append --
   ------------

   procedure Append (List      : in out Argument_List;
                     Argument  : Argument_Type'Class) is
   begin
      List.Contents.Append (Argument);
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear (List : in out Argument_List) is
   begin
      List.Contents.Clear;
   end Clear;

   ------------
   -- Format --
   ------------

   function Format (List          : Argument_List;
                    Position      : Natural;
                    Message       : Wide_String;
                    Format_String : Wide_String;
                    Locale        : Locale_Type;
                    Raise_Errors  : Boolean;
                    Error_Handler : access Error_Handler_Type'Class
                                       := Standard_Error_Handler'Access)
      return Wide_String is

      use Ada.Strings.Wide_Fixed;
      use ZanyBlue.Text.Buffer;

      Type_Name      : constant Wide_String :=
                                   Type_Name_Prefix (Format_String);
      Template_Start : Positive := Format_String'First + Type_Name'Length;
      Template_End   : Natural  := Format_String'Last;
      Format_Locale  : Locale_Type := Locale;
      Buffer         : Buffer_Type;

   begin
      if Head (Format_String (Template_Start .. Template_End), 1) = "," then
         Template_Start := Template_Start + 1;
      end if;
      if Tail (Format_String, 1) = "*" then
         Template_End := Format_String'Last - 1;
         Format_Locale := Root_Locale;
      end if;
      if Position >= Natural (List.Contents.Length) then
         Error_Handler.Missing_Argument (Message, Position, Type_Name,
                                         Raise_Errors);
         --  If error handling did not raise an exception, then format as
         --  as a placeholder for the missing argument.
         Add (Buffer, Missing_Start);
         Accumulate (Buffer, Position, Root_Locale);
         Add (Buffer, Missing_End);
         return To_String (Buffer);
      else
         return List.Contents.Element (Position).Format (
                   Type_Name,
                   Format_String (Template_Start .. Template_End),
                   Format_Locale);
      end if;
   end Format;

   ------------
   -- Length --
   ------------

   function Length (List : Argument_List) return Natural is
   begin
      return Natural (List.Contents.Length);
   end Length;

   ----------------------
   -- Type_Name_Prefix --
   ----------------------

   function Type_Name_Prefix (Format_String : Wide_String) return Wide_String
   is
   begin
      return Type_Name_Search (Format_String, True);
   end Type_Name_Prefix;

   ----------------------
   -- Type_Name_Search --
   ----------------------

   function Type_Name_Search (Type_Name   : Wide_String;
                              Return_Name : Boolean) return Wide_String is
      use Ada.Strings.Wide_Fixed;
      Left             : Positive := Format_Mappings'First;
      Right            : Positive := Format_Mappings'Last + 1;
      Center           : Positive;
      Candidate        : Constant_String_Access;
      Candidate_Length : Natural;
   begin
      if Type_Name < Format_Mappings (Left).Format_Name.all then
         return "";
      end if;
      loop
         Center := Left + (Right - Left) / 2;
         Candidate := Format_Mappings (Center).Format_Name;
         Candidate_Length := Candidate.all'Length;
         if Head (Type_Name, Candidate_Length) = Candidate.all
           and then (Type_Name'Length = Candidate_Length
                     or else Type_Name (Candidate_Length + 1) = ',')
         then
            if Return_Name then
               return Format_Mappings (Center).Format_Name.all;
            else
               return Format_Mappings (Center).Category_Name.all;
            end if;
         end if;

         if Right - Left <= 1 then
            return "";
         elsif Type_Name < Candidate.all then
            Right := Center;
         else
            Left := Center;
         end if;
      end loop;
   end Type_Name_Search;

   ---------------------------
   -- Type_Name_To_Category --
   ---------------------------

   function Type_Name_To_Category (Type_Name : Wide_String) return Wide_String
   is
   begin
      return Type_Name_Search (Type_Name, False);
   end Type_Name_To_Category;

end ZanyBlue.Text.Arguments;
