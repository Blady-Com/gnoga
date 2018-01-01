--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2016, Michael Rohan <mrohan@zanyblue.com>
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

--
--  Example using Codecs type to convert an input file with a source
--  encoding to an output file with a target encoding.
--

with Ada.Directories;
with Ada.Direct_IO;
with Ada.Strings.Wide_Unbounded;
with Messages.Econv_Prints;
with Messages.Econv_Exceptions;
with ZanyBlue.Wide_Command_Line;
with ZanyBlue.Text.Codecs;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Formatting;

procedure ZBX_Econv is

   use Ada.Directories;
   use Ada.Strings.Wide_Unbounded;
   use Messages.Econv_Prints;
   use Messages.Econv_Exceptions;
   use ZanyBlue.Text;
   use ZanyBlue.Text.Codecs;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Wide_Command_Line;
   use ZanyBlue.Text.Formatting;

   procedure Process_Command_Line;

   Input_Codecs : Codecs_Type := Current_Locale.Codecs;
   Input_Name : Unbounded_Wide_String;
   Output_Codecs : Codecs_Type := Current_Locale.Codecs;
   Output_Name : Unbounded_Wide_String;
   Usage_Error, Help_Error : exception;

   function Read_File (File_Name : Unbounded_Wide_String) return String;
   procedure Write_File (File_Name : Unbounded_Wide_String;
                         Data      : String);
   function Get_Opt_Value (Index : Positive; Option : Wide_String)
      return Unbounded_Wide_String;
   function Get_Opt_Value (Index : Positive; Option : Wide_String)
      return Wide_String;

   -------------------
   -- Get_Opt_Value --
   -------------------

   function Get_Opt_Value (Index : Positive; Option : Wide_String)
      return Wide_String is
   begin
      return To_Wide_String (Get_Opt_Value (Index, Option));
   end Get_Opt_Value;

   -------------------
   -- Get_Opt_Value --
   -------------------

   function Get_Opt_Value (Index : Positive; Option : Wide_String)
      return Unbounded_Wide_String
   is
   begin
      if Index <= Wide_Argument_Count then
         return To_Unbounded_Wide_String (Wide_Argument (Index));
      else
         Raise_00006 (Usage_Error'Identity, +Option);
      end if;
   end Get_Opt_Value;

   --------------------------
   -- Process_Command_Line --
   --------------------------

   procedure Process_Command_Line is
      Index : Positive := 1;
   begin
      while Index <= Wide_Argument_Count loop
         declare
            Option : constant Wide_String := Wide_Argument (Index);
         begin
            if Option = "-f" then
               Index := Index + 1;
               Input_Codecs := Make_Codecs (Get_Opt_Value (Index, "-f"));
            elsif Option = "-t" then
               Index := Index + 1;
               Output_Codecs := Make_Codecs (Get_Opt_Value (Index, "-t"));
            elsif Input_Name = Null_Unbounded_Wide_String then
               Input_Name := To_Unbounded_Wide_String (Wide_Argument (Index));
            elsif Output_Name = Null_Unbounded_Wide_String then
               Output_Name := To_Unbounded_Wide_String (Wide_Argument (Index));
            elsif Option = "-h" then
               raise Help_Error;
            else
               raise Usage_Error;
            end if;
         end;
         Index := Index + 1;
      end loop;
      if Input_Name = Null_Unbounded_Wide_String then
         Print_00007;
         raise Usage_Error;
      end if;
      if Output_Name = Null_Unbounded_Wide_String then
         Print_00008;
         raise Usage_Error;
      end if;
   end Process_Command_Line;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (File_Name : Unbounded_Wide_String) return String is
      File_Name_S : constant String := To_UTF8 (To_Wide_String (File_Name));
      File_Size : constant Natural := Natural (Size (File_Name_S));
      subtype Element_Type is String (1 .. File_Size);
      package File_IO is new Ada.Direct_IO (Element_Type);
      Input : File_IO.File_Type;
      Result : Element_Type;
   begin
      File_IO.Open (Input, File_IO.In_File, File_Name_S);
      File_IO.Read (Input, Result);
      File_IO.Close (Input);
      return Result;
   end Read_File;

   ----------------
   -- Write_File --
   ----------------

   procedure Write_File (File_Name : Unbounded_Wide_String;
                         Data      : String) is
      File_Name_S : constant String := To_UTF8 (To_Wide_String (File_Name));
      subtype Element_Type is String (Data'Range);
      package File_IO is new Ada.Direct_IO (Element_Type);
      Output : File_IO.File_Type;
   begin
      File_IO.Create (Output, File_IO.Out_File, File_Name_S);
      File_IO.Write (Output, Data);
      File_IO.Close (Output);
   end Write_File;

begin
   Print_00001 (+ZanyBlue.Version_Major, +ZanyBlue.Version_Minor,
                +ZanyBlue.Version_Patch);
   Print_00002;
   --  Raise exceptions for unknown encodings and invalid data
   --  Set_Unsupported_Encoding_Action (Raise_Exception);
   Process_Command_Line;
   --  Print_Argument (+Input_Codecs.Name);
   --  Print_Argument (+Input_Name);
   --  Print_Argument (+Output_Codecs.Name);
   --  Print_Argument (+Output_Name);
   Set_Unicode_Encode_Action (Raise_Exception);
   Set_Unicode_Decode_Action (Raise_Exception);
   Write_File (Output_Name,
       Output_Codecs.Encode (Input_Codecs.Decode (Read_File (Input_Name))));
exception
when Help_Error =>
   Print_00004;
when Usage_Error =>
   Print_00005;
end ZBX_Econv;
