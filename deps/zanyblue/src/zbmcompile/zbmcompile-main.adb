--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2017, Michael Rohan <mrohan@zanyblue.com>
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

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Command_Line;
with ZanyBlue.Text.Formatting;
with ZBMCompile.Messages;
with ZBMCompile.Message_Filter;

procedure ZBMCompile.Main is

   use Ada.Calendar;
   use Ada.Exceptions;
   use Ada.Command_Line;
   use ZanyBlue.Text;
   use ZanyBlue.Text.Formatting;
   use ZBMCompile;
   use ZBMCompile.Message_Filter;

   Usage   : exception;

   function Banner return Time;
   procedure Process_Command_Line (Options : in out Parameter_Set_Type);
   procedure Trailer (Start_Time : Time);
   --  Process the command line arguments.

   ------------
   -- Banner --
   ------------

   function Banner return Time is
      Arguments : Argument_List;
      Start_Time : constant Time := Clock;
   begin
      Append (Arguments, +ZanyBlue.Version_Major);
      Append (Arguments, +ZanyBlue.Version_Minor);
      Append (Arguments, +ZanyBlue.Version_Patch);
      Append (Arguments, +ZanyBlue.Revision);
      Append (Arguments, +Start_Time);
      Print_Line (ZBMCompile_Facility, "I00001", Arguments);
      Print_Line (ZBMCompile_Facility, "I00002", +ZanyBlue.Copyright_Year);
      return Start_Time;
   end Banner;

   --------------------------
   -- Process_Command_Line --
   --------------------------

   procedure Process_Command_Line (Options : in out Parameter_Set_Type) is

      Index         : Positive := 1;
      Seen_a_Option : Boolean := False;
      Seen_G_Option : Boolean := False;

      function Get_Option_Value (Ch : Character) return Natural;

      function Get_Option_Value (Ch : Character) return Wide_String;

      procedure Set_Accessor_Type (Type_Name : Wide_String;
                                   On_Off    : Boolean);

      ----------------------
      -- Get_Option_Value --
      ----------------------

      function Get_Option_Value (Ch : Character) return Wide_String is
      begin
         Index := Index + 1;
         if Index > Argument_Count then
            Raise_Exception (Usage'Identity, ZBMCompile_Facility, "E00001",
                             Argument0 => +Ch);
         end if;
         return Wide_From_UTF8 (Argument (Index));
      end Get_Option_Value;

      ----------------------
      -- Get_Option_Value --
      ----------------------

      function Get_Option_Value (Ch : Character) return Natural is
         Buffer : constant Wide_String := Get_Option_Value (Ch);
      begin
         return Natural'Wide_Value (Buffer);
      exception
      when Constraint_Error =>
         Raise_Exception (Usage'Identity, ZBMCompile_Facility, "E00024",
                          Argument0 => +Buffer);
      end Get_Option_Value;

      procedure Set_Accessor_Type (Type_Name : Wide_String;
                                   On_Off    : Boolean) is
      begin
         for I in Accessor_Types'Range loop
            if Type_Name = Accessor_Types (I).all then
               --  Valid accessor type name, set it and return
               Options.Set_Boolean ("accessor:" & Type_Name, On_Off);
               return;
            end if;
         end loop;
         Raise_Exception (Usage'Identity,
                          ZBMCompile_Facility, "E00025",
                          Argument0 => +Type_Name);
      end Set_Accessor_Type;

   begin
      Options.Set_Boolean ("accessor_comments",    True);
      Options.Set_Boolean ("ascii_only",           False);
      Options.Set_Boolean ("base_locale",          False);
      Options.Set_Boolean ("body_initialize",      False);
      Options.Set_Boolean ("debug",                False);
      Options.Set_Boolean ("disable_checks",       False);
      Options.Set_Boolean ("force",                False);
      Options.Set_Boolean ("generate_accessors",   False);
      Options.Set_Boolean ("optimize",             False);
      Options.Set_Boolean ("parameter_modes",      False);
      Options.Set_Boolean ("positional_elements",  False);
      Options.Set_Boolean ("quiet",                False);
      Options.Set_Boolean ("use_export_name",      False);
      Options.Set_Boolean ("usage",                False);
      Options.Set_Boolean ("verbose",              False);
      for I in Accessor_Types'Range loop
         Set_Accessor_Type (Accessor_Types (I).all, False);
      end loop;
      Options.Set_Integer ("comment_size",           Output_Comment_Size);
      Options.Set_Integer ("n_facilities",           0);
      Options.Set_Integer ("pool_size",              Output_Pool_Size);
      Options.Set_String ("extension",               "properties");
      Options.Set_String ("output_directory",        ".");
      Options.Set_String ("reference_locale",        "");
      Options.Set_String ("source_root_locale",      "");
      Options.Set_String ("invalid_ada_key_handler", "error");
      Options.Set_String ("cur_dir",                 ".");
      while Index <= Argument_Count loop
         declare
            Value : constant String := Argument (Index);
         begin
            if Value = "-a" then
               Options.Set_Boolean ("generate_accessors", True);
               for I in Accessor_Types'Range loop
                  Set_Accessor_Type (Accessor_Types (I).all, True);
               end loop;
               Seen_a_Option := True;
               if Seen_G_Option then
                  Raise_Exception (Usage'Identity,
                                   ZBMCompile_Facility, "E00026");
               end if;
            elsif Value = "-A" then
               Options.Set_Boolean ("ascii_only", True);
            elsif Value = "-B" then
               Options.Set_Boolean ("base_locale", True);
            elsif Value = "-C" then
               Options.Set_Boolean ("accessor_comments", False);
            elsif Value = "-d" then
               Options.Set_String ("cur_dir", Get_Option_Value ('d'));
            elsif Value = "-D" then
               Options.Set_Boolean ("debug", True);
            elsif Value = "-e" then
               Options.Set_String ("extension", Get_Option_Value ('e'));
            elsif Value = "-F" then
               Options.Set_Boolean ("force", True);
            elsif Value = "-g" then
               Options.Set_Boolean ("optimize", False);
            elsif Value = "-G" then
               Options.Set_Boolean ("generate_accessors", True);
               Set_Accessor_Type (Get_Option_Value ('G'), True);
               Seen_G_Option := True;
               if Seen_a_Option then
                  Raise_Exception (Usage'Identity,
                                   ZBMCompile_Facility, "E00026");
               end if;
            elsif Value = "-h" then
               Options.Set_Boolean ("usage", True);
            elsif Value = "-i" then
               Options.Set_Boolean ("body_initialize", True);
            elsif Value = "-L" then
               Options.Append ("locales", Get_Option_Value ('L'));
            elsif Value = "-m" then
               Options.Set_Boolean ("parameter_modes", True);
            elsif Value = "-o" then
               Options.Set_String ("output_directory", Get_Option_Value ('o'));
            elsif Value = "-O" then
               Options.Set_Boolean ("optimize", True);
            elsif Value = "-p" then
               Options.Set_Boolean ("positional_elements", True);
            elsif Value = "-q" then
               Options.Set_Boolean ("quiet", True);
            elsif Value = "-r" then
               Options.Set_String ("reference_locale", Get_Option_Value ('r'));
            elsif Value = "-s" then
               Options.Set_String ("source_root_locale",
                                   Get_Option_Value ('R'));
            elsif Value = "-S" then
               Options.Set_String ("stamp_file", Get_Option_Value ('S'));
            elsif Value = "-T" then
               declare
                  Target : constant Wide_String := Get_Option_Value ('T');
               begin
                  if not Options.Is_Defined (Target & "_size") then
                     Raise_Exception (Usage'Identity,
                                      ZBMCompile_Facility, "E00023",
                                      Argument0 => +Target);
                  end if;
                  Options.Set_Integer (Target & "_size",
                                       Get_Option_Value ('T'));
               end;
            elsif Value = "-u" then
               Options.Set_Boolean ("disable_checks", True);
            elsif Value = "-v" then
               Options.Set_Boolean ("verbose", True);
            elsif Value = "-x" then
               Options.Set_Boolean ("use_export_name", True);
               Options.Set_String ("export_name", Get_Option_Value ('x'));
            elsif Value = "-X" then
               declare
                  Handling : constant Wide_String := Get_Option_Value ('X');
               begin
                  if Handling = "ignore" then
                     Options.Set_String ("invalid_ada_key_handler", "ignore");
                  elsif Handling = "error" then
                     Options.Set_String ("invalid_ada_key_handler", "error");
                  else
                     Raise_Exception (Usage'Identity,
                                      ZBMCompile_Facility, "E00029",
                                      Argument0 => +Handling);
                  end if;
               end;
            elsif Value'Length > 0
              and then Value (Value'First) = '-'
            then
               Raise_Exception (Usage'Identity, ZBMCompile_Facility, "E00020",
                                Argument0 => +Value);
            elsif not Options.Is_Defined ("package") then
               Options.Set_String ("package", Wide_From_UTF8 (Value));
            else
               Options.Append ("mesg_dirs", Options.Get_String ("cur_dir"));
               Options.Append ("facilities", Wide_From_UTF8 (Value));
               Options.Increment ("n_facilities");
            end if;
         end;
         Index := Index + 1;
      end loop;
      --  Consisteny check on the command line arguments given
      if Options.Get_Boolean ("usage") then
         --  Usage request, no consistency checks needed
         return;
      end if;
      if not Options.Is_Defined ("package") then
         Raise_Exception (Usage'Identity,
                          ZBMCompile_Facility, "E00006");
      end if;
      if Options.Get_Integer ("n_facilities") = 0 then
         Raise_Exception (Usage'Identity,
                          ZBMCompile_Facility, "E00007");
      end if;
      if Options.Get_Boolean ("disable_checks")
        and then Options.Get_Boolean ("generate_accessors")
      then
         Raise_Exception (Usage'Identity,
                          ZBMCompile_Facility, "E00016");
      end if;
      if not Options.Get_Boolean ("base_locale")
        and then not Options.Is_Defined ("locales")
      then
         Options.Append ("locales", "");
      end if;
      if Options.Get_Boolean ("debug") then
         Options.Dump (Standard_Output);
      end if;
      if Options.Get_Boolean ("debug") then
         Filters.Output_Level := Debug;
      elsif Options.Get_Boolean ("verbose") then
         Filters.Output_Level := Verbose;
      elsif Options.Get_Boolean ("quiet") then
         Filters.Output_Level := Quiet;
      else
         Filters.Output_Level := Normal;
      end if;
   exception
   when E : others =>
      Raise_Exception (Usage'Identity, ZBMCompile_Facility, "E00022",
                       Argument0 => +E);
   end Process_Command_Line;

   -------------
   -- Trailer --
   -------------

   procedure Trailer (Start_Time : Time) is
      Now : constant Time := Clock;
      Elapsed : constant Duration := Now - Start_Time;
   begin
      Print_Line (ZBMCompile_Facility, "I00003", +Now, +Elapsed);
   end Trailer;

   Start_Time   : Ada.Calendar.Time;
   Options      : Parameter_Set_Type;

begin
   ZBMCompile.Messages.Initialize;
   Set_Filter (Filters'Access);
   Disable_Wide_IO;
   Options.Set_Name ("OPTIONS");
   Process_Command_Line (Options);
   Start_Time := Banner;
   if Options.Get_Boolean ("usage") then
      Print_Line (ZBMCompile_Facility, "E00002");
   elsif not ZBMCompile.Process (Options) then
      Set_Exit_Status (Failure);
   end if;
   Trailer (Start_Time);
exception
when E : Usage =>
   Start_Time := Banner;
   Print_Line (ZBMCompile_Facility, "E00008", +Exception_Message (E));
   Print_Line (ZBMCompile_Facility, "E00002");
   Trailer (Start_Time);
   Set_Exit_Status (Failure);
end ZBMCompile.Main;
