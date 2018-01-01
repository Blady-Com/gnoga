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

with Ada.Text_IO;
with ZanyBlue.OS;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Catalogs;
with ZanyBlue.Text.Arguments;
with ZanyBlue.Text.Formatting;

with ZBTest_Messages.ZBTest_Prints;
with ZBTest_Messages.Commands_Prints;

pragma Elaborate_All (ZanyBlue.OS);
pragma Elaborate_All (ZBTest_Messages.ZBTest_Prints);

package body ZBTest.Commands is

   use Ada.Text_IO;
   use ZanyBlue.OS;
   use ZanyBlue.Text;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Catalogs;
   use ZanyBlue.Text.Arguments;
   use ZanyBlue.Text.Formatting;
   use ZBTest_Messages.ZBTest_Prints;
   use ZBTest_Messages.Commands_Prints;

   type Simple_Message_Printer is
      access procedure (Destination : File_Type    := Current_Output;
                        With_NL     : Boolean      := True;
                        Locale      : Locale_Type  := Current_Locale;
                        Catalog     : Catalog_Type := Standard_Catalog);

   type Argument_Message_Printer is
      access procedure (Argument0   : Any_Category_Type'Class;
                        Destination : File_Type    := Current_Output;
                        With_NL     : Boolean      := True;
                        Locale      : Locale_Type  := Current_Locale;
                        Catalog     : Catalog_Type := Standard_Catalog);

   type Command_Definition is
      record
         Name           : Wide_String_Access;
         Implementation : Command_Type;
         Usage          : Simple_Message_Printer;
         Summary        : Argument_Message_Printer;
         Help           : Simple_Message_Printer;
      end record;
   --  Information on a command.

   type Command_List is array (Natural range <>) of Command_Definition;
   --  List of known commands

   procedure Append_Command (State   : in out State_Type;
                             Args    : List_Type);
   procedure Begin_Command (State   : in out State_Type;
                            Args    : List_Type);

   procedure Compare_Command (State   : in out State_Type;
                              Args    : List_Type);
   procedure Copy_Command (State   : in out State_Type;
                           Args    : List_Type);
   procedure Delenv_Command (State   : in out State_Type;
                             Args    : List_Type);
   procedure Delete_Command (State   : in out State_Type;
                             Args    : List_Type);
   procedure Desc_Command (State   : in out State_Type;
                           Args    : List_Type);
   procedure Dump_Command (State   : in out State_Type;
                           Args    : List_Type);
   procedure Echo_Command (State   : in out State_Type;
                           Args    : List_Type);
   procedure End_Command (State   : in out State_Type;
                          Args    : List_Type);
   procedure Execute_Command (State   : in out State_Type;
                              Args    : List_Type);
   procedure Exit_Command (State   : in out State_Type;
                           Args    : List_Type);
   procedure Filestat_Command (State   : in out State_Type;
                               Args    : List_Type);
   procedure Getenv_Command (State   : in out State_Type;
                             Args    : List_Type);
   procedure Help_Command (State   : in out State_Type;
                           Args    : List_Type);
   procedure Incr_Command (State   : in out State_Type;
                           Args    : List_Type);
   procedure Mkdir_Command (State   : in out State_Type;
                            Args    : List_Type);
   procedure Noop_Command (State   : in out State_Type;
                           Args    : List_Type);
   procedure Prepend_Command (State   : in out State_Type;
                              Args    : List_Type);
   procedure Print_Command (State   : in out State_Type;
                            Args    : List_Type);
   procedure Rename_Command (State   : in out State_Type;
                             Args    : List_Type);
   procedure Run_Command (State   : in out State_Type;
                          Args    : List_Type);
   procedure Set_Command (State   : in out State_Type;
                          Args    : List_Type);
   procedure Setenv_Command (State   : in out State_Type;
                             Args    : List_Type);
   procedure Unknown_Command (State   : in out State_Type;
                              Args    : List_Type);
   procedure Which_Command (State   : in out State_Type;
                            Args    : List_Type);

   Command_Table : constant Command_List := (
                 (Name           => new Wide_String'("?"),
                  Implementation => Unknown_Command'Access,
                  Usage          => Print_20000'Access,
                  Summary        => Print_30000'Access,
                  Help           => Print_40000'Access),

                 (Name           => new Wide_String'("append"),
                  Implementation => Append_Command'Access,
                  Usage          => Print_Append_Usage'Access,
                  Summary        => Print_Append_Summary'Access,
                  Help           => Print_Append_Docstring'Access),

                 (Name           => new Wide_String'("begin"),
                  Implementation => Begin_Command'Access,
                  Usage          => Print_Begin_Usage'Access,
                  Summary        => Print_Begin_Summary'Access,
                  Help           => Print_Begin_Docstring'Access),

                 (Name           => new Wide_String'("compare"),
                  Implementation => Compare_Command'Access,
                  Usage          => Print_Compare_Usage'Access,
                  Summary        => Print_Compare_Summary'Access,
                  Help           => Print_Compare_Docstring'Access),

                 (Name           => new Wide_String'("copy"),
                  Implementation => Copy_Command'Access,
                  Usage          => Print_Copy_Usage'Access,
                  Summary        => Print_Copy_Summary'Access,
                  Help           => Print_Copy_Docstring'Access),

                 (Name           => new Wide_String'("delenv"),
                  Implementation => Delenv_Command'Access,
                  Usage          => Print_Delenv_Usage'Access,
                  Summary        => Print_Delenv_Summary'Access,
                  Help           => Print_Delenv_Docstring'Access),

                 (Name           => new Wide_String'("delete"),
                  Implementation => Delete_Command'Access,
                  Usage          => Print_Delete_Usage'Access,
                  Summary        => Print_Delete_Summary'Access,
                  Help           => Print_Delete_Docstring'Access),

                 (Name           => new Wide_String'("desc"),
                  Implementation => Desc_Command'Access,
                  Usage          => Print_Desc_Usage'Access,
                  Summary        => Print_Desc_Summary'Access,
                  Help           => Print_Desc_Docstring'Access),

                 (Name           => new Wide_String'("dump"),
                  Implementation => Dump_Command'Access,
                  Usage          => Print_Dump_Usage'Access,
                  Summary        => Print_Dump_Summary'Access,
                  Help           => Print_Dump_Docstring'Access),

                 (Name           => new Wide_String'("echo"),
                  Implementation => Echo_Command'Access,
                  Usage          => Print_Echo_Usage'Access,
                  Summary        => Print_Echo_Summary'Access,
                  Help           => Print_Echo_Docstring'Access),

                 (Name           => new Wide_String'("end"),
                  Implementation => End_Command'Access,
                  Usage          => Print_End_Usage'Access,
                  Summary        => Print_End_Summary'Access,
                  Help           => Print_End_Docstring'Access),

                 (Name           => new Wide_String'("execute"),
                  Implementation => Execute_Command'Access,
                  Usage          => Print_Execute_Usage'Access,
                  Summary        => Print_Execute_Summary'Access,
                  Help           => Print_Execute_Docstring'Access),

                 (Name           => new Wide_String'("exit"),
                  Implementation => Exit_Command'Access,
                  Usage          => Print_Exit_Usage'Access,
                  Summary        => Print_Exit_Summary'Access,
                  Help           => Print_Exit_Docstring'Access),

                 (Name           => new Wide_String'("filestat"),
                  Implementation => Filestat_Command'Access,
                  Usage          => Print_Filestat_Usage'Access,
                  Summary        => Print_Filestat_Summary'Access,
                  Help           => Print_Filestat_Docstring'Access),

                 (Name           => new Wide_String'("getenv"),
                  Implementation => Getenv_Command'Access,
                  Usage          => Print_Getenv_Usage'Access,
                  Summary        => Print_Getenv_Summary'Access,
                  Help           => Print_Getenv_Docstring'Access),

                 (Name           => new Wide_String'("help"),
                  Implementation => Help_Command'Access,
                  Usage          => Print_Help_Usage'Access,
                  Summary        => Print_Help_Summary'Access,
                  Help           => Print_Help_Docstring'Access),

                 (Name           => new Wide_String'("incr"),
                  Implementation => Incr_Command'Access,
                  Usage          => Print_Incr_Usage'Access,
                  Summary        => Print_Incr_Summary'Access,
                  Help           => Print_Incr_Docstring'Access),

                 (Name           => new Wide_String'("mkdir"),
                  Implementation => Mkdir_Command'Access,
                  Usage          => Print_Mkdir_Usage'Access,
                  Summary        => Print_Mkdir_Summary'Access,
                  Help           => Print_Mkdir_Docstring'Access),

                 (Name           => new Wide_String'("noop"),
                  Implementation => Noop_Command'Access,
                  Usage          => Print_Noop_Usage'Access,
                  Summary        => Print_Noop_Summary'Access,
                  Help           => Print_Noop_Docstring'Access),

                 (Name           => new Wide_String'("prepend"),
                  Implementation => Prepend_Command'Access,
                  Usage          => Print_Prepend_Usage'Access,
                  Summary        => Print_Prepend_Summary'Access,
                  Help           => Print_Prepend_Docstring'Access),

                 (Name           => new Wide_String'("print"),
                  Implementation => Print_Command'Access,
                  Usage          => Print_Print_Usage'Access,
                  Summary        => Print_Print_Summary'Access,
                  Help           => Print_Print_Docstring'Access),

                 (Name           => new Wide_String'("rename"),
                  Implementation => Rename_Command'Access,
                  Usage          => Print_Rename_Usage'Access,
                  Summary        => Print_Rename_Summary'Access,
                  Help           => Print_Rename_Docstring'Access),

                 (Name           => new Wide_String'("run"),
                  Implementation => Run_Command'Access,
                  Usage          => Print_Run_Usage'Access,
                  Summary        => Print_Run_Summary'Access,
                  Help           => Print_Run_Docstring'Access),

                 (Name           => new Wide_String'("set"),
                  Implementation => Set_Command'Access,
                  Usage          => Print_Set_Usage'Access,
                  Summary        => Print_Set_Summary'Access,
                  Help           => Print_Set_Docstring'Access),

                 (Name           => new Wide_String'("setenv"),
                  Implementation => Setenv_Command'Access,
                  Usage          => Print_Setenv_Usage'Access,
                  Summary        => Print_Setenv_Summary'Access,
                  Help           => Print_Setenv_Docstring'Access),

                 (Name           => new Wide_String'("which"),
                  Implementation => Which_Command'Access,
                  Usage          => Print_Which_Usage'Access,
                  Summary        => Print_Which_Summary'Access,
                  Help           => Print_Which_Docstring'Access));

   function Find_Index (Name : Wide_String) return Natural;
   --  Return the index in the command table for the named command.  If the
   --  command is not found, 0 is returned.

   --------------------
   -- Append_Command --
   --------------------

   procedure Append_Command (State   : in out State_Type;
                             Args    : List_Type) is
      separate;

   -------------------
   -- Begin_Command --
   -------------------

   procedure Begin_Command (State   : in out State_Type;
                            Args    : List_Type) is
      separate;

   ------------------
   -- Command_Name --
   ------------------

   function Command_Name (Index : Natural) return Wide_String is
   begin
      return Command_Table (Index).Name.all;
   end Command_Name;

   ---------------------
   -- Compare_Command --
   ---------------------

   procedure Compare_Command (State   : in out State_Type;
                              Args    : List_Type) is
      separate;

   ------------------
   -- Copy_Command --
   ------------------

   procedure Copy_Command (State   : in out State_Type;
                           Args    : List_Type) is
      separate;

   --------------------
   -- Delenv_Command --
   --------------------

   procedure Delenv_Command (State   : in out State_Type;
                             Args    : List_Type) is
      separate;

   --------------------
   -- Delete_Command --
   --------------------

   procedure Delete_Command (State   : in out State_Type;
                             Args    : List_Type) is
      separate;

   ------------------
   -- Desc_Command --
   ------------------

   procedure Desc_Command (State   : in out State_Type;
                           Args    : List_Type) is
      separate;

   ------------------
   -- Dump_Command --
   ------------------

   procedure Dump_Command (State   : in out State_Type;
                           Args    : List_Type) is
      separate;

   ------------------
   -- Echo_Command --
   ------------------

   procedure Echo_Command (State   : in out State_Type;
                           Args    : List_Type) is
      separate;

   -----------------
   -- End_Command --
   -----------------

   procedure End_Command (State   : in out State_Type;
                          Args    : List_Type) is
      separate;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command (State   : in out State_Type;
                              Args    : List_Type) is
      separate;

   ------------------
   -- Exit_Command --
   ------------------

   procedure Exit_Command (State   : in out State_Type;
                           Args    : List_Type) is
      separate;

   ----------------------
   -- Filestat_Command --
   ----------------------

   procedure Filestat_Command (State   : in out State_Type;
                               Args    : List_Type) is
      separate;

   ----------
   -- Find --
   ----------

   function Find (Name : Wide_String) return Command_Type is
   begin
      return Command_Table (Find_Index (Name)).Implementation;
   end Find;

   ----------------
   -- Find_Index --
   ----------------

   function Find_Index (Name : Wide_String) return Natural is
   begin
      for I in 1 .. Command_Table'Last loop
         if Name = Command_Table (I).Name.all then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Index;

   --------------------
   -- Getenv_Command --
   --------------------

   procedure Getenv_Command (State   : in out State_Type;
                             Args    : List_Type) is
      separate;

   ------------------
   -- Help_Command --
   ------------------

   procedure Help_Command (State   : in out State_Type;
                           Args    : List_Type) is
      separate;

   ------------------
   -- Incr_Command --
   ------------------

   procedure Incr_Command (State   : in out State_Type;
                           Args    : List_Type) is
      separate;

   ------------------------
   -- Number_Of_Commands --
   ------------------------

   function Number_Of_Commands return Positive is
   begin
      return Command_Table'Last;
   end Number_Of_Commands;

   -------------------
   -- Mkdir_Command --
   -------------------

   procedure Mkdir_Command (State   : in out State_Type;
                            Args    : List_Type) is
      separate;

   ------------------
   -- Noop_Command --
   ------------------

   procedure Noop_Command (State   : in out State_Type;
                           Args    : List_Type) is
      separate;

   ---------------------
   -- Prepend_Command --
   ---------------------

   procedure Prepend_Command (State   : in out State_Type;
                              Args    : List_Type) is
      separate;

   -------------------
   -- Print_Command --
   -------------------

   procedure Print_Command (State   : in out State_Type;
                            Args    : List_Type) is
      separate;

   ------------------------
   -- Print_Command_Help --
   ------------------------

   procedure Print_Command_Help (Name : Wide_String) is
   begin
      Command_Table (Find_Index (Name)).Help.all;
   end Print_Command_Help;

   ---------------------------
   -- Print_Command_Summary --
   ---------------------------

   procedure Print_Command_Summary (Name  : Wide_String;
                                    Index : Positive) is
   begin
      Command_Table (Find_Index (Name)).Summary.all (+Index);
   end Print_Command_Summary;

   -------------------------
   -- Print_Command_Usage --
   -------------------------

   procedure Print_Command_Usage (Name : Wide_String) is
   begin
      Command_Table (Find_Index (Name)).Usage.all;
   end Print_Command_Usage;

   --------------------
   -- Rename_Command --
   --------------------

   procedure Rename_Command (State   : in out State_Type;
                             Args    : List_Type) is
      separate;

   -----------------
   -- Run_Command --
   -----------------

   procedure Run_Command (State   : in out State_Type;
                          Args    : List_Type) is
      separate;

   -----------------
   -- Set_Command --
   -----------------

   procedure Set_Command (State   : in out State_Type;
                          Args    : List_Type) is
      separate;

   --------------------
   -- Setenv_Command --
   --------------------

   procedure Setenv_Command (State   : in out State_Type;
                             Args    : List_Type) is
      separate;

   ---------------------
   -- Unknown_Command --
   ---------------------

   procedure Unknown_Command (State   : in out State_Type;
                              Args    : List_Type) is
      separate;

   -------------------
   -- Which_Command --
   -------------------

   procedure Which_Command (State   : in out State_Type;
                            Args    : List_Type) is
      separate;

begin   -- ZBTest.Commands
   for I in Command_Table'First .. Command_Table'Last - 1 loop
      if Command_Table (I).Name.all > Command_Table (I + 1).Name.all then
         raise Command_Table_Not_Sorted
            with Wide_To_UTF8 (Command_Table (I + 1).Name.all);
      end if;
   end loop;
end ZBTest.Commands;
