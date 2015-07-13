------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                       G N O G A _ M A K E . M A I N                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2014 David Botton                      --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Directories;

with Ada.Exceptions;
with GNAT.Traceback.Symbolic;

with Gnoga;

with Gnoga_Make;

procedure Gnoga_Make.Main is
begin
   if Argument_Count = 0 then
      Gnoga_Make.Display_Help;
      return;
   end if;

   declare
      Command : String := Translate (Argument (1), Lower_Case_Map);
   begin
      if Command = "version" or Command = "-v" or Command = "/v" then
         Gnoga_Make.Version;

      elsif Command = "help" or Command = "-h" or Command = "/h" then
         Gnoga_Make.Display_Help;

      elsif Command = "new" then
         if Argument_Count < 2 then
            Gnoga_Make.Display_New_Usage;
         elsif Argument_Count = 2 then
            Gnoga_Make.New_Application (Argument (2), "multi_connect");
         else
            Gnoga_Make.New_Application (Argument (2), Argument (3));
         end if;

      elsif Command = "list" then
         Gnoga_Make.List_Templates;

      elsif Command = "view" then
         if Argument_Count < 3 then
            Gnoga_Make.Display_View_Usage;
         end if;

      elsif Command = "controller" then
         if Argument_Count < 3 then
            Gnoga_Make.Display_Controller_Usage;
         end if;

      else
         Gnoga_Make.Display_Help;
      end if;
   end;
exception
   when E : others =>
      Gnoga.Log (Ada.Exceptions.Exception_Name (E) & " - " &
                   Ada.Exceptions.Exception_Message (E));
      Gnoga.Log (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Gnoga_Make.Main;
