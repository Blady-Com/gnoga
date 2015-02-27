------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--              G N O G A . G U I . P L U G I N S . J Q U E R Y             --
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
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Exceptions;

with Gnoga.Server.Connection;

package body Gnoga.Gui.Plugin.jQuery is
   function Gnoga_Var (Object : jQuery_Type) return String;
   --  Return the java script variable containing the jQuery result

   ---------------
   -- Gnoga_Var --
   ---------------

   function Gnoga_Var (Object : jQuery_Type) return String is
   begin
      return "gnoga['" &
        Ada.Strings.Unbounded.To_String (Object.Unique_ID) & "']";
   end Gnoga_Var;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Object : in out jQuery_Type)
   is
   begin
      Object.Unique_ID := Ada.Strings.Unbounded.To_Unbounded_String
        (Gnoga.Server.Connection.New_GID);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Object : in out jQuery_Type)
   is
   begin
      if not Gnoga.Server.Connection.Shutting_Down then
         if Object.Connection_ID /= Gnoga.Types.No_Connection then
            Gnoga.Server.Connection.Execute_Script
              (Object.Connection_ID,
               "delete " & Gnoga_Var (Object) & ";");
         end if;
      end if;

      Object.Connection_ID := Gnoga.Types.No_Connection;
   exception
      when Gnoga.Server.Connection.Connection_Error =>
         null; --  Socket error to browser
      when E : others =>
         Log ("Error finalizing jQuery Object - " & Gnoga_Var (Object));
         Log (Ada.Exceptions.Exception_Name (E) & " - " &
                Ada.Exceptions.Exception_Message (E));
   end Finalize;

   ------------
   -- jQuery --
   ------------

   procedure jQuery (Object : in out jQuery_Type;
                     ID     : in     Gnoga.Types.Connection_ID;
                     Query  : in String)
   is
   begin
      Object.Connection_ID := ID;

      Gnoga.Server.Connection.Execute_Script
        (Object.Connection_ID, Gnoga_Var (Object) & "=$(" & Query & ");");
   end jQuery;

   -------------
   -- Execute --
   -------------

   procedure Execute (Object : in out jQuery_Type; Method : in String) is
   begin
      Gnoga.Server.Connection.Execute_Script
        (Object.Connection_ID, Gnoga_Var (Object) & "." & Method);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (Object : jQuery_Type; Method : String) return String is
   begin
      return Gnoga.Server.Connection.Execute_Script
        (Object.Connection_ID, Gnoga_Var (Object) & "." & Method);
   end Execute;

end Gnoga.Gui.Plugin.jQuery;
