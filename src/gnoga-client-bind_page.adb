------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                 G N O G A . C L I E N T . B I N D _ P A G E              --
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

with Ada.Strings.Fixed;

with Gnoga.Server.Connection;
with Gnoga.Gui.Element;

package body Gnoga.Client.Bind_Page is

   ---------------
   -- Bind_Page --
   ---------------

   procedure Bind_Page (View : in out Gnoga.Gui.View.View_Base_Type'Class) is
      use Ada.Strings.Fixed;
   begin
      Gnoga.Server.Connection.Execute_Script
        (View.Connection_ID,
         "gnoga['idbuf']=""""; $(""[id]"").each ( function (index, n)" &
           " { gnoga['idbuf'] = gnoga['idbuf'] + " &
           " $(this).attr(""id"") + ""|""; } );");

      declare
         Buf : constant String  := Gnoga.Server.Connection.Execute_Script
           (View.Connection_ID, "gnoga['idbuf']");
         S   : Integer := Buf'First;
         F   : Integer := Buf'First - 1;

         procedure Split;

         procedure Split is
         begin
            S := F + 1;
            if S <= Buf'Last then
               F := Index (Source  => Buf,
                           Pattern => "|",
                           From    => S);
               declare
                  ID : constant String := Buf (S .. (F - 1));
                  E  : constant Gnoga.Gui.Element.Element_Access :=
                         new Gnoga.Gui.Element.Element_Type;
               begin
                  E.Attach_Using_Parent (Parent  => View,
                                         ID      => ID);
                  View.Add_Element (ID, E);
               end;

               Split;
            end if;
         end Split;
      begin
         Split;
      end;
   end Bind_Page;

end Gnoga.Client.Bind_Page;
