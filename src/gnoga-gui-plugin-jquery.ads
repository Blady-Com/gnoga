------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--              G N O G A . G U I . P L U G I N S . J Q U E R Y             --
--                                                                          --
--                                 S p e c                                  --
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
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

--  General binding to jQuery

with Ada.Finalization;

with Gnoga.Types;

package Gnoga.Gui.Plugin.jQuery is
   -------------------------------------------------------------------------
   --  jQuery_Type
   -------------------------------------------------------------------------
   --  Binding to allow for general jQuery use outside of Gnoga Objects

   type jQuery_Type is new Ada.Finalization.Limited_Controlled with private;
   type jQuery_Access is access all jQuery_Type;
   type Pointer_To_jQuery_Class is access all jQuery_Type'Class;

   overriding
   procedure Initialize (Object : in out jQuery_Type);

   overriding
   procedure Finalize (Object : in out jQuery_Type);

   -------------------------------------------------------------------------
   --  jQuery_Type - Methods
   -------------------------------------------------------------------------

   procedure jQuery (Object : in out jQuery_Type;
                     ID     : in     Gnoga.Types.Connection_ID;
                     Query  : in     String);
   --  Create a jQuery object based on Query and store results in Object
   --  Object = $(Query).
   --  Note: that most queries require adding quotes, for example:
   --     Select all Div's -> jQuery (Main_Window, "'div'");
   --  However, selecting the document, window or body or script var would not
   --     jQuery (Main_Window, "window");

   procedure Execute (Object : in out jQuery_Type; Method : in String);
   --  Execute method on jQuery Object. Method should include () but not
   --  initial '.', Object.Some_Method(); in Ada would be:
   --  Object.Execute ("Some_Method()");

   function Execute (Object : jQuery_Type; Method : String) return String;
   --  Return results of executing method on jQuery Object as a String
private
   type jQuery_Type is new Ada.Finalization.Limited_Controlled with
      record
         Unique_ID     : Gnoga.Types.Web_ID;
         Connection_ID : Gnoga.Types.Connection_ID :=
                           Gnoga.Types.No_Connection;
      end record;
end Gnoga.Gui.Plugin.jQuery;
