------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--     G N O G A . G U I . E L E M E N T . F O R M . D A T A _ L I S T      --
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
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

--  Create a data list to use for autocomplete for form inputs

with Gnoga.Gui.View;

package Gnoga.Gui.Element.Form.Data_List is

   -------------------------------------------------------------------------
   --  Data_List_Types
   -------------------------------------------------------------------------

   type Data_List_Type is new Gnoga.Gui.View.View_Type with private;
   type Data_List_Access is access all Data_List_Type;
   type Pointer_To_Data_List_Class is access all Data_List_Type'Class;

   -------------------------------------------------------------------------
   --  Data_List_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (List          : in out Data_List_Type;
      Parent        : in out Gnoga.Gui.Base.Base_Type'Class;
      ID            : in     String);
   overriding
   procedure Create
     (List          : in out Data_List_Type;
      Parent        : in out Gnoga.Gui.Base.Base_Type'Class;
      Attach        : in     Boolean;
      ID            : in     String);

   -------------------------------------------------------------------------
   --  Data_List_Type - Methods
   -------------------------------------------------------------------------

   procedure Add_Option (List  : in out Data_List_Type;
                         Value : in     String);
   --  Add option to Data_LIst

private
   type Data_List_Type is new Gnoga.Gui.View.View_Type with null record;
end Gnoga.Gui.Element.Form.Data_List;
