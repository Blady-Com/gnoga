------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--               G N O G A . G U I . E L E M E N T . I F R A M E            --
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
-- For more inIFrameation please go to http://www.gnoga.com                 --
------------------------------------------------------------------------------

with Gnoga.Gui.Window;

package Gnoga.Gui.Element.IFrame is

   -------------------------------------------------------------------------
   --  From_Element_Types
   -------------------------------------------------------------------------
   --  Parent type for IFrame elements

   type IFrame_Type is new Gnoga.Gui.Element.Element_Type with private;
   type IFrame_Access is access all IFrame_Type;
   type Pointer_To_IFrame_Class is access all IFrame_Type'Class;


   -------------------------------------------------------------------------
   --  IFrame_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (IFrame   : in out IFrame_Type;
                     Parent   : in out Gnoga.Gui.Base.Base_Type'Class;
                     URL      : in     String;
                     Seamless : in     Boolean := False;
                     ID       : in     String := "");
   --  Create a IFrame element with URL

   -------------------------------------------------------------------------
   --  IFrame_Type - Properties
   -------------------------------------------------------------------------

   function Window (IFrame : IFrame_Type)
                    return Gnoga.Gui.Window.Window_Access;

private
   type IFrame_Type is new Gnoga.Gui.Element.Element_Type with
      record
         Frame : aliased Gnoga.Gui.Window.Window_Type;
      end record;
end Gnoga.Gui.Element.IFrame;
