------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                       G N O G A . G U I . V I E W                        --
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

--  Views are used to handle auto insertion of objects in to the DOM and
--  placement.

with Ada.Containers.Vectors;

with Gnoga.Gui.Base;
with Gnoga.Gui.Element;

package Gnoga.Gui.View is

   -------------------------------------------------------------------------
   --  View_Types
   -------------------------------------------------------------------------

   type View_Type is new Gnoga.Gui.Element.Element_Type with private;
   type View_Access is access all View_Type;
   type Pointer_To_View_Class is access all View_Type'Class;

   overriding
   procedure Finalize (Object : in out View_Type);
   --  Deallocate any child element that was marked as Dynamic before
   --  being added to View

   -------------------------------------------------------------------------
   --  View_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (View    : in out View_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Attach  : in     Boolean := True;
      ID      : in     String  := "");
   --  If Parent is a Window_Type'Class will automatically set itself
   --  as the View on Parent if Attach is True

   -------------------------------------------------------------------------
   --  View_Type - Methods
   -------------------------------------------------------------------------

   procedure Put_Line (View    : in out View_Type;
                       Message : in     String;
                       Class   : in     String := "";
                       ID      : in     String := "");
   --  Create a new DIV with Message and append to end of view.
   --  Use View.Overflow (Scroll) to allow scroll bars to view overflow
   --  of data added by Put_Line. Class is an optional CSS class and
   --  ID and option DOM ID.

   procedure Put (View    : in out View_Type;
                  Message : in     String;
                  Class   : in     String := "";
                  ID      : in     String := "");
   --  Create a new SPAN with Message and append to end of view.
   --  Spans are added inline unlike DIVs that are blocks taking up an
   --  an entire row.

   procedure New_Line (View : in out View_Type);
   --  Create a new DIV with <br /> and append to end of view

   procedure Put_HTML (View  : in out View_Type;
                       HTML  : in     String;
                       Class : in     String := "";
                       ID    : in     String := "");
   --  Place HTML directly in to view. The HTML will not be wrapped in a DIV
   --  or Span.

   -------------------------------------------------------------------------
   --  View_Type - Event Methods
   -------------------------------------------------------------------------

   overriding
   procedure On_Child_Added
     (View  : in out View_Type;
      Child : in out Gnoga.Gui.Base.Base_Type'Class);
   --  All children of views should be Element_Type'Class, if it is not
   --  it will be ignored. Any child professing the View as its parent
   --  will automatially have Element.Place_Inside_Bottom_Of (View) applied
   --  to it.

private
   package Base_Type_Arrays is
     new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Gnoga.Gui.Base.Pointer_To_Base_Class,
        "="          => Gnoga.Gui.Base."=");

   type View_Type is new Gnoga.Gui.Element.Element_Type with
      record
         Base_Type_Array : Base_Type_Arrays.Vector;
      end record;
end Gnoga.Gui.View;
