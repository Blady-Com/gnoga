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
   --  being added to View. Child element's marked as Dynamic by
   --  Element_Type.Dynamic created with a View as the parent should
   --  never be dealocated except by Finalize. If you plan on dealocating
   --  a child element in your code, do not mark as Dynamic.

   -------------------------------------------------------------------------
   --  View_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (View    : in out View_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Attach  : in     Boolean := True;
      ID      : in     String  := "");
   --  If Parent is a Window_Type'Class will automatically set itself
   --  as the View on Parent if Attach is True.

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

   procedure Add_Element
     (View    : in out View_Type;
      Name    : in     String;
      Element : access Gnoga.Gui.Element.Element_Type'Class);
   --  Add Element to associative array of elements at Name and available using
   --  the View_Type's Element property. This does not re-parent the Element to
   --  View if it was created with a different parent nor does it add Element
   --  to the View's DOM.

   function New_Element
     (View    : access View_Type;
      Name    : String;
      Element : access Gnoga.Gui.Element.Element_Type'Class)
      return Gnoga.Gui.Element.Pointer_To_Element_Class;
   --  Only use for dynamic objects.
   --  Performs like Add_Element (View, Name, Element); Element.Dynamic;
   --  It returns Element in order to allow for this idiom:
   --  Common.Button_Access
   --   (View.New_Element ("my button", new Common.Button_Type)).Create (View);

   function Add
     (View    : access View_Type;
      Element : access Gnoga.Gui.Element.Element_Type'Class)
      return Gnoga.Gui.Element.Pointer_To_Element_Class;
   --  Only use for dynamic objects.
   --  Marks Element as Dynamic and returns Element. This is primarly of value
   --  for creating a dynamic element that you will no longer interact with
   --  in the future since all reference is lost. Use the idiom:
   --  Common.Button_Access
   --    (View.Add (new Common.Button_Type)).Create (View);

   -------------------------------------------------------------------------
   --  View_Type - Properties
   -------------------------------------------------------------------------

   function Element (View : View_Type; Name : String)
                     return Gnoga.Gui.Element.Pointer_To_Element_Class;
   --  Access elements added by Add_Element and New_Element

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
   --  to it. If an element is marked as dynamic before its Create is called
   --  it is slated for garbage collection.

private
   type View_Type is new Gnoga.Gui.Element.Element_Type with
      record
         Child_Array : Gnoga.Gui.Base.Base_Type_Array;
         Element_Map : Gnoga.Gui.Base.Base_Type_Map;
      end record;
end Gnoga.Gui.View;
