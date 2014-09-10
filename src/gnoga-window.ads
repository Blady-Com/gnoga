------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                        G N O G A . W I N D O W                           --
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
------------------------------------------------------------------------------                                                                          --

with Gnoga.Types;
with Gnoga.Base;
with Gnoga.Document;

package Gnoga.Window is

   -------------------------------------------------------------------------
   --  Window_Type
   -------------------------------------------------------------------------
   --  Window_Type is the class encapsulating an individual Gnoga browser
   --  windows and tabs. It can also encapsulate any window or iFrame
   --  that is reachable by the DOM in a Gnoga browser window or tab, i.e.
   --  that was loaded with a connection via websocktes to the Gnoga app.
   --  usually with Gnoga's standard bootstrap file.


   type Window_Type is new Gnoga.Base.Base_Type with private;
   type Window_Access is access all Window_Type;
   type Pointer_To_Window_Class is access all Window_Type'Class;

   procedure Attach (Window        : in out Window_Type;
                     Connection_ID : in     Gnoga.Types.Connection_ID;
                     ID            : in     String                    := "");
   --  Attach a Gnoga Window_Type to Connection_ID identified as ID in the DOM.
   --  ID should only be set when attaching to a reference to a window stored
   --  in a javascript variable.

   -------------------------------------------------------------------------
   --  Window_Type - Properties
   -------------------------------------------------------------------------

   function Document (Window : Window_Type)
                      return Gnoga.Document.Document_Access;
   --  DOM Document Node

   procedure Name (Window : in out Window_Type; Value : String);
   function Name (Window : Window_Type) return String;
   --  Hyperlink "target" Name for Window

   procedure Inner_Height (Window : in out Window_Type; Value : in Integer);
   function Inner_Height (Window : Window_Type) return Integer;

   procedure Inner_Width (Window : in out Window_Type; Value : in Integer);
   function Inner_Width (Window : Window_Type) return Integer;

   procedure Outer_Height (Window : in out Window_Type; Value : in Integer);
   function Outer_Height (Window : Window_Type) return Integer;

   procedure Outer_Width (Window : in out Window_Type; Value : in Integer);
   function Outer_Width (Window : Window_Type) return Integer;

   function X_Offset (Window : Window_Type) return Integer;

   function Y_Offset (Window : Window_Type) return Integer;

   function Top (Window : Window_Type) return Integer;

   function Left (Window : Window_Type) return Integer;

   -------------------------------------------------------------------------
   --  Window_Type - Methods
   -------------------------------------------------------------------------

   procedure Alert (Window : in out Window_Type; Message : String);
   --  Display Alert box on Window with Message

   procedure Close (Window : in out Window_Type);

   procedure Focus (Window : in out Window_Type);

   procedure Blur (Window : in out Window_Type);

   procedure Print (Window : in out Window_Type);

   procedure Resize_By (Window : in out Window_Type; Width, Height : Integer);

   procedure Resize_To (Window : in out Window_Type; Width, Height : Integer);

   procedure Move_By (Window : in out Window_Type; X, Y : Integer);

   procedure Move_To (Window : in out Window_Type; X, Y : Integer);

   procedure Scroll_By (Window : in out Window_Type; X, Y : Integer);

   procedure Scroll_To (Window : in out Window_Type; X, Y : Integer);
private
   type Window_Type is new Gnoga.Base.Base_Type with
      record
         DOM_Document : aliased Gnoga.Document.Document_Type;
      end record;
end Gnoga.Window;
