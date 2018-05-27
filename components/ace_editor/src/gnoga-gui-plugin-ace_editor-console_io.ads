------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--           G N O G A . G U I . P L U G I N . A C E _ E D I T O R .        --
--                            C O N S O L E _ I O                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2017 Pascal Pignard                    --
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

package Gnoga.Gui.Plugin.Ace_Editor.Console_IO is

   type Console_IO_Type is new Ace_Editor_Type with private;
   type Console_IO_Access is access all Console_IO_Type;
   type Pointer_To_Console_IO_Class is access all Console_IO_Type'Class;

   subtype Count is Natural range 0 .. Natural'Last;
   subtype Positive_Count is Count range 1 .. Count'Last;
   Unbounded : constant Count := 0;   --  Line and page length

   End_Error : exception;

   ---------------------
   -- File Management --
   ---------------------

   overriding procedure Create
     (Console : in out Console_IO_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      ID      : in     String := "");
   --  Ace Editor must have been loaded before

   --------------------------------------------
   -- Specification of line and page lengths --
   --------------------------------------------

   procedure Set_Line_Length (Console : in out Console_IO_Type; To : Count);

   procedure Set_Page_Length (Console : in Console_IO_Type; To : Count);

   function Line_Length (Console : in Console_IO_Type) return Count;

   function Page_Length (Console : in Console_IO_Type) return Count;

   ------------------------------------
   -- Column, Line, and Page Control --
   ------------------------------------

   procedure New_Line
     (Console : in out Console_IO_Type;
      Spacing :        Positive_Count := 1);

   procedure Skip_Line
     (Console : in out Console_IO_Type;
      Spacing :        Positive_Count := 1);

   function End_Of_Line (Console : in out Console_IO_Type) return Boolean;

   procedure New_Page (Console : in out Console_IO_Type);

   procedure Skip_Page (Console : in Console_IO_Type);

   function End_Of_Page (Console : in Console_IO_Type) return Boolean;

   procedure Set_Col (Console : in out Console_IO_Type; To : Positive_Count);

   procedure Set_Line (Console : in out Console_IO_Type; To : Positive_Count);

   function Col (Console : in Console_IO_Type) return Positive_Count;

   function Line (Console : in Console_IO_Type) return Positive_Count;

   function Page (Console : in Console_IO_Type) return Positive_Count;

   ----------------------------
   -- Character Input-Output --
   ----------------------------

   procedure Get (Console : in out Console_IO_Type; Item : out Character);
   procedure Put (Console : in out Console_IO_Type; Item : Character);

   procedure Look_Ahead
     (Console     : in out Console_IO_Type;
      Item        :    out Character;
      End_Of_Line :    out Boolean);

   procedure Get_Immediate
     (Console : in out Console_IO_Type;
      Item    :    out Character);

   procedure Get_Immediate
     (Console   : in out Console_IO_Type;
      Item      :    out Character;
      Available :    out Boolean);

   -------------------------
   -- String Input-Output --
   -------------------------

   procedure Get (Console : in out Console_IO_Type; Item : out String);
   procedure Put (Console : in out Console_IO_Type; Item : String);

   procedure Get_Line
     (Console : in out Console_IO_Type;
      Item    :    out String;
      Last    :    out Natural);

   function Get_Line (Console : in out Console_IO_Type) return String;

   procedure Put_Line (Console : in out Console_IO_Type; Item : String);

private
   Ring_Size : constant := 100;
   type Ring_Index is mod Ring_Size;
   type Ring_Buffer is array (Ring_Index) of Character;

   protected type Ring_Char is
      entry Write (Ch : in Character);
      entry Read (Ch : out Character);
      procedure Get (Ch : out Character; Available : out Boolean);
      procedure Look (Ch : out Character; Available : out Boolean);
   private
      Buffer                  : Ring_Buffer;
      Read_Index, Write_Index : Ring_Index                   := 0;
      Count                   : Natural range 0 .. Ring_Size := 0;
   end Ring_Char;

   type Console_IO_Type is new Ace_Editor_Type with record
      Anchor : Anchor_Type;
      Ring   : Ring_Char;
   end record;

   --  Avoid ambiguous declaration coming from View_Base_Type
   overriding procedure Put_Line
     (View    : in out Console_IO_Type;
      Message : in     String;
      Class   : in     String := "";
      ID      : in     String := "") is null;
   overriding procedure Put
     (View    : in out Console_IO_Type;
      Message : in     String;
      Class   : in     String := "";
      ID      : in     String := "") is null;
   overriding procedure New_Line (View : in out Console_IO_Type) is null;
end Gnoga.Gui.Plugin.Ace_Editor.Console_IO;
