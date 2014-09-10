------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                        G N O G A . W I N D O W                           --
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

package body Gnoga.Window is

   ------------
   -- Create --
   ------------

   procedure Attach
     (Window        : in out Window_Type;
      Connection_ID : in     Gnoga.Types.Connection_ID;
      ID            : in     String                    := "")
   is
      function Adjusted_ID return String is
      begin
         if ID = "" then
            return "window";
         else
            return ID;
         end if;
      end Adjusted_ID;
   begin
      Attach (Object        => Window,
              Connection_ID => Connection_ID,
              ID            => Adjusted_ID,
              ID_Type       => Gnoga.Types.Script);
   end Attach;

   ----------
   -- Name --
   ----------

   procedure Name (Window : in out Window_Type; Value : String) is
   begin
      Window.Property ("name", Value);
   end Name;

   ----------
   -- Name --
   ----------

   function Name (Window : Window_Type) return String is
   begin
      return Window.Property ("name");
   end Name;

   ------------------
   -- Inner_Height --
   ------------------

   procedure Inner_Height (Window : in out Window_Type; Value : in Integer) is
   begin
      Window.Property ("innerHeight", Value);
   end Inner_Height;

   ------------------
   -- Inner_Height --
   ------------------

   function Inner_Height (Window : Window_Type) return Integer is
   begin
      return Window.Property ("innerHeight");
   end Inner_Height;

   -----------------
   -- Inner_Width --
   -----------------

   procedure Inner_Width (Window : in out Window_Type; Value : in Integer) is
   begin
      Window.Property ("innerWidth", Value);
   end Inner_Width;

   -----------------
   -- Inner_Width --
   -----------------

   function Inner_Width (Window : Window_Type) return Integer is
   begin
      return Window.Property ("innerWidth");
   end Inner_Width;

   ------------------
   -- Outer_Height --
   ------------------

   procedure Outer_Height (Window : in out Window_Type; Value : in Integer) is
   begin
      Window.Property ("outerHeight", Value);
   end Outer_Height;

   ------------------
   -- Outer_Height --
   ------------------

   function Outer_Height (Window : Window_Type) return Integer is
   begin
      return Window.Property ("outerHeight");
   end Outer_Height;

   -----------------
   -- Outer_Width --
   -----------------

   procedure Outer_Width (Window : in out Window_Type; Value : in Integer) is
   begin
      Window.Property ("outerWidth", Value);
   end Outer_Width;

   -----------------
   -- Outer_Width --
   -----------------

   function Outer_Width (Window : Window_Type) return Integer is
   begin
      return Window.Property ("outerWidth");
   end Outer_Width;

   --------------
   -- X_Offset --
   --------------

   function X_Offset (Window : Window_Type) return Integer is
   begin
      return Window.Property ("pageXOffset");
   end X_Offset;

   --------------
   -- Y_Offset --
   --------------

   function Y_Offset (Window : Window_Type) return Integer is
   begin
      return Window.Property ("pageYOffset");
   end Y_Offset;

   ---------
   -- Top --
   ---------

   function Top (Window : Window_Type) return Integer is
   begin
      return Window.Property ("screenY");
   end Top;

   ----------
   -- Left --
   ----------

   function Left (Window : Window_Type) return Integer is
   begin
      return Window.Property ("screenX");
   end Left;

   -----------
   -- Alert --
   -----------

   procedure Alert (Window : in out Window_Type; Message : String) is
   begin
      Window.Execute ("alert (""" & Escape_Quotes (Message) & """);");
   end Alert;

   -----------
   -- Close --
   -----------

   procedure Close (Window : in out Window_Type) is
   begin
      Window.Execute ("close();");
   end Close;

   -----------
   -- Focus --
   -----------

   procedure Focus (Window : in out Window_Type) is
   begin
      Window.Execute ("focus();");
   end Focus;

   ----------
   -- Blur --
   ----------

   procedure Blur (Window : in out Window_Type) is
   begin
      Window.Execute ("blur();");
   end Blur;

   -----------
   -- Print --
   -----------

   procedure Print (Window : in out Window_Type) is
   begin
      Window.Execute ("print();");
   end Print;

   ---------------
   -- Resize_By --
   ---------------

   procedure Resize_By
     (Window : in out Window_Type;
      Width, Height : Integer)
   is
   begin
      Window.Execute ("resizeBy(" & Width'Img & "," & Height'Img & ");");
   end Resize_By;

   ---------------
   -- Resize_To --
   ---------------

   procedure Resize_To
     (Window : in out Window_Type;
      Width, Height : Integer)
   is
   begin
      Window.Execute ("resizeTo(" & Width'Img & "," & Height'Img & ");");
   end Resize_To;

   -------------
   -- Move_By --
   -------------

   procedure Move_By (Window : in out Window_Type; X, Y : Integer) is
   begin
      Window.Execute ("moveBy(" & X'Img & "," & Y'Img & ");");
   end Move_By;

   -------------
   -- Move_To --
   -------------

   procedure Move_To (Window : in out Window_Type; X, Y : Integer) is
   begin
      Window.Execute ("moveTo(" & X'Img & "," & Y'Img & ");");
   end Move_To;

   ---------------
   -- Scroll_By --
   ---------------

   procedure Scroll_By (Window : in out Window_Type; X, Y : Integer) is
   begin
      Window.Execute ("scrollBy(" & X'Img & "," & Y'Img & ");");
   end Scroll_By;

   ---------------
   -- Scroll_To --
   ---------------

   procedure Scroll_To (Window : in out Window_Type; X, Y : Integer) is
   begin
      Window.Execute ("scrollTo(" & X'Img & "," & Y'Img & ");");
   end Scroll_To;

end Gnoga.Window;
