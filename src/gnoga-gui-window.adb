------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                      G N O G A . G U I . W I N D O W                     --
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
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Calendar.Formatting;

with Gnoga.Client.Storage;
with Gnoga.Server.Connection;
with Gnoga.Gui.Element;

package body Gnoga.Gui.Window is
   use type Gnoga.Gui.Base.Action_Event;

   ------------
   -- Attach --
   ------------

   overriding procedure Attach
     (Window        : in out Window_Type;
      Connection_ID : in     Gnoga.Types.Connection_ID;
      ID            : in     String                     := "window";
      ID_Type       : in     Gnoga.Types.ID_Enumeration := Gnoga.Types.Script)
   is
      use type Gnoga.Types.ID_Enumeration;
   begin
      if ID_Type = Gnoga.Types.DOM_ID then
         raise Invalid_ID_Type;
      end if;

      Gnoga.Gui.Base.Attach (Object        => Gnoga.Gui.Base.Base_Type (Window),
                             Connection_ID => Connection_ID,
                             ID            => ID,
                             ID_Type       => ID_Type);

      Window.DOM_Document.Attach (Connection_ID, ID, ID_Type);

      Window.Location.Attach
        (Connection_ID => Connection_ID,
         ID            => Window.jQuery & ".prop ('location')",
         ID_Type       => Gnoga.Types.Script);

      Window.Bind_Event (Event   => "resize",
                         Message => "");
   end Attach;

   procedure Attach
     (Window  : in out Window_Type;
      Parent  : in out Window_Type'Class;
      ID      : in     String;
      ID_Type : in     Gnoga.Types.ID_Enumeration := Gnoga.Types.Script)
   is
      use type Gnoga.Types.ID_Enumeration;

      CID : String := Gnoga.Server.Connection.Execute_Script
        (Parent.Connection_ID,
         Base.Script_Accessor (ID, ID_Type) & ".gnoga['Connection_ID']");
   begin
      if ID_Type = Gnoga.Types.DOM_ID then
         raise Invalid_ID_Type;
      end if;

      Attach (Window, Gnoga.Types.Connection_ID'Value (CID));
   exception
      when others =>
         Log ("Unable to find gnoga['Connection_ID'] on " & ID &
                " eval returned : " & CID);
         raise Not_A_Gnoga_Window;
   end Attach;

   --------------
   -- Reattach --
   --------------

   procedure Reattach (Window : in out Window_Type;
                       Parent : in out Window_Type'Class)
   is
      CID : String := Gnoga.Server.Connection.Execute_Script
        (Parent.Connection_ID,
         Base.Script_Accessor (Window.ID, Window.ID_Type) &
                 ".gnoga['Connection_ID']");
   begin
      Window.Connection_ID (Gnoga.Types.Connection_ID'Value (CID));
   end Reattach;

   --------------
   -- Set_View --
   --------------

   procedure Set_View (Window : in out Window_Type;
                       Object : in out Gnoga.Gui.Base.Base_Type'Class;
                       Place  : in     Boolean := True)
   is
      use Gnoga.Types;
      use Gnoga.Gui.Element;
   begin
      if not (Object.ID_Type = DOM_ID or Object.ID_Type = Gnoga_ID) then
         raise Invalid_ID_Type;
      end if;

      if not (Object in Element_Type'Class) then
         raise Invalid_ID_Type with "Not in Element_Type'Class";
      end if;

      if Place then
         Element_Type (Object).Place_Inside_Top_Of
           (Window.Document.Body_Element.all);
      end if;

      Window.View_ID := Ada.Strings.Unbounded.To_Unbounded_String (Object.ID);

      Element_Type (ObjecT).Box_Sizing (Border_Box);
      Element_Type (Object).Position (Gnoga.Gui.Element.Fixed);
      Element_Type (Object).Display ("block");
      Element_Type (Object).Left (0);
      Element_Type (Object).Top (0);
      Element_Type (Object).Box_Height (Window.Height);
      Element_Type (Object).Box_Width (Window.Width);

      Window.Has_View := True;
   end Set_View;

   -----------------
   -- Remove_View --
   -----------------

   procedure Remove_View (Window : in out Window_Type) is
   begin
      Window.Has_View := False;
   end Remove_View;

   --------------
   -- Document --
   --------------

   function Document (Window : Window_Type)
                      return Gnoga.Gui.Document.Document_Access
   is
   begin
      return Window.DOM_Document'Unrestricted_Access;
   end Document;

   --------------
   -- Location --
   --------------

   function Location (Window : Window_Type)
                      return Gnoga.Gui.Location.Location_Access
   is
   begin
      return Window.Location'Unrestricted_Access;
   end Location;

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

   ----------------------
   -- Search_Parameter --
   ----------------------

   function Search_Parameter (Window : Window_Type; Name  : String)
                              return String
   is
   begin
      return Gnoga.Server.Connection.Search_Parameter (Window.Connection_ID,
                                                       Name);
   end Search_Parameter;

   ----------------------
   -- Gnoga_Session_ID --
   ----------------------

   function Gnoga_Session_ID (Window : Window_Type; Name : String := "gid")
                              return String
   is
      use Gnoga.Client.Storage;

      function Generate_Session_ID return String is
         use Ada.Strings.Fixed;
         use Ada.Strings;
         use Ada.Calendar.Formatting;

         Now : Ada.Calendar.Time := Ada.Calendar.Clock;
      begin
         return Trim (Year (Now)'Img, Side => Both)
           & Trim (Month (Now)'Img, Side => Both)
           & Trim (Day (Now)'Img, Side => Both)
           & Trim (Hour (Now)'Img, Side => Both)
           & Trim (Minute (Now)'Img, Side => Both)
           & Trim (Second (Now)'Img, Side => Both)
           & Translate (Trim (Sub_Second (Now)'Img, Side => Both),
                        Ada.Strings.Maps.To_Mapping (".", "0"));
      end Generate_Session_ID;

      S   : Session_Storage_Type := Session_Storage (Window.Connection_ID);
      Gid : String               := S.Get (Name);
   begin
      if Gid = "null" then
         declare
            New_Session : String := Generate_Session_ID;
         begin
            S.Set (Name, New_Session);

            return New_Session;
         end;
      else
         return Gid;
      end if;
   end Gnoga_Session_ID;

   ------------
   -- Launch --
   ------------

   procedure Launch (Window   : in out Window_Type;
                     Parent   : in out Window_Type'Class;
                     URL      : in     String;
                     Width    : in     Integer := -1;
                     Height   : in     Integer := -1;
                     Left     : in     Integer := -1;
                     Top      : in     Integer := -1;
                     Menu     : in     Boolean := False;
                     Status   : in     Boolean := False;
                     Tool_Bar : in     Boolean := False;
                     Title    : in     Boolean := False)
   is
      GID : constant String := Gnoga.Server.Connection.New_GID;

      function Params return String is
         use Ada.Strings.Unbounded;

         P : Unbounded_String;
         C : Boolean := False;

         procedure Add_Param (S : String; V : String) is
         begin
            if C then
               P := P & ", ";
            end if;

            P := P & To_Unbounded_String (S) & "=" & To_Unbounded_String (V);
            C := True;
         end Add_Param;
      begin
         if Width > -1 then
            Add_Param ("width", Width'Img);
         end if;

         if Height > -1 then
            Add_Param ("height", Height'Img);
         end if;

         if Top > -1 then
            Add_Param ("top", Top'Img);
         end if;

         if Left > -1 then
            Add_Param ("left", Left'Img);
         end if;

         Add_Param ("menubar", Menu'Img);
         Add_Param ("status", Status'Img);
         Add_Param ("toolbar", Tool_Bar'Img);
         Add_Param ("menubar", Menu'Img);
         Add_Param ("titlebar", Title'Img);

         return To_String (P);
      end Params;

   begin
      Window.Create_With_Script
        (Connection_ID => Parent.Connection_ID,
         ID            => GID,
         Script        => "gnoga['" & GID & "']=" & Parent.jQuery &
           ".get(0).open ('" & URL & "', '_blank', '" & Params & "')",
         ID_Type       => Gnoga.Types.Gnoga_ID);

      Window.DOM_Document.Attach
        (Parent.Connection_ID, GID, Gnoga.Types.Gnoga_ID);

      Window.Location.Attach
        (Connection_ID => Parent.Connection_ID,
         ID            => Window.jQuery & ".prop ('location')",
         ID_Type       => Gnoga.Types.Script);
   end Launch;

   -----------
   -- Alert --
   -----------

   procedure Alert (Window : in out Window_Type; Message : String) is
   begin
      Window.Execute ("alert (""" & Escape_Quotes (Message) & """);");
   end Alert;

   ---------
   -- Log --
   ---------

   procedure Log (Window : in out Window_Type; Message : String) is
   begin
      Window.Execute ("console.log (""" & Escape_Quotes (Message) & """);");
   end Log;

   -----------
   -- Error --
   -----------

   procedure Error (Window : in out Window_Type; Message : String) is
   begin
      Window.Execute ("console.error (""" & Escape_Quotes (Message) & """);");
   end Error;

   -----------
   -- Close --
   -----------

   procedure Close (Window : in out Window_Type) is
   begin
      Window.Execute ("close();");
   end Close;

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
     (Window        : in out Window_Type;
      Width, Height : Integer)
   is
   begin
      Window.Execute ("resizeBy(" & Width'Img & "," & Height'Img & ");");
   end Resize_By;

   ---------------
   -- Resize_To --
   ---------------

   procedure Resize_To
     (Window        : in out Window_Type;
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

   --------------
   -- On_Abort --
   --------------

   procedure On_Abort_Handler (Window  : in out Window_Type;
                               Handler : in     Gnoga.Gui.Base.Action_Event)
   is
   begin
      Window.On_Abort_Event := Handler;

      Window.Bind_Event (Event   => "abort",
                         Message => "");
   end On_Abort_Handler;

   procedure Fire_On_Abort (Window : in out Window_Type)
   is
   begin
      if Window.On_Abort_Event /= null then
         Window.On_Abort_Event (Window);
      end if;
   end Fire_On_Abort;

   --------------
   -- On_Error --
   --------------

   procedure On_Error_Handler
     (Window  : in out Window_Type;
      Handler : in     Gnoga.Gui.Base.Action_Event)
   is
   begin
      Window.On_Error_Event := Handler;

      Window.Bind_Event (Event   => "error",
                         Message => "");
   end On_Error_Handler;

   procedure Fire_On_Error (Window : in out Window_Type)
   is
   begin
      if Window.On_Error_Event /= null then
         Window.On_Error_Event (Window);
      end if;
   end Fire_On_Error;

   --------------------
   -- On_Hash_Change --
   --------------------

   procedure On_Hash_Change_Handler
     (Window  : in out Window_Type;
      Handler : in     Gnoga.Gui.Base.Action_Event)
   is
   begin
      Window.On_Hash_Change_Event := Handler;

      Window.Bind_Event (Event   => "hashchange",
                         Message => "");
   end On_Hash_Change_Handler;

   procedure Fire_On_Hash_Change (Window : in out Window_Type)
   is
   begin
      if Window.On_Hash_Change_Event /= null then
         Window.On_Hash_Change_Event (Window);
      end if;
   end Fire_On_Hash_Change;

   ---------------------------
   -- On_Orientation_Change --
   ---------------------------

   procedure On_Orientation_Change_Handler
     (Window  : in out Window_Type;
      Handler : in     Gnoga.Gui.Base.Action_Event)
   is
   begin
      Window.On_Orientation_Change_Event := Handler;

      Window.Bind_Event (Event   => "orientationchange",
                         Message => "");
   end On_Orientation_Change_Handler;

   procedure Fire_On_Orientation_Change (Window : in out Window_Type)
   is
   begin
      if Window.On_Orientation_Change_Event /= null then
         Window.On_Orientation_Change_Event (Window);
      end if;
   end Fire_On_Orientation_Change;

   ----------------
   -- On_Storage --
   ----------------

   procedure On_Storage_Handler (Window  : in out Window_Type;
                                 Handler : in     Gnoga.Gui.Base.Action_Event)
   is
   begin
      Window.On_Storage_Event := Handler;

      Window.Bind_Event (Event   => "storage",
                         Message => "");
   end On_Storage_Handler;

   procedure Fire_On_Storage (Window : in out Window_Type)
   is
   begin
      if Window.On_Storage_Event /= null then
         Window.On_Storage_Event (Window);
      end if;
   end Fire_On_Storage;

   ---------------
   -- On_Resize --
   ---------------

   procedure On_Resize (Window : in out Window_Type) is
      use Gnoga.Types;
   begin
      if Window.Has_View then
         declare
            Object : Gnoga.Gui.Element.Element_Type;
         begin
            Object.Attach_Using_Parent
              (Window,
               Ada.Strings.Unbounded.To_String (Window.View_ID));
            Object.Box_Height (Window.Height);
            Object.Box_Width (Window.Width);
         end;
      end if;
   end On_Resize;

   ----------------
   -- On_Message --
   ----------------

   procedure On_Message (Object  : in out Window_Type;
                         Event   : in     String;
                         Message : in     String)
   is
   begin
      -- Network Event --
      if Event = "abort" then
         Object.Fire_On_Abort;
      elsif Event = "error" then
         Object.Fire_On_Error;
      elsif Event = "hashchange" then
         Object.Fire_On_Hash_Change;
      elsif Event = "orientationchange" then
         Object.Fire_On_Orientation_Change;
      elsif Event = "storage" then
         Object.Fire_On_Storage;
      elsif Event = "resize" then
         Object.On_Resize;
         Object.Fire_On_Resize;
      else
         Gnoga.Gui.Base.Base_Type (Object).On_Message (Event, Message);
      end if;
   end On_Message;
end Gnoga.Gui.Window;
