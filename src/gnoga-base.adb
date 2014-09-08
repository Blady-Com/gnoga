------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                          G N O G A . B A S E                             --
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
------------------------------------------------------------------------------                                                                          --

with Ada.Strings.Unbounded;
with Gnoga.Connections;

package body Gnoga.Base is

   function jQuery (Object : Base_Type) return String;
   --  Returns the jQuery selector for Object
   
   ------------
   -- jQuery --
   ------------

   function jQuery (Object : Base_Type) return String is
   begin
      return "$('" & Object.Script_Accessor & "')";
   end jQuery;
   
   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Base_Type) is
   begin
      Gnoga.Connections.New_Unique_ID (Object.Unique_ID);
   end Initialize;
   
   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Base_Type) is
   begin
      Object.On_Destroy;
      Object.Detach_From_Message_Queue;
   end Finalize;

   -------------------------------------------------------------------------
   --  Base_Type - Creation Methods
   -------------------------------------------------------------------------

   -----------------
   -- Create_Root --
   -----------------

   procedure Create_Root (Object        : in out Base_Type;
                          Connection_ID : in     Gnoga.Types.Connection_ID;
                          ID            : in     String;
                          HTML          : in     String)
   is
   begin
      Object.Create_With_Script
        (Connection_ID => Connection_ID,
         ID            => ID,
         Script        => "$(""body"").html(""" &
           Escape_Quotes (HTML) & """);");
   end Create_Root;

   -------------------
   -- Create_Inside --
   -------------------
   
   procedure Create_Inside (Object        : in out Base_Type;
                            Parent        : in out Base_Type'Class;
                            ID            : in     String;
                            HTML          : in     String)
   is
   begin
      Object.Create_With_Script
        (Connection_ID => Parent.Connection_ID,
         ID            => ID,
         Script        => jQuery (Base_Type (Parent)) &
           ".html(""" & Escape_Quotes (HTML) & """);");
   end Create_Inside;
   
   --------------------------
   -- Create_Inside_At_Top --
   --------------------------
   
   procedure Create_Inside_At_Top (Object        : in out Base_Type;
                                   Parent        : in out Base_Type'Class;
                                   ID            : in     String;
                                   HTML          : in     String)
   is
   begin
      Object.Create_With_Script
        (Connection_ID => Parent.Connection_ID,
         ID            => ID,
         Script        => jQuery (Base_Type (Parent)) &
           ".prepend(""" & Escape_Quotes (HTML) & """);");      
   end Create_Inside_At_Top;
   
   -----------------------------
   -- Create_Inside_At_Bottom --
   -----------------------------
   
   procedure Create_Inside_At_Bottom (Object        : in out Base_Type;
                                      Parent        : in out Base_Type'Class;
                                      ID            : in     String;
                                      HTML          : in     String)
   is
   begin
      Object.Create_With_Script
        (Connection_ID => Parent.Connection_ID,
         ID            => ID,
         Script        => jQuery (Base_Type (Parent)) &
           ".append(""" & Escape_Quotes (HTML) & """);");      
   end Create_Inside_At_Bottom;
   
   ------------------
   -- Create_After --
   ------------------
   
   procedure Create_After (Object        : in out Base_Type;
                           Target        : in out Base_Type'Class;
                           ID            : in     String;
                           HTML          : in     String)
   is
   begin
      Object.Create_With_Script
        (Connection_ID => Target.Connection_ID,
         ID            => ID,
         Script        => "$(""" & Escape_Quotes (HTML) & """).insertAfter(""#" &
           Target.ID & """);");
   end Create_After;

   -------------------
   -- Create_Before --
   -------------------
   
   procedure Create_Before (Object        : in out Base_Type;
                            Target        : in out Base_Type'Class;
                            ID            : in     String;
                            HTML          : in     String)
   is
   begin
      Object.Create_With_Script
        (Connection_ID => Target.Connection_ID,
         ID            => ID,
         Script        => "$(""" & Escape_Quotes (HTML) &
           """).insertBefore(""#" & Target.ID & """);");
   end Create_Before;

   ------------------------
   -- Create_With_Script --
   ------------------------

   procedure Create_With_Script
     (Object        : in out Base_Type;
      Connection_ID : in     Gnoga.Types.Connection_ID;
      ID            : in     String;
      Script        : in     String)
   is
   begin      
      Gnoga.Connections.Execute_Script (ID     => Connection_ID,
                                        Script => Script);

      Object.Attach (Connection_ID => Connection_ID,
                     ID            => ID);
      
      Object.On_Create;      
   end Create_With_Script;

   ------------
   -- Attach --
   ------------
   
   procedure Attach (Object        : in out Base_Type;
                     Connection_ID : in     Gnoga.Types.Connection_ID;
                     ID            : in     String)
   is
   begin
      Object.DOM_ID        := Ada.Strings.Unbounded.To_Unbounded_String (ID);
      Object.Connection_ID := Connection_ID;      

      Object.Attach_To_Message_Queue;
   end Attach;

   -------------------------------------------------------------------------
   --  Base_Type - Properties
   -------------------------------------------------------------------------

   ---------------
   -- Unique_ID --
   ---------------
   
   function Unique_ID (Object : Base_Type) return Gnoga.Types.Unique_ID is
   begin
      return Object.Unique_ID;
   end Unique_ID;

   function Unique_ID (Object : Base_Type) return String is
   begin
      return Object.Unique_ID'Img;
   end Unique_ID;
   
   -------------------
   -- Connection_ID --
   -------------------
   
   function Connection_ID (Object : Base_Type)
			  return Gnoga.Types.Connection_ID
   is
   begin
      return Object.Connection_ID;
   end Connection_ID;
   
   --------
   -- ID --
   --------
   
   function ID (Object : Base_Type) return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Object.DOM_ID);
   end ID;

   ------------
   -- Height --
   ------------
   
   procedure Height (Object : in out Base_Type; Value : in Integer) is
      Message_Script : constant String := jQuery(Object) &
        ".height(" & Left_Trim (Value'Img) & ");";
   begin
      Gnoga.Connections.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);
   end Height;
   
   function Height (Object : Base_Type) return Integer is
      Message_Script : constant String := jQuery(Object) & ".height();";

      R : String := Gnoga.Connections.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);
   begin
      return Integer (Float'Value (R));
   end Height;
   
   -----------
   -- Width --
   -----------
   
   procedure Width (Object : in out Base_Type; Value : in Integer) is
      Message_Script : constant String := jQuery(Object) &
        ".width(" & Left_Trim (Value'Img) & ");";
   begin
      Gnoga.Connections.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);
   end Width;
         
   function Width (Object : Base_Type) return Integer is
      Message_Script : constant String := jQuery(Object) & ".width();";
      R : String := Gnoga.Connections.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);
   begin
      return Integer (Float'Value (R));
   end Width;

   -------------
   -- Visible --
   -------------
   
   procedure Visible (Object : in out Base_Type; Value : Boolean := True)
   is
   begin
      if Value then
         Object.Style ("visibility", "visible");
      else
         Object.Style ("visibility", "hidden");
      end if;
   end Visible;
   
   function Visible (Object : Base_Type) return Boolean
   is
   begin
      return Object.Style ("visibility") = "visible";
   end Visible;
   
   --------------
   -- Property --
   --------------
   
   procedure Property (Object : in out Base_Type;
                       Name   : in String;
                       Value  : in String)
   is
      Message_Script : constant String := jQuery(Object) &
        ".prop ('" & Name & "')=""" & Escape_Quotes (Value) & """";
   begin
      Gnoga.Connections.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);               
   end Property;
   
   function Property (Object : Base_Type; Name : String) return String is
      Message_Script : constant String := jQuery(Object) &
        ".prop ('" & Name & "');";
   begin
      return Gnoga.Connections.Execute_Script (ID     => Object.Connection_ID,
                                               Script => Message_Script);
   end Property;

   -----------
   -- Style --
   -----------
   
   procedure Style (Object : in out Base_Type;
                    Name   : in String;
                    Value  : in String)
   is
      Message_Script : constant String := jQuery(Object) &
        ".css ('" & Name & "', """ & Escape_Quotes (Value) & """);";
   begin
      Gnoga.Connections.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);               
   end Style;
   
   function Style (Object : Base_Type; Name : String) return String is
      Message_Script : constant String := jQuery(Object) &
        ".css ('" & Name & "');";
   begin
      return Gnoga.Connections.Execute_Script (ID     => Object.Connection_ID,
                                               Script => Message_Script);
   end Style;

   
   ---------------
   -- Attribute --
   ---------------
   
   procedure Attribute (Object : in out Base_Type;
                        Name   : in String;
                        Value  : in String)
   is
      Message_Script : constant String := jQuery(Object) &
        ".attr ('" & Name & "')=""" & Escape_Quotes (Value) & """";
   begin
      Gnoga.Connections.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);               
   end Attribute;
         
   function Attribute (Object : Base_Type; Name : String) return String is
      Message_Script : constant String := jQuery(Object) &
        ".attr ('" & Name & "');";
   begin
      return Gnoga.Connections.Execute_Script (ID     => Object.Connection_ID,
                                               Script => Message_Script);
   end Attribute;

   -------------------------------------------------------------------------
   --  Base_Type - Methods
   -------------------------------------------------------------------------

   ------------   
   -- Execute--
   ------------
   
   procedure Execute (Object : in out Base_Type; Method : in String) is
      Message_Script : constant String := jQuery(Object) & "." & Method;
   begin
      Gnoga.Connections.Execute_Script
        (ID     => Object.Connection_ID,
         Script => Message_Script);         
   end Execute;

   function Execute (Object : Base_Type; Method : in String) return String is
      Message_Script : constant String := jQuery(Object) & "." & Method;
   begin
      return Gnoga.Connections.Execute_Script (ID     => Object.Connection_ID,
                                               Script => Message_Script);
   end Execute;
   
   -------------------------------------------------------------------------
   --  Base_Type - Events
   -------------------------------------------------------------------------

   
   --------------
   -- On_Click --
   --------------   

   procedure On_Click_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event)
   is
   begin
      Object.On_Click_Event := Handler;      
      
      Object.Bind_Event (Event   => "click",
                         Message => "");
   end On_Click_Handler;
   
   procedure Fire_On_Click (Object : in out Base_Type)
   is
   begin
      if Object.On_Click_Event /= null then
         Object.On_Click_Event (Object);
      end if;
   end Fire_On_Click;

      
   
   ---------------
   -- On_Create --
   ---------------   
   
   procedure On_Create (Object : in out Base_Type)
   is
   begin      
      Object.Fire_On_Create;
   end On_Create;
   
   procedure On_Create_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event)
   is
   begin
      Object.On_Create_Event := Handler;      
   end On_Create_Handler;
   
   procedure Fire_On_Create (Object : in out Base_Type)
   is
   begin
      if Object.On_Create_Event /= null then
         Object.On_Create_Event (Object);
      end if;
   end Fire_On_Create;
   
   ----------------
   -- On_Destroy --
   ----------------   
   
   procedure On_Destroy (Object : in out Base_Type)
   is
   begin
      Object.Fire_On_Destroy;
   end On_Destroy;
   
   procedure On_Destroy_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event)
   is
   begin
      Object.On_Destroy_Event := Handler;      
   end On_Destroy_Handler;
   
   procedure Fire_On_Destroy (Object : in out Base_Type)
   is
   begin
      if Object.On_Destroy_Event /= null then
         Object.On_Destroy_Event (Object);
      end if;
   end Fire_On_Destroy;
   
   ----------------
   -- On_Message --
   ----------------
   
   procedure On_Message (Object  : in out Base_Type;
                         Event   : in     String;
                         Message : in     String)
   is
   begin
      if Event = "click" then
         Object.Fire_On_Click;
      end if;

      Object.Fire_On_Message (Event, Message);
   end On_Message;
   
   procedure On_Message_Handler (Object  : in out Base_Type;
                                 Handler : in     Message_Event)
   is
   begin
      Object.On_Message_Event := Handler;      
   end On_Message_Handler;
   
   procedure Fire_On_Message (Object  : in out Base_Type;
                              Event   : in     String;
                              Message : in     String)
   is
   begin
      if Object.On_Message_Event /= null then
         Object.On_Message_Event (Object, Event, Message);
      end if;
   end Fire_On_Message;

   -------------------------------------------------------------------------
   --  Base_Type - Event Internals
   -------------------------------------------------------------------------

   ----------------
   -- Bind_Event --
   ----------------
   
   procedure Bind_Event (Object  : in out Base_Type;
                         Event   : in     String;
                         Message : in     String)
   is
      US : constant String := Object.Unique_ID'Img;
      
      Full_Message : constant String := US (US'First + 1 .. US'Last) &
        "|" & Event & "|" & Message;
   begin
      Bind_Event_Script (Object => Object,
                         Event  => Event,
                         Script => "ws.send (""" &
                           Escape_Quotes (Full_Message) & """);");
   end Bind_Event;

   -----------------------
   -- Bind_Event_Script --
   -----------------------
   
   procedure Bind_Event_Script (Object : in out Base_Type;
                                Event  : in     String;
                                Script : in     String)
   is
   begin
      Bind_Event_Script (Connection_ID => Object.Connection_ID,
                         Query         => "#" & Object.ID,
                         Event         => Event,
                         Script        => Script);
   end Bind_Event_Script;
   
   -----------------------
   -- Bind_Event_Script --
   -----------------------
   
   procedure Bind_Event_Script (Connection_ID : in Gnoga.Types.Connection_ID;
                                Query         : in String;
                                Event         : in String;
                                Script        : in String)
   is
      Message_Script : constant String :=
        "$(""" & Query & """).on (""" & Event &
        """, function () {" &
        Script & "}" &
        "); ";
   begin
      Gnoga.Connections.Execute_Script
        (ID     => Connection_ID,
         Script => Message_Script);
   end Bind_Event_Script;

   -----------------------------
   -- Attach_To_Message_Queue --
   -----------------------------
   
   procedure Attach_To_Message_Queue (Object : in out Base_Type) is
   begin
      Gnoga.Connections.Add_To_Message_Queue (Object);
   end Attach_To_Message_Queue;

   --------------------------------
   -- Detach_From_Message_Queue --
   --------------------------------
   
   procedure Detach_From_Message_Queue (Object : in out Base_Type) is
   begin
      Gnoga.Connections.Delete_From_Message_Queue (Object);
   end Detach_From_Message_Queue;

   ---------------------
   -- Script_Accessor --
   ---------------------
   
   function Script_Accessor (Object : Base_Type) return String is
   begin
      return "#" & Object.ID;
   end Script_Accessor;
    
end Gnoga.Base;
