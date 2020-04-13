------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                    G N O G A . C L I E N T . F I L E S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2020 Pascal Pignard                    --
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

with Gnoga.Server.Connection;
with Gnoga.Types;

package body Gnoga.Client.Files is

   ------------
   -- Create --
   ------------

   procedure Create
     (Reader : in out File_Reader_Type;
      Window : in     Gnoga.Gui.Window.Window_Type)
   is
      GID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Reader.Create_With_Script
        (Window.Connection_ID, GID, "gnoga['" & GID & "'] = new FileReader()",
         Gnoga.Types.Gnoga_ID);
   end Create;

   ----------------
   -- Error_Code --
   ----------------

   function Error_Code (Reader : File_Reader_Type) return Integer is
   begin
      if Reader.Property ("error") /= "null" then
         return Reader.jQuery_Execute ("prop ('error').code");
      else
         return 0;
      end if;
   end Error_Code;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message (Reader : File_Reader_Type) return String is
   begin
      if Reader.Property ("error") /= "null" then
         return Reader.jQuery_Execute ("prop ('error').message");
      else
         return "";
      end if;
   end Error_Message;

   ----------------
   -- Error_Name --
   ----------------

   function Error_Name (Reader : File_Reader_Type) return String is
   begin
      if Reader.Property ("error") /= "null" then
         return Reader.jQuery_Execute ("prop ('error').name");
      else
         return "";
      end if;
   end Error_Name;

   -----------
   -- State --
   -----------

   function State (Reader : File_Reader_Type) return State_Type is
   begin
      return State_Type'Val (Reader.Property ("readyState"));
   end State;

   -------------
   -- Content --
   -------------

   function Content (Reader : File_Reader_Type) return String is
   begin
      return Reader.Property ("result");
   end Content;

   ---------------------
   -- Transfert_Abort --
   ---------------------

   procedure Transfert_Abort (Reader : in out File_Reader_Type) is
   begin
      Reader.Execute ("abort()");
   end Transfert_Abort;

   -------------------------
   -- Transfert_As_Binary --
   -------------------------

   procedure Transfert_As_Binary
     (Reader : in out File_Reader_Type;
      Files  : in     Gnoga.Gui.Element.Form.File_Type'class;
      Index  : in     Positive := 1)
   is
   begin
      Reader.Execute
        ("readAsText(" & Files.Script_Accessor & ".prop ('files')[" &
         Natural'Image (Index - 1) & "])");
   end Transfert_As_Binary;

   -----------------------
   -- Transfert_As_Text --
   -----------------------

   procedure Transfert_As_Text
     (Reader : in out File_Reader_Type;
      Files  : in     Gnoga.Gui.Element.Form.File_Type'class;
      Index  : in     Positive := 1; Encoding : in String := "UTF-8")
   is
   begin
      Reader.Execute
        ("readAsText(" & Files.Script_Accessor & ".prop ('files')[" &
         Natural'Image (Index - 1) & "],'" & Encoding & "')");
   end Transfert_As_Text;

   --------------
   -- On_Abort --
   --------------

   procedure On_Abort_Handler
     (Reader : in out File_Reader_Type; Handler : in File_Reader_Event)
   is
   begin
      if Reader.On_Abort_Event /= null then
         Reader.Unbind_Event ("abort");
      end if;

      Reader.On_Abort_Event := Handler;

      if Handler /= null then
         Reader.Bind_Event (Event => "abort", Message => "");
      end if;
   end On_Abort_Handler;

   procedure Fire_On_Abort
     (Reader : in out File_Reader_Type; Event : in String)
   is
   begin
      if Reader.On_Abort_Event /= null then
         Reader.On_Abort_Event (Reader, Event);
      end if;
   end Fire_On_Abort;

   ---------------
   -- On_Error_ --
   ---------------

   procedure On_Error_Handler
     (Reader : in out File_Reader_Type; Handler : in File_Reader_Event)
   is
   begin
      if Reader.On_Error_Event /= null then
         Reader.Unbind_Event ("error");
      end if;

      Reader.On_Error_Event := Handler;

      if Handler /= null then
         Reader.Bind_Event (Event => "error", Message => "");
      end if;
   end On_Error_Handler;

   procedure Fire_On_Error
     (Reader : in out File_Reader_Type; Event : in String)
   is
   begin
      if Reader.On_Error_Event /= null then
         Reader.On_Error_Event (Reader, Event);
      end if;
   end Fire_On_Error;

   -------------
   -- On_Load --
   -------------

   procedure On_Load_Handler
     (Reader : in out File_Reader_Type; Handler : in File_Reader_Event)
   is
   begin
      if Reader.On_Load_Event /= null then
         Reader.Unbind_Event ("load");
      end if;

      Reader.On_Load_Event := Handler;

      if Handler /= null then
         Reader.Bind_Event (Event => "load", Message => "");
      end if;
   end On_Load_Handler;

   procedure Fire_On_Load (Reader : in out File_Reader_Type; Event : in String)
   is
   begin
      if Reader.On_Load_Event /= null then
         Reader.On_Load_Event (Reader, Event);
      end if;
   end Fire_On_Load;

   ------------------
   -- On_Load_End --
   ------------------

   procedure On_Load_End_Handler
     (Reader : in out File_Reader_Type; Handler : in File_Reader_Event)
   is
   begin
      if Reader.On_Load_End_Event /= null then
         Reader.Unbind_Event ("loadend");
      end if;

      Reader.On_Load_End_Event := Handler;

      if Handler /= null then
         Reader.Bind_Event (Event => "loadend", Message => "");
      end if;
   end On_Load_End_Handler;

   procedure Fire_On_Load_End
     (Reader : in out File_Reader_Type; Event : in String)
   is
   begin
      if Reader.On_Load_End_Event /= null then
         Reader.On_Load_End_Event (Reader, Event);
      end if;
   end Fire_On_Load_End;

   -------------------
   -- On_Load_Start --
   -------------------

   procedure On_Load_Start_Handler
     (Reader : in out File_Reader_Type; Handler : in File_Reader_Event)
   is
   begin
      if Reader.On_Load_Start_Event /= null then
         Reader.Unbind_Event ("loadstart");
      end if;

      Reader.On_Load_Start_Event := Handler;

      if Handler /= null then
         Reader.Bind_Event (Event => "loadstart", Message => "");
      end if;
   end On_Load_Start_Handler;

   procedure Fire_On_Load_Start
     (Reader : in out File_Reader_Type; Event : in String)
   is
   begin
      if Reader.On_Load_Start_Event /= null then
         Reader.On_Load_Start_Event (Reader, Event);
      end if;
   end Fire_On_Load_Start;

   -----------------
   -- On_Progress --
   -----------------

   procedure On_Progress_Handler
     (Reader : in out File_Reader_Type; Handler : in File_Reader_Event)
   is
   begin
      if Reader.On_Progress_Event /= null then
         Reader.Unbind_Event ("progress");
      end if;

      Reader.On_Progress_Event := Handler;

      if Handler /= null then
         Reader.Bind_Event (Event => "progress", Message => "");
      end if;
   end On_Progress_Handler;

   procedure Fire_On_Progress
     (Reader : in out File_Reader_Type; Event : in String)
   is
   begin
      if Reader.On_Progress_Event /= null then
         Reader.On_Progress_Event (Reader, Event);
      end if;
   end Fire_On_Progress;

   ----------------
   -- On_Message --
   ----------------

   overriding procedure On_Message
     (Reader : in out File_Reader_Type; Event : in String; Message : in String)
   is
   begin
      -- Reader Event --
      if Event = "abort" then
         Reader.Fire_On_Abort (Event);
      elsif Event = "error" then
         Reader.Fire_On_Error (Event);
      elsif Event = "load" then
         Reader.Fire_On_Load (Event);
      elsif Event = "loadend" then
         Reader.Fire_On_Load_End (Event);
      elsif Event = "loadstart" then
         Reader.Fire_On_Load_Start (Event);
      elsif Event = "progress" then
         Reader.Fire_On_Progress (Event);
      else
         Gnoga.Gui.Base.Base_Type (Reader).On_Message (Event, Message);
      end if;
   end On_Message;

end Gnoga.Client.Files;
