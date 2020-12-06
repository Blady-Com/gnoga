------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--         G N O G A . A P P L I C A T I O N . G T K _ W I N D O W          --
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
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

package body Gnoga.Application.Gtk_Window is

   pragma Linker_Options ("gnoga_gtk_window.o");
   pragma Linker_Options ("-lwebkit2gtk-3.0");
   pragma Linker_Options ("-lgtk-3");
   pragma Linker_Options ("-lgdk-3");
   pragma Linker_Options ("-lpangocairo-1.0");
   pragma Linker_Options ("-lpango-1.0");
   pragma Linker_Options ("-latk-1.0");
   pragma Linker_Options ("-lcairo-gobject");
   pragma Linker_Options ("-lcairo");
   pragma Linker_Options ("-lgdk_pixbuf-2.0");
   pragma Linker_Options ("-lsoup-2.4");
   pragma Linker_Options ("-lgio-2.0");
   pragma Linker_Options ("-lgobject-2.0");
   pragma Linker_Options ("-ljavascriptcoregtk-3.0");
   pragma Linker_Options ("-lglib-2.0");
   pragma Linker_Options ("-lpixman-1");

   Global_Port   : Integer;
   Global_Width  : Integer;
   Global_Height : Integer;

   task Gtk_Win_Launcher is
      entry Start;
   end Gtk_Win_Launcher;

   task body Gtk_Win_Launcher is
      nul : constant Unicode_Character := Unicode_Character'Val (0);

      procedure Launch_Gtk_Window
        (URL           : String;
         Width, Height : Integer);
      pragma Import (C, Launch_Gtk_Window, "Launch_Gtk_Window");
   begin
      accept Start;

      Launch_Gtk_Window
        (URL    => "http://127.0.0.1:" & Gnoga.Left_Trim (From_Latin_1 (Global_Port'Img)) & nul, Width => Global_Width,
         Height => Global_Height);
   end Gtk_Win_Launcher;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Port   : Integer := 8_080;
      Width  : Integer := 1_024;
      Height : Integer := 600)
   is
   begin
      Global_Port   := Port;
      Global_Width  := Width;
      Global_Height := Height;

      Gtk_Win_Launcher.Start;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
      procedure gtk_main_quit;
      pragma Import (C, gtk_main_quit, "gtk_main_quit");
   begin
      gtk_main_quit;
   end Finalize;

end Gnoga.Application.Gtk_Window;
