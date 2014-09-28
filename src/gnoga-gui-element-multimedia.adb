------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--         G N O G A . G U I . E L E M E N T . M U L T I M E D I A          --
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
package body Gnoga.Gui.Element.Multimedia is

   ----------
   -- Play --
   ----------

   procedure Play (Media : in out Multimedia_Type) is
   begin
      Media.Execute ("play()");
   end Play;

   -----------
   -- Pause --
   -----------

   procedure Pause (Media : in out Multimedia_Type) is
   begin
      Media.Execute ("pause()");
   end Pause;

   ----------
   -- Load --
   ----------

   procedure Load (Media : in out Multimedia_Type) is
   begin
      Media.Execute ("load()");
   end Load;

   ------------
   -- Create --
   ------------

   procedure Create (Audio     : in out Audio_Type;
                     Parent    : in out Gnoga.Gui.Base.Base_Type'Class;
                     Source    : in     String  := "";
                     Controls  : in     Boolean := True;
                     Preload   : in     Boolean := False;
                     Autoplay  : in     Boolean := False;
                     Autoloop  : in     Boolean := False;
                     Muted     : in     Boolean := False;
                     ID        : in     String  := "")
   is
      function Has_Controls return String is
      begin
         if Controls then
            return " controls";
         else
            return "";
         end if;
      end Has_Controls;

      function Has_Preload return String is
      begin
         if Preload then
            return " preload='auto'";
         else
            return "";
         end if;
      end Has_Preload;

      function Has_Autoplay return String is
      begin
         if Autoplay then
            return " autoplay";
         else
            return "";
         end if;
      end Has_Autoplay;

      function Has_Autoloop return String is
      begin
         if Autoloop then
            return " loop";
         else
            return "";
         end if;
      end Has_Autoloop;

      function Has_Muted return String is
      begin
         if Muted then
            return " muted";
         else
            return "";
         end if;
      end Has_Muted;

      function Has_Source return String is
      begin
         if Source /= "" then
            return " src=""" & Escape_Quotes (Source) & """";
         else
            return "";
         end if;
      end Has_Source;
   begin
      Audio.Create_From_HTML (Parent => Parent,
                              HTML   => "<audio" &
                                Has_Controls &
                                Has_Preload &
                                Has_Autoplay &
                                Has_AutoLoop &
                                Has_Muted &
                                Has_Source &
                                " />",
                              ID     => ID);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create (Video     : in out Video_Type;
                     Parent    : in out Gnoga.Gui.Base.Base_Type'Class;
                     Source    : in     String  := "";
                     Controls  : in     Boolean := True;
                     Preload   : in     Boolean := False;
                     Poster    : in     String  := "";
                     Autoplay  : in     Boolean := False;
                     Autoloop  : in     Boolean := False;
                     Muted     : in     Boolean := False;
                     ID        : in     String  := "")
   is
      function Has_Controls return String is
      begin
         if Controls then
            return " controls";
         else
            return "";
         end if;
      end Has_Controls;

      function Has_Preload return String is
      begin
         if Preload then
            return " preload='auto'";
         else
            return "";
         end if;
      end Has_Preload;

      function Has_Autoplay return String is
      begin
         if Autoplay then
            return " autoplay";
         else
            return "";
         end if;
      end Has_Autoplay;

      function Has_Autoloop return String is
      begin
         if Autoloop then
            return " loop";
         else
            return "";
         end if;
      end Has_Autoloop;

      function Has_Muted return String is
      begin
         if Muted then
            return " muted";
         else
            return "";
         end if;
      end Has_Muted;

      function Has_Poster return String is
      begin
         if Source /= "" then
            return " poster=""" & Escape_Quotes (Poster) & """";
         else
            return "";
         end if;
      end Has_Poster;

      function Has_Source return String is
      begin
         if Source /= "" then
            return " src=""" & Escape_Quotes (Source) & """";
         else
            return "";
         end if;
      end Has_Source;
   begin
      Video.Create_From_HTML (Parent => Parent,
                              HTML   => "<video" &
                                Has_Controls &
                                Has_Preload &
                                Has_Autoplay &
                                Has_AutoLoop &
                                Has_Muted &
                                Has_Poster &
                                Has_Source &
                                " />",
                              ID     => ID);
   end Create;

end Gnoga.Gui.Element.Multimedia;
