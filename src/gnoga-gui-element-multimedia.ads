------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--         G N O G A . G U I . E L E M E N T . M U L T I M E D I A          --
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

package Gnoga.Gui.Element.Multimedia is

   -------------------------------------------------------------------------
   --  Multimedia_Types
   -------------------------------------------------------------------------
   --  Base type for multimedia Elements

   type Multimedia_Type is new Gnoga.Gui.Element.Element_Type with private;
   type Multimedia_Access is access all Multimedia_Type;
   type Pointer_To_Multimedia_Class is access all Multimedia_Type'Class;

   -------------------------------------------------------------------------
   --  Audio_Type - Methods
   -------------------------------------------------------------------------

   procedure Play (Media : in out Multimedia_Type);

   procedure Pause (Media : in out Multimedia_Type);

   procedure Load (Media : in out Multimedia_Type);
   --  Loads or reloads media

   -------------------------------------------------------------------------
   --  Audio_Types
   -------------------------------------------------------------------------

   type Audio_Type is new Multimedia_Type with private;
   type Audio_Access is access all Audio_Type;
   type Pointer_To_Audio_Class is access all Audio_Type'Class;

   -------------------------------------------------------------------------
   --  Audio_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Audio     : in out Audio_Type;
                     Parent    : in out Gnoga.Gui.Base.Base_Type'Class;
                     Source    : in     String  := "";
                     Controls  : in     Boolean := True;
                     Preload   : in     Boolean := False;
                     Autoplay  : in     Boolean := False;
                     Autoloop  : in     Boolean := False;
                     Muted     : in     Boolean := False;
                     ID        : in     String  := "");
   --  Create an Audio control with audio from Content

   -------------------------------------------------------------------------
   --  Audio_Type - Properties
   -------------------------------------------------------------------------

   -------------------------------------------------------------------------
   --  Audio_Type - Methods
   -------------------------------------------------------------------------

   -------------------------------------------------------------------------
   --  Video_Types
   -------------------------------------------------------------------------

   type Video_Type is new Multimedia_Type with private;
   type Video_Access is access all Video_Type;
   type Pointer_To_Video_Class is access all Video_Type'Class;

   -------------------------------------------------------------------------
   --  Video_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Video     : in out Video_Type;
                     Parent    : in out Gnoga.Gui.Base.Base_Type'Class;
                     Source    : in     String  := "";
                     Controls  : in     Boolean := True;
                     Preload   : in     Boolean := False;
                     Poster    : in     String  := "";
                     Autoplay  : in     Boolean := False;
                     Autoloop  : in     Boolean := False;
                     Muted     : in     Boolean := False;
                     ID        : in     String  := "");
   --  Create an Video control with Video from Content. Poster is a URL
   --  to an image to display until play begins.

   -------------------------------------------------------------------------
   --  Video_Type - Properties
   -------------------------------------------------------------------------

   -------------------------------------------------------------------------
   --  Video_Type - Methods
   -------------------------------------------------------------------------
private
   type Multimedia_Type is new Gnoga.Gui.Element.Element_Type with null record;
   type Audio_Type is new Multimedia_Type with null record;
   type Video_Type is new Multimedia_Type with null record;
end Gnoga.Gui.Element.Multimedia;
