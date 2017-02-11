------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--          G N O G A . G U I . P L U G I N . P I X I . S P R I T E         --
--                                                                          --
--                                 B o d y                                  --
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
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

with Ada.Numerics.Elementary_Functions;
with Gnoga.Server.Connection;
with Gnoga.Types;

package body Gnoga.Gui.Plugin.Pixi.Sprite is

   ------------
   -- Create --
   ------------

   procedure Create
     (Sprite                        : in out Sprite_Type;
      Parent                        : in out Container_Type'Class;
      Image_Path                    : in     String;
      Row, Column                   : in     Integer;
      Row_Velocity, Column_Velocity : in     Velocity := 0.0)
   is
      Sprite_ID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Sprite.ID (Sprite_ID, Gnoga.Types.Gnoga_ID);
      Sprite.Connection_ID (Parent.Connection_ID);
      Gnoga.Server.Connection.Execute_Script
        (Sprite.Connection_ID,
         "gnoga['" &
         Sprite_ID &
         "'] = new PIXI.Sprite.fromImage('" &
         Image_Path &
         "');");
      Sprite.Locate (Row, Column);
      Sprite.Motion (Row_Velocity, Column_Velocity);
      Parent.Add_Child (Sprite);
   end Create;

   ------------
   -- Locate --
   ------------

   procedure Locate (Sprite : in out Sprite_Type; Row, Column : in Integer) is
   begin
      Sprite.Property ("x", Column);
      Sprite.Property ("y", Row);
   end Locate;

   --------------
   -- Position --
   --------------

   procedure Position (Sprite : in Sprite_Type; Row, Column : out Integer) is
   begin
      Row    := Sprite.Property ("y");
      Column := Sprite.Property ("x");
   end Position;

   ---------
   -- Row --
   ---------

   function Row (Sprite : in Sprite_Type) return Integer is
      V : constant Float := Sprite.Property ("y");
   begin
      return Integer (V);
   end Row;

   ------------
   -- Column --
   ------------

   function Column (Sprite : in Sprite_Type) return Integer is
      V : constant Float := Sprite.Property ("x");
   begin
      return Integer (V);
   end Column;

   -------------
   -- Pattern --
   -------------

   procedure Pattern (Sprite : in Sprite_Type; Image_Data : in String) is
   begin
      null; -- TODO
   end Pattern;

   -------------
   -- Pattern --
   -------------

   function Pattern (Sprite : in Sprite_Type) return String is
   begin
      return ""; -- TODO
   end Pattern;

   ------------
   -- Motion --
   ------------

   procedure Motion
     (Sprite                        : in out Sprite_Type;
      Row_Velocity, Column_Velocity : in     Velocity)
   is
   begin
      Sprite.Property ("gnoga_vx", Column_Velocity / 60.0);
      Sprite.Property ("gnoga_vy", Row_Velocity / 60.0);
   end Motion;

   ------------------
   -- Row_Velocity --
   ------------------

   function Row_Velocity (Sprite : in Sprite_Type) return Velocity is
   begin
      return Sprite.Property ("gnoga_vy") * 60.0;
   end Row_Velocity;

   ---------------------
   -- Column_Velocity --
   ---------------------

   function Column_Velocity (Sprite : in Sprite_Type) return Velocity is

   begin
      return Sprite.Property ("gnoga_vx") * 60.0;
   end Column_Velocity;

   -----------------
   -- Coincidence --
   -----------------

   function Coincidence
     (Sprite1, Sprite2 : in Sprite_Type;
      Tolerance        : in Natural) return Boolean
   is
   begin
      return Distance (Sprite1, Sprite2) <= Tolerance;
   end Coincidence;

   -----------------
   -- Coincidence --
   -----------------

   function Coincidence
     (Sprite      : in Sprite_Type;
      Row, Column : in Integer;
      Tolerance   : in Natural) return Boolean
   is
   begin
      return Distance (Sprite, Row, Column) <= Tolerance;
   end Coincidence;

   --------------
   -- Distance --
   --------------

   function Distance (Sprite1, Sprite2 : in Sprite_Type) return Natural is
   begin
      return Natural
          (Ada.Numerics.Elementary_Functions.Sqrt
             (Float (Sprite2.Row - Sprite1.Row)**2 +
              Float (Sprite2.Column - Sprite1.Column)**2));
   end Distance;

   --------------
   -- Distance --
   --------------

   function Distance
     (Sprite      : in Sprite_Type;
      Row, Column : in Integer) return Natural
   is
   begin
      return Natural
          (Ada.Numerics.Elementary_Functions.Sqrt
             (Float (Sprite.Row - Row)**2 +
              Float (Sprite.Column - Column)**2));
   end Distance;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Sprite : in out Sprite_Type;
      Parent : in out Container_Type'Class)
   is
   begin
      Parent.Remove_Child (Sprite);
   end Delete;

   ----------------
   -- Delete_All --
   ----------------

   procedure Delete_All (Parent : in out Container_Type'Class) is

   begin
      Gnoga.Server.Connection.Execute_Script
        (Parent.Connection_ID,
         "var gnoga_list = [];" &
         " for (var gnoga_sprite of gnoga['" &
         Parent.ID &
         "'].children) if (gnoga_sprite instanceof PIXI.Sprite) gnoga_list.push(gnoga_sprite);" &
         " for (var gnoga_sprite of gnoga_list)" &
         " gnoga['" &
         Parent.ID &
         "'].removeChild(gnoga_sprite);");
   end Delete_All;

end Gnoga.Gui.Plugin.Pixi.Sprite;
