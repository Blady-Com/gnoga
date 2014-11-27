--                                                                    --
--  package Object                  Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2002       --
--  Single tasking version                                            --
--                                Last revision :  10:25 26 Dec 2009  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Tags;        use Ada.Tags;

with Ada.Unchecked_Deallocation;
with System; use type System.Address;

package body Object is

   function Equal
            (  Left  : Entity;
               Right : Entity'Class;
               Flag  : Boolean := False
            )  return Boolean is
   begin
      if Flag or else Right in Entity then
         return Left'Address = Right'Address;
      else
         return Equal (Right, Left, True);
      end if;
   end Equal;

   procedure Finalize (Object : in out Entity) is
   begin
      if 0 /= Object.Use_Count then
         Raise_Exception
         (  Program_Error'Identity,
            (  Ada.Tags.Expanded_Name (Entity'Class (Object)'Tag)
            &  " is still in use"
         )  );
      end if;
   end Finalize;

   procedure Increment_Count (Object : in out Entity) is
   begin
      Object.Use_Count := Object.Use_Count + 1;
   end Increment_Count;

   procedure Initialize (Object : in out Entity) is
   begin
      null;
   end Initialize;

   function Less
            (  Left  : Entity;
               Right : Entity'Class;
               Flag  : Boolean := False
            )  return Boolean is
   begin
      if Flag or else Right in Entity then
         return Left'Address < Right'Address;
      else
         return
            not (Less (Right, Left, True) or else Equal (Right, Left));
      end if;
   end Less;

   procedure Decrement_Count (Object : in out Entity) is
   begin
      if Object.Use_Count = 0 then
         Raise_Exception
         (  Program_Error'Identity,
            (  Expanded_Name (Entity'Class (Object)'Tag)
            &  " has zero count"
         )  );
      else
         Object.Use_Count := Object.Use_Count - 1;
      end if;
   end Decrement_Count;

   procedure Put_Traceback (Object : Entity'Class) is
   begin
      null;
   end Put_Traceback;

   procedure Release (Ptr : in out Entity_Ptr) is
      procedure Free is new
         Ada.Unchecked_Deallocation (Entity'Class, Entity_Ptr);
   begin
      if Ptr /= null then
         declare
            Object : Entity'Class renames Ptr.all;
         begin
            Decrement_Count (Object);
            if Object.Use_Count > 0 then
               return;
            end if;
         end;
         Free (Ptr);
      end if;
   end Release;

   procedure Set_Trace_File (File : String) is
   begin
      null;
   end Set_Trace_File;

end Object;
