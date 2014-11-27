--                                                                    --
--  package Test_Stack_Items        Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Autumn, 2003       --
--                                                                    --
--                                Last revision :  20:47 23 Jun 2010  --
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

with Ada.Text_IO;  use Ada.Text_IO;

package body Test_Stack_Item is
   type Item_Log is array (Integer range 1..1000) of Boolean;
   No  : Natural  := 0;
   Log : Item_Log := (others => False);

   procedure Finalize (Object : in out Item) is
   begin
      Put_Line ("Deleting" & Integer'Image (Object.No));
      Log (Object.No) := False;      
      Stack_Objects.Finalize (Stack_Object (Object));
   end Finalize;

   procedure Initialize (Object : in out Item) is
   begin
      Stack_Objects.Initialize (Stack_Object (Object));
      No := No + 1;
      Object.No := No;
      Log (No) := True;
      Put_Line ("Created" & Integer'Image (No));
   end Initialize;

   function Is_Allocated (No : Positive) return Boolean is
   begin
      return Log (No);
   end Is_Allocated;

end Test_Stack_Item;
