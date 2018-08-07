--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Generic_Shared_Object                       Spring, 2018       --
--  Implementation                                                    --
--                                Last revision :  19:18 30 Apr 2018  --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with Ada.Tags;           use Ada.Tags;

package body Synchronization.Interprocess.Generic_Shared_Object is

   procedure Finalize (Object : in out Shared_Object) is
   begin
      null;
   end Finalize;

   procedure Generic_Call (Object : in out Shared_Object) is
      Lock : Holder (Object.Lock.all'Unchecked_Access);
   begin
      Operation (Object.Value.all);
   end Generic_Call;

   function Get (Object : Shared_Object) return Object_Type is
      Lock : Holder (Object.Lock.all'Unchecked_Access);
   begin
      return Object.Value.all;
   end Get;

   function Get_Signature (Object : Shared_Object) return Unsigned_16 is
   begin
      return Get_Signature (External_Tag (Shared_Object'Tag));
   end Get_Signature;

   function Get_Size (Object : Shared_Object) return Storage_Count is
   begin
      return Round (Object_Type'Max_Size_In_Storage_Elements);
   end Get_Size;

   procedure Set
             (  Object : in out Shared_Object;
                Value  : Object_Type
             )  is
      Lock : Holder (Object.Lock.all'Unchecked_Access);
   begin
      Object.Value.all := Value;
   end Set;

   procedure Map
             (  Object   : in out Shared_Object;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             )  is
      package Mapper is new Generic_Memory_Mapper (Object_Type);
      This : Abstract_Shared_Object_Ptr := Shared.First;
   begin
      while This /= null and then This /= Object'Unchecked_Access loop
         if This.all in Mutex'Class then
            Object.Lock := Mutex'Class (This.all)'Unchecked_Access;
         end if;
         This := This.Next;
      end loop;
      if Object.Lock = null then
         Raise_Exception
         (  Mode_Error'Identity,
            (  "The environment contains no mutex record member "
            &  "appearing before the shared object"
         )  );
      end if;
      Object.Value := Mapper.Map (Location, Owner).all'Unchecked_Access;
   end Map;

end Synchronization.Interprocess.Generic_Shared_Object;
