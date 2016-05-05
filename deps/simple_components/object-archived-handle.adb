--                                                                    --
--  package Object.Archived.Handle  Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2003       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with Ada.Unchecked_Conversion;

package body Object.Archived.Handle is

   function To_Deposit_Ptr is
      new Ada.Unchecked_Conversion (Object_Ptr_Type, Deposit_Ptr);

   function From_Deposit_Ptr is
      new Ada.Unchecked_Conversion (Deposit_Ptr, Object_Ptr_Type);

   function To_Object_Ptr (Object : Deposit_Ptr)
      return Object_Ptr_Type is
   begin
      if Object.all in Object_Type'Class then
         return From_Deposit_Ptr (Object);
      else
         raise Constraint_Error;
      end if;
   end To_Object_Ptr;

   procedure Add
             (  Container : in out Deposit_Container'Class;
                Object    : Handle;
                Backward  : Boolean := False
             )  is
   begin
      if Is_Valid (Object) then
         Add (Container, To_Deposit_Ptr (Ptr (Object)), Backward);
      else
         raise Constraint_Error;
      end if;
   end Add;

   procedure Delete (Object : in out Handle) is
   begin
      if Is_Valid (Object) then
         Delete (Ptr (Object).all);
         Invalidate (Object);
      end if;
   end Delete;

   function Get_Class (Object : Handle) return String is
   begin
      return Get_Class (Ptr (Object).all);
   end Get_Class;

   procedure Get_References
             (  Object    : Handle;
                Container : in out Deposit_Container'Class
             )  is
      This     : constant Deposit_Ptr := To_Deposit_Ptr (Ptr (Object));
      Iterator : References_Iterator (Container'Unchecked_Access);
   begin
      if This /= null then
         Enumerate (Iterator, This.all);
      end if;
   end Get_References;

   procedure Invalidate (Object : in out Handle) is
   begin
      Handles.Invalidate (Handles.Handle (Object));
   end Invalidate;

   function Is_Backward
            (  Container : Deposit_Container'Class;
               Object    : Handle
            )  return Boolean is
   begin
      return Is_Backward (Container, To_Deposit_Ptr (Ptr (Object)));
   end Is_Backward;

   function Is_Dependent
            (  Dependant : Handle;
               Referent  : Handle
            )  return Boolean is
      Result   : aliased Deposit_Set;
      Iterator : Reference_To_Object (Result'Access);
   begin
      if not (Is_Valid (Referent) or else Is_Valid (Dependant)) then
         return False;
      end if;
      Iterator.Search_For := Referent;
      Enumerate (Iterator, Ptr (Dependant).all);
      return False;
   exception
      when End_Error =>
         return True;
   end Is_Dependent;

   function Is_Dependent
            (  Dependant : Handle;
               Referents : Deposit_Container'Class
            )  return Boolean is
      Result   : aliased Deposit_Set;
      Iterator : Reference_To_Any_Of (Result'Access);
   begin
      if not (Is_Empty (Referents) or else Is_Valid (Dependant)) then
         return False;
      end if;
      Iterator.Search_For := Referents'Unchecked_Access;
      Enumerate (Iterator, Ptr (Dependant).all);
      return False;
   exception
      when End_Error =>
         return True;
   end Is_Dependent;

   function Is_In
            (  Container : Deposit_Container'Class;
               Object    : Handle
            )  return Boolean is
   begin
      return Is_In (Container, To_Deposit_Ptr (Ptr (Object)));
   end Is_In;

   function Is_Valid (Object : Handle) return Boolean is
   begin
      return Handles.Is_Valid (Handles.Handle (Object));
   end Is_Valid;

   procedure On_Each
             (  Iterator : in out Reference_To_Object;
                Referent : Deposit_Ptr
             )  is
   begin
      if To_Deposit_Ptr (Ptr (Iterator.Search_For)) = Referent then
         raise End_Error;
      end if;
   end On_Each;

   procedure On_Each
             (  Iterator : in out Reference_To_Any_Of;
                Referent : Deposit_Ptr
             )  is
   begin
      if Is_In (Iterator.Search_For.all, Referent) then
         raise End_Error;
      end if;
   end On_Each;

   function Ref (Thing : Object_Ptr_Type) return Handle is
   begin
      return (Handles.Ref (Thing) with null record);
   end Ref;

   function Ref
            (  Container : Deposit_Container'Class;
               Index     : Positive
            )  return Handle is
   begin
      return Ref (To_Object_Ptr (Get (Container, Index)));
   end Ref;

   function References (Object : Handle) return Deposit_Set is
      Result   : aliased Deposit_Set;
      Iterator : References_Iterator (Result'Access);
      This     : constant Deposit_Ptr := To_Deposit_Ptr (Ptr (Object));
   begin
      if This /= null then
         Enumerate (Iterator, This.all);
      end if;
      return Result;
   end References;

   procedure Set (Object : in out Handle; Thing : Object_Ptr_Type) is
   begin
      Handles.Set (Handles.Handle (Object), Thing);
   end Set;

end Object.Archived.Handle;
