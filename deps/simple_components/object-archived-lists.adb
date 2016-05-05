--                                                                    --
--  package Object.Archived.Lists   Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Autumn, 2004       --
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

package body Object.Archived.Lists is

   function "<" (Left, Right : Deposit_Ptr) return Boolean is
   begin
      if Left = null then
         return Right /= null;
      elsif Right = null then
         return False;
      else
         return Less (Left.all, Right.all);
      end if;
   end "<";

   procedure Add
             (  Container : in out Deposit_List;
                Object    : Deposit_Ptr;
                Backward  : Boolean := False
             )  is
      This : Dependency_List_Ptr := Ptr (Container.Handle);
   begin
      if Object = null then
         return;
      end if;
      if This = null then
         --
         -- Create new list body
         --
         This := new Dependency_List;
         Container.Handle := Ref (This);
      elsif This.Use_Count > 1 then
         --
         -- The  list  body is shared, so we have to clone it before any
         -- change made.
         --
         declare
            List : Dependency_List'Class renames This.all;
         begin
            This := new Dependency_List;
            Container.Handle := Ref (This);
            This.Size        := List.Size;
            This.Links       := List.Links;
            This.References  := List.References;
         end;
      end if;
      declare
         List  : Dependency_List'Class renames This.all;
         Index : constant Integer := List.Size + 1;
         Key   : Integer := Index;
      begin
         Put (List.Links, Index, Object);
         if Backward then
            if Is_In (List.References, Object) then
               Key := Get (List.References, Index);
               if Key > 0 then
                  Replace (List.References, Object, -Key);
               end if;
            else
               Add (List.References, Object, -Key);
            end if;
         else
            if not Is_In (List.References, Object) then
               Add (List.References, Object, Key);
            end if;
         end if;
         List.Size := Index;
      end;
   end Add;

   procedure Erase (Container : in out Deposit_List) is
      This : constant Dependency_List_Ptr := Ptr (Container.Handle);
   begin
      if This = null then
         return;
      end if;
      declare
         List : Dependency_List'Class renames This.all;
      begin
         if List.Use_Count > 1 then
            Invalidate (Container.Handle);
         else
            for Index in 1..List.Size loop
               Put (List.Links, Index, null);
            end loop;
            Erase (List.References);
            List.Size := 0;
         end if;
      end;
   end Erase;

   function Get (Container : Deposit_List; Index : Positive)
      return Deposit_Ptr is
      This : constant Dependency_List_Ptr := Ptr (Container.Handle);
   begin
      if This = null or else Index > This.Size then
         raise Constraint_Error;
      end if;
      return Get (This.Links, Index);
   end Get;

   function Get_Size (Container : Deposit_List) return Natural is
      This : constant Dependency_List_Ptr := Ptr (Container.Handle);
   begin
      if This = null then
         return 0;
      else
         return This.Size;
      end if;
   end Get_Size;

   function Get_Total (Container : Deposit_List) return Natural is
      This : constant Dependency_List_Ptr := Ptr (Container.Handle);
   begin
      if This = null then
         return 0;
      else
         return Get_Size (This.References);
      end if;
   end Get_Total;

   function Is_Backward
            (  Container : Deposit_List;
               Object    : Deposit_Ptr
            )  return Boolean is
   begin
      return Get (Ptr (Container.Handle).References, Object) < 0;
   end Is_Backward;

   function Is_First
            (  Container : Deposit_List;
               Index     : Positive
            )  return Boolean is
      List : Dependency_List'Class renames Ptr (Container.Handle).all;
   begin
      return abs Get (List.References, Get (List.Links, Index)) = Index;
   end Is_First;

   function Is_In
            (  Container : Deposit_List;
               Object    : Deposit_Ptr
            )  return Boolean is
      This : constant Dependency_List_Ptr := Ptr (Container.Handle);
   begin
      if This = null then
         return False;
      else
         return Is_In (Ptr (Container.Handle).References, Object);
      end if;
   end Is_In;

end Object.Archived.Lists;
