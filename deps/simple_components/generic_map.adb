--                                                                    --
--  package Generic_Map             Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2003       --
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

with Ada.Unchecked_Deallocation;

package body Generic_Map is

   procedure Delete is
      new Ada.Unchecked_Deallocation (Data, Data_Ptr);

   procedure Delete is
      new Ada.Unchecked_Deallocation (Token_Array, Token_Array_Ptr);

   function Is_Empty (Container : Map) return Boolean is
   begin
      return
         Container.Object = null or else Container.Object.Size = 0;
   end Is_Empty;

   function Is_In (Container : Map; Key : Key_Type)
      return Boolean is
   begin
      return Find (Container, Key) > 0;
   end Is_In;

   procedure Replace
             (  Container : in out Map;
                Index     : Positive;
                Item      : Object_Type
             )  is
   begin
      if (  Container.Object = null
         or else
            Index > Container.Object.Size
         )
      then
         raise Constraint_Error;
      end if;
      if Container.Object.Use_Count > 0 then
         Clone (Container);
      end if;
      Container.Object.Vector (Index).Item := Item;
   end Replace;

   procedure Insert
             (  Container : in out Map;
                Index     : Positive;
                Key       : Key_Type;
                Item      : Object_Type
             )  is
   begin
      if Container.Object = null then
         Container.Object := new Data;
      elsif Container.Object.Use_Count > 0 then
         Clone (Container);
      end if;
      declare
         Object : Data renames Container.Object.all;
      begin
         if Object.Vector = null then
            Object.Vector := new Token_Array (1..Minimal_Size);
            Object.Vector (1).Key  := Key;
            Object.Vector (1).Item := Item;
         elsif Object.Size = Object.Vector'Last then
            declare
               Ptr : constant Token_Array_Ptr :=
                  new Token_Array
                      (  1
                      .. (  Object.Size
                         +  Natural'Max
                            (  Minimal_Size,
                               (  (  Object.Size
                                  *  (100 + Increment)
                                  )
                               /  100
                      )  )  )  );
            begin
               Ptr (1..Index - 1) := Object.Vector (1..Index - 1);
               Ptr (Index).Key  := Key;
               Ptr (Index).Item := Item;
               Ptr (Index + 1..Object.Size + 1) :=
                  Object.Vector (Index..Object.Size);
               Delete (Object.Vector);
               Object.Vector := Ptr;
            end;
         else
            Object.Vector (Index + 1..Object.Size + 1) :=
               Object.Vector (Index..Object.Size);
            Object.Vector (Index).Key  := Key;
            Object.Vector (Index).Item := Item;
         end if;
         Object.Size := Object.Size + 1;
      end;
   end Insert;

   procedure Add
             (  Container : in out Map;
                Items     : Map
             )  is
   begin
      if (  Container.Object /= Items.Object
         and then
            not Is_Empty (Items)
         )
      then
         declare
            Vector : Token_Array renames Items.Object.Vector.all;
            Index  : Integer;
            subtype Index_Range is Integer range Vector'Range;
            Last   : constant Index_Range := Items.Object.Size;
         begin
            for Item in Vector'First..Last loop
               Index := Find (Container, Vector (Item).Key);
               if Index < 0 then
                  Insert
                  (  Container => Container,
                     Index     => -Index,
                     Key       => Vector (Item).Key,
                     Item      => Vector (Item).Item
                  );
               end if;
            end loop;
         end;
      end if;
   end Add;

   procedure Add
             (  Container : in out Map;
                Key       : Key_Type;
                Item      : Object_Type
             )  is
      Index : constant Integer := Find (Container, Key);
   begin
      if Index < 0 then
         Insert
         (  Container => Container,
            Index     => -Index,
            Key       => Key,
            Item      => Item
         );
      else
         raise Constraint_Error;
      end if;
   end Add;

   procedure Adjust (Container : in out Map) is
   begin
      if Container.Object /= null then
         Container.Object.Use_Count :=
            Container.Object.Use_Count + 1;
      end if;
   end Adjust;

   procedure Clone (Container : in out Map) is
   begin
      if (  Container.Object /= null
         and then
            Container.Object.Use_Count > 0
         )
      then
         declare
            Source : Data renames Container.Object.all;
            Copy   : constant Data_Ptr := new Data;
         begin
            Copy.Size := Source.Size;
            if 0 /= Source.Size then
               Copy.Vector := new Token_Array (1..Source.Size);
               Copy.Vector.all := Source.Vector (1..Source.Size);
            end if;
            Source.Use_Count := Source.Use_Count - 1;
            Container.Object := Copy;
         end;
      end if;
   end Clone;

   function Create return Map is
      Result : Map;
   begin
      return Result;
   end Create;

   procedure Erase (Container : in out Map) is
   begin
      if Container.Object /= null then
         if Container.Object.Use_Count > 0 then
            Container.Object.Use_Count :=
               Container.Object.Use_Count - 1;
            Container.Object := null;
         elsif Container.Object.Size > 0 then
            declare
               Fill : Token; -- Uses default constructor if any
                  pragma Warnings (Off, Fill);
               Vector : Token_Array renames Container.Object.Vector.all;
               subtype Index_Range is Integer range Vector'Range;
               Last   : constant Index_Range := Container.Object.Size;
            begin
               for Item in Vector'First..Last loop
                  Vector (Item) := Fill;
               end loop;
            end;
            Container.Object.Size := 0;
         end if;
      end if;
   end Erase;

   procedure Finalize (Container : in out Map) is
   begin
      if Container.Object /= null then
         if Container.Object.Use_Count > 0 then
            Container.Object.Use_Count :=
               Container.Object.Use_Count - 1;
         else
            Delete (Container.Object.Vector);
            Delete (Container.Object);
         end if;
      end if;
   end Finalize;

   function Find
            (  Vector : Token_Array;
               Size   : Positive;
               Key    : Key_Type
            )  return Integer is
      From : Natural := 0;
      To   : Natural := Size + 1;
      This : Natural;
   begin
      loop
         This := (From + To) / 2;
         if Key = Vector (This).Key then
            return This;
         elsif Key < Vector (This).Key then
            if This - From <= 1 then
               return -This;
            end if;
            To := This;
         else
            if To - This <= 1 then
               return - This - 1;
            end if;
            From := This;
         end if;
      end loop;
   end Find;

   function Find (Container : Map; Key : Key_Type)
      return Integer is
   begin
      if Is_Empty (Container) then
         return -1;
      else
         return
            Find
            (  Container.Object.Vector.all,
               Container.Object.Size,
               Key
            );
      end if;
   end Find;

   function Get (Container : Map; Key : Key_Type)
      return Object_Type is
      Index : constant Integer := Find (Container, Key);
   begin
      if Index <= 0 then
         raise Constraint_Error;
      end if;
      return Container.Object.Vector (Index).Item;
   end Get;

   function Get (Container : Map; Index : Positive)
      return Object_Type is
   begin
      if (  Container.Object = null
         or else
            Index > Container.Object.Size
         )
      then
         raise Constraint_Error;
      end if;
      return Container.Object.Vector (Index).Item;
   end Get;

   function Get_Key (Container : Map; Index : Positive)
      return Key_Type is
   begin
      if (  Container.Object = null
         or else
            Index > Container.Object.Size
         )
      then
         raise Constraint_Error;
      end if;
      return Container.Object.Vector (Index).Key;
   end Get_Key;

   function Get_Size (Container : Map) return Natural is
   begin
      if Container.Object = null then
         return 0;
      else
         return Container.Object.Size;
      end if;
   end Get_Size;

   procedure Remove
             (  Container : in out Map;
                Index     : Positive
             )  is
   begin
      if (  Container.Object = null
         or else
            Index > Container.Object.Size
         )
      then
         raise Constraint_Error;
      else
         Clone (Container);
         declare
            Fill : Token; -- Uses default constructor if any
               pragma Warnings (Off, Fill);
            Object : Data renames Container.Object.all;
         begin
            Object.Vector (Index..Object.Size - 1) :=
               Object.Vector (Index + 1..Object.Size);
            Object.Vector (Object.Size) := Fill;
            Object.Size := Object.Size - 1;
         end;
      end if;
   end Remove;

   procedure Remove
             (  Container : in out Map;
                Key       : Key_Type
             )  is
      Index : constant Integer := Find (Container, Key);
   begin
      if Index > 0 then
         Remove (Container, Index);
      end if;
   end Remove;

   procedure Remove
             (  Container : in out Map;
                Items     : Map
             )  is
   begin
      if Container.Object = Items.Object then
         Erase (Container);
      elsif not Is_Empty (Container) and then not Is_Empty (Items) then
         declare
            Vector : Token_Array renames Items.Object.Vector.all;
            Index  : Integer;
            subtype Index_Range is Integer range Vector'Range;
            Last   : constant Index_Range := Items.Object.Size;
         begin
            for Item in Vector'First..Last loop
               Index := Find (Container, Vector (Item).Key);
               if Index > 0 then
                  Remove (Container, Index);
               end if;
            end loop;
         end;
      end if;
   end;

   procedure Replace
             (  Container : in out Map;
                Items     : Map
             )  is
   begin
      if (  Container.Object /= Items.Object
         and then
            not Is_Empty (Items)
         )
      then
         declare
            Vector : Token_Array renames Items.Object.Vector.all;
            Index  : Integer;
            subtype Index_Range is Integer range Vector'Range;
            Last   : constant Index_Range := Items.Object.Size;
         begin
            for Item in Vector'First..Last loop
               Index := Find (Container, Vector (Item).Key);
               if Index > 0 then
                  Replace (Container, Index, Vector (Item).Item);
               else
                  Insert
                  (  Container => Container,
                     Index     => -Index,
                     Key       => Vector (Item).Key,
                     Item      => Vector (Item).Item
                  );
               end if;
            end loop;
         end;
      end if;
   end Replace;

   procedure Replace
             (  Container : in out Map;
                Key       : Key_Type;
                Item      : Object_Type
             )  is
      Index : constant Integer := Find (Container, Key);
   begin
      if Index > 0 then
         Replace (Container, Index, Item);
      else
         Insert
         (  Container => Container,
            Index     => -Index,
            Key       => Key,
            Item      => Item
         );
      end if;
   end Replace;

   function "=" (Left, Right : Map) return Boolean is
   begin
      if Left.Object = Right.Object then
         return True;
      elsif Left.Object = null or else 0 = Left.Object.Size then
         return Right.Object = null or else 0 = Right.Object.Size;
      elsif Right.Object = null or else 0 = Right.Object.Size then
         return False;
      elsif Left.Object.Size /= Right.Object.Size then
         return False;
      else
         declare
            First  : Token_Array renames Left.Object.Vector.all;
            Second : Token_Array renames Right.Object.Vector.all;
         begin
            for Index in 1..Left.Object.Size loop
              if First (Index) /= Second (Index) then
                 return False;
              end if;
            end loop;
         end;
         return True;
      end if;
   end "=";

end Generic_Map;
