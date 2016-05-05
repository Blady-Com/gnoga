--                                                                    --
--  package Generic_Indefinite_Set  Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2012       --
--                                                                    --
--                                Last revision :  20:01 04 Apr 2016  --
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

package body Generic_Indefinite_Set is

   procedure Delete is
      new Ada.Unchecked_Deallocation (Data, Data_Ptr);

   procedure Delete is
      new Ada.Unchecked_Deallocation (Object_Array, Object_Array_Ptr);

   procedure Delete is
      new Ada.Unchecked_Deallocation (Object_Type, Object_Type_Ptr);

   function Is_Empty (Container : Set) return Boolean is
   begin
      return
         Container.Object = null or else Container.Object.Size = 0;
   end Is_Empty;

   function Is_In (Container : Set; Item : Object_Type)
      return Boolean is
   begin
      return Find (Container, Item) > 0;
   end Is_In;

   procedure Assign
             (  Slot : in out Object_Type_Ptr;
                Item : Object_Type
             )  is
      pragma Inline (Assign);
      Old : Object_Type_Ptr := Slot;
   begin
      Slot := new Object_Type'(Item);
      Delete (Old);
   end Assign;

   procedure Insert
             (  Container : in out Set;
                Item      : Object_Type;
                Replace   : in out Boolean;
                Location  : out Integer
             )  is
   begin
      Location := Find (Container, Item);
      if Location >= 0 then
         if Replace then
            if Container.Object.Use_Count > 0 then
               Clone (Container);
            end if;
            Assign (Container.Object.Vector (Location), Item);
         end if;
      else
         if Container.Object = null then
            Container.Object := new Data;
         elsif Container.Object.Use_Count > 0 then
            Clone (Container);
         end if;
         declare
            Index  : constant Positive := -Location;
            Object : Data renames Container.Object.all;
         begin
            if Object.Vector = null then
               Object.Vector :=
                  new Object_Array'(1..Minimal_Size => null);
               Object.Vector (1) := new Object_Type'(Item);
            elsif Object.Size = Object.Vector'Last then
               declare
                  Ptr : Object_Array_Ptr :=
                     new Object_Array'
                         (  1
                         .. (  Object.Size
                            +  Natural'Max
                               (  Minimal_Size,
                                  (  (  Object.Size
                                     *  (100 + Increment)
                                     )
                                  /  100
                            )  )  )  => null
                         );
               begin
                  Ptr (1..Index - 1) := Object.Vector (1..Index - 1);
                  Ptr (Index) := new Object_Type'(Item);
                  Ptr (Index + 1..Object.Size + 1) :=
                     Object.Vector (Index..Object.Size);
                  Delete (Object.Vector);
                  Object.Vector := Ptr;
               exception
                  when others =>
                     for Index in Ptr'Range loop
                        Delete (Ptr (Index));
                     end loop;
                     Delete (Ptr);
                     raise;
               end;
            else
               Object.Vector (Index + 1..Object.Size + 1) :=
                  Object.Vector (Index..Object.Size);
               Object.Vector (Index) := new Object_Type'(Item);
            end if;
            Object.Size := Object.Size + 1;
         end;
         Replace := False;
      end if;
   end Insert;

   procedure Add (Container : in out Set; Items : Set) is
   begin
      if (  Container.Object /= Items.Object
         and then
            not Is_Empty (Items)
         )
      then
         declare
            Vector   : Object_Array renames Items.Object.Vector.all;
            subtype Index_Range is Integer range Vector'Range;
            Last     : constant Index_Range := Items.Object.Size;
            Location : Integer;
            Replace  : Boolean;
         begin
            for Index in Vector'First..Last loop
               Replace := False;
               Insert
               (  Container,
                  Vector (Index).all,
                  Replace,
                  Location
               );
            end loop;
         end;
      end if;
   end Add;

   procedure Add
             (  Container : in out Set;
                Item      : Object_Type
             )  is
      Location : Integer;
      Replace  : Boolean := False;
   begin
      Insert (Container, Item, Replace, Location);
   end Add;

   procedure Adjust (Container : in out Set) is
   begin
      if Container.Object /= null then
         Container.Object.Use_Count := Container.Object.Use_Count + 1;
      end if;
   end Adjust;

   procedure Clone (Container : in out Set) is
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
               Copy.Vector := new Object_Array (1..Source.Size);
               for Index in Copy.Vector'Range loop
                  Copy.Vector (Index) :=
                     new Object_Type'(Source.Vector (Index).all);
               end loop;
            end if;
            Source.Use_Count := Source.Use_Count - 1;
            Container.Object := Copy;
         end;
      end if;
   end Clone;

   function Create return Set is
      Result : Set;
   begin
      return Result;
   end Create;

   procedure Erase (Container : in out Set) is
   begin
      if Container.Object /= null then
         declare
            Object : Data renames Container.Object.all;
         begin
            if Object.Use_Count > 0 then
               Object.Use_Count :=  Object.Use_Count - 1;
               Container.Object := null;
            elsif Object.Size > 0 then
               for Index in 1..Object.Size loop
                  Delete (Object.Vector (Index));
               end loop;
               Object.Size := 0;
            end if;
         end;
      end if;
   end Erase;

   procedure Finalize (Container : in out Set) is
   begin
      if Container.Object /= null then
         if Container.Object.Use_Count > 0 then
            Container.Object.Use_Count :=
               Container.Object.Use_Count - 1;
         else
            declare
               Object : Data renames Container.Object.all;
            begin
               for Index in 1..Object.Size loop
                  Delete (Object.Vector (Index));
               end loop;
               Delete (Object.Vector);
               Delete (Container.Object);
            end;
         end if;
      end if;
   end Finalize;

   function Find
            (  Vector : Object_Array;
               Size   : Positive;
               Item   : Object_Type
            )  return Integer is
      From : Natural := 0;
      To   : Natural := Size + 1;
      This : Natural;
   begin
      loop
         This := (From + To) / 2;
         if Item = Vector (This).all then
            return This;
         elsif Item < Vector (This).all then
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

   function Find (Container : Set; Item : Object_Type)
      return Integer is
   begin
      if Is_Empty (Container) then
         return -1;
      else
         return
            Find
            (  Container.Object.Vector.all,
               Container.Object.Size,
               Item
            );
      end if;
   end Find;

   function Get (Container : Set; Index : Positive)
      return Object_Type is
   begin
      if (  Container.Object = null
         or else
            Index > Container.Object.Size
         )
      then
         raise Constraint_Error;
      end if;
      return Container.Object.Vector (Index).all;
   end Get;

   function Get_Size (Container : Set) return Natural is
   begin
      if Container.Object = null then
         return 0;
      else
         return Container.Object.Size;
      end if;
   end Get_Size;

   procedure Insert
             (  Container : in out Set;
                Item      : in out Object_Type
             )  is
      Location : Integer;
      Replace  : Boolean := False;
   begin
      Insert (Container, Item, Replace, Location);
      if Location > 0 then
         Item := Get (Container, Location);
      end if;
   end Insert;

   procedure Insert
             (  Container : in out Set;
                Item      : Object_Type;
                Inserted  : out Boolean
             )  is
      Location : Integer;
   begin
      Inserted := False;
      Insert (Container, Item, Inserted, Location);
      Inserted := not Inserted;
   end Insert;

   procedure Remove
             (  Container : in out Set;
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
            Object : Data renames Container.Object.all;
         begin
            Delete (Object.Vector (Index));
            Object.Vector (Index..Object.Size - 1) :=
               Object.Vector (Index + 1..Object.Size);
            Object.Size := Object.Size - 1;
         end;
      end if;
   end Remove;

   procedure Remove
             (  Container : in out Set;
                Item      : Object_Type
             )  is
      Index : constant Integer := Find (Container, Item);
   begin
      if Index > 0 then
         Remove (Container, Index);
      end if;
   end Remove;

   procedure Remove
             (  Container : in out Set;
                Item      : Object_Type;
                Removed   : out Boolean
             )  is
      Index : constant Integer := Find (Container, Item);
   begin
      if Index > 0 then
         Remove (Container, Index);
         Removed := True;
      else
         Removed := False;
      end if;
   end Remove;

   procedure Remove
             (  Container : in out Set;
                Items     : Set
             )  is
   begin
      if Container.Object = Items.Object then
         Erase (Container);
      elsif not Is_Empty (Container) and then not Is_Empty (Items) then
         declare
            Vector : Object_Array renames Items.Object.Vector.all;
            subtype Index_Range is Integer range Vector'Range;
            Last   : constant Index_Range := Items.Object.Size;
         begin
            for Index in Vector'First..Last loop
               Remove (Container, Vector (Index).all);
            end loop;
         end;
      end if;
   end;

   procedure Replace
             (  Container : in out Set;
                Items     : Set
             )  is
   begin
      if (  Container.Object /= Items.Object
         and then
            not Is_Empty (Items)
         )
      then
         declare
            Vector   : Object_Array renames Items.Object.Vector.all;
            subtype Index_Range is Integer range Vector'Range;
            Last     : constant Index_Range := Items.Object.Size;
            Location : Integer;
            Replace  : Boolean;
         begin
            for Index in Vector'First..Last loop
               Replace := True;
               Insert
               (  Container,
                  Vector (Index).all,
                  Replace,
                  Location
               );
            end loop;
         end;
      end if;
   end Replace;

   procedure Replace
             (  Container : in out Set;
                Item      : Object_Type;
                Exchange  : Exchange_Condition;
                Insert    : Insert_Condition;
                Updated   : out Boolean
             )  is
      Location : Integer;
   begin
      Location := Find (Container, Item);
      if Location >= 0 then
         if Exchange (Item, Container.Object.Vector (Location).all)
         then
            if Container.Object.Use_Count > 0 then
               Clone (Container);
            end if;
            Assign (Container.Object.Vector (Location), Item);
            Updated := True;
         else
            Updated := False;
         end if;
      else
         if not Insert (Item) then
            Updated := False;
            return;
         end if;
         if Container.Object = null then
            Container.Object := new Data;
         elsif Container.Object.Use_Count > 0 then
            Clone (Container);
         end if;
         declare
            Index  : constant Positive := -Location;
            Object : Data renames Container.Object.all;
         begin
            if Object.Vector = null then
               Object.Vector :=
                  new Object_Array'(1..Minimal_Size => null);
               Object.Vector (1) := new Object_Type'(Item);
            elsif Object.Size = Object.Vector'Last then
               declare
                  Ptr : Object_Array_Ptr :=
                     new Object_Array'
                         (  1
                         .. (  Object.Size
                            +  Natural'Max
                               (  Minimal_Size,
                                  (  (  Object.Size
                                     *  (100 + Increment)
                                     )
                                  /  100
                            )  )  )  => null
                         );
               begin
                  Ptr (1..Index - 1) := Object.Vector (1..Index - 1);
                  Ptr (Index) := new Object_Type'(Item);
                  Ptr (Index + 1..Object.Size + 1) :=
                     Object.Vector (Index..Object.Size);
                  Delete (Object.Vector);
                  Object.Vector := Ptr;
               exception
                  when others =>
                     for Index in Ptr'Range loop
                        Delete (Ptr (Index));
                     end loop;
                     Delete (Ptr);
                     raise;
               end;
            else
               Object.Vector (Index + 1..Object.Size + 1) :=
                  Object.Vector (Index..Object.Size);
               Object.Vector (Index) := new Object_Type'(Item);
            end if;
            Object.Size := Object.Size + 1;
         end;
         Updated := True;
      end if;
   end Replace;

   procedure Replace
             (  Container : in out Set;
                Item      : Object_Type
             )  is
      Location : Integer;
      Replace  : Boolean := True;
   begin
      Insert (Container, Item, Replace, Location);
   end Replace;

   function "and" (Left, Right : Set) return Set is
   begin
      if Left.Object = Right.Object then
         return Left;
      end if;
      declare
         Result : Set;
      begin
         if not (Is_Empty (Left) or else Is_Empty (Right)) then
            declare
               Size1   : constant Positive := Left.Object.Size;
               Size2   : constant Positive := Right.Object.Size;
               Vector1 : Object_Array renames Left.Object.Vector.all;
               Vector2 : Object_Array renames Right.Object.Vector.all;
            begin
               for Index in 1..Size1 loop
                  if Find (Vector2, Size2, Vector1 (Index).all) > 0
                  then
                     Add (Result, Vector1 (Index).all);
                  end if;
               end loop;
            end;
         end if;
         return Result;
      end;
   end "and";

   function "or" (Left, Right : Set) return Set is
   begin
      if Left.Object = Right.Object then
         return Left;
      end if;
      declare
         Result : Set;
      begin
         Add (Result, Left);
         Add (Result, Right);
         return Result;
      end;
   end "or";

   function "xor" (Left, Right : Set) return Set is
   begin
      if Is_Empty (Left) then
         return Right;
      elsif Is_Empty (Right) then
         return Left;
      end if;
      declare
         Result : Set;
      begin
         if Left.Object /= Right.Object then
            declare
               Size1   : constant Positive := Left.Object.Size;
               Size2   : constant Positive := Right.Object.Size;
               Vector1 : Object_Array renames Left.Object.Vector.all;
               Vector2 : Object_Array renames Right.Object.Vector.all;
            begin
               for Index in 1..Size1 loop
                  if Find (Vector2, Size2, Vector1 (Index).all) <= 0
                  then
                     Add (Result, Vector1 (Index).all);
                  end if;
               end loop;
               for Index in 1..Size2 loop
                   if Find (Vector1, Size1, Vector2 (Index).all) <= 0
                   then
                      Add (Result, Vector2 (Index).all);
                   end if;
               end loop;
            end;
         end if;
         return Result;
      end;
   end "xor";

   function "-" (Left, Right : Set) return Set is
      Result : Set := Left;
   begin
      Remove (Result, Right);
      return Result;
   end "-";

   function "=" (Left, Right : Set) return Boolean is
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
            First  : Object_Array renames Left.Object.Vector.all;
            Second : Object_Array renames Right.Object.Vector.all;
         begin
            for Index in 1..Left.Object.Size loop
              if First (Index).all /= Second (Index).all then
                 return False;
              end if;
            end loop;
         end;
         return True;
      end if;
   end "=";

end Generic_Indefinite_Set;
