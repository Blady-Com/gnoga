--                                                                    --
--  package Generic_Discrete_Set    Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2012       --
--                                                                    --
--                                Last revision :  10:09 24 May 2020  --
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

package body Generic_Discrete_Set is

   procedure Add
             (  Container : in out Set;
                Items     : Set
             )  is
      use Range_Sets;
      This : Range_Type;
   begin
      for Index in 1..Get_Size (Items) loop
         This := Get (Items.Ranges, Index);
         Add (Container, This.From, This.To);
      end loop;
   end Add;

   procedure Add
             (  Container : in out Set;
                Item      : Object_Type
             )  is
      use Range_Sets;
      Set   : Range_Sets.Set renames Container.Ranges;
      Index : Integer := Find (Container, Item);
   begin
      if Index < 0 then
         Index := -Index;
         declare
            This : Range_Type;
            Prev : Range_Type;
         begin
            if Index <= Get_Size (Set) then
               This := Get (Set, Index);
               if Index > 1 then -- There is a range before this one
                  Prev := Get (Set, Index - 1);
                  if Object_Type'Succ (Prev.To) = Item then
                     if Object_Type'Pred (This.From) = Item then
                        Remove (Set, Index); -- Merging two ranges
                        Remove (Set, Index - 1);
                        Add (Set, (Prev.From, This.To));
                     else
                        Remove (Set, Index - 1); -- Attach to previous
                        Add (Set, (Prev.From, Item));
                     end if;
                     return;
                  end if;
               end if;
               if Object_Type'Pred (This.From) = Item then
                  Remove (Set, Index); -- Attach to next
                  Add (Set, (Item, This.To));
                  return;
               end if;
            else
               if Index > 1 then -- There is a range before this one
                  Index := Index - 1;
                  Prev  := Get (Set, Index);
                  if Object_Type'Succ (Prev.To) = Item then
                     Remove (Set, Index); -- Attach to this range
                     Add (Set, (Prev.From, Item));
                     return;
                  end if;
               end if;
            end if;
            Add (Set, (Item, Item));
         end;
      end if;
   end Add;

   procedure Add (Container : in out Set; From, To : Object_Type) is
      use Range_Sets;
      Set : Range_Sets.Set renames Container.Ranges;
   begin
      if From > To then
         if Object_Type'Pred (From) /= To then
            raise Constraint_Error;
         end if;
      else
         if Get_Size (Set) = 0 then
            Add (Set, (From, To));
         else
            declare
               Start : Object_Type := From;
               This  : Range_Type;
               Index : Integer := Find (Container, From, To);
            begin
               if Index < 0 then -- Range is not contained
                  Index := -Index;
                  if Index > 1 then -- There is a range before this one
                     This := Get (Set, Index - 1);
                     if Object_Type'Succ (This.To) >= From then
                        Start := This.From;
                        Index := Index - 1;
                        Remove (Set, Index);
                     end if;
                  end if;
                  while Index <= Get_Size (Set) loop
                     This := Get (Set, Index);
                     if (  This.From > To
                        and then
                           Object_Type'Pred (This.From) /= To
                        )
                     then -- Not in the next range
                        Add (Set, (Start, To));
                        return;
                     end if;
                     Remove (Set, Index);
                     if This.To >= To then
                        Add (Set, (Start, This.To)); -- Last point
                        return;
                     end if;
                  end loop;
                  Add (Set, (Start, To));
               end if;
            end;
         end if;
      end if;
   end Add;

   function Create return Set is
      Result : Set;
   begin
      return Result;
   end Create;

   function Create (Item : Object_Type) return Set is
      use Range_Sets;
      Result : Set;
   begin
      Add (Result.Ranges, (Item, Item));
      return Result;
   end Create;

   function Create (From, To : Object_Type) return Set is
      use Range_Sets;
      Result : Set;
   begin
      if To < From then
         if Object_Type'Pred (From) = To then
            raise Constraint_Error;
         end if;
      else
         Add (Result.Ranges, (From, To));
      end if;
      return Result;
   end Create;

   procedure Erase (Container : in out Set) is
      use Range_Sets;
   begin
      Erase (Container.Ranges);
   end Erase;

   procedure Finalize (Container : in out Set) is
   begin
      null;
   end Finalize;

   function Find (Container : Set; Item : Object_Type) return Integer is
      use Range_Sets;
      Set   : Range_Sets.Set renames Container.Ranges;
      Index : constant Integer := abs Find (Set, (Item, Item));
   begin
      if (  Index <= Get_Size (Set)
         and then
            Get (Set, Index).From <= Item
         )
      then
         return Index;
      else
         return -Index;
      end if;
   end Find;

   function Find (Container : Set; From, To : Object_Type)
      return Integer is
      use Range_Sets;
   begin
      if To < From then
         raise Constraint_Error;
      end if;
      declare
         Set   : Range_Sets.Set renames Container.Ranges;
         Index : constant Integer := abs Find (Set, (From, From));
         This  : Range_Type;
      begin
         if Index <= Get_Size (Set) then
            This := Get (Set, Index);
            if From >= This.From and then To <= This.To then
               return Index;
            end if;
         end if;
         return -Index;
      end;
   end Find;

   function From (Container : Set; Index : Positive)
      return Object_Type is
      use Range_Sets;
   begin
      return Get (Container.Ranges, Index).From;
   end From;

   procedure Get
             (  Container : Set;
                Index     : Positive;
                From      : out Object_Type;
                To        : out Object_Type
             )  is
      use Range_Sets;
      This : constant Range_Type := Get (Container.Ranges, Index);
   begin
      From := This.From;
      To   := This.To;
   end Get;

   function Get_Size (Container : Set) return Natural is
      use Range_Sets;
   begin
      return Get_Size (Container.Ranges);
   end Get_Size;

   function Intersect (Left, Right : Range_Type) return Boolean is
   begin
      return Left.To >= Right.From and then Left.From <= Right.To;
   end Intersect;

   function Is_Empty (Container : Set) return Boolean is
      use Range_Sets;
   begin
      return Is_Empty (Container.Ranges);
   end Is_Empty;

   function Is_In (Container : Set; Item : Object_Type)
      return Boolean is
      use Range_Sets;
   begin
      return Find (Container.Ranges, (Item, Item)) > 0;
   end Is_In;

   function Is_In (Container : Set; From, To : Object_Type)
      return Boolean is
      use Range_Sets;
   begin
      if To < From then
         return False;
      else
         return Find (Container.Ranges, (From, To)) > 0;
      end if;
   end Is_In;

   function Is_Not_In (Container : Set; From, To : Object_Type)
      return Boolean is
      use Range_Sets;
   begin
      if To < From then
         return True;
      else
         declare
            Index : Integer := Find (Container.Ranges, (From, From));
         begin
            if Index > 0 then
               return False;
            elsif From = To then
               return True;
            else
               Index := -Index;
               return
               (  Index > Get_Size (Container)
               or else
                  not Intersect
                      (  (From, To),
                         Get (Container.Ranges, Index)
               )      );
            end if;
         end;
      end if;
   end Is_Not_In;

   function Less (Left, Right : Range_Type) return Boolean is
   begin
      return Left.To < Right.From;
   end Less;

   procedure Range_Remove (Container : in out Set; Index : Positive) is
      use Range_Sets;
   begin
      Remove (Container.Ranges, Index);
   end;

   procedure Remove (Container : in out Set; Item : Object_Type) is
      use Range_Sets;
      Set   : Range_Sets.Set renames Container.Ranges;
      Index : constant Integer := Find (Container, Item);
   begin
      if Index > 0 then
         declare
            This : constant Range_Type := Get (Set, Index);
         begin
            Remove (Set, Index);
            if This.From /= This.To then
               if This.From = Item then
                  Add (Set, (Object_Type'Succ (Item), This.To));
               elsif This.To = Item then
                  Add (Set, (This.From, Object_Type'Pred (Item)));
               else
                  Add (Set, (This.From, Object_Type'Pred (Item)));
                  Add (Set, (Object_Type'Succ (Item), This.To));
               end if;
            end if;
         end;
      end if;
   end Remove;

   procedure Remove (Container : in out Set; From, To : Object_Type) is
      use Range_Sets;
      Set   : Range_Sets.Set renames Container.Ranges;
      Index : Integer;
      This  : Range_Type;
   begin
      if To >= From then
         Index := abs Find (Set, (From, From));
         while Index <= Get_Size (Set) loop
            This := Get (Set, Index);
            exit when This.From > To;
            Remove (Set, Index);
            if This.From >= From then
               if This.To >= To then -- Remove head
                  Add (Set, (Object_Type'Succ (To), This.To));
                  return;
               end if; -- Remove completely
            else -- Leave the head
               Add (Set, (This.From, Object_Type'Pred (From)));
               if This.To > To then -- Keep the tail
                  Add (Set, (Object_Type'Succ (To), This.To));
                  return;
               end if;
               Index := Index + 1; -- Remove tail
            end if;
         end loop;
      end if;
   end Remove;

   procedure Remove (Container : in out Set; Items : Set) is
      use Range_Sets;
      Set  : Range_Sets.Set renames Container.Ranges;
      This : Range_Type;
   begin
      for Index in reverse 1..Get_Size (Set) loop
         This := Get (Set, Index);
         Remove (Container, This.From, This.To);
      end loop;
   end;

   function To (Container : Set; Index : Positive) return Object_Type is
      use Range_Sets;
   begin
      return Get (Container.Ranges, Index).To;
   end To;

   function "and" (Left, Right : Set) return Set is
      function Remove (Left : Set; Right : Range_Sets.Set) return Set is
         Result : Set := Left;
         From   : Object_Type;
         This   : Range_Type;
         use Range_Sets;
      begin
         if Is_Empty (Right) then
            Erase (Result);
         else
            This := Get (Right, 1);
            From := This.To;
            if This.From > Object_Type'First then
               Remove
               (  Result,
                  Object_Type'First,
                  Object_Type'Pred (This.From)
               );
            end if;
            for Index in 2..Get_Size (Left) loop
               This := Get (Left.Ranges, Index);
               Remove
               (  Result,
                  Object_Type'Succ (From),
                  Object_Type'Pred (This.From)
               );
               From := This.To;
            end loop;
            if From < Object_Type'Last then
               Remove
               (  Result,
                  Object_Type'Succ (From),
                  Object_Type'Last
               );
            end if;
         end if;
         return Result;
      end Remove;
   begin
      if Get_Size (Left) < Get_Size (Right) then
         return Remove (Right, Left.Ranges);
      else
         return Remove (Left, Right.Ranges);
      end if;
   end "and";

   function "not" (Left : Set) return Set is
      From   : Object_Type;
      This   : Range_Type;
      Result : Set;
      use Range_Sets;
   begin
      if Is_Empty (Left) then
         Add
         (  Result.Ranges,
            (  Object_Type'First,
               Object_Type'Last
         )  );
      else
         This := Get (Left.Ranges, 1);
         From := This.To;
         if This.From > Object_Type'First then
            Add
            (  Result.Ranges,
               (  Object_Type'First,
                  Object_Type'Pred (This.From)
            )  );
         end if;
         for Index in 2..Get_Size (Left) loop
            This := Get (Left.Ranges, Index);
            Add
            (  Result.Ranges,
               (  Object_Type'Succ (From),
                  Object_Type'Pred (This.From)
            )  );
            From := This.To;
         end loop;
         if From < Object_Type'Last then
            Add
            (  Result.Ranges,
               (  Object_Type'Succ (From),
                  Object_Type'Last
            )  );
         end if;
      end if;
      return Result;
   end "not";

   function "or" (Left, Right : Set) return Set is
      function Append (Left : Set; Right : Range_Sets.Set) return Set is
         use Range_Sets;
         Result : Set := Left;
         This   : Range_Type;
      begin
         for Index in 1..Get_Size (Right) loop
            This := Get (Right, Index);
            Add (Result, This.From, This.To);
         end loop;
         return Result;
      end Append;
   begin
      if Get_Size (Left) < Get_Size (Right) then
         return Append (Right, Left.Ranges);
      else
         return Append (Left, Right.Ranges);
      end if;
   end "or";

   function "xor" (Left, Right : Set) return Set is
   begin
      return (Left - Right) or (Right - Left);
   end "xor";

   function "-" (Left, Right : Set) return Set is
      Result : Set := Left;
   begin
      Remove (Result, Right);
      return Result;
   end "-";

   function "=" (Left, Right : Set) return Boolean is
      use Range_Sets;
   begin
      return Left.Ranges = Right.Ranges;
   end "=";

end Generic_Discrete_Set;
