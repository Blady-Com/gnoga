--                                                                    --
--  package Generic_Discrete_Map    Copyright (c)  Dmitry A. Kazakov  --
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

package body Generic_Discrete_Map is
   use Range_Maps;

   procedure Add
             (  Container : in out Map;
                Items     : Map
             )  is
      Other : Range_Maps.Map renames Items.Ranges;
      This  : Range_Type;
   begin
      for Index in 1..Get_Size (Other) loop
         This := Get_Key (Other, Index);
         Add (Container, This.From, This.To, Get (Other, Index));
      end loop;
   end Add;

   procedure Locate
             (  Map   : in out Range_Maps.Map;
                Keys  : in out Range_Type;
                First : out Integer;
                Last  : out Integer;
                Item  : Object_Type
             )  is
      Size : constant Integer := Get_Size (Map);
   begin
         -- Lower bound
      First := Find (Map, (Keys.From, Keys.From));
      if First < 0 then -- Keys.From is before -From_Index
         First := -First;
         if First > 1 then
            declare
               This : constant Range_Type := Get_Key (Map, First - 1);
            begin
               if (  Key_Type'Succ (This.To) = Keys.From
                  and then
                     Get (Map, First - 1) = Item
                  )
               then
                  Keys.From := This.From;
                  First     := First - 1;
               end if;
            end;
         end if;
      else -- Keys.From is inside From_Index
         Keys.From := Get_Key (Map, First).From;
      end if;
         -- Upper bound
      Last := First;
      if Last <= Size then
         loop
            declare
               This : constant Range_Type := Get_Key (Map, Last);
            begin
               if Keys.To <= This.To then -- Ends here
                  if Keys.To >= This.From then
                     Keys.To := This.To;
                  elsif Keys.To = Key_Type'Pred (This.From) then
                     if Get (Map, Last) = Item then
                        Keys.To := This.To;
                     else
                        Last := Last - 1;
                     end if;
                  else
                     Last := Last - 1;
                  end if;
                  exit;
               end if;
            end;
            exit when Last = Size;
            Last := Last + 1;
         end loop;
      else
         Last := Size;
      end if;
   end Locate;

   procedure Add
             (  Container : in out Map;
                Key       : Key_Type;
                Item      : Object_Type
             )  is
   begin
      Add (Container, Key, Key, Item);
   end Add;

   procedure Add
             (  Container : in out Map;
                From, To  : Key_Type;
                Item      : Object_Type
             )  is
      Map   : Range_Maps.Map renames Container.Ranges;
      First : Integer;
      Last  : Integer;
      Keys  : Range_Type := (From, To);
   begin
      if From <= To then
         Locate (Map, Keys, First, Last, Item);
         for Index in First..Last loop
            if Get (Map, Index) /= Item then
               raise Constraint_Error;
            end if;
         end loop;
         for Index in reverse First..Last loop
            Remove (Map, Index);
         end loop;
         Add (Map, (Keys.From, Keys.To), Item);
      elsif Key_Type'Pred (From) /= To then
         raise Constraint_Error;
      end if;
   end Add;

   function Create return Map is
      Result : Map;
   begin
      return Result;
   end Create;

   function Create (Key : Key_Type; Item : Object_Type) return Map is
      Result : Map;
   begin
      Add (Result.Ranges, (Key, Key), Item);
      return Result;
   end Create;

   function Create (From, To : Key_Type; Item : Object_Type)
      return Map is
      Result : Map;
   begin
      if To < From then
         raise Constraint_Error;
      end if;
      Add (Result.Ranges, (From, To), Item);
      return Result;
   end Create;

   procedure Erase (Container : in out Map) is
   begin
      Erase (Container.Ranges);
   end Erase;

   procedure Finalize (Container : in out Map) is
   begin
      Finalize (Container.Ranges);
   end Finalize;

   function Find (Container : Map; Key : Key_Type) return Integer is
      Map   : Range_Maps.Map renames Container.Ranges;
      Index : constant Integer := abs Find (Map, (Key, Key));
   begin
      if (  Index <= Get_Size (Map)
         and then
            Get_Key (Map, Index).From <= Key
         )
      then
         return Index;
      else
         return -Index;
      end if;
   end Find;

   function Find (Container : Map; From, To : Key_Type)
      return Integer is
   begin
      if To < From then
         raise Constraint_Error;
      end if;
      declare
         Map   : Range_Maps.Map renames Container.Ranges;
         Index : constant Integer := abs Find (Map, (From, From));
         This  : Range_Type;
      begin
         if Index <= Get_Size (Map) then
            This := Get_Key (Map, Index);
            if From >= This.From and then To <= This.To then
               return Index;
            end if;
         end if;
         return -Index;
      end;
   end Find;

   function From (Container : Map; Index : Positive) return Key_Type is
   begin
      return Get_Key (Container.Ranges, Index).From;
   end From;

   function Get (Container : Map; Key : Key_Type) return Object_Type is
   begin
      return Get (Container.Ranges, Find (Container, Key));
   end Get;

   procedure Get_Key
             (  Container : Map;
                Index     : Positive;
                From      : out Key_Type;
                To        : out Key_Type
             )  is
      This : constant Range_Type := Get_Key (Container.Ranges, Index);
   begin
      From := This.From;
      To   := This.To;
   end Get_Key;

   function Get_Size (Container : Map) return Natural is
   begin
      return Get_Size (Container.Ranges);
   end Get_Size;

   function Intersect (Left, Right : Range_Type) return Boolean is
   begin
      return Left.To >= Right.From and then Left.From <= Right.To;
   end Intersect;

   function Is_Empty (Container : Map) return Boolean is
   begin
      return Is_Empty (Container.Ranges);
   end Is_Empty;

   function Is_In (Container : Map; Key : Key_Type)
      return Boolean is
   begin
      return Find (Container.Ranges, (Key, Key)) > 0;
   end Is_In;

   function Is_In (Container : Map; From, To : Key_Type)
      return Boolean is
   begin
      if To < From then
         return False;
      else
         return Find (Container.Ranges, (From, To)) > 0;
      end if;
   end Is_In;

   function Is_Not_In (Container : Map; From, To : Key_Type)
      return Boolean is
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
                         Get_Key (Container.Ranges, Index)
               )      );
            end if;
         end;
      end if;
   end Is_Not_In;

   function Less (Left, Right : Range_Type) return Boolean is
   begin
      return Left.To < Right.From;
   end Less;

   function Range_Get (Container : Map; Index : Positive)
      return Object_Type is
   begin
      return Get (Container.Ranges, Index);
   end Range_Get;

   procedure Range_Remove (Container : in out Map; Index : Positive) is
   begin
      Remove (Container.Ranges, Index);
   end Range_Remove;

   procedure Range_Replace
             (  Container : in out Map;
                Index     : Positive;
                Item      : Object_Type
             )  is
   begin
      Replace (Container.Ranges, Index, Item);
   end Range_Replace;

   procedure Remove (Container : in out Map; Key : Key_Type) is
      Map   : Range_Maps.Map renames Container.Ranges;
      Index : constant Integer := Find (Container, Key);
   begin
      if Index > 0 then
         declare
            This : constant Range_Type  := Get_Key (Map, Index);
            Item : constant Object_Type := Get (Map, Index);
         begin
            Remove (Map, Index);
            if This.From /= This.To then
               if This.From = Key then
                  Add (Map, (Key_Type'Succ (Key), This.To), Item);
               elsif This.To = Key then
                  Add (Map, (This.From, Key_Type'Pred (Key)), Item);
               else
                  Add (Map, (This.From, Key_Type'Pred (Key)), Item);
                  Add (Map, (Key_Type'Succ (Key), This.To), Item);
               end if;
            end if;
         end;
      end if;
   end Remove;

   procedure Remove (Container : in out Map; From, To : Key_Type) is
      Map   : Range_Maps.Map renames Container.Ranges;
      Index : Integer;
   begin
      if To >= From then
         Index := abs Find (Map, (From, From));
         while Index <= Get_Size (Map) loop
            declare
               This : constant Range_Type := Get_Key (Map, Index);
               Item : constant Object_Type := Get (Map, Index);
            begin
               exit when This.From > To;
               Remove (Map, Index);
               if This.From >= From then
                  if This.To >= To then -- Remove head
                     Add (Map, (Key_Type'Succ (To), This.To), Item);
                     return;
                  end if; -- Remove completely
               else -- Leave the head
                  Add (Map, (This.From, Key_Type'Pred (From)), Item);
                  if This.To > To then -- Keep the tail
                     Add (Map, (Key_Type'Succ (To), This.To), Item);
                     return;
                  end if;
                  Index := Index + 1; -- Remove tail
               end if;
            end;
         end loop;
      end if;
   end Remove;

   procedure Remove (Container : in out Map; Items : Map) is
      Other : Range_Maps.Map renames Items.Ranges;
      This  : Range_Type;
   begin
      for Index in reverse 1..Get_Size (Other) loop
         This := Get_Key (Other, Index);
         Remove (Container, This.From, This.To);
      end loop;
   end;

   procedure Replace
             (  Container : in out Map;
                Key       : Key_Type;
                Item      : Object_Type
             )  is
   begin
      Replace (Container, Key, Key, Item);
   end Replace;

   procedure Replace
             (  Container : in out Map;
                From, To  : Key_Type;
                Item      : Object_Type
             )  is
      Map   : Range_Maps.Map renames Container.Ranges;
      First : Integer;
      Last  : Integer;
      Keys  : Range_Type := (From, To);
   begin
      if From > To then
         raise Constraint_Error;
      end if;
      Locate (Map, Keys, First, Last, Item);
      if First < Last then -- Several intervals
         if Keys.To > To then
            declare
               Object : constant Object_Type := Get (Map, Last);
            begin
               if Object /= Item then
                  Remove (Map, Last);
                  Add (Map, (Key_Type'Succ (To), Keys.To), Object);
                  Keys.To := To;
                  Last    := Last - 1;
               end if;
            end;
         end if;
         if Keys.From < From then
            declare
               Object : constant Object_Type := Get (Map, First);
            begin
               if Object /= Item then
                  Remove (Map, First);
                  Add (Map, (Keys.From, Key_Type'Pred (From)), Object);
                  Keys.From := From;
                  First     := First + 1;
               end if;
            end;
         end if;
         for Index in reverse First..Last loop
            Remove (Map, Index);
         end loop;
         Add (Map, (Keys.From, Keys.To), Item);
      elsif First = Last then -- Single interval
         declare
            Object : constant Object_Type := Get (Map, First);
         begin
            if Object = Item then
               if Get_Key (Map, First) /= (Keys.From, Keys.To) then
                  Remove (Map, First);
                  Add (Map, (Keys.From, Keys.To), Item);
               end if;
            else
               Remove (Map, First);
               if Keys.From < From then
                  Add (Map, (Keys.From, Key_Type'Pred (From)), Object);
               end if;
               Add (Map, (From, To), Item);
               if Keys.To > To then
                  Add (Map, (Key_Type'Succ (To), Keys.To), Object);
               end if;
            end if;
         end;
      else -- Adding new interval
         Add (Map, (Keys.From, Keys.To), Item);
      end if;
   end Replace;

   procedure Replace (Container : in out Map; Items : Map) is
      Other : Range_Maps.Map renames Items.Ranges;
      This  : Range_Type;
   begin
      for Index in 1..Get_Size (Other) loop
         This := Get_Key (Other, Index);
         Replace (Container, This.From, This.To, Get (Other, Index));
      end loop;
   end Replace;

   function To (Container : Map; Index : Positive) return Key_Type is
   begin
      return Get_Key (Container.Ranges, Index).To;
   end To;

   function "=" (Left, Right : Map) return Boolean is
   begin
      return Left.Ranges = Right.Ranges;
   end "=";

end Generic_Discrete_Map;
