--                                                                    --
--  package Generic_Bounded_Map     Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2020       --
--                                                                    --
--                                Last revision :  08:25 05 May 2020  --
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

package body Generic_Bounded_Map is

   function "mod" (Container : Map; Index : Positive) return Positive is
   begin
      if Container.Empty or else Index > Get_Size (Container) then
         raise Constraint_Error;
      end if;
      declare
         Offset : constant Positive := Container.First + Index - 1;
      begin
         if Offset > Container.Size then
            return Offset - Container.Size;
         else
            return Offset;
         end if;
      end;
   end "mod";

   procedure Insert
             (  Container : in out Map;
                Index     : Positive;
                Key       : Key_Type;
                Item      : Object_Type;
                Override  : Overriding_Policy
             )  is
      First : Positive    renames Container.First;
      Last  : Positive    renames Container.Last;
      Data  : Token_Array renames Container.Data;
   begin
      if Container.Empty then -- Empty map
         Container.Empty := False;
         First := 1;
         Last  := 1;
         Data (1).Key  := Key;
         Data (1).Item := Item;
         return;
      end if;
      declare
         Offset : Positive;
         Size   : constant Natural := Get_Size (Container);
      begin
         if Index = 1 then -- Prepend
            --          |        XXXXXXXXXXXXXXXXXXXXXXXXXXX  |
            --                   | First = Offset          | Last
            if Size >= Container.Size then -- Full map, dropping last
               if Last = 1 then
                  Last := Container.Size;
               else
                  Last := Last - 1;
               end if;
            end if;
            if First = 1 then
               First := Container.Size;
            else
               First := First - 1;
            end if;
            Offset := First;
         elsif Index > Size then -- Append
            --          |        XXXXXXXXXXXXXXXXXXX          |
            --                   | First      Last || Offset
            if Size >= Container.Size then -- Full map, dropping first
               if First = Container.Size then
                  First := 1;
               else
                  First := First + 1;
               end if;
            end if;
            if Last = Container.Size then
               Last := 1;
            else
               Last := Last + 1;
            end if;
            Offset := Last;
         else
            Offset := First + Index - 1;
            if Offset > Container.Size then
               Offset := Offset - Container.Size;
            end if;
            if Size >= Container.Size then -- Full map
               case Override is
                  when Override_Least =>
                     if First = Container.Size then
                        First := 1;
                     else
                        First := First + 1;
                     end if;
                  when Override_Greatest =>
                     if Last = 1 then
                        Last := Container.Size;
                     else
                        Last := Last - 1;
                     end if;
               end case;
            end if;
            if First <= Last then
               --       |        XXXXXXXXXXXXXXXXXXXXXXXXXXX  |
               --                | First                   | Last
               if Last - Offset >= Offset - First then -- Insert
                  Offset := Offset - 1;
                  if First = 1 then
                     -- |<<<<<<<<<<<XXXXXXXXXXXXXXXX          |
                     --  | First   | Offset        | Last
                     Data (Container.Size) := Data (1);
                     Data (1..Offset - 1) := Data (2..Offset);
                     First := Container.Size;
                  else
                     -- |  <<<<<<<<<XXXXXXXXXXXXXXX           |
                     --    | First | Offset        | Last
                     Data (First - 1..Offset - 1) :=
                        Data (First..Offset);
                     First := First - 1;
                  end if;
               else
                  if Last = Container.Size then
                     -- |        XXXXXXXXXXXXXXXXX>>>>>>>>>>>>|
                     --          | First          | Offset   | Last
                     Data (1) := Data (Last);
                     Data (Offset + 1..Last) := Data (Offset..Last - 1);
                     Last := 1;
                  else
                     -- |        XXXXXXXXXXXXXXXXX>>>>>>>>>>>>   |
                     --          | First          | Offset   | Last
                     Data (Offset + 1..Last + 1) := Data (Offset..Last);
                     Last := Last + 1;
                  end if;
               end if;
            else
               --       |XXXXXXXXXXXXXXXXXXXX          XXXXXXX|
               --                           | Last     | First
               if Offset > First then
                  Offset := Offset - 1;
                  --    |XXXXXXXXXXXXX          <<<<<<<<<<XXXX|
                  --                 | Last     | First  | Offset
                  Data (First - 1..Offset - 1) := Data (First..Offset);
                  First := First - 1;
               else
                  --    |XXX>>>>>>>>>>          XXXXXXXXXXXXXX|
                  --        | Offset | Last     | First
                  Data (Offset + 1..Last + 1) := Data (Offset..Last);
                  Last := Last + 1;
               end if;
            end if;
         end if;
         Data (Offset).Key  := Key;
         Data (Offset).Item := Item;
      end;
   end Insert;

   procedure Add
             (  Container : in out Map;
                Key       : Key_Type;
                Item      : Object_Type;
                Override  : Overriding_Policy
             )  is
      Index : constant Integer := Find (Container, Key);
   begin
      if Index <= 0 then
         Insert
         (  Container => Container,
            Index     => -Index,
            Key       => Key,
            Item      => Item,
            Override  => Override
         );
      else
         raise Constraint_Error;
      end if;
   end Add;

   procedure Add
             (  Container : in out Map;
                Key       : Key_Type;
                Item      : Object_Type;
                Override  : Overriding_Policy;
                Index     : out Positive
             )  is
      Offset : constant Integer := Find (Container, Key);
   begin
      if Offset <= 0 then
         Index := -Offset;
         Insert
         (  Container => Container,
            Index     => Offset,
            Key       => Key,
            Item      => Item,
            Override  => Override
         );
      else
         raise Constraint_Error;
      end if;
   end Add;

   procedure Erase (Container : in out Map) is
   begin
      if Container.Empty then
         return;
      end if;
      declare
         First : Positive    renames Container.First;
         Last  : Positive    renames Container.Last;
         Data  : Token_Array renames Container.Data;
         Fill  : Token; -- Uses default constructor if any
                pragma Warnings (Off, Fill);
      begin
         if Last >= First then
            for Index in First..Last loop
               Data (Index) := Fill;
            end loop;
         else
            for Index in First..Container.Size loop
               Data (Index) := Fill;
            end loop;
            for Index in 1..Last loop
               Data (Index) := Fill;
            end loop;
         end if;
         Container.Empty := True;
      end;
   end Erase;

   procedure Finalize (Container : in out Map) is
   begin
      Erase (Container);
   end Finalize;

   function Find
            (  Data  : Token_Array;
               Size  : Positive;
               First : Positive;
               Key   : Key_Type
            )  return Integer is
      From   : Natural := 0;
      To     : Natural := Size + 1;
      This   : Natural;
      Offset : Positive;
   begin
      loop
         This   := (From + To) / 2;
         Offset := First + This - 1;
         if Offset > Data'Length then
            Offset := Offset - Data'Length;
         end if;
         if Key = Data (Offset).Key then
            return This;
         elsif Key < Data (Offset).Key then
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
      if Container.Empty then
         return -1;
      elsif Key < Container.Data (Container.First).Key then
         return -1;
      else
         declare
            Size : constant Natural := Get_Size (Container);
         begin
            if Container.Data (Container.Last).Key < Key then
               return -1 - Size;
            else
               return Find
                      (  Data  => Container.Data,
                         Size  => Size,
                         First => Container.First,
                         Key   => Key
                      );
            end if;
         end;
      end if;
   end Find;

   function Get (Container : Map; Key : Key_Type) return Object_Type is
      Index : constant Integer := Find (Container, Key);
   begin
      if Index <= 0 then
         raise Constraint_Error;
      end if;
      return Container.Data (Container mod Index).Item;
   end Get;

   function Get (Container : Map; Index : Positive)
      return Object_Type is
   begin
      return Container.Data (Container mod Index).Item;
   end Get;

   function Get_Key (Container : Map; Index : Positive)
      return Key_Type is
   begin
      return Container.Data (Container mod Index).Key;
   end Get_Key;

   function Get_Size (Container : Map) return Natural is
   begin
      if Container.Empty then
         return 0;
      else
         declare
            Diff : constant Integer :=
                            Container.Last - Container.First + 1;
         begin
            if Diff > 0 then
               return Diff;
            else
               return Container.Size + Diff;
            end if;
         end;
      end if;
   end Get_Size;

   function Inf (Container : Map; Key : Key_Type) return Natural is
   begin
      if Container.Empty then
         return 0;
      end if;
      declare
         Index : Integer := Find (Container, Key);
      begin
         if Index > 0 then
            return Index;
         else
            Index := -Index;
            if Index = 1 then
               return 0;
            else
               return Index - 1;
            end if;
         end if;
      end;
   end Inf;

   function Is_Empty (Container : Map) return Boolean is
   begin
      return Container.Empty;
   end Is_Empty;

   function Is_Full (Container : Map) return Boolean is
   begin
      if Container.Empty then
         return False;
      else
         declare
            Diff : constant Integer :=
                            Container.Last - Container.First + 1;
         begin
            return Diff = 0 or else Diff = Container.Size;
         end;
      end if;
   end Is_Full;

   function Is_In (Container : Map; Key : Key_Type) return Boolean is
   begin
      return Find (Container, Key) > 0;
   end Is_In;

   procedure Remove
             (  Container : in out Map;
                Index     : Positive
             )  is
      Offset : constant Positive := Container mod Index;
      First  : Positive    renames Container.First;
      Last   : Positive    renames Container.Last;
      Data   : Token_Array renames Container.Data;
      Fill   : Token; -- Uses default constructor if any
         pragma Warnings (Off, Fill);
   begin
      if First = Last then
         Data (First)    := Fill;
         Container.Empty := True;
      elsif Offset = First then
         Data (First) := Fill;
         if First = Container.Size then -- Remove first
            First := 1;
         else
            First := First + 1;
         end if;
      elsif Offset = Last then -- Remove last
         Data (Last) := Fill;
         if Last = 1 then
            Last := Container.Size;
         else
            Last := Last - 1;
         end if;
      elsif First <= Last then
         --       |        XXXXXXXXXXXXXXXXXXXXXXXXXXX  |
         --                | First                   | Last
         if Last - Offset >= Offset - First then
            --    |  >>>>>>>>>XXXXXXXXXXXXXXX           |
            --       | First | Offset        | Last
            Data (First + 1..Offset) := Data (First..Offset - 1);
            Data (First) := Fill;
            First := First + 1;
         else
            --    |     XXXXXXXXXXXXXXXXX<<<<<<<<<<<<   |
            --          | First          | Offset   | Last
            Data (Offset..Last - 1) := Data (Offset + 1..Last);
            Data (Last) := Fill;
            Last := Last - 1;
         end if;
      else
         --       |XXXXXXXXXXXXXXXXXXXX          XXXXXXX|
         --                           | Last     | First
         if Offset >= First then
            --    |XXXXXXXXXXXXX          >>>>>>>>>>XXXXX|
            --                 | Last     | First  | Offset
            Data (First + 1..Offset) := Data (First..Offset - 1);
            Data (First) := Fill;
            First := First + 1;
         else
            --    |XXX<<<<<<<<<<          XXXXXXXXXXXXXX|
            --        | Offset | Last     | First
            Data (Offset..Last - 1) := Data (Offset + 1..Last);
            Data (Last) := Fill;
            Last := Last - 1;
         end if;
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

   procedure Replace
             (  Container : in out Map;
                Key       : Key_Type;
                Item      : Object_Type;
                Override  : Overriding_Policy
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
            Item      => Item,
            Override  => Override
         );
      end if;
   end Replace;

   procedure Replace
             (  Container : in out Map;
                Key       : Key_Type;
                Item      : Object_Type;
                Override  : Overriding_Policy;
                Index     : out Positive
             )  is
      Offset : constant Integer := Find (Container, Key);
   begin
      if Offset > 0 then
         Index := Offset;
         Replace (Container, Index, Item);
      else
         Index := -Offset;
         Insert
         (  Container => Container,
            Index     => Index,
            Key       => Key,
            Item      => Item,
            Override  => Override
         );
      end if;
   end Replace;

   procedure Replace
             (  Container : in out Map;
                Index     : Positive;
                Item      : Object_Type
             )  is
   begin
      Container.Data (Container mod Index).Item := Item;
   end Replace;

   function Sup (Container : Map; Key : Key_Type) return Natural is
   begin
      if Container.Empty then
         return 0;
      end if;
      declare
         Index : Integer := Find (Container, Key);
      begin
         if Index > 0 then
            return Index;
         else
            Index := -Index;
            if Index > Get_Size (Container) then
               return 0;
            else
               return Index;
            end if;
         end if;
      end;
   end Sup;

   function "<" (Container : Map; Key : Key_Type) return Boolean is
   begin
      if Container.Empty then
         return False;
      else
         return Container.Data (Container.Last).Key < Key;
      end if;
   end "<";

   function "<" (Key : Key_Type; Container : Map) return Boolean is
   begin
      if Container.Empty then
         return True;
      else
         return Key < Container.Data (Container.First).Key;
      end if;
   end "<";

   function "<=" (Container : Map; Key : Key_Type) return Boolean is
   begin
      if Container.Empty then
         return False;
      else
         declare
            Upper : Key_Type renames
                             Container.Data (Container.Last).Key;
         begin
            return Upper = Key or else Upper < Key;
         end;
      end if;
   end "<=";

   function "<=" (Key : Key_Type; Container : Map) return Boolean is
   begin
      if Container.Empty then
         return True;
      else
         declare
            Lower : Key_Type renames
                             Container.Data (Container.First).Key;
         begin
            return Key = Lower or else Key < Lower;
         end;
      end if;
   end "<=";

   function ">=" (Container : Map; Key : Key_Type) return Boolean is
   begin
      if Container.Empty then
         return False;
      else
         declare
            Lower : Key_Type renames
                             Container.Data (Container.First).Key;
         begin
            return Key = Lower or else Key < Lower;
         end;
      end if;
   end ">=";

   function ">=" (Key : Key_Type; Container : Map) return Boolean is
   begin
      if Container.Empty then
         return True;
      else
         declare
            Upper : Key_Type renames
                             Container.Data (Container.Last).Key;
         begin
            return Key = Upper or else Upper < Key;
         end;
      end if;
   end ">=";

   function ">" (Container : Map; Key : Key_Type) return Boolean is
   begin
      if Container.Empty then
         return False;
      else
         return Key < Container.Data (Container.First).Key;
      end if;
   end ">";

   function ">" (Key : Key_Type; Container : Map) return Boolean is
   begin
      if Container.Empty then
         return True;
      else
         return Container.Data (Container.Last).Key < Key;
      end if;
   end ">";

end Generic_Bounded_Map;
