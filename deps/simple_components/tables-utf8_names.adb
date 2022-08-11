--                                                                    --
--  package Tables.UTF8_Names       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2008       --
--                                                                    --
--                                Last revision :  10:00 19 May 2022  --
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

with Ada.IO_Exceptions;          use Ada.IO_Exceptions;
with Strings_Edit.UTF8.Mapping;  use Strings_Edit.UTF8.Mapping;

package body Tables.UTF8_Names is

   type Extended_Equality is (Less, Equal, Prefix, Greater);

   function Canonize (Name : String) return String is
      Result : String (1..Name'Length);
      From   : Integer := Name'First;
      To     : Integer := Result'First;
      Code   : UTF8_Code_Point;
   begin
      while From <= Name'Last loop
         Get (Name, From, Code);
         if not Is_In (Code, Ignored) then
            Put (Result, To, Code);
         end if;
      end loop;
      return Result (1..To - 1);
   end Canonize;
--
-- Compare -- String with a pattern
--
--    Source  - The string
--    Pointer - The position in it
--    Item    - The pattern
--
-- Pointer is advanced if the outcome is Equal or Prefix
--
-- Returns :
--
--    Comparison result
--
   function Compare
            (  Source  : String;
               Pointer : access Integer;
               Item    : String
            )  return Extended_Equality is
      pragma Inline (Compare);
      Index_1 : Integer := Pointer.all;
      Index_2 : Integer := Item'First;
      Code_1  : UTF8_Code_Point;
      Code_2  : UTF8_Code_Point;
   begin
      loop
         if Index_2 > Item'Last then
            --
            -- No  more  code points to match. If Source ends here, then
            -- Item  is  equal  to  it. Otherwise it is a prefix. Before
            -- checking for this we need to skip all  code  points  from
            -- Ignored.
            --
            while Index_1 <= Source'Last loop
               begin
                  Pointer.all := Index_1;
                  Get (Source, Index_1, Code_1);
                  if not Is_In (Code_1, Ignored) then
                     return Prefix;
                  end if;
               exception
                  when Data_Error => -- Source is invalid, stop here
                     return Prefix;
               end;
            end loop;
            Pointer.all := Index_1;
            return Equal;
         elsif Index_1 > Source'Last then
            --
            -- Source ends here, it is less than Item
            --
            Pointer.all := Index_1;
            return Less;
         end if;
         --
         -- Get next Source code point. When  the  code  point  is  from
         -- Ignored, we just continue to the next code point. When it is
         -- invalid we stop as if Source ended here.
         --
         begin
            Get (Source, Index_1, Code_1);
         exception
            when Data_Error => -- Source is invalid, stop here
               return Less;
         end;
         if not Is_In (Code_1, Ignored) then
            --
            -- Get next Item code point. When Item is invalid Data_Error
            -- is propagated.
            --
            Get (Item, Index_2, Code_2);
            if Is_In (Code_1, Blanks) then
               if Is_In (Code_2, Blanks) then
                  --
                  -- Both code points are blank characters  we  have  to
                  -- skip all following blanks in both strings.
                  --
                  loop  -- Skipping blanks in Item
                     if Index_2 > Item'Last then
                        --
                        -- Item  ends  here.  When  Source contains only
                        -- blanks  and ignored code points then Item and
                        -- Source  are equal. Otherwise Item is a prefix
                        -- of Source.
                        --
                        loop
                           if Index_1 > Source'Last then
                              Pointer.all := Index_1;
                              return Equal;
                           end if;
                           Get (Source, Index_1, Code_1);
                           if not
                              (  Is_In (Code_1, Ignored)
                              or else
                                 Is_In (Code_1, Blanks)
                              )
                           then
                              Pointer.all := Index_1;
                              return Prefix;
                           end if;
                        end loop;
                     end if;
                     Get (Item, Index_2, Code_2);
                     exit when not Is_In (Code_2, Blanks);
                  end loop;
                  --
                  -- We have a non-blank code point in Item.  If  Source
                  -- ends with blanks and ignored points, then Source is
                  -- less than Item,  because it already has a non-blank
                  -- code point.
                  --
                  loop  -- Skipping blanks in Source
                     if Index_1 > Source'Last then
                        return Less;
                     end if;
                     Get (Source, Index_1, Code_1);
                     exit when not
                               (  Is_In (Code_1, Ignored)
                               or else
                                  Is_In (Code_1, Blanks)
                               );
                  end loop;
               else
                  --
                  -- Source has blank Item does not, source is less
                  --
                  return Less;
               end if;
            elsif Is_In (Code_2, Blanks) then
               --
               -- Source has non-blank Item has, source is greater
               --
               return Greater;
            end if;
            Code_1 := To_Lowercase (Code_1);
            Code_2 := To_Lowercase (Code_2);
            if Code_1 > Code_2 then
               return Greater;
            elsif Code_1 < Code_2 then
               return Less;
            end if;
         end if;
      end loop;
   end Compare;

   function Search
            (  Folder : Dictionary;
               Name   : String
            )  return Integer is
      Low     : Integer := 0;
      High    : Integer := Folder.Size + 1;
      This    : Integer;
      Pointer : aliased Integer;
   begin
      if High = 1 then
         return -1;
      end if;
      loop
         This := (Low + High) / 2;
         Pointer := Name'First;
         case Compare (Name, Pointer'Access, Folder.List (This).Name) is
            when Less =>
               High := This;
               if High - Low = 1 then
                  return -This;
               end if;
            when Equal =>
               return This;
            when Prefix | Greater =>
               Low := This;
               if High - Low = 1 then
                  return -(This + 1);
               end if;
         end case;
      end loop;
   end Search;

   procedure Add
             (  Folder : in out Dictionary;
                Name   : String;
                Data   : Tag
             )  is
   begin
      Check_Spelling (Name);
      declare
         Index : constant Integer := Search (Folder, Name);
      begin
         if Index > 0 then
            raise Name_Error;
         else
            Insert (Folder, -Index, Canonize (Name), Data);
         end if;
      end;
   end Add;

   procedure Add
             (  Folder : in out Dictionary;
                Name   : String;
                Data   : Tag;
                Offset : out Positive
             )  is
   begin
      Check_Spelling (Name);
      declare
         Index : Integer := Search (Folder, Name);
      begin
         if Index > 0 then
            raise Name_Error;
         else
            Index := -Index;
            Insert (Folder, Index, Canonize (Name), Data);
            Offset := Index;
         end if;
      end;
   end Add;

   procedure Delete (Folder : in out Dictionary; Name : String) is
      Index : constant Integer := Search (Folder, Name);
   begin
      if Index > 0 then
         Delete (Folder, Index);
      end if;
   exception
      when Data_Error => -- Ignore improperly encoded strings
         null;
   end Delete;

   function Find (Folder : Dictionary; Name : String) return Tag is
      Index : constant Integer := Search (Folder, Name);
   begin
      if Index > 0 then
         return Folder.List (Index).Data;
      else
         raise End_Error;
      end if;
   exception
      when Data_Error =>
         raise End_Error;
   end Find;

   function IsIn (Folder : Dictionary; Name : String) return Boolean is
   begin
      return Search (Folder, Name) > 0;
   exception
      when Data_Error => -- Ignore improperly encoded strings
         return False;
   end IsIn;

   function Locate (Folder : Dictionary; Name : String)
      return Natural is
      Index : constant Integer := Search (Folder, Name);
   begin
      if Index > 0 then
         return Index;
      else
         return 0;
      end if;
   end Locate;

   procedure Locate
             (  Source  : String;
                Pointer : in out Integer;
                Folder  : Dictionary;
                Offset  : out Natural
             )  is
      Found : Integer := 0;
      Low   : Integer := 0;
      High  : Integer := Folder.Size + 1;
      This  : Integer;
      Next  : Integer;
      Index : aliased Integer;
   begin
      if (  Pointer < Source'First
         or else
            (  Pointer > Source'Last
            and then
               Pointer - 1 > Source'Last
         )  )
      then
         raise Layout_Error;
      end if;
      while High - Low /= 1 loop
         This := (Low + High) / 2;
         Index := Pointer;
         case Compare (Source, Index'Access, Folder.List (This).Name) is
            when Less =>
               High := This;
            when Equal | Prefix =>
               Found := This;
               Next  := Index;
               Low   := This;
            when Greater =>
               for Lower in reverse Low + 1 .. This - 1 loop
                  exit when
                     (  Found /= 0
                     and then
                        (  Folder.List (Found).Name'Length
                        >  Folder.List (Lower).Name'Length
                     )  );
                  Index := Pointer;
                  case Compare
                       (  Source,
                          Index'Access,
                          Folder.List (Lower).Name
                       )  is
                     when Less =>       -- Rest items could be only
                        exit;           -- lesser than this, exit
                     when Equal | Prefix =>
                        Found := Lower; -- Here we are. Ignore the rest
                        Next  := Index; -- lesser, i.e. shorter, items
                        exit;
                     when Greater =>
                        null;           -- Undecided, continue
                  end case;
               end loop;
               Low := This;
         end case;
      end loop;
      if (  Found = 0
         or else
            (  Next <= Source'Last
            and then
               not Check_Matched (Source, Next)
         )  )
      then
         Offset := 0;
      else
         Offset  := Found;
         Pointer := Next;
      end if;
   end Locate;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Folder  : Dictionary;
                Data    : out Tag;
                Got_It  : out Boolean
             )  is
      Found : Integer := 0;
      Low   : Integer := 0;
      High  : Integer := Folder.Size + 1;
      This  : Integer;
      Next  : Integer;
      Index : aliased Integer;
   begin
      if (  Pointer < Source'First
         or else
            (  Pointer > Source'Last
            and then
               Pointer - 1 > Source'Last
         )  )
      then
         raise Layout_Error;
      end if;
      while High - Low /= 1 loop
         This := (Low + High) / 2;
         Index := Pointer;
         case Compare (Source, Index'Access, Folder.List (This).Name) is
            when Less =>
               High := This;
            when Equal | Prefix =>
               Found := This;
               Next  := Index;
               Low   := This;
            when Greater =>
               for Lower in reverse Low + 1 .. This - 1 loop
                  exit when
                     (  Found /= 0
                     and then
                        (  Folder.List (Found).Name'Length
                        >  Folder.List (Lower).Name'Length
                     )  );
                  Index := Pointer;
                  case Compare
                       (  Source,
                          Index'Access,
                          Folder.List (Lower).Name
                       )  is
                     when Less =>       -- Rest items could be only
                        exit;           -- less than this, exit
                     when Equal | Prefix =>
                        Found := Lower; -- Here we are. Ignore the rest
                        Next  := Index; -- lesser i.e. shorter, items
                        exit;
                     when Greater =>
                        null;           -- Undecided, continue
                  end case;
               end loop;
               Low := This;
         end case;
      end loop;
      if Found = 0 then
         Got_It := False;
         return;
      end if;
      if (  Next <= Source'Last
         and then
            not Check_Matched (Source, Next)
         )
      then
         Got_It := False;
         return;
      end if;
      Pointer := Next;
      Data    := Folder.List (Found).Data;
      Got_It  := True;
   end Get;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Folder  : Dictionary;
                Data    : out Tag
             )  is
      Got_It : Boolean;
   begin
      Get (Source, Pointer, Folder, Data, Got_It);
      if not Got_It then
         raise End_Error;
      end if;
   end Get;

   procedure Replace
             (  Folder : in out Dictionary;
                Name   : String;
                Data   : Tag
             )  is
   begin
      Check_Spelling (Name);
      declare
         Index : constant Integer := Search (Folder, Name);
      begin
         if Index > 0 then
            Folder.List (Index).Data := Data;
         else
            Insert (Folder, -Index, Canonize (Name), Data);
         end if;
      end;
   end Replace;

   procedure Replace
             (  Folder : in out Dictionary;
                Name   : String;
                Data   : Tag;
                Offset : out Positive
             )  is
   begin
      Check_Spelling (Name);
      declare
         Index : Integer := Search (Folder, Name);
      begin
         if Index > 0 then
            Folder.List (Index).Data := Data;
            Offset := Index;
         else
            Index := -Index;
            Insert (Folder, Index, Canonize (Name), Data);
            Offset := Index;
         end if;
      end;
   end Replace;

end Tables.UTF8_Names;
