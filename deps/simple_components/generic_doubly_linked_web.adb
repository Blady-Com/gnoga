--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Doubly_Linked_Web                   Luebeck            --
--  Implementation                                 Autumn, 2006       --
--                                                                    --
--                                Last revision :  10:35 22 Oct 2011  --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body Generic_Doubly_Linked_Web is

   Offset : Storage_Offset := -1;   -- Offset to the object's dope
   Blocks : Address := Null_Address;

   type List_Header is limited record
      Next : Address;
      Prev : Address;
   end record;
   type Item_Header is array (List_Identification_Type) of List_Header;
   Header_Size : constant Storage_Offset :=
                    Item_Header'Max_Size_In_Storage_Elements;
   type Item_Header_Ptr is access Item_Header;
   for Item_Header_Ptr'Storage_Pool use Pool;

   procedure Free is
      new Ada.Unchecked_Deallocation (List_Item_Type, Node);

   function Header is
      new Ada.Unchecked_Conversion (Address, Item_Header_Ptr);

   function Address_To_Item is
      new Ada.Unchecked_Conversion (Address, Node);

   function Item_To_Address is
      new Ada.Unchecked_Conversion (Node, Address);

   function Ptr (Addr : Address) return Node is
      pragma Inline (Ptr);
   begin
      return Address_To_Item (Addr + Offset);
   end Ptr;

   function Deref (Element : Node) return Address is
   begin
      if Element = null then
         raise Constraint_Error;
      end if;
      if Offset < 0 then
         --
         -- Searching  for the memory block closest to the given address
         -- from the left. The offset between Item and the block address
         -- is the size of the dope plus the size of the header.
         --
         if Blocks = Null_Address then
            raise Program_Error;
         end if;
         Offset := Storage_Offset'Last;
         declare
            Addr    : constant Address := Item_To_Address (Element);
            Current : Address := Blocks;
         begin
            loop
               if Current < Addr then
                  Offset := Storage_Offset'Min (Offset, Addr - Current);
               end if;
               declare
                  This : List_Header renames
                     Header (Current) (List_Identification_Type'First);
               begin
                  Current := This.Next;
                  This.Next := Null_Address;
               end;
               exit when Current = Blocks;
            end loop;
         end;
         if Offset = Storage_Offset'Last then
            raise Program_Error;
         end if;
      end if;
      return Item_To_Address (Element) - Offset;
   end Deref;

   function Header (Element : Node) return Item_Header_Ptr is
      pragma Inline (Header);
   begin
      return Header (Deref (Element));
   end Header;

   procedure Allocate
             (  Pool            : in out Items_Storage_Pool;
                Storage_Address : out Address;
                Size            : Storage_Count;
                Alignment       : Storage_Count
             )  is
      Header_Alignment : constant Storage_Count :=
         Storage_Count'Max (Item_Header'Alignment, Alignment);
      Header_Offset    : constant Storage_Offset :=
         Header_Size + (-Header_Size) mod Header_Alignment;
   begin
      Allocate
      (  Pool.Host.all,
         Storage_Address,
         Size + Header_Offset,
         Header_Alignment
      );
      declare
         Block : Item_Header renames Header (Storage_Address).all;
      begin
         for List in Block'Range loop
            Block (List).Next := Null_Address;
         end loop;
         if Offset < 0 then
            --
            -- The  offset  to  the  object  address  according  to  the
            -- attribute X'Address is unknown. For this reason the block
            -- allocated is added to the list  of  allocated  blocks  to
            -- determine the offset later.
            --
            declare
               This : List_Header renames
                         Block (List_Identification_Type'First);
            begin
               if Blocks = Null_Address then
                  This.Next := Storage_Address;
                  This.Prev := Storage_Address;
                  Blocks    := Storage_Address;
               else
                  declare
                     Head : List_Header renames
                               Header (Blocks)
                                  (List_Identification_Type'First);
                     Tail : List_Header renames
                               Header (Head.Prev)
                                  (List_Identification_Type'First);
                  begin
                     This.Prev := Head.Prev;
                     This.Next := Blocks;
                     Tail.Next := Storage_Address;
                     Head.Prev := Storage_Address;
                  end;
               end if;
            end;
         end if;
      end;
      Storage_Address := Storage_Address + Header_Offset;
   end Allocate;

   procedure Deallocate
             (  Pool            : in out Items_Storage_Pool;
                Storage_Address : in Address;
                Size            : Storage_Count;
                Alignment       : Storage_Count
             )  is
      Header_Alignment : constant Storage_Count :=
         Storage_Count'Max (Item_Header'Alignment, Alignment);
      Header_Offset    : constant Storage_Offset :=
         Header_Size + (-Header_Size) mod Header_Alignment;
   begin
      if Offset < 0 then
         --
         -- The item is deallocated before placement of any  other  item
         -- in any list. It  is  removed  from  the  list  of  allocated
         -- blocks.
         --
         if Blocks = Null_Address then
            raise Program_Error;
         end if;
         declare
            Freed : constant Address := Storage_Address - Header_Offset;
            This  : List_Header renames
                      Header (Freed) (List_Identification_Type'First);
         begin
            if This.Next = Freed then
               Blocks := Null_Address;
            else
               if Blocks = Freed then
                  Blocks := This.Next;
               end if;
               declare
                  Prev : constant Address := This.Prev;
                  Next : constant Address := This.Next;
               begin
                  Header (Prev) (List_Identification_Type'First).Next :=
                     Next;
                  Header (Next) (List_Identification_Type'First).Prev :=
                     Prev;
               end;
            end if;
         end;
      else
         --
         -- Checking  for  dangling pointers. No deallocated item can be
         -- in any of the lists.
         --
         declare
            Block : Item_Header renames
               Header (Storage_Address - Header_Offset).all;
         begin
            for List in Block'Range loop
               if Block (List).Next /= Null_Address then
                  raise Program_Error;
               end if;
            end loop;
         end;
      end if;
      Deallocate
      (  Pool.Host.all,
         Storage_Address - Header_Offset,
         Size + Header_Offset,
         Header_Alignment
      );
   end Deallocate;

   function Storage_Size (Pool : Items_Storage_Pool)
      return Storage_Count is
   begin
      return Storage_Size (Pool.Host.all);
   end Storage_Size;

   procedure Append
             (  Brand     : List_Identification_Type;
                Container : in out Web;
                Element   : Node
             )  is
      Addr : constant Address := Deref (Element);
      This : List_Header renames Header (Addr) (Brand);
   begin
      if This.Next /= Null_Address then
         raise Constraint_Error;
      end if;
      if Container = null then
         This.Next := Addr;
         This.Prev := Addr;
         Container := Web (Element);
      else
         declare
            Next : List_Header renames Header (Node (Container))(Brand);
            Prev : List_Header renames Header (Next.Prev) (Brand);
         begin
            This.Prev := Next.Prev;
            This.Next := Prev.Next;
            Prev.Next := Addr;
            Next.Prev := Addr;
         end;
      end if;
   end Append;

   procedure Append
             (  Brand     : List_Identification_Type;
                Container : in out Web;
                Element   : Node;
                Source    : in out Web
             )  is
   begin
      if Node (Container) /= Element then
         if Container = Source then
            declare
               Temp : Web := Source;
            begin
               Remove (Brand, Temp, Element);
            end;
         else
            Remove (Brand, Source, Element);
         end if;
         Append (Brand, Container, Element);
      end if;
   end Append;

   procedure Erase
             (  Brand     : List_Identification_Type;
                Container : in out Web
             )  is
      Element : Node;
   begin
      while Container /= null loop
         Element := (Node (Container));
         declare
            Addr : constant Address := Deref (Element);
            This : List_Header renames Header (Addr) (Brand);
         begin
            if This.Next = Addr then
               Container := null;
            else
               Container := Web (Ptr (This.Next));
            end if;
            declare
               Prev : constant Address := This.Prev;
               Next : constant Address := This.Next;
            begin
               Header (Next) (Brand).Prev := Prev;
               Header (Prev) (Brand).Next := Next;
            end;
            This.Next := Null_Address;
            declare
               Head : Item_Header renames Header (Addr).all;
            begin
               for List in Head'Range loop
                  if Head (List).Next /= Null_Address then
                     Element := null;
                  end if;
               end loop;
               Free (Element);
            end;
         end;
      end loop;
   end Erase;

   procedure Delete
             (  Brand     : List_Identification_Type;
                Container : in out Web;
                Element   : in out Node
             )  is
   begin
      if Element = null then
         return;
      end if;
      declare
         Addr : constant Address := Deref (Element);
         This : List_Header renames Header (Addr) (Brand);
      begin
         if This.Next /= Null_Address then
            if Element = Node (Container) then
               if This.Next = Addr then
                  Container := null;
               else
                  Container := Web (Ptr (This.Next));
               end if;
            end if;
            declare
               Prev : constant Address := This.Prev;
               Next : constant Address := This.Next;
            begin
               Header (Next) (Brand).Prev := Prev;
               Header (Prev) (Brand).Next := Next;
            end;
            This.Next := Null_Address;
         end if;
         declare
            Head : Item_Header renames Header (Addr).all;
         begin
            for List in Head'Range loop
               if Head (List).Next /= Null_Address then
                  return;
               end if;
            end loop;
         end;
         Free (Element);
      end;
   end Delete;

   function Dope_Size return Storage_Offset is
   begin
      if Offset < 0 then
         raise Constraint_Error;
      else
         return Offset - Header_Size;
      end if;
   end Dope_Size;

   procedure Insert
             (  Brand    : List_Identification_Type;
                Position : Node;
                Element  : Node
             )  is
      Addr : constant Address := Deref (Element);
      This : List_Header renames Header (Addr) (Brand);
   begin
      if This.Next /= Null_Address or else Position = null then
         raise Constraint_Error;
      else
         declare
            Prev : List_Header renames Header (Position)  (Brand);
            Next : List_Header renames Header (Prev.Next) (Brand);
         begin
            This.Prev := Next.Prev;
            This.Next := Prev.Next;
            Prev.Next := Addr;
            Next.Prev := Addr;
         end;
      end if;
   end Insert;

   procedure Insert
             (  Brand    : List_Identification_Type;
                Position : Node;
                Element  : Node;
                Source   : in out Web
             )  is
   begin
      if Position /= Element then
         Remove (Brand, Source, Element);
         Insert (Brand, Position, Element);
      end if;
   end Insert;

   function Is_Empty
            (  Brand     : List_Identification_Type;
               Container : Web
            )  return Boolean is
   begin
      return Container = null;
   end Is_Empty;

   function Is_In
            (  Brand   : List_Identification_Type;
               Element : Node
            )  return Boolean is
   begin
      return
      (  Element /= null
      and then
         Header (Element) (Brand).Next /= Null_Address
      );
   end Is_In;

   function Is_In (Element : Node) return Boolean is
   begin
      if Element = null then
         return False;
      end if;
      declare
         Addr : constant Address := Deref (Element);
         Head : Item_Header renames Header (Addr).all;
      begin
         for List in Head'Range loop
            if Head (List).Next /= Null_Address then
               return True;
            end if;
         end loop;
      end;
      return False;
   end Is_In;

   procedure Merge
             (  Brand : List_Identification_Type;
                Head  : in out Web;
                Tail  : in out Web
             )  is
   begin
      if Head = null then
         Head := Tail;
      elsif Tail = null then
         Tail := Head;
      elsif Tail /= Head then
         --             .____.   .____.   .____.
         --       (1)-->|    |-->|    |-->|   B|-->(1) = 3
         --   4 = (2)<--|A___|<--|____|<--|____|<--(2)
         --             .____.   .____.   .____.
         --       (3)-->|    |-->|    |-->|   C|-->(3) = 1
         --   2 = (4)<--|D___|<--|____|<--|____|<--(4)
         --
         declare
            A : Address renames Header (Node (Head)) (Brand).Prev;
            B : Address renames Header (Ptr (A)) (Brand).Next;
            D : Address renames Header (Node (Tail)) (Brand).Prev;
            C : Address renames Header (Ptr (D)) (Brand).Next;
            S : Address;
         begin
            S := A; A := D; D := S; -- Swap A and D
            S := B; B := C; C := S; -- Swap B and C
         end;
      end if;
   end Merge;

   function Next
            (  Brand   : List_Identification_Type;
               Element : Node
            )  return Node is
      Next : constant Address := Header (Element) (Brand).Next;
   begin
      if Next = Null_Address then
         raise Constraint_Error;
      else
         return Ptr (Next);
      end if;
   end Next;

   function Next
            (  Brand     : List_Identification_Type;
               Container : Web
            )  return Node is
   begin
      return Next (Brand, Node (Container));
   end Next;

   procedure Prepend
             (  Brand     : List_Identification_Type;
                Container : in out Web;
                Element   : Node
             )  is
      Addr : constant Address := Deref (Element);
      This : List_Header renames Header (Addr) (Brand);
   begin
      if This.Next /= Null_Address then
         raise Constraint_Error;
      end if;
      if Container = null then
         This.Next := Addr;
         This.Prev := Addr;
         Container := Web (Element);
         return;
      end if;
      declare
         Next : List_Header renames Header (Node (Container)) (Brand);
         Prev : List_Header renames Header (Next.Prev) (Brand);
      begin
         This.Prev := Next.Prev;
         This.Next := Prev.Next;
         Prev.Next := Addr;
         Next.Prev := Addr;
         Container := Web (Element);
      end;
   end Prepend;

   procedure Prepend
             (  Brand     : List_Identification_Type;
                Container : in out Web;
                Element   : Node;
                Source    : in out Web
             )  is
   begin
      if Node (Container) /= Element then
         if Container = Source then
            declare
               Temp : Web := Source;
            begin
               Remove (Brand, Temp, Element);
            end;
         else
            Remove (Brand, Source, Element);
         end if;
         Prepend (Brand, Container, Element);
      end if;
   end Prepend;

   function Previous
            (  Brand   : List_Identification_Type;
               Element : Node
            )  return Node is
      Prev : constant Address := Header (Element) (Brand).Prev;
   begin
      if Prev = Null_Address then
         raise Constraint_Error;
      else
         return Ptr (Prev);
      end if;
   end Previous;

   function Previous
            (  Brand     : List_Identification_Type;
               Container : Web
            )  return Node is
   begin
      return Previous (Brand, Node (Container));
   end Previous;

   procedure Remove
             (  Brand     : List_Identification_Type;
                Container : in out Web;
                Element   : Node
             )  is
   begin
      if Element = null then
         return;
      end if;
      declare
         Addr : constant Address := Deref (Element);
         This : List_Header renames Header (Addr) (Brand);
      begin
         if This.Next = Null_Address then
            return;
         end if;
         if Element = Node (Container) then
            if This.Next = Addr then
               Container := null;
            else
               Container := Web (Ptr (This.Next));
            end if;
         end if;
         declare
            Prev : constant Address := This.Prev;
            Next : constant Address := This.Next;
         begin
            Header (Next) (Brand).Prev := Prev;
            Header (Prev) (Brand).Next := Next;
         end;
         This.Next := Null_Address;
      end;
   end Remove;

   procedure Take
             (  Brand     : List_Identification_Type;
                Container : in out Web;
                Element   : out Node
             )  is
   begin
      Element := Node (Container);
      if Element /= null then
         declare
            Addr : constant Address := Deref (Element);
            This : List_Header renames Header (Addr) (Brand );
         begin
            if This.Next = Addr then
               Container := null;
            else
               Container := Web (Ptr (This.Next));
            end if;
            declare
               Prev : constant Address := This.Prev;
               Next : constant Address := This.Next;
            begin
               Header (Next) (Brand).Prev := Prev;
               Header (Prev) (Brand).Next := Next;
            end;
            This.Next := Null_Address;
         end;
      end if;
   end Take;

end Generic_Doubly_Linked_Web;
