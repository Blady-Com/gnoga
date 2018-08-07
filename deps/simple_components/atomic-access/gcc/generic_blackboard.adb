--                                                                    --
--  package Generic_Blackboard      Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Autumn, 2007       --
--                                                                    --
--                                Last revision :  17:44 21 Jul 2018  --
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

with Ada.Exceptions;  use Ada.Exceptions;

with System.Address_To_Access_Conversions;

package body Generic_Blackboard is
   use System;

   Storage_Offset_Size : constant Storage_Count :=
      (Storage_Count'Size + Storage_Unit - 1) / Storage_Unit;
   Aligned_Storage_Offset_Size : constant Storage_Count :=
      (  Storage_Offset_Size
      -  (Storage_Offset_Size mod (-Element_Type'Alignment))
      );

   Address_Offset : Storage_Offset := 0; -- To the "official" address
--
-- Stored_Offset -- A sequence  of  storage  elements  of  the  size  of
--                  Storage_Offset. It is used  to  pack  Storage_Offset
--                  items into arrays of Storage_Element.
--
   subtype Stored_Offset is Buffer (1..Storage_Offset_Size);

   package Stored_Offset_Access is
      new System.Address_To_Access_Conversions (Stored_Offset);
   use Stored_Offset_Access;

   package Element_Type_Access is
      new System.Address_To_Access_Conversions (Element_Type);
   use Element_Type_Access;

   function Get_Offset
            (  Storage : Blackboard'Class;
               Pointer : Reference
            )  return Storage_Offset is
      pragma Inline (Get_Offset);
   begin
      return Storage_Offset (Pointer mod Reference (Storage.Size));
   end Get_Offset;

   function Get_Block_Size
            (  Storage : Blackboard'Class;
               Offset  : Storage_Offset
            )  return Storage_Offset is
      pragma Inline (Get_Block_Size);
      Size : aliased Storage_Offset;
      To   : constant Storage_Offset := Offset + Storage_Offset_Size;
   begin
      if To <= Storage.Data'Last then
         To_Pointer (Size'Address).all :=
            Storage.Data (Offset + 1..Offset + Storage_Offset_Size);
         return Size;
      else
         return 0;
      end if;
   end Get_Block_Size;

   procedure Allocate
             (  Storage   : in out Blackboard;
                Address   : out System.Address;
                Size      : Storage_Count;
                Alignment : Storage_Count
             )  is
      Upper : Storage_Count;
      Moved : Reference;
      Lower : Storage_Count :=
                 Get_Offset
                 (  Storage,
                    Load (Storage.Lower'Unchecked_Access)
                 );
   begin
      Storage.Length := Size + Aligned_Storage_Offset_Size;
      if Alignment /= 0 then
         -- Rounding Length to have all following elements aligned
         Storage.Length :=
            Storage.Length - (Storage.Length mod (-Alignment));
      end if;
      Storage.Pointer := Load (Storage.Upper'Unchecked_Access);
      Upper := Get_Offset (Storage, Storage.Pointer);
      if Upper + Storage.Length > Storage.Size then
         if Storage.Length > Storage.Size then   -- Too large to fit
            Raise_Exception
            (  Storage_Error'Identity,
               (  "Too large object"
               &  Storage_Count'Image (Storage.Length)
               &  " >"
               &  Storage_Count'Image (Storage.Size)
            )  );
         end if;
         --
         -- We have to wrap the buffer
         --
         if Lower >= Upper then
            --
            -- |<-current->|             |<-previous->|
            -- |           |<---------new-block-------|--->
            -- |           |Upper        |Lower       |
            -- |<Lower                                |
            --
            Moved := Load (Storage.Lower'Unchecked_Access) +
                     Reference (Storage.Size - Lower);
            if Moved > Storage.Preserved then
               raise Constraint_Error;
            else
               Store (Storage.Lower'Access, Moved);
            end if;
         end if;
         if Upper + Storage.Length > Storage.Size then
            --
            -- |   |<-current->|<-----new-block-------|-->
            -- |   |<Lower     |<Upper                |
            -- |               |0000000000000000000000|
            -- |<Upper
            --
            for Index in Upper + 1..Storage.Size loop
               Storage.Data (Index) := 0;
            end loop;
            --
            -- Moving to the next frame
            --
            Storage.Pointer :=
               Storage.Pointer + Reference (Storage.Size - Upper);
            Upper := 0;
         end if;
      end if;
      --
      -- Here  we  already  know  that  the new block would fit into the
      -- buffer. We have to move Storage.Lower by block if necessary.
      --
      if (  Storage.Lower /= Storage.Upper -- Not empty
         and then            -- |<-current->|             |<-previous->|
            Lower >= Upper   -- |           |<--new-block-|-->         |
         and then            -- |           |Upper        |Lower       |
            Lower - Upper < Storage.Length
         )
      then
         --
         -- We have to move Storage.Lower by  block  sizes  until  there
         -- would be Length place between Upper and Lower.
         --
         declare
            Old  : constant Storage_Offset := Lower;
            Size : Storage_Offset;
         begin
            Size := Get_Block_Size (Storage, Lower);
            if Size > 0 then
               loop
                  Lower := Lower + Size;
                  exit when Lower >= Storage.Size;
                  Size := Get_Block_Size (Storage, Lower);
                  exit when Size <= 0;
                  if Lower - Upper >= Storage.Length then
                     --
                     -- Now we have the place. Storage.Lower is advanced
                     -- to this index staying in the current frame.
                     --
                     Moved := Load (Storage.Lower'Unchecked_Access) +
                              Reference (Lower - Old);
                     if Moved > Storage.Preserved then
                        raise Constraint_Error;
                     else
                        Store (Storage.Lower'Access, Moved);
                        goto Done;
                     end if;
                  end if;
               end loop;
            end if;
            --
            -- The rest of the buffer is unused. Storage.Lower  is
            -- advanced to the next frame.
            --
            declare
               Old_Lower : constant Reference :=
                              Load (Storage.Lower'Unchecked_Access);
            begin
               Moved :=
                  (  Old_Lower
                  +  Reference
                     (  Storage.Size
                     -  Get_Offset (Storage, Old_Lower)
                  )  );
            end;
            if Moved > Storage.Preserved then
               raise Constraint_Error;
            else
               Store (Storage.Lower'Access, Moved);
            end if;
         end;
      end if;
<<Done>>
      Storage.Index := Upper + 1;
      Address :=
         Storage.Data (Upper + 1)'Address + Aligned_Storage_Offset_Size;
   end Allocate;

   procedure Deallocate
             (  Storage   : in out Blackboard;
                Address   : System.Address;
                Size      : Storage_Count;
                Alignment : Storage_Count
             )  is
   begin
      null;
   end Deallocate;

   function First (Storage : Blackboard) return Reference is
   begin
      return Load (Storage.Lower'Unchecked_Access);
   end First;

   function Get (Storage : Blackboard; Pointer : Reference)
      return Element_Type is
      Index  : Reference     := Pointer;
      Offset : Storage_Count := Get_Offset (Storage, Index);
      Size   : Storage_Offset;
   begin
      if Index >= Storage.Upper'Unchecked_Access then
         raise Constraint_Error;
      end if;
      Size :=
         Get_Block_Size (Storage, Offset) - Aligned_Storage_Offset_Size;
      if Size <= 0 then
         if Size = -Aligned_Storage_Offset_Size then -- Wrapping
            Index := Index + Reference (Storage.Size - Offset);
            if Index >= Storage.Upper'Unchecked_Access then
               raise Constraint_Error;  -- Wrapped reference is outside
            end if;
            Offset := 0;
            Size   := Get_Block_Size (Storage, Offset) -
                      Aligned_Storage_Offset_Size;
            if Size <= 0 or else Index < Storage.Lower'Unchecked_Access
            then
               raise Constraint_Error;
            end if;
         else
            raise Constraint_Error;
         end if;
      end if;
      --
      -- The  size  must  be valid, that means we can use it to copy the
      -- data from the buffer
      --
      Offset := Offset + Aligned_Storage_Offset_Size;
      declare
         Result : Buffer (1..Size) :=
                     Storage.Data (Offset + 1..Offset + Size);
      begin
         --
         -- Here Result contains a raw bit-wise copy of  the  referenced
         -- item. If the reference is no longer valid, we just  drop  an
         -- exception. Otherwise  it  is  safe  to  convert  it  to  the
         -- legitimate  element  type.  Address to pointer conversion is
         -- used for. Note how Address_Offset is  added  to  obtain  the
         -- "official" address. The offset  is  0  for  all  types,  but
         -- indefinite arrays. Though the offset is static it need to be
         -- evaluated, which the procedure Put does.
         --
         if Index < Storage.Lower'Unchecked_Access then
            raise Constraint_Error;
         else
            return To_Pointer (Result'Address + Address_Offset).all;
         end if;
      end;
   end Get;

   function Image (Pointer : Reference) return String is
      Result : constant String := Reference'Image (Pointer);
   begin
      return Result (Result'First + 1..Result'Last);
   end Image;

   function Is_Valid (Storage : Blackboard; Pointer : Reference)
      return Boolean is
   begin
      return Pointer >= Storage.Lower'Unchecked_Access and then
             Pointer <  Storage.Upper'Unchecked_Access;
   end Is_Valid;

   procedure Next
             (  Storage : Blackboard;
                Pointer : in out Reference;
                Sequent : out Boolean
             )  is
      Index  : Reference := Pointer;
      Offset : Storage_Count;
      Size   : Storage_Offset;
      Moved  : Boolean := False;
   begin
      Sequent := True;
      if Index >= Storage.Upper'Unchecked_Access then
         return;
      end if;
      Offset := Get_Offset (Storage, Index);
      Size   := Get_Block_Size (Storage, Offset);
      if Index < Storage.Lower'Unchecked_Access then
         Pointer := Load (Storage.Lower'Unchecked_Access);
         Sequent := False;
         return;
      end if;
      if Size <= 0 then
         Index := Index + Reference (Size + Storage.Size - Offset);
         if Index >= Storage.Upper'Unchecked_Access then -- Wrapped to
            return;                                      -- the end
         end if;
         Offset := 0;
         Size   := Get_Block_Size (Storage, Offset);
         if Index < Storage.Lower'Unchecked_Access then
            Pointer := Load (Storage.Lower'Unchecked_Access);
            Sequent := False;
            return;
         end if;
      end if;
      Pointer := Index + Reference (Size);
   end Next;

   procedure Put
             (  Storage : in out Blackboard;
                Element : Element_Type;
                Pointer : out Reference
             )  is
      --
      -- The implementation of Blackboard is a storage pool. So in order
      -- to  copy something into, we just allocate it there. For this an
      -- access type is created.
      --
      type Element_Type_Ptr is access Element_Type;
      for Element_Type_Ptr'Storage_Pool use Storage;

      Ptr : Element_Type_Ptr;
   begin
      Storage.Preserved := Reference'Last;
      Ptr := new Element_Type'(Element);
      --
      -- A  copy of Element is allocated and Ptr is a pointer to it. The
      -- pointer might be shifted if Element  is  an  indefinite  array.
      -- Usually  the  array address is the address of its first element
      -- rather than of the first allocated storage element. This offset
      -- is type specific and need to be evaluated. Which is the purpose
      -- of Address_Offset.
      --
      Address_Offset :=
         (  Ptr.all'Address
         -  Storage.Data (Storage.Index)'Address
         -  Aligned_Storage_Offset_Size
         );
      Storage.Data       -- Store the block size in front of the element
      (  Storage.Index
      .. Storage.Index + Storage_Offset_Size - 1
      )  := To_Pointer (Storage.Length'Address).all;
      --
      -- Finally, move the upper reference making new element accessible
      --
      Pointer := Storage.Pointer;
      Store
      (  Storage.Upper'Access,
         Pointer + Reference (Storage.Length)
      );
   end Put;

   procedure Put
             (  Storage : in out Blackboard;
                Element : Element_Type
             )  is
      type Element_Type_Ptr is access Element_Type;
      for Element_Type_Ptr'Storage_Pool use Storage;

      Ptr : Element_Type_Ptr;
   begin
      Storage.Preserved := Reference'Last;
      Ptr := new Element_Type'(Element);
      --
      -- A  copy of Element is allocated and Ptr is a pointer to it. The
      -- pointer might be shifted if Element  is  an  indefinite  array.
      -- Usually  the  array address is the address of its first element
      -- rather than of the first allocated storage element. This offset
      -- is type specific and need to be evaluated. Which is the purpose
      -- of Address_Offset.
      --
      Address_Offset :=
         (  Ptr.all'Address
         -  Storage.Data (Storage.Index)'Address
         -  Aligned_Storage_Offset_Size
         );
      Storage.Data       -- Store the block size in front of the element
      (  Storage.Index
      .. Storage.Index + Storage_Offset_Size - 1
      )  := To_Pointer (Storage.Length'Address).all;
      --
      -- Finally, move the upper reference making new element accessible
      --
      Store
      (  Storage.Upper'Access,
         Storage.Pointer + Reference (Storage.Length)
      );
   end Put;

   procedure Put
             (  Storage  : in out Blackboard;
                Element  : Element_Type;
                Preserve : Reference;
                Pointer  : out Reference;
                Success  : out Boolean
             )  is
      --
      -- The implementation of Blackboard is a storage pool. So in order
      -- to  copy something into, we just allocate it there. For this an
      -- access type is created.
      --
      type Element_Type_Ptr is access Element_Type;
      for Element_Type_Ptr'Storage_Pool use Storage;

      Ptr : Element_Type_Ptr;
   begin
      Storage.Preserved := Preserve;
      Ptr := new Element_Type'(Element);
      --
      -- A  copy of Element is allocated and Ptr is a pointer to it. The
      -- pointer might be shifted if Element  is  an  indefinite  array.
      -- Usually  the  array address is the address of its first element
      -- rather than of the first allocated storage element. This offset
      -- is type specific and need to be evaluated. Which is the purpose
      -- of Address_Offset.
      --
      Address_Offset :=
         (  Ptr.all'Address
         -  Storage.Data (Storage.Index)'Address
         -  Aligned_Storage_Offset_Size
         );
      Storage.Data       -- Store the block size in front of the element
      (  Storage.Index
      .. Storage.Index + Storage_Offset_Size - 1
      )  := To_Pointer (Storage.Length'Address).all;
      --
      -- Finally, move the upper reference making new element accessible
      --
      Pointer := Storage.Pointer;
      Store
      (  Storage.Upper'Access,
         Pointer + Reference (Storage.Length)
      );
      Success := True;
   exception
      when Constraint_Error =>
         Success := False;
   end Put;

   procedure Put
             (  Storage  : in out Blackboard;
                Element  : Element_Type;
                Preserve : Reference;
                Success  : out Boolean
             )  is
      Pointer : Reference;
   begin
      Put (Storage, Element, Preserve, Pointer, Success);
   end Put;

   function Storage_Size (Storage : Blackboard) return Storage_Count is
   begin
      return Storage.Size;
   end Storage_Size;

   function Upper (Storage : Blackboard) return Reference is
   begin
      return Load (Storage.Upper'Unchecked_Access);
   end Upper;

   function "<" (Storage : Blackboard; Pointer : Reference)
      return Boolean is
   begin
     return LE (Load (Storage.Upper'Unchecked_Access), Pointer);
   end "<";

   function "<" (Pointer : Reference; Storage : Blackboard)
      return Boolean is
   begin
     return LT (Pointer, Load (Storage.Lower'Unchecked_Access));
   end "<";

   function "<"
            (  Left  : Reference;
               Right : Atomic_Reference_Ptr
            )  return Boolean is
   begin
      return LT (Left, Load (Right));
   end "<";

   function "<"  (Left, Right : Reference) return Boolean is
   begin
      return LT (Left, Right);
   end "<";

   function ">" (Storage : Blackboard; Pointer : Reference)
      return Boolean is
   begin
     return GT (Load (Storage.Lower'Unchecked_Access), Pointer);
   end ">";

   function ">" (Pointer : Reference; Storage : Blackboard)
      return Boolean is
   begin
     return GE (Pointer, Load (Storage.Upper'Unchecked_Access));
   end ">";

   function ">" (Left, Right : Reference) return Boolean is
   begin
      return GT (Left, Right);
   end ">";

   function "<=" (Left, Right : Reference) return Boolean is
   begin
      return LE (Left, Right);
   end "<=";

   function ">=" (Left, Right : Reference) return Boolean is
   begin
      return GE (Left, Right);
   end ">=";

   function ">="
            (  Left  : Reference;
               Right : Atomic_Reference_Ptr
            )  return Boolean is
   begin
      return GE (Left, Load (Right));
   end ">=";

end Generic_Blackboard;
