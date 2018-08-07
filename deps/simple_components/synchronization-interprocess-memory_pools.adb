--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Memory_Pools                                Spring, 2018       --
--  Implementation                                                    --
--                                Last revision :  19:18 30 Apr 2018  --
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

with Ada.Calendar;           use Ada.Calendar;
with Ada.Exceptions;         use Ada.Exceptions;
with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

package body Synchronization.Interprocess.Memory_Pools is

   Pool_Init : constant String := "The memory pool is not " &
                                  "initialized or already finalized";

   function Invariant (Data : Pool_Data) return Boolean is
   begin
      return Data.Size = Data.Free_Blocks * 2 + Data.Free_Count
                       + Data.Used_Blocks * 2 + Data.Used_Count;
   end Invariant;

--     procedure Dump (Text : String; Data : Pool_Data) is
--        use Ada.Text_IO;
--        Core  : Pool_Array renames Data.Core;
--        Index : Reference := Core'First;
--        Count : Natural   := 0;
--     begin
--        Put_Line
--        (  Text
--        &  ", First free "
--        &  Image (Integer (Data.First_Free))
--        &  ", free blocks "
--        &  Image (Integer (Data.Free_Blocks))
--        &  ", used blocks "
--        &  Image (Integer (Data.Used_Blocks))
--        &  ", free count "
--        &  Image (Integer (Data.Free_Count))
--        &  ", used count "
--        &  Image (Integer (Data.Used_Count))
--        &  ", free space "
--        &  Image (Integer (Data.Free_Blocks * 2 + Data.Free_Count))
--        &  " + used space "
--        &  Image (Integer (Data.Used_Blocks * 2 + Data.Used_Count))
--        &  " = "
--        &  Image
--           (  Integer
--              (  Data.Free_Blocks * 2 + Data.Free_Count
--              +  Data.Used_Blocks * 2 + Data.Used_Count
--           )  )
--        &  ", size "
--        &  Image (Integer (Data.Size))
--        &  ":"
--        );
--        while Index <= Core'Last loop
--           if Core (Index) > 0 then -- Free block margin
--              Count := Count + 1;
--              Put_Line
--              (  Image (Count)
--              &  " free block: margin "
--              &  Image (Integer (Core (Index)))
--              &  " at "
--              &  Image (Integer (Index))
--              &  ", prev "
--              &  Image (Integer (Core (Index + 1)))
--              &  ", next "
--              &  Image (Integer (Core (Index + 2)))
--              &  ", margin "
--              &  Image (Integer (Core (Index + Core (Index) + 1)))
--              &  " at "
--              &  Image (Integer (Index + Core (Index) + 1))
--              );
--              Index := Index + Core (Index) + 2;
--           else
--              Count := Count + 1;
--              Put_Line
--              (  Image (Count)
--              &  " used block: margin "
--              &  Image (Integer (Core (Index)))
--              &  " at "
--              &  Image (Integer (Index))
--              &  ", margin "
--              &  Image (Integer (Core (Index - Core (Index) + 1)))
--              &  " at "
--              &  Image (Integer (Index - Core (Index) + 1))
--              );
--              Index := Index - Core (Index) + 2;
--           end if;
--        end loop;
--        Put_Line ("-------------------------------------------");
--     end Dump;

   procedure Allocate
             (  Pool      : in out Interprocess_Pool;
                Address   : out System.Address;
                Size      : Storage_Count;
                Alignment : Storage_Count
             )  is
      Started  : constant Time := Clock;
   begin
      if Pool.Data = null then
         Raise_Exception (Status_Error'Identity, Pool_Init);
      end if;
      declare
         Data : Pool_Data  renames Pool.Data.all;
         Core : Pool_Array renames Data.Core;
      begin
         Seize (Pool.Lock.all, Pool.Timeout);
--           Dump
--           (  (  "Allocating "
--              &  Image (Integer (Size))
--              &  " = "
--              &  Image (Integer (To_Reference (Size)))
--              &  " start"
--              ),
--              Data
--           );
         declare
            Count : Reference := Reference'Max (To_Reference (Size), 2);
            This  : Reference := Data.First_Free;
         begin
            if This = 0 then
               Raise_Exception
               (  Storage_Error'Identity,
                  "No free block in the pool left"
               );
            end if;
            loop
               if Core (This) < Count then -- This block is too small
                  This := Core (This + 1); -- The next block
                  if This = Data.First_Free then
                     Raise_Exception
                     (  Storage_Error'Identity,
                        "No space left in the pool blocks"
                     );
                  end if;
               elsif Core (This) > Count + 2 then -- Split this block
                  declare
                     Space : constant Reference :=
                                      Core (This) - Count - 2;
                  begin
                        -- Mark block margins of reduced size
                     Core (This) := Space;
                     This        := This + Space + 1;
                     Core (This) := Space;
                     This        := This + 1;  -- New used block
                     Address     := Core (This + 1)'Address;
                        -- Mark block margins as used
                     Core (This) := -Count;
                     This        := This + Count + 1;
                     Core (This) := -Count;
                        -- Statistics
                     Data.Used_Count  := Data.Used_Count  + Count;
                     Data.Free_Count  := Data.Free_Count  - Count - 2;
                     Data.Used_Blocks := Data.Used_Blocks + 1;
                     if not Invariant (Data) then
                        Raise_Exception
                        (  Storage_Error'Identity,
                           "Splitting block error in Allocate"
                        );
                     end if;
--                       Dump ("Allocate + split", Data);
                     exit;
                  end;
               else -- Use all block
                  declare
                     Prev : constant Reference := Core (This + 1);
                     Next : constant Reference := Core (This + 2);
                  begin
                     if Next = This then -- The last block
                        Data.First_Free := 0;
                     else -- Remove the block from the list
                        Core (Prev + 2) := Next;
                        Core (Next + 1) := Prev;
                        Data.First_Free := Next;
                     end if;
                     Count   := Core (This);
                     Address := Core (This + 1)'Address;
                        -- Mark block margins as used
                     Core (This) := -Count;
                     This        := This + Count + 1;
                     Core (This) := -Count;
                        -- Statistics
                     Data.Used_Count  := Data.Used_Count  + Count;
                     Data.Free_Count  := Data.Free_Count  - Count;
                     Data.Free_Blocks := Data.Free_Blocks - 1;
                     Data.Used_Blocks := Data.Used_Blocks + 1;
                     if not Invariant (Data) then
                        Raise_Exception
                        (  Storage_Error'Identity,
                           "Taking block error in Allocate"
                        );
                     end if;
--                       Dump ("Allocate + take all", Data);
                     exit;
                  end;
               end if;
            end loop;
            Release (Pool.Lock.all);
         exception
            when others =>
               Release (Pool.Lock.all);
               raise;
         end;
      end;
   end Allocate;

   procedure Check (Data : Pool_Data; Index : Reference) is
      Core : Pool_Array renames Data.Core;
   begin
      if Index not in 2..Data.Size - 1 then
         Raise_Exception
         (  Storage_Error'Identity,
            "A reference is outside the pool bounds"
         );
      end if;
--        Dump ("Check reference " & Image (Integer (Index)), Data);
      declare
         Size : constant Reference := -Core (Index - 1);
         That : constant Reference := Index + Size;
      begin
         if Size < 2 then
--              Dump ("Used block size is less than 2", Data);
            Raise_Exception
            (  Storage_Error'Identity,
               (  "Used block size is less than 2 items (at "
               &  Image (Integer (Index))
               &  ")"
            )  );
         elsif That not in Index + 1..Data.Size then
--              Dump ("Block's end margin is outside the pool", Data);
            Raise_Exception
            (  Storage_Error'Identity,
               (  "Block's end margin is outside the pool bounds (at "
               &  Image (Integer (That))
               &  ")"
            )  );
         elsif Size /= -Core (That) then
--              Dump ("Block's margins do not match", Data);
            Raise_Exception
            (  Storage_Error'Identity,
               (  "Block's margins do not match (at "
               &  Image (Integer (Index))
               &  ".."
               &  Image (Integer (That))
               &  ")"
            )  );
         end if;
      end;
   end Check;

   procedure Deallocate
             (  Pool      : in out Interprocess_Pool;
                Address   : System.Address;
                Size      : Storage_Count;
                Alignment : Storage_Count
             )  is
      Started  : constant Time := Clock;
   begin
      if Pool.Data = null then
         Raise_Exception (Status_Error'Identity, Pool_Init);
      end if;
      declare
         Data : Pool_Data  renames Pool.Data.all;
         Core : Pool_Array renames Data.Core;
         This : Reference := To_Reference (Data, Address) - 1;
      begin
         Seize (Pool.Lock.all, Pool.Timeout);
--           Dump ("Deallocate start", Data);
         declare
            Length : Reference := -Core (This);
            That   : Reference := This + Length + 1;
         begin
            if (  That not in This..Data.Size
               or else
                  Length /= -Core (That)
               or else
                  Length < 2
               )
            then
               Raise_Exception
               (  Storage_Error'Identity,
                  "The address points to a corrupted block"
               );
            elsif To_Reference (Size) > Length then
               Raise_Exception
               (  Storage_Error'Identity,
                  "The size is greater than the block size"
               );
            end if;
            Data.Used_Count  := Data.Used_Count  - Length;
            Data.Used_Blocks := Data.Used_Blocks - 1;
            if This > 1 and then Core (This - 1) > 0 then
               if That < Data.Size and then Core (That + 1) > 0 then
                  -- Merge three blocks
                  Data.Free_Count  := Data.Free_Count  + Length + 4;
                  Data.Free_Blocks := Data.Free_Blocks - 1;

                  This   := This - 1;
                  Length := Length + Core (This) + 2;
                  That   := That + 1;
                  declare
                     Next : constant Reference := Core (That + 1);
                     Prev : constant Reference := Core (That + 2);
                  begin
                     if That = Data.First_Free then
                        Data.First_Free := Next;
                     end if;
                     Core (Prev + 2) := Prev;
                     Core (Next + 1) := Next;
                  end;
                  This   := This   - Core (This) - 1;
                  Length := Length + Core (That) + 2;
                  That   := This   + Length + 1;
                  Core (This) := Length;
                  Core (That) := Length;
                  if not Invariant (Data) then
                     Raise_Exception
                     (  Storage_Error'Identity,
                        "Merging with three blocks in Deallocate"
                     );
                  end if;
--                    Dump ("Deallocate + merge 3", Data);
               else
                  -- Merge with the blocks ahead
                  Data.Free_Count  := Data.Free_Count + Length + 2;

                  This   := This - 1;
                  Length := Length + Core (This) + 2;
                  This   := That - Length - 1;
                  Core (This) := Length;
                  Core (That) := Length;
                  if not Invariant (Data) then
                     Raise_Exception
                     (  Storage_Error'Identity,
                        "Merging with preceding block in Deallocate"
                     );
                  end if;
--                    Dump ("Deallocate + merge with previous", Data);
               end if;
            else
               if That < Data.Size and then Core (That + 1) > 0 then
                  -- Merge with the block behind
                  Data.Free_Count := Data.Free_Count + Length + 2;
                  That := That + 1;
                  declare
                     Size : constant Reference := Core (That);
                     Next : constant Reference := Core (That + 1);
                     Prev : constant Reference := Core (That + 2);
                  begin
                     if That = Data.First_Free then
                        Data.First_Free := This;
                     end if;
                     Core (This + 1) := Prev;
                     Core (This + 2) := Next;
                     Core (Prev + 2) := This;
                     Core (Next + 1) := This;
                     That := That + Size + 1;
                  end;
                  Length := That - This - 1;
                  Core (This) := Length;
                  Core (That) := Length;
                  if not Invariant (Data) then
                     Raise_Exception
                     (  Storage_Error'Identity,
                        "Merging with following block in Deallocate"
                     );
                  end if;
--                    Dump ("Deallocate + merge with next", Data);
               else
                  -- Free this block
                  Data.Free_Count  := Data.Free_Count  + Length;
                  Data.Free_Blocks := Data.Free_Blocks + 1;

                  Core (This) := Length;
                  Core (That) := Length;
                  if Data.First_Free = 0 then -- Empty list
                     Data.First_Free := This;
                     Core (This + 1) := This;
                     Core (This + 2) := This;
                  else
                     declare
                        Next : constant Reference := Data.First_Free;
                        Prev : constant Reference := Core (Next + 1);
                     begin
                        Core (This + 1) := Prev;
                        Core (This + 2) := Next;
                        Core (Prev + 2) := This;
                        Core (Next + 1) := This;
                     end;
                  end if;
                  if not Invariant (Data) then
                     Raise_Exception
                     (  Storage_Error'Identity,
                        "Freeing block error in Deallocate"
                     );
                  end if;
--                    Dump ("Deallocate", Data);
               end if;
            end if;
            Release (Pool.Lock.all);
         exception
            when others =>
               Release (Pool.Lock.all);
               raise;
         end;
      end;
   end Deallocate;

   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Pool   : Interprocess_Pool
             )  is
   begin
       Enumerate (Stream, Pool.Object);
   end Enumerate;

   procedure Free
             (  Pool    : in out Interprocess_Pool;
                Pointer : in out Reference
             )  is
   begin
      if Pointer /= Null_Reference then
         Deallocate
         (  Pool      => Pool,
            Address   => To_Address (Pool, Pointer),
            Size      => 0,
            Alignment => 0
         );
         Pointer := Null_Reference;
      end if;
   end Free;

   function Get_Offset
            (  Pool : Interprocess_Pool
            )  return Storage_Offset is
   begin
      return Pool.Object.Offset;
   end Get_Offset;

   function Get_Size (Object : Shared_Object)
      return Storage_Count is
      subtype Data is Pool_Data (To_Reference (Object.Pool.Size));
   begin
      return Round (Data'Max_Size_In_Storage_Elements);
   end Get_Size;

   procedure Get_Statistics
             (  Pool        : Interprocess_Pool;
                Free_Blocks : out Natural;
                Used_Blocks : out Natural;
                Free_Space  : out Storage_Count;
                Used_Space  : out Storage_Count
             )  is
   begin
      if Pool.Data = null then
         Raise_Exception (Status_Error'Identity, Pool_Init);
      end if;
      declare
         Data : Pool_Data  renames Pool.Data.all;
      begin
         Free_Blocks := Natural (Data.Free_Blocks);
         Used_Blocks := Natural (Data.Used_Blocks);
         Free_Space :=
            (  Storage_Count (Data.Free_Count)
            *  Integer'Max_Size_In_Storage_Elements
            );
         Used_Space :=
            (  Storage_Count (Data.Used_Count)
            *  Integer'Max_Size_In_Storage_Elements
            );
      end;
   end Get_Statistics;

   function Get_Timeout (Pool : Interprocess_Pool) return Duration is
   begin
      return Pool.Timeout;
   end Get_Timeout;

   procedure Map
             (  Object   : in out Shared_Object;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             )  is
      Pool : Interprocess_Pool'Class renames Object.Pool.all;
      subtype Data is Pool_Data (To_Reference (Pool.Size));
      package Mapper is new Generic_Memory_Mapper (Data);
      This : Abstract_Shared_Object_Ptr := Shared.First;
   begin
      while (  This /= null
            and then
               This /= Pool.Object'Unchecked_Access
            )  loop
         if This.all in Mutex'Class then
            Pool.Lock := Mutex'Class (This.all)'Unchecked_Access;
         end if;
         This := This.Next;
      end loop;
      if Pool.Lock = null then
         Raise_Exception
         (  Mode_Error'Identity,
            (  "The environment contains no mutex "
            &  "record member appearing before the pool object"
         )  );
      end if;
      Pool.Data := Mapper.Map (Location, Owner).all'Unchecked_Access;
      if Owner then
         declare
            Data : Pool_Data renames Pool.Data.all;
         begin
            Data.Core (1)         := Data.Size - 2;
            Data.Core (2)         := 1;
            Data.Core (3)         := 1;
            Data.Core (Data.Size) := Data.Size - 2;
         end;
      end if;
   end Map;

   procedure Set_Timeout
             (  Pool    : in out Interprocess_Pool;
                Timeout : Duration
             )  is
   begin
      Pool.Timeout := Timeout;
   end Set_Timeout;

   function Storage_Size
            (  Pool : Interprocess_Pool
            )  return Storage_Count is
   begin
      return Pool.Size;
   end Storage_Size;

   function To_Address
            (  Pool    : Interprocess_Pool;
               Pointer : Reference
            )  return System.Address is
   begin
      Check (Pool.Data.all, Pointer);
      return Pool.Data.Core (Pointer)'Address;
   exception
      when Constraint_Error =>
         if Pool.Data = null then
            Raise_Exception (Status_Error'Identity, Pool_Init);
         else
            raise;
         end if;
   end To_Address;

   function To_Reference
            (  Pool    : Interprocess_Pool;
               Address : System.Address
            )  return Reference is
   begin
      return To_Reference (Pool.Data.all, Address);
   exception
      when Constraint_Error =>
         Raise_Exception (Status_Error'Identity, Pool_Init);
   end To_Reference;

   function To_Reference
            (  Data    : Pool_Data;
               Address : System.Address
            )  return Reference is
      Result : Reference;
   begin
      if (  Address < Data.Core (2)'Address
         or else
            Address > Data.Core (Data.Size - 1)'Address
         )
      then
         Raise_Exception
         (  Storage_Error'Identity,
            "The address is outside the pool bounds"
         );
      end if;
      Result := To_Reference (Address - Data.Core (1)'Address) + 1;
      Check (Data, Result);
      return Result;
   end To_Reference;

   function To_Reference (Count : Storage_Offset) return Reference is
      Size : constant Storage_Offset :=
                      Reference'Max_Size_In_Storage_Elements;
   begin
      return Reference ((Count + Size - 1) / Size);
   end To_Reference;

end Synchronization.Interprocess.Memory_Pools;

