--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Persistent.Memory_Pools                    Luebeck            --
--  Implementation                                 Winter, 2014       --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

with Ada.Unchecked_Deallocation;

package body Persistent.Memory_Pools is
--  --
--  -- Dump_List -- Debugging dump of a list of free blocks
--  --
--  --    Pool   - The pool
--  --    List   - The list head
--  --    Prefix - The prefix to decorate the output
--  --
--     procedure Dump_List
--               (  Pool   : Persistent_Pool;
--                  List   : Byte_Index;
--                  Prefix : String
--               )  is
--        This  : Byte_Index := List;
--        Count : Natural    := 0;
--     begin
--        Ada.Text_IO.Put_Line
--        (  Prefix & "List at" & Byte_Index'Image (List)
--        );
--        loop
--           declare
--              Block    : Block_Type renames Load (Pool.File, This).all;
--              Offset   : Block_Offset := Get_Offset (This);
--              Previous : Byte_Index;
--              Next     : Byte_Index;
--           begin
--              Previous := Get (Block, Offset);
--              Next     := Get (Block, Offset + 8);
--              Ada.Text_IO.Put_Line
--              (  Prefix
--              &  " "
--              &  Byte_Index'Image (Previous)
--              &  " -->"
--              &  Byte_Index'Image (This)
--              &  " -->"
--              &  Byte_Index'Image (Next)
--              );
--              Count := Count + 1;
--              if Previous = Next and Count > 2 then
--                 Ada.Text_IO.Put_Line (Prefix & " BROKEN!");
--              end if;
--              exit when Next = List;
--              if Next = This then
--                 Ada.Text_IO.Put_Line (Prefix & " BROKEN!");
--              end if;
--              This := Next;
--           end;
--        end loop;
--        Ada.Text_IO.Put_Line
--        (  Prefix & "End of list at" & Byte_Index'Image (List)
--        );
--     end Dump_List;

   protected body Persistent_Mutex is
      function Get_Blocks_Free return Byte_Index is
      begin
         return Pool.Blocks_Free;
      end Get_Blocks_Free;

      function Get_Blocks_Used return Byte_Index is
      begin
         return Pool.Blocks_Used;
      end Get_Blocks_Used;

      function Get_Bytes_Free return Byte_Index is
      begin
         return Pool.Bytes_Free;
      end Get_Bytes_Free;

      function Get_Bytes_Used return Byte_Index is
      begin
         return Pool.Bytes_Used;
      end Get_Bytes_Used;

      function Get_Root_Index (Index : Root_Index) return Byte_Index is
      begin
         return Pool.Root (Index);
      end Get_Root_Index;

      procedure Release is
      begin
         Locked := False;
      end Release;

      entry Seize when not Locked is
      begin
         Locked := True;
      end Seize;

      procedure Set_Root_Index
                (  Index : Root_Index;
                   Value : Byte_Index
                )  is
      begin
         Pool.Root (Index) := Value;
      end Set_Root_Index;
   end Persistent_Mutex;

   function Get
            (  Block  : Byte_Array;
               Offset : Block_Offset
            )  return Unsigned_16 is
      pragma Inline (Get);
   begin
      return
      (              Unsigned_16 (Block (Offset    ))
      or Shift_Left (Unsigned_16 (Block (Offset + 1)), 8)
      );
   end Get;

   function Get
            (  Block  : Byte_Array;
               Offset : Block_Offset
            )  return Unsigned_64 is
      pragma Inline (Get);
   begin
      return
      (              Unsigned_64 (Block (Offset    ))
      or Shift_Left (Unsigned_64 (Block (Offset + 1)),  8)
      or Shift_Left (Unsigned_64 (Block (Offset + 2)), 16)
      or Shift_Left (Unsigned_64 (Block (Offset + 3)), 24)
      or Shift_Left (Unsigned_64 (Block (Offset + 4)), 32)
      or Shift_Left (Unsigned_64 (Block (Offset + 5)), 40)
      or Shift_Left (Unsigned_64 (Block (Offset + 6)), 48)
      or Shift_Left (Unsigned_64 (Block (Offset + 7)), 52)
      );
   end Get;

   function Get
            (  Block  : Block_Type;
               Offset : Block_Offset
            )  return Block_Count is
      pragma Inline (Get);
   begin
      return Block_Count (Unsigned_64'(Get (Block, Offset)));
   end Get;

   procedure Put
             (  Block  : in out Byte_Array;
                Offset : Block_Offset;
                Value  : Unsigned_16
             )  is
      pragma Inline (Put);
   begin
      Block (Offset..Offset + 1) :=
         (  1 => Unsigned_8 (16#FF# and Value),
            2 => Unsigned_8 (16#FF# and Shift_Right (Value, 8))
         );
   end Put;

   procedure Put
             (  Block  : in out Byte_Array;
                Offset : Block_Offset;
                Value  : Unsigned_64
             )  is
      pragma Inline (Put);
   begin
      Block (Offset..Offset + 7) :=
         (  1 => Unsigned_8 (16#FF# and Value),
            2 => Unsigned_8 (16#FF# and Shift_Right (Value, 8)),
            3 => Unsigned_8 (16#FF# and Shift_Right (Value, 16)),
            4 => Unsigned_8 (16#FF# and Shift_Right (Value, 24)),
            5 => Unsigned_8 (16#FF# and Shift_Right (Value, 32)),
            6 => Unsigned_8 (16#FF# and Shift_Right (Value, 40)),
            7 => Unsigned_8 (16#FF# and Shift_Right (Value, 48)),
            8 => Unsigned_8 (16#FF# and Shift_Right (Value, 52))
         );
   end Put;

   procedure Put
             (  Block  : in out Byte_Array;
                Offset : Block_Offset;
                Value  : Byte_Index
             )  is
      pragma Inline (Put);
   begin
      Put (Block, Offset, Unsigned_64 (Value));
   end Put;

   procedure Mark
             (  Block  : in out Block_Type;
                Offset : Block_Offset;
                Size   : Unsigned_16;
                Free   : Boolean
             )  is
      Margin : Unsigned_16 := Size / Min_Size;
   begin
      if not Free then
         Margin := Margin or 16#8000#;
      end if;
      Put (Block, Offset - 2, Margin);
      Put (Block, Offset + Block_Offset (Size - 4), Margin);
   end Mark;

   procedure Mark_Chained
             (  Block  : in out Block_Type;
                Offset : Block_Offset;
                Size   : Unsigned_16
             )  is
      Byte : constant Unsigned_8 := Block (Offset - 1) or 16#40#;
   begin
      Block (Offset - 1) := Byte;
      Block (Offset + Block_Offset (Size - 3)) := Byte;
   end Mark_Chained;

   procedure Mark_Unchained
             (  Block  : in out Block_Type;
                Offset : Block_Offset;
                Size   : Unsigned_16
             )  is
      Byte : constant Unsigned_8 := Block (Offset - 1) and not 16#40#;
   begin
      Block (Offset - 1) := Byte;
      Block (Offset + Block_Offset (Size - 3)) := Byte;
   end Mark_Unchained;
--
-- Mark_Used -- Mark block used in its margins
--
--    Block  - The file block
--    Offset - To the first used byte of the block
--    Size   - The block size (including margins)
--
   procedure Mark_Used
             (  Block  : in out Block_Type;
                Offset : Block_Offset;
                Size   : Unsigned_16
             )  is
      pragma Inline (Mark_Used);
      Byte : constant Unsigned_8 := Block (Offset - 1) or 16#80#;
   begin
      Block (Offset - 1) := Byte;
      Block (Offset + Block_Offset (Size - 3)) := Byte;
   end Mark_Used;

   procedure Add
             (  Pool  : in out Persistent_Pool;
                Index : Byte_Index;
                Size  : Unsigned_16
             )  is
      Block  : Block_Type_Ptr := Get (Pool.File, Index);
      Offset : Block_Offset   := Get_Offset (Index);
      Space  : Unsigned_16    := Size;
   begin
      if Index > First_Block and then Offset > 2 then
         -- Checking the block before this one
         if Is_Free (Block.all, Offset - 2) then -- Merge these two
            declare
               Free : constant Unsigned_16 :=
                      Get_Size (Block.all, Offset - 2);
            begin
               Offset := Offset - Block_Offset (Free);
               Space  := Space  + Free;
               Fetch (Pool, Block.all, Free, Offset);
               Block  := Get (Pool.File, Index);
               Pool.Bytes_Free := Pool.Bytes_Free + 8;
            end;
         end if;
      end if;
      if Unsigned_16 (Offset) + Space < Block_Byte_Size then
         -- Checking the block after this one
         declare
            Next : constant Block_Offset :=
                   Offset + Block_Offset (Space);
         begin
            if Is_Free (Block.all, Next) then
               declare
                  Free : constant Unsigned_16 :=
                         Get_Size (Block.all, Next);
               begin
                  Fetch (Pool, Block.all, Free, Next);
                  Block := Get (Pool.File, Index);
                  Space := Space + Free;
                  Pool.Bytes_Free := Pool.Bytes_Free + 8;
               end;
            end if;
         end;
      end if;
      Mark (Block.all, Offset, Space, True); -- Set the margins up
      Insert
      (  Pool,
         Pool.Free (Block_Size_Index (Space / Min_Size)),
         Get_First (Index) or Byte_Index (Offset)
      );
      Pool.Bytes_Free  := Pool.Bytes_Free  + Byte_Index (Space - 4);
      Pool.Blocks_Free := Pool.Blocks_Free + 1;
   end Add;

   function Allocate
            (  Pool : Persistent_Pool;
               Size : Byte_Count
            )  return Byte_Index is
      Lock : Holder (Pool.Mutex.Pool);
   begin
      return Unchecked_Allocate (Pool, Size);
   end Allocate;

   procedure Close (Pool : in out Persistent_Pool) is
      Lock : Holder (Pool'Unchecked_Access);
   begin
      Unchecked_Close (Pool);
   end Close;

   procedure Commit (Container : in out Persistent_Pool) is
      Lock : Holder (Container'Unchecked_Access);
   begin
      Unchecked_Flush (Container);
      Commit (Container.File.all);
   end Commit;

   procedure Deallocate
             (  Pool  : in out Persistent_Pool;
                Index : Byte_Index
             )  is
      Lock : Holder (Pool'Unchecked_Access);
   begin
      Unchecked_Deallocate (Pool, Index);
   end Deallocate;

   function Expand
            (  Pool  : Persistent_Pool;
               Index : Byte_Index
            )  return Byte_Count is
      Lock : Holder (Pool.Mutex.Pool);
   begin
      return Unchecked_Expand (Pool, Index);
   end Expand;

   procedure Finalize (Pool : in out Persistent_Pool) is
      Lock : Holder (Pool'Unchecked_Access);
   begin
      Unchecked_Close (Pool);
   end Finalize;

   procedure Finalize (Object : in out Holder) is
   begin
      Object.Pool.Mutex.Release;
   end Finalize;

   procedure Flush (Pool : in out Persistent_Pool) is
      Lock : Holder (Pool'Unchecked_Access);
   begin
      Unchecked_Flush (Pool);
   end Flush;

   procedure Fetch
             (  Pool   : in out Persistent_Pool;
                Block  : in out Block_Type;
                Size   : Unsigned_16;
                Offset : Block_Offset
             )  is
   begin
      Remove
      (  Pool,
         Block,
         Pool.Free (Block_Size_Index (Size / Min_Size)),
         Offset
      );
      Pool.Bytes_Free  := Pool.Bytes_Free  - Byte_Index (Size) - 4;
      Pool.Blocks_Free := Pool.Blocks_Free - 1;
   end Fetch;

   function Fetch
            (  Pool : Persistent_Pool;
               Size : Byte_Count
            )  return Byte_Index is
      Lock : Holder (Pool.Mutex.Pool);
   begin
      return Unchecked_Fetch (Pool, Size);
   end Fetch;

   function Get
            (  Block  : Byte_Array;
               Offset : Block_Offset
            )  return Byte_Index is
   begin
      return Byte_Index (Unsigned_64'(Get (Block, Offset)));
   end Get;

   function Get_Block_Size
            (  Pool  : Persistent_Pool;
               Index : Byte_Index
            )  return Byte_Count is
      Lock : Holder (Pool.Mutex.Pool);
   begin
      return Byte_Count
             (  Get_Size
                (  Load (Pool.File, Index).all,
                   Get_Offset (Index)
             )  );
   end Get_Block_Size;

   function Get_Blocks_Free (Pool : Persistent_Pool)
      return Block_Count is
   begin
      return Block_Count (Pool.Blocks_Free);
   end Get_Blocks_Free;

   function Get_Blocks_Used (Pool : Persistent_Pool)
      return Block_Count is
   begin
      return Block_Count (Pool.Blocks_Used);
   end Get_Blocks_Used;

   function Get_Bytes_Free (Pool : Persistent_Pool)
      return Byte_Index is
   begin
      return Pool.Bytes_Free;
   end Get_Bytes_Free;

   function Get_Bytes_Used (Pool : Persistent_Pool)
      return Byte_Index is
   begin
      return Pool.Bytes_Used;
   end Get_Bytes_Used;

   function Get_Name (Pool : Persistent_Pool) return String is
      Lock : Holder (Pool.Mutex.Pool);
   begin
      return Get_Name (Pool.File.all);
   end Get_Name;

   function Get_Root_Index
            (  Pool  : Persistent_Pool;
               Index : Root_Index
            )  return Byte_Index is
   begin
      return Pool.Mutex.Get_Root_Index (Index);
   end Get_Root_Index;

   function Get_Size
            (  Block  : Block_Type;
               Offset : Block_Offset
            )  return Unsigned_16 is
      Margin : constant Unsigned_16 := Get (Block, Offset - 2);
   begin
      return (Margin and 16#3FFF#) * Min_Size;
   exception
      when Error : Constraint_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            "Broken block margin at" & Block_Offset'Image (Offset)
         );
   end Get_Size;

   function Get_Size
            (  Pool  : Persistent_Pool;
               Index : Byte_Index
            )  return Byte_Count is
      Lock   : Holder (Pool.Mutex.Pool);
      Block  : Block_Type renames Load (Pool.File, Index).all;
      Margin : constant Unsigned_16 :=
               Get (Block, Get_Offset (Index) - 2);
   begin
      if 0 = (Margin and 16#4000#) then
         return Byte_Count ((Margin and 16#3FFF#) * Min_Size - 4);
      else
         return Byte_Count ((Margin and 16#3FFF#) * Min_Size - 12);
      end if;
   exception
      when Error : Constraint_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            (  "Broken block margin at"
            &  Block_Offset'Image (Get_Offset (Index))
         )  );
   end Get_Size;

   function Get_Space (Pool : Persistent_Pool) return Byte_Index is
      Lock : Holder (Pool.Mutex.Pool);
   begin
      return Get_Size (Pool.File.all) - Head_Size;
   end Get_Space;

   procedure Initialize (Object : in out Holder) is
   begin
      Object.Pool.Mutex.Seize;
   end Initialize;

   procedure Initialize (Pool : in out Persistent_Pool) is
      Lock : Holder (Pool'Unchecked_Access);
   begin
      if not Is_Open (Pool.File.all) then
         Raise_Exception (Program_Error'Identity, "No file open");
      end if;
      if Get_Size (Pool.File.all) < Block_Byte_Size then
         declare
            Block : Block_Type renames Get (Pool.File, 0).all;
         begin
            Pool.Bytes_Used  := 0;
            Pool.Bytes_Free  := 0;
            Pool.Blocks_Used := 0;
            Pool.Blocks_Free := 0;
            Pool.Free        := (others => 0);
            Pool.Root        := (others => 0);
            Add (Pool, First_Block, Tail_Size);
         end;
      else
         declare
            Block  : Block_Type renames Load (Pool.File, 0).all;
            Offset : Block_Offset := 40;
         begin
            Pool.Bytes_Used  := Get (Block,  0);
            Pool.Bytes_Free  := Get (Block,  8);
            Pool.Blocks_Used := Get (Block, 16);
            Pool.Blocks_Free := Get (Block, 32);
            for Index in Pool.Free'Range loop
               Pool.Free (Index) := Get (Block, Offset);
               Offset := Offset + 8;
            end loop;
            for Index in Pool.Root'Range loop
               Pool.Root (Index) := Get (Block, Offset);
               Offset := Offset + 8;
            end loop;
         end;
      end if;
   end Initialize;

   procedure Insert
             (  Pool  : in out Persistent_Pool;
                List  : in out Byte_Index;
                Index : Byte_Index
             )  is
   begin
      if List = 0 then
         declare
            Block  : Block_Type renames Update (Pool.File, Index).all;
            Offset : constant Block_Offset := Get_Offset (Index);
         begin
            List := Index;
            Put (Block, Offset,     Index);
            Put (Block, Offset + 8, Index);
         end;
      else
         declare
            Previous : Byte_Index;
            Next     : Byte_Index;
         begin
            declare
               First_Block : Block_Type renames
                             Update (Pool.File, List).all;
               Offset : constant Block_Offset := Get_Offset (List);
            begin
               Previous := Get (First_Block, Offset);
               Put (First_Block, Offset, Index);
            end;
            declare
               Last_Block : Block_Type renames
                            Update (Pool.File, Previous).all;
               Offset : constant Block_Offset :=
                        Get_Offset (Previous) + 8;
            begin
               Next := Get (Last_Block, Offset);
               Put (Last_Block, Offset, Index);
            end;
            declare
               Block  : Block_Type renames
                        Update (Pool.File, Index).all;
               Offset : constant Block_Offset :=
                        Get_Offset (Index);
            begin
               Put (Block, Offset,     Previous);
               Put (Block, Offset + 8, Next);
            end;
         end;
      end if;
   end Insert;

   function Is_Chained
            (  Block  : Block_Type;
               Offset : Block_Offset
            )  return Boolean is
   begin
      return 0 /= (Block (Offset - 1) and 16#40#);
   end Is_Chained;

   function Is_Free
            (  Block  : Block_Type;
               Offset : Block_Offset
            )  return Boolean is
   begin
      return 0 = (Block (Offset - 1) and 16#80#);
   end Is_Free;

   function Is_Open (Pool : Persistent_Pool) return Boolean is
      Lock : Holder (Pool.Mutex.Pool);
   begin
      return Is_Open (Pool.File.all);
   end Is_Open;

   procedure Set_Root_Index
             (  Pool  : in out Persistent_Pool;
                Index : Root_Index;
                Value : Byte_Index
             )  is
   begin
      Pool.Mutex.Set_Root_Index (Index, Value);
   end Set_Root_Index;

   procedure Remove
             (  Pool   : in out Persistent_Pool;
                Block  : in out Block_Type;
                List   : in out Byte_Index;
                Offset : Block_Offset
             )  is
      Previous : constant Byte_Index := Get (Block, Offset);
      Next     : constant Byte_Index := Get (Block, Offset + 8);
   begin
      if List = 0 then
         Raise_Exception
         (  Data_Error'Identity,
            "Removing block from an empty list"
         );
      end if;
      declare
         Right  : Block_Type renames Update (Pool.File, Next).all;
         Offset : constant Block_Offset := Get_Offset (Next);
      begin
         if List = Get (Right, Offset) then -- Removed is the head
            if List = Next then
               List := 0;
               return;
            end if;
            List := Next;
         end if;
         Put (Right, Offset, Previous);
      end;
      Put
      (  Update (Pool.File, Previous).all,
         Get_Offset (Previous) + 8,
         Next
      );
   end Remove;

   procedure Rollback (Container : in out Persistent_Pool) is
      Lock : Holder (Container'Unchecked_Access);
   begin
      Commit (Container.File.all);
   end Rollback;

   procedure Truncate
             (  Pool  : in out Persistent_Pool;
                Index : Byte_Index;
                Size  : Byte_Count
             )  is
      Lock : Holder (Pool'Unchecked_Access);
   begin
      Unchecked_Truncate (Pool, Index, Size);
   end Truncate;

   function Unchecked_Allocate
            (  Pool : Persistent_Pool;
               Size : Byte_Count
            )  return Byte_Index is
      This   : Persistent_Pool'Class renames Pool.Mutex.Pool.all;
      Result : Byte_Index;
      --
      -- Take -- Get the memory from the specified block
      --
      --    Block  - The memory block
      --    Space  - The block's size
      --    Offset - The block's offset
      --
      procedure Take
                (  Block  : in out Block_Type;
                   Space  : Unsigned_16;
                   Offset : Block_Offset
                )  is
         Free : Unsigned_16 := Space - Unsigned_16 (Size);
         Used : Unsigned_16;
      begin
         Free := Free - Free mod Min_Size;
         if Free < Min_Size then -- Use all block
            Used := Space;
            Mark_Used (Block, Offset, Used);
            Remove
            (  This,
               Block,
               This.Free (Block_Size_Index (Space / Min_Size)),
               Offset
            );
            This.Blocks_Free := This.Blocks_Free - 1;
            This.Bytes_Free  := This.Bytes_Free - Byte_Index (Used - 4);
         else -- Split the block into two
            Used := Space - Free;
            Remove
            (  This,
               Block,
               This.Free (Block_Size_Index (Space / Min_Size)),
               Offset
            );
            declare -- Bring the block back if unloaded
               Block : Block_Type renames
                       Update (This.File, Result).all;
            begin
               Mark (Block, Offset,        Used, False);
               Mark (Block, Offset + Block_Offset (Used), Free, True);
            end;
            Insert
            (  This,
               This.Free (Block_Size_Index (Free / Min_Size)),
               Result + Byte_Index (Used)
            );
            This.Bytes_Free := This.Bytes_Free - Byte_Index (Used);
         end if;
         This.Blocks_Used := This.Blocks_Used + 1;
         This.Bytes_Used  := This.Bytes_Used + Byte_Index (Used - 4);
      end Take;

   begin
      if not Is_Open (This.File.all) then
         Raise_Exception (Use_Error'Identity, "File is not open");
      end if;
      for List in Block_Size_Index
                   (  (Integer (Size) + Min_Size - 1)
                   /  Min_Size
                   )
                .. Block_Size_Index'Last
      loop
         Result := This.Free (List);
         if Result /= 0 then -- There is a free block to take
            declare
               Block  : Block_Type renames
                        Update (This.File, Result).all;
               Offset : constant Block_Offset := Get_Offset (Result);
            begin
               Take (Block, Get_Size (Block, Offset), Offset);
               return Result;
            end;
         end if;
      end loop;
      --
      -- Allocate a new file block
      --
      Result := Get_Size (This.File.all) + 2;
      declare
         Block : Block_Type renames Get (This.File, Result).all;
      begin
         Add (This, Result, Max_Size);
         Take (Block, Max_Size, 2);
         return Result;
      end;
   end Unchecked_Allocate;

   procedure Unchecked_Close (Pool : in out Persistent_Pool) is
   begin
      Unchecked_Flush (Pool);
      Pool.Bytes_Used  := 0;
      Pool.Bytes_Free  := 0;
      Pool.Blocks_Used := 0;
      Pool.Blocks_Free := 0;
      Pool.Free := (others => 0);
      Pool.Root := (others => 0);
--    Close (Pool.File.all);
   end Unchecked_Close;

   procedure Unchecked_Deallocate
             (  Pool  : in out Persistent_Pool;
                Index : Byte_Index
             )  is
      This : Byte_Index := Index;
   begin
      loop
         declare
            Block  : Block_Type renames Update (Pool.File, This).all;
            Offset : constant Block_Offset := Get_Offset (This);
            Margin : constant Unsigned_16  := Get (Block, Offset - 2);
            Size   : constant Unsigned_16  :=
                        (Margin and 16#3FFF#) * Min_Size;
         begin
            if 0 = (Margin and 16#4000#) then
               Add (Pool, This, Size);
               Pool.Blocks_Used := Pool.Blocks_Used - 1;
               Pool.Bytes_Used :=
                  Pool.Bytes_Used - Byte_Index (Size - 4);
               return;
            else
               declare -- Next block in the chain
                  Next : constant Byte_Index := Get (Block, Offset);
               begin
                  Add (Pool, This, Size);
                  Pool.Blocks_Used := Pool.Blocks_Used - 1;
                  Pool.Bytes_Used :=
                     Pool.Bytes_Used - Byte_Index (Size - 4);
                  This := Next;
               end;
            end if;
         exception
            when Error : Constraint_Error =>
               Raise_Exception
               (  Data_Error'Identity,
                  "Broken block margin at" & Block_Offset'Image (Offset)
               );
         end;
      end loop;
   end Unchecked_Deallocate;

   function Unchecked_Expand
            (  Pool  : Persistent_Pool;
               Index : Byte_Index
            )  return Byte_Count is
      This   : Persistent_Pool'Class renames Pool.Mutex.Pool.all;
      Block  : Block_Type renames Update (This.File, Index).all;
      Offset : constant Block_Offset := Get_Offset (Index);
      Size   : constant Unsigned_16  := Get_Size (Block, Offset);
   begin
      if Unsigned_16 (Offset) + Size < Block_Byte_Size then
         declare
            Next : constant Block_Offset :=
                   Offset + Block_Offset (Size);
         begin
            if Is_Free (Block, Next) then
               declare
                  Free : constant Unsigned_16 := Get_Size (Block, Next);
               begin
                  Remove
                  (  This,
                     Block,
                     This.Free (Block_Size_Index (Free / Min_Size)),
                     Next
                  );
                  Mark
                  (  Update (This.File, Index).all,
                     Offset,
                     Size + Free,
                     False
                  );
                  This.Blocks_Free := This.Blocks_Free - 1;
                  This.Bytes_Free :=
                     This.Bytes_Free - Byte_Index (Free - 4);
                  This.Bytes_Used :=
                     This.Bytes_Used + Byte_Index (Free);
                  return Byte_Count (Free);
               end;
            end if;
         end;
      end if;
      return 0;
   end Unchecked_Expand;

   function Unchecked_Fetch
            (  Pool : Persistent_Pool;
               Size : Byte_Count
            )  return Byte_Index is
      This : Persistent_Pool'Class renames Pool.Mutex.Pool.all;
      --
      -- Take -- Get the memory from the specified block
      --
      --    Block  - The memory block
      --    List   - Free block's list
      --    Offset - The block's offset
      --
      procedure Take
                (  Block  : in out Block_Type;
                   List   : in out Byte_Index;
                   Offset : Block_Offset
                )  is
         pragma Inline (Take);
         Size : constant Unsigned_16 := Get_Size (Block, Offset);
      begin
         Mark_Used (Block, Offset, Size);
         Remove (This, Block, List, Offset);
         This.Blocks_Free := This.Blocks_Free - 1;
         This.Bytes_Free  := This.Bytes_Free - Byte_Index (Size - 4);
         This.Blocks_Used := This.Blocks_Used + 1;
         This.Bytes_Used  := This.Bytes_Used + Byte_Index (Size - 4);
      end Take;

      Result : Byte_Index;
   begin
      if not Is_Open (This.File.all) then
         Raise_Exception (Use_Error'Identity, "File is not open");
      end if;
      for List in reverse Block_Size_Index
                          (  (Integer (Size) + Min_Size - 1)
                          /  Min_Size
                          )
                       .. Block_Size_Index'Last
      loop
         Result := This.Free (List);
         if Result /= 0 then -- There is a free block to take
            declare
               Block : Block_Type renames
                       Update (This.File, Result).all;
            begin
               Take (Block, This.Free (List), Get_Offset (Result));
               return Result;
            end;
         end if;
      end loop;
      --
      -- Allocate a new file block
      --
      Result := Get_Size (This.File.all) + 2;
      declare
         Block : Block_Type renames Get (This.File, Result).all;
      begin
         Add (This, Result, Max_Size);
         Take (Block, This.Free (This.Free'Last), 2);
      end;
      return Result;
   end Unchecked_Fetch;

   procedure Unchecked_Flush (Pool : in out Persistent_Pool) is
      File : Persistent_Array'Class renames Pool.File.all;
   begin
      if Is_Open (File) and then Is_Writable (File) then
         declare
            Block  : Block_Type renames Get (Pool.File, 0).all;
            Offset : Block_Offset := 40;
         begin
            Put (Block,  0, Pool.Bytes_Used);
            Put (Block,  8, Pool.Bytes_Free);
            Put (Block, 16, Pool.Blocks_Used);
            Put (Block, 32, Pool.Blocks_Free);
            for Index in Pool.Free'Range loop
               Put (Block, Offset, Pool.Free (Index));
               Offset := Offset + 8;
            end loop;
            for Index in Pool.Root'Range loop
               Put (Block, Offset, Pool.Root (Index));
               Offset := Offset + 8;
            end loop;
         end;
         Flush (File);
      end if;
   end Unchecked_Flush;

   procedure Unchecked_Truncate
             (  Pool  : in out Persistent_Pool;
                Index : Byte_Index;
                Size  : Byte_Count
             )  is
      Block  : Block_Type renames Update (Pool.File, Index).all;
      Offset : constant Block_Offset := Get_Offset (Index);
      Space  : constant Unsigned_16  := Get_Size (Block, Offset) - 4;
      Next   : Byte_Index   := 0;
      Free   : Unsigned_16;
      Used   : Unsigned_16;
   begin
      if Space <= Unsigned_16 (Size) then
         return;
      end if;
      Free := Space - Unsigned_16 (Size);
      if Is_Chained (Block, Offset) then
         if Free < Min_Size + 8 then
            return;
         end if;
         Next := Get (Block, Offset);
         Free := ((Free - 8) / Min_Size) * Min_Size;
      else
         if Free <= Min_Size then
            return;
         end if;
         Free := (Free / Min_Size) * Min_Size;
      end if;
      Used := Space - Free + 4; -- Including margins
      Mark (Block, Offset, Used, False);
      Pool.Bytes_Used := Pool.Bytes_Used - Byte_Index (Free);
      Add (Pool, Index + Byte_Index (Used), Free);
      if Next /= 0 then
         Unchecked_Deallocate (Pool, Next);
      end if;
   exception
      when Error : Constraint_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            (  "Broken block margin at" & Block_Offset'Image (Offset)
         )  );
   end Unchecked_Truncate;

end Persistent.Memory_Pools;
