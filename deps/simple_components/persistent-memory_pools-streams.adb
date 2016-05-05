--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Persistent.Memory_Pools.                   Luebeck            --
--         Streams                                 Winter, 2014       --
--  Implementation                                                    --
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

package body Persistent.Memory_Pools.Streams is
--
--   ._Block___________________________________________________________.
--   |    |    |    |    |    |    |////////////////////|    |    |    |
--   | Margin  |   Next / Length   |////////////////////|    | Margin  |
--   |____|____|____|____|____|____|////////////////////|____|____|____|
--   |         |                   |                    |    |         |
--   |    Offset          Offset + 8  Offset + 8 + Length    |         |
--   |                                       Offset + Size - 4         |
--   |<------------------------ Size --------------------------------->|
--
--   Length <= Size - 8 + 4
--
   function Image (Data : Byte_Array) return String is
      Result : String (1..Data'Length);
      Index  : Integer := Result'First;
   begin
      for Element in Data'Range loop
         Result (Index) := Character'Val (Data (Element));
         Index := Index + 1;
      end loop;
      return Result;
   end Image;

   procedure Look_Ahead
             (  Stream    : Input_Stream'Class;
                Visit     : Visitor_Type;
                User_Data : in out User_Data_Type
             )  is
      Lock     : Holder (Stream.Pool);
      Offset   : Block_Offset := Stream.Offset;
      Current  : Byte_Index   := Stream.Current;
      Size     : Unsigned_16;
      Continue : Boolean;
   begin
      if Current < Head_Size then
         Raise_Exception (Use_Error'Identity, "Stream is not open");
      end if;
      declare
         Block : Block_Type renames Load (Stream.Pool.File, Current).all;
      begin
         Visit (Block (Offset..Stream.Next - 1), User_Data, Continue);
         if not Continue then
            return;
         end if;
         Offset := Get_Offset (Current);
         if not Is_Chained (Block, Offset) then
            return;
         end if;
      end;
      loop
         declare
            Block : Block_Type renames
                    Load (Stream.Pool.File, Current).all;
         begin
            Current := Get (Block, Offset);
            Offset  := Get_Offset (Current);
            Size    := Get_Size (Block, Offset);
            Current := Get (Block, Offset);
            if Is_Chained (Block, Offset) then
               Visit
               (  Block
                  (  Offset + 8
                  .. Offset + Byte_Count (Size - 4) - 1
                  ),
                  User_Data, Continue
               );
               if Current > Byte_Index (Size - (8 + 4)) then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "Wrong last block count of used bytes (Look_Ahead)"
                  );
               end if;
            else
               Visit
               (  Block
                  (  Offset + 8
                  .. Offset + 8 + Byte_Count (Current) - 1
                  ),
                  User_Data, Continue
               );
               return;
            end if;
         end;
      end loop;
   end Look_Ahead;

   procedure Append
             (  Stream : in out Output_Stream;
                Index  : Byte_Index
             )  is
      Lock : Holder (Stream.Pool);
   begin
      Append (Unchecked_Output_Stream (Stream), Index);
   end Append;

   procedure Append
             (  Stream : in out Unchecked_Output_Stream;
                Index  : Byte_Index
             )  is
   begin
      Stream.Current := Index;
      loop
         if Stream.Current < Head_Size then
            Raise_Exception (Use_Error'Identity, "Wrong block index");
         end if;
         declare
            Block  : Block_Type renames
                        Load (Stream.Pool.File, Stream.Current).all;
            Offset : constant Block_Offset :=
                     Get_Offset (Stream.Current);
            Size   : constant Unsigned_16 :=
                     Get_Size (Block, Offset);
         begin
            if Size <= 4 + 8 then
               Raise_Exception
               (  Data_Error'Identity,
                  "Wrong memory block size (Append)"
               );
            end if;
            if not Is_Chained (Block, Offset) then
               declare
                  Length : constant Byte_Index := Get (Block, Offset);
               begin
                  if Length > Byte_Index (Size - (8 + 4)) then
                     Raise_Exception
                     (  Data_Error'Identity,
                        "Wrong last block count of used bytes (Append)"
                     );
                  end if;
                  Stream.Next   := Offset + Byte_Count (Size - 4);
                  Stream.Offset := Offset + 8 + Byte_Count (Length);
                  Stream.Start := Index;
                  return;
               end;
            end if;
            Stream.Current := Get (Block, Offset);
         end;
      end loop;
   end Append;

   procedure Close (Stream : in out Input_Stream) is
      Lock : Holder (Stream.Pool);
   begin
      Stream.Start := 0;
   end Close;

   procedure Close (Stream : in out Unchecked_Input_Stream) is
   begin
      Stream.Start := 0;
   end Close;

   procedure Close (Stream : in out Output_Stream) is
      Lock : Holder (Stream.Pool);
   begin
      Stream.Start := 0;
   end Close;

   procedure Close (Stream : in out Unchecked_Output_Stream) is
   begin
      Stream.Start := 0;
   end Close;

   function Compare
            (  Left  : Input_Stream;
               Right : Stream_Element_Array
            )  return Precedence is
      This : Stream_Element_Offset := Right'First;

      type On_Data_Ptr is access procedure
           (  Contents : Byte_Array;
              Result   : in out Precedence;
              Continue : out Boolean
           );
      procedure On_Data
                (  Contents : Byte_Array;
                   Result   : in out Precedence;
                   Continue : out Boolean
                )  is
      begin
         for Index in Contents'Range loop
            if This > Right'Last then
               Result   := Greater;
               Continue := False;
               return;
            end if;
            declare
               L : constant Unsigned_8 := Contents (Index);
               R : constant Unsigned_8 :=
                   Stream_Element'Pos (Right (This));
            begin
               if L /= R then
                  if L < R then
                     Result := Less;
                  else
                     Result := Greater;
                  end if;
                  Continue := False;
                  return;
               end if;
               This := This + 1;
            end;
         end loop;
         Continue := True;
      end On_Data;

      procedure Do_Compare is new Look_Ahead (Precedence, On_Data_Ptr);

      Result : Precedence := Equal;
   begin
      if Left.Start < Head_Size then
         if Right'Length > 0 then
            return Less;
         else
            return Equal;
         end if;
      end if;
      Do_Compare (Left, On_Data'Access, Result);
      return Result;
   end Compare;

   function Compare
            (  Left  : Input_Stream;
               Right : String
            )  return Precedence is
      Pointer : Integer := Right'First;

      type On_Data_Ptr is access procedure
           (  Contents : Byte_Array;
              Result   : in out Precedence;
              Continue : out Boolean
           );
      procedure On_Data
                (  Contents : Byte_Array;
                   Result   : in out Precedence;
                   Continue : out Boolean
                )  is
      begin
         for Index in Contents'Range loop
            if Pointer > Right'Last then
               Result   := Greater;
               Continue := False;
               return;
            end if;
            declare
               L : constant Unsigned_8 := Contents (Index);
               R : constant Unsigned_8 :=
                   Character'Pos (Right (Pointer));
            begin
               if L /= R then
                  if L < R then
                     Result := Less;
                  else
                     Result := Greater;
                  end if;
                  Continue := False;
                  return;
               end if;
               Pointer := Pointer + 1;
            end;
         end loop;
         Continue := True;
      end On_Data;

      procedure Do_Compare is new Look_Ahead (Precedence, On_Data_Ptr);

      Result : Precedence := Equal;
   begin
      if Left.Start < Head_Size then
         if Right'Length > 0 then
            return Less;
         else
            return Equal;
         end if;
      end if;
      Do_Compare (Left, On_Data'Access, Result);
      return Result;
   end Compare;

   function Count
            (  Stream : Unchecked_Input_Stream;
               Index  : Byte_Index;
               Length : Block_Offset
            )  return Stream_Element_Count is
      Count   : Stream_Element_Offset;
      Current : Byte_Index := Index;
   begin
      Count := -Stream_Element_Offset (Length);
      loop
         declare
            Block  : Block_Type renames
                     Load (Stream.Pool.File, Current).all;
            Offset : constant Block_Offset := Get_Offset (Current);
            Size   : constant Unsigned_16  :=
                     Get_Size (Block, Offset);
         begin
            Current := Get (Block, Offset);
            exit when not Is_Chained (Block, Offset);
            Count := Count + Stream_Element_Offset (Size - (4 + 8));
         end;
      end loop;
      return Count + Stream_Element_Count (Current);
   end Count;

   function Count
            (  Stream : Unchecked_Output_Stream;
               Index  : Byte_Index;
               Next   : Block_Offset
            )  return Stream_Element_Count is
      Count   : Stream_Element_Offset := 0;
      Current : Byte_Index := Stream.Start;
   begin
      while Current /= Stream.Current loop
         declare
            Block  : Block_Type renames
                     Load (Stream.Pool.File, Current).all;
            Offset : constant Block_Offset := Get_Offset (Current);
            Size   : constant Unsigned_16  := Get_Size (Block, Offset);
         begin
            Current := Get (Block, Offset);
            if not Is_Chained (Block, Offset) then
               Raise_Exception
               (  Use_Error'Identity,
                  "Data error, blocks chain ends prematurely"
               );
            end if;
            Count := Count + Stream_Element_Offset (Size - (4 + 8));
         end;
      end loop;
      declare
         Offset : constant Block_Offset := Get_Offset (Current) + 8;
      begin
         if Offset > Next then
            Raise_Exception
            (  Use_Error'Identity,
                  "Data error, wrong block offset"
            );
         end if;
         return Count + Stream_Element_Count (Next - Offset);
      end;
   end Count;

   function End_Of (Stream : Input_Stream) return Boolean is
      Lock : Holder (Stream.Pool);
   begin
      return End_Of (Unchecked_Input_Stream (Stream));
   end End_Of;

   function End_Of (Stream : Unchecked_Input_Stream) return Boolean is
   begin
      if Stream.Start = 0 then
         return True;
      elsif Stream.Next /= Stream.Offset then
         return False;
      else
         declare
            Block  : Block_Type renames
                     Load (Stream.Pool.File, Stream.Current).all;
            Offset : constant Block_Offset :=
                     Get_Offset (Stream.Current);
         begin
            return not Is_Chained (Block, Offset);
         end;
      end if;
   end End_Of;

   function Equal
            (  Stream : Input_Stream;
               Item   : Stream_Element_Array
            )  return Boolean is
   begin
      return Compare (Stream, Item) = Equal;
   end Equal;

   function Equal
            (  Stream : Input_Stream;
               Text   : String
            )  return Boolean is
   begin
      return Compare (Stream, Text) = Equal;
   end Equal;

   function Get_First (Stream : Input_Stream) return Byte_Index is
      Lock : Holder (Stream.Pool);
   begin
      return Get_First (Unchecked_Input_Stream (Stream));
   end Get_First;

   function Get_First
            (  Stream : Unchecked_Input_Stream
            )  return Byte_Index is
   begin
      if Stream.Start = 0 then
         Raise_Exception
         (  Use_Error'Identity,
            "Input stream is not yet open"
         );
      else
         return Stream.Start;
      end if;
   end Get_First;

   function Get_First (Stream : Output_Stream) return Byte_Index is
      Lock : Holder (Stream.Pool);
   begin
      return Get_First (Unchecked_Output_Stream (Stream));
   end Get_First;

   function Get_First
            (  Stream : Unchecked_Output_Stream
            )  return Byte_Index is
   begin
      if Stream.Start = 0 then
         Raise_Exception
         (  Use_Error'Identity,
            "Output stream is not yet open or written"
         );
      else
         return Stream.Start;
      end if;
   end Get_First;

   function Get_Length
            (  Stream : Input_Stream
            )  return Stream_Element_Count is
      Lock : Holder (Stream.Pool);
   begin
      return Get_Length (Unchecked_Input_Stream (Stream));
   end Get_Length;

   function Get_Length
            (  Stream : Unchecked_Input_Stream
            )  return Stream_Element_Count is
   begin
      if Stream.Start < Head_Size then
         return 0;
      else
         return Count (Stream, Stream.Start, 0);
      end if;
   end Get_Length;

   function Get_Unread
            (  Stream : Input_Stream
            )  return Stream_Element_Count is
      Lock : Holder (Stream.Pool);
   begin
      return Get_Unread (Unchecked_Input_Stream (Stream));
   end Get_Unread;

   function Get_Unread
            (  Stream : Unchecked_Input_Stream
            )  return Stream_Element_Count is
   begin
      if Stream.Start < Head_Size then
         return 0;
      else
         declare
            Block  : Block_Type renames
                     Load (Stream.Pool.File, Stream.Current).all;
            Offset : constant Block_Offset :=
                     Get_Offset (Stream.Current);
         begin
            return Count
                   (  Stream,
                      Stream.Current,
                      Stream.Offset - Offset - 8
                   );
         end;
      end if;
   end Get_Unread;

   function Get_Written
            (  Stream : Output_Stream
            )  return Stream_Element_Count is
      Lock : Holder (Stream.Pool);
   begin
      return Get_Written (Unchecked_Output_Stream (Stream));
   end Get_Written;

   function Get_Written
            (  Stream : Unchecked_Output_Stream
            )  return Stream_Element_Count is
   begin
      if Stream.Start < Head_Size then
         return 0;
      else
         return Count (Stream, Stream.Current, Stream.Offset);
      end if;
   end Get_Written;

   function Less
            (  Stream : Input_Stream;
               Text   : String
            )  return Boolean is
   begin
      return Compare (Stream, Text) = Less;
   end Less;

   function Less
            (  Stream : Input_Stream;
               Item   : Stream_Element_Array
            )  return Boolean is
   begin
      return Compare (Stream, Item) = Less;
   end Less;

   procedure Open
             (  Stream : in out Input_Stream;
                Index  : Byte_Index
             )  is
      Lock : Holder (Stream.Pool);
   begin
      Open (Unchecked_Input_Stream (Stream), Index);
   end Open;

   procedure Open
             (  Stream : in out Unchecked_Input_Stream;
                Index  : Byte_Index
             )  is
   begin
      Stream.Start := 0;
      if Index < Head_Size then
         Raise_Exception (Use_Error'Identity, "Wrong block index");
      end if;
      declare
         Block  : Block_Type renames Load (Stream.Pool.File, Index).all;
         Offset : constant Block_Offset := Get_Offset (Index);
         Size   : constant Unsigned_16  := Get_Size (Block, Offset);
      begin
         if Size <= 4 + 8 then
            Raise_Exception
            (  Data_Error'Identity,
               "Wrong memory block size (Open input stream)"
            );
         end if;
         Stream.Offset := Offset + 8;
         if Is_Chained (Block, Offset) then
            Stream.Next := Offset + Byte_Count (Size - 4);
         else
            declare
               Length : constant Byte_Index := Get (Block, Offset);
            begin
               if Length > Byte_Index (Size - (8 + 4)) then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Wrong last block count of used "
                     &  "bytes (Open input stream)"
                  )  );
               end if;
               Stream.Next := Stream.Offset + Byte_Count (Length);
            end;
         end if;
         Stream.Current := Index;
         Stream.Start   := Index;
      end;
   end Open;

   procedure Open
             (  Stream : in out Output_Stream;
                Index  : Byte_Index
             )  is
      Lock : Holder (Stream.Pool);
   begin
      Open (Unchecked_Output_Stream (Stream), Index);
   end Open;

   procedure Open
             (  Stream : in out Unchecked_Output_Stream;
                Index  : Byte_Index
             )  is
   begin
      Stream.Start := 0;
      if Index < Head_Size then
         Raise_Exception (Use_Error'Identity, "Wrong block index");
      end if;
      declare
         Block  : Block_Type renames Load (Stream.Pool.File, Index).all;
         Offset : constant Block_Offset := Get_Offset (Index);
         Size   : constant Unsigned_16  := Get_Size (Block, Offset);
      begin
         if Size <= 4 + 8 then
            Raise_Exception
            (  Data_Error'Identity,
               "Wrong memory block size (Open output stream)"
            );
         end if;
         Stream.Offset  := Offset + 8;
         Stream.Next    := Offset + Byte_Count (Size - 4);
         Stream.Current := Index;
         Stream.Start   := Index;
      end;
   end Open;

   procedure Read
             (  Stream : in out Input_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
      Lock : Holder (Stream.Pool);
   begin
      Read (Unchecked_Input_Stream (Stream), Item, Last);
   end Read;

   procedure Read
             (  Stream : in out Unchecked_Input_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
      procedure Put (Block  : Block_Type; Length : Byte_Count) is
         pragma Inline (Put);
      begin
         for Index in Stream.Offset..Stream.Offset + Length - 1 loop
            Last := Last + 1;
            Item (Last) := Stream_Element'Val (Block (Index));
         end loop;
         Stream.Offset := Stream.Offset + Length;
      end Put;
   begin
      Last := Item'First - 1;
      if Stream.Start > 0 and then Item'Length > 0 then
         loop
            declare
               Space : constant Stream_Element_Offset :=
                       Item'Last - Last;
               Count : constant Byte_Count :=
                       Stream.Next - Stream.Offset;
               Block : Block_Type renames
                       Load (Stream.Pool.File, Stream.Current).all;
            begin
               if Space <= Stream_Element_Count (Count) then
                  Put (Block, Byte_Count (Space));
                  return;
               end if;
               Put (Block, Count);
               if not Is_Chained (Block, Get_Offset (Stream.Current))
               then
                  return;
               end if;
               Stream.Current :=
                  Get (Block, Get_Offset (Stream.Current));
            end;
            declare
               Block  : Block_Type renames
                        Load (Stream.Pool.File, Stream.Current).all;
               Offset : constant Block_Offset :=
                        Get_Offset (Stream.Current);
               Size   : constant Unsigned_16 :=
                        Get_Size (Block, Offset);
            begin
               Stream.Offset := Offset + 8;
               if Is_Chained (Block, Offset) then
                  Stream.Next := Offset + Byte_Count (Size - 4);
--                    Ada.Text_IO.Put_Line
--                    (  "Block: '"
--                    &  Image (Block (Stream.Offset..Stream.Next - 1))
--                    &  "' at"
--                    &  Byte_Index'Image (Stream.Current)
--                    &  ", next at"
--                    &  Byte_Index'Image (Get (Block, Offset))
--                    );
               else
                  declare
                     Length : constant Byte_Index :=
                              Get (Block, Offset);
                  begin
                     if Length > Byte_Index (Size - (8 + 4)) then
                        Raise_Exception
                        (  Data_Error'Identity,
                           "Wrong last block count of used bytes (Read)"
                        );
                     end if;
                     Stream.Next := Stream.Offset + Byte_Count (Length);
--                       Ada.Text_IO.Put_Line
--                       (  "Block: '"
--                       &  Image
--                          (  Block
--                             (  Stream.Offset
--                             .. Stream.Next - 1
--                          )  )
--                       &  "' at"
--                       &  Byte_Index'Image (Stream.Current)
--                       &  ", last"
--                       &  Byte_Index'Image (Length)
--                       );
                  end;
               end if;
            end;
         end loop;
      end if;
   end Read;

   procedure Read
             (  Stream : in out Output_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
   begin
      Raise_Exception
      (  Use_Error'Identity,
         "Output stream cannot be read"
      );
   end Read;

   procedure Read
             (  Stream : in out Unchecked_Output_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
   begin
      Raise_Exception
      (  Use_Error'Identity,
         "Output stream cannot be read"
      );
   end Read;

   procedure Rewind (Stream : in out Input_Stream) is
      Lock : Holder (Stream.Pool);
   begin
      Rewind (Unchecked_Input_Stream (Stream));
   end Rewind;

   procedure Rewind (Stream : in out Unchecked_Input_Stream) is
   begin
      if Stream.Start /= 0 then
         Open (Stream, Stream.Start);
      end if;
   end Rewind;

   procedure Write
             (  Stream : in out Input_Stream;
                Item   : Stream_Element_Array
             )  is
   begin
      Raise_Exception
      (  Use_Error'Identity,
         "Input stream cannot be written"
      );
   end Write;

   procedure Write
             (  Stream : in out Unchecked_Input_Stream;
                Item   : Stream_Element_Array
             )  is
   begin
      Raise_Exception
      (  Use_Error'Identity,
         "Input stream cannot be written"
      );
   end Write;

   procedure Write
             (  Stream : in out Output_Stream;
                Item   : Stream_Element_Array
             )  is
      Lock : Holder (Stream.Pool);
   begin
      Write (Unchecked_Output_Stream (Stream), Item);
   end Write;

   procedure Write
             (  Stream : in out Unchecked_Output_Stream;
                Item   : Stream_Element_Array
             )  is
      Pointer : Stream_Element_Offset := Item'First;
--        Start   : Block_Offset;
   begin
      if Item'Length = 0 then
         return;
      end if;
      if Stream.Start = 0 then -- Open new stream
         Stream.Current :=
            Unchecked_Fetch
            (  Stream.Pool.all,
               Byte_Count
               (  Stream_Element_Count'Min
                  (  Item'Length + 8,
                     Max_Size - 4
            )  )  );
         Stream.Offset := Get_Offset (Stream.Current);
         declare
            Block : Block_Type renames
                    Load (Stream.Pool.File, Stream.Current).all;
            Size  : constant Unsigned_16 :=
                    Get_Size (Block, Stream.Offset);
         begin
            Stream.Next := Stream.Offset + Byte_Count (Size - (4 + 8));
         end;
         Stream.Offset := Stream.Offset + 8;
         Stream.Start  := Stream.Current;
      end if;
--        Start := Stream.Offset;
      loop
         declare
            Block : Block_Type renames
                    Update (Stream.Pool.File, Stream.Current).all;
         begin
            while Stream.Offset < Stream.Next loop
               Block (Stream.Offset) :=
                  Unsigned_8 (Stream_Element'Pos (Item (Pointer)));
--                 Ada.Text_IO.Put_Line
--                 (  "   '"
--                 &  Character'Val (Block (Stream.Offset))
--                 &  '''
--                 );
               Stream.Offset := Stream.Offset + 1;
               if Pointer = Item'Last then
                  declare -- Set last block length
                     Offset : constant Block_Offset :=
                              Get_Offset (Stream.Current);
                     Size   : constant Block_Offset :=
                              Stream.Offset - Offset;
                  begin
--                       Ada.Text_IO.Put_Line
--                       (  "Written:'"
--                       &  Image (Block (Start..Stream.Offset - 1))
--                       &  "' at"
--                       &  Byte_Index'Image (Stream.Current)
--                       );
                     Put (Block, Offset, Byte_Index (Size - 8));
                     Unchecked_Truncate
                     (  Stream.Pool.all,
                        Stream.Current,
                        Size
                     );
--                       Ada.Text_IO.Put_Line
--                       (  "Block:  '"
--                       &  Image
--                          (  Load (Stream.Pool.File, Stream.Current)
--                             (  Offset + 8
--                             .. Stream.Offset - 1
--                          )  )
--                       &  "' at"
--                       &  Byte_Index'Image (Stream.Current)
--                       &  " last"
--                       &  Unsigned_16'Image
--                          (  Get_Size
--                             (  Load
--                                (  Stream.Pool.File,
--                                   Stream.Current
--                                ) .all,
--                                Get_Offset (Stream.Current)
--                       )  )  );
                  end;
                  declare
                     Block  : Block_Type renames
                              Load
                              (  Stream.Pool.File,
                                 Stream.Current
                              ) .all;
                     Offset : constant Block_Offset :=
                              Get_Offset (Stream.Current);
                     Size   : constant Unsigned_16 :=
                              Get_Size (Block, Offset);
                  begin
                     Stream.Next := Offset + Byte_Count (Size - 4);
                  end;
                  return;
               end if;
               Pointer := Pointer + 1;
            end loop;
--              Ada.Text_IO.Put_Line
--              (  "Written:'"
--              &  Image (Block (Start..Stream.Offset - 1))
--              &  "' at"
--              &  Byte_Index'Image (Stream.Current)
--              );
            --
            -- No more data in the block
            --
            declare
               Offset : constant Block_Offset :=
                        Get_Offset (Stream.Current);
               Size   : constant Unsigned_16 :=
                        Get_Size (Block, Offset);
            begin
               if Is_Chained (Block, Offset) then
--                    Ada.Text_IO.Put_Line
--                    (  "Block:  '"
--                    &  Image (Block (Offset + 8..Stream.Offset - 1))
--                    &  "' at"
--                    &  Byte_Index'Image (Stream.Current)
--                    &  " next (chained) at "
--                    &  Byte_Index'Image (Get (Block, Offset))
--                    );
                  Stream.Current := Get (Block, Offset);
                  declare -- Set the cursor to the block's beginning
                     Block  : Block_Type renames
                              Load (Stream.Pool.File, Stream.Current).all;
                    Offset  : constant Block_Offset :=
                              Get_Offset (Stream.Current);
                    Size    : constant Unsigned_16 :=
                              Get_Size (Block, Offset);
                  begin
                    Stream.Offset := Offset + 8;
                    Stream.Next   := Offset + Byte_Count (Size - 4);
--                  Start         := Stream.Offset;
                  end;
               elsif 0 < Unchecked_Expand
                         (  Stream.Pool.all,
                            Stream.Current
                         )  then
                  declare -- Expanded the current block
                     Block  : Block_Type renames
                              Load (Stream.Pool.File, Stream.Current).all;
                     Offset : constant Block_Offset :=
                              Get_Offset (Stream.Current);
                     Size   : constant Unsigned_16 :=
                              Get_Size (Block, Offset);
                  begin
                     Stream.Next := Offset + Byte_Count (Size - 4);
--                   Start       := Stream.Offset;
                  end;
               else -- Allocating next block and chaining to it
                  declare
                     Next  : constant Byte_Index :=
                             Unchecked_Fetch
                             (  Stream.Pool.all,
                                Byte_Count
                                (  Stream_Element_Count'Min
                                   (  Item'Last - Pointer + 1 + 8,
                                      Max_Size - 4
                             )  )  );
                     Block : Block_Type renames
                             Update (Stream.Pool.File, Stream.Current).all;
                  begin -- Mark as chained and store the link
                     Put (Block, Offset, Next);
                     Mark_Chained (Block, Offset, Size);
--                       Ada.Text_IO.Put_Line
--                       (  "Block:  '"
--                       &  Image
--                          (  Block
--                             (  Offset + 8
--                             .. Stream.Offset - 1
--                          )  )
--                       &  "' at"
--                       &  Byte_Index'Image (Stream.Current)
--                       &  " next at "
--                       &  Byte_Index'Image (Next)
--                       );
                     Stream.Current := Next;
                  end;
                  declare -- Set the cursor to the block's beginning
                     Block  : Block_Type renames
                        Load (Stream.Pool.File, Stream.Current).all;
                     Offset : constant Block_Offset :=
                              Get_Offset (Stream.Current);
                     Size   : constant Unsigned_16 :=
                              Get_Size (Block, Offset);
                  begin
                    Stream.Offset := Offset + 8;
                    Stream.Next   := Offset + Byte_Count (Size - 4);
--                  Start         := Stream.Offset;
                  end;
               end if;
            end;
         end;
      end loop;
   end Write;

end Persistent.Memory_Pools.Streams;
