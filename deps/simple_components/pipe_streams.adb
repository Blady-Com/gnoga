--                                                                    --
--  package Pipe_Streams            Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Autumn, 2022       --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body Pipe_Streams is

--   function Image (Data : Stream_Element_Array) return String is
--      Result : String (1..Data'Length);
--      To     : Integer := Result'First;
--   begin
--      for Index in Data'Range loop
--         Result (To) := Character'Val (Data (Index));
--         To := To + 1;
--      end loop;
--      return Result;
--   end Image;

   procedure Erase (Stream : in out Pipe_Stream) is
   begin
      Stream.Pipe.Erase;
   end Erase;

   function Available_To_Read (Stream : Pipe_Stream)
      return Stream_Element_Count is
   begin
      return Stream.Pipe.Available_To_Read;
   end Available_To_Read;

   function Available_To_Write (Stream : Pipe_Stream)
      return Stream_Element_Count is
   begin
      return Stream.Pipe.Available_To_Write;
   end Available_To_Write;

   procedure Close_Read (Stream : in out Pipe_Stream) is
   begin
      Stream.Pipe.Close_Read;
   end Close_Read;

   procedure Close_Write (Stream : in out Pipe_Stream) is
   begin
      Stream.Pipe.Close_Write;
   end Close_Write;

   function Is_Empty (Stream : Pipe_Stream) return Boolean is
   begin
      return Stream.Pipe.Is_Empty;
   end Is_Empty;

   function Is_Full (Stream : Pipe_Stream) return Boolean is
   begin
      return Stream.Pipe.Is_Full;
   end Is_Full;

   function Is_Read_Closed (Stream : Pipe_Stream) return Boolean is
   begin
      return Stream.Pipe.Get_Out = Closed;
   end Is_Read_Closed;

   function Is_Sink (Stream : Pipe_Stream) return Boolean is
   begin
      return Stream.Pipe.Get_In = Sink;
   end Is_Sink;

   function Is_Write_Closed (Stream : Pipe_Stream) return Boolean is
   begin
      return Stream.Pipe.Get_In = Closed;
   end Is_Write_Closed;

   procedure Read
             (  Stream : in out Pipe_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
   begin
      if Item'Length > 0 then
         Last := Item'First;
         loop
            Stream.Pipe.Read (Item (Last..Item'Last), Last);
            exit when Last = Item'Last;
            Last := Last + 1;
         end loop;
      end if;
      Last := Item'Last;
   end Read;

   procedure Sink (Stream : in out Pipe_Stream) is
   begin
      Stream.Pipe.Sink_Write;
   end Sink;

   procedure Write
             (  Stream : in out Pipe_Stream;
                Item   : Stream_Element_Array
             )  is
      Last : Stream_Element_Offset;
   begin
      if Item'Length > 0 then
         Last := Item'First;
         loop
            Stream.Pipe.Write (Item (Last..Item'Last), Last);
            exit when Last = Item'Last;
            Last := Last + 1;
         end loop;
      end if;
   end Write;

   protected body Pipe_Object is
      function Available_To_Read return Stream_Element_Count is
      begin
         case Out_End is
            when Closed | Empty =>
               return 0;
            when Readable =>
               if In_Index < Out_Index then
                  --
                  -- [XX         XXXX]
                  --    | In     | Out
                  --
                  return Buffer'Length - Out_Index + In_Index;
               else
                  --
                  -- [  XXXXXXXXX    ]
                  --    | Out    | In
                  --
                  return In_Index - Out_Index;
               end if;
         end case;
      end Available_To_Read;

      function Available_To_Write return Stream_Element_Count is
      begin
         case In_End is
            when Closed | Full =>
               return 0;
            when Writable =>
               if In_Index < Out_Index then
                  --
                  -- [XX         XXXX]
                  --    | In     | Out
                  --
                  return Out_Index - In_Index;
               else
                  --
                  -- [  XXXXXXXXX    ]
                  --    | Out    | In
                  --
                  return Buffer'Length - In_Index + Out_Index;
               end if;
            when Sink =>
               return Stream_Element_Count'Last;
         end case;
      end Available_To_Write;

      procedure Close_Read is
      begin
         Out_End := Closed;
      end Close_Read;

      procedure Close_Write is
      begin
         In_End := Closed;
      end Close_Write;

      function Get_In return In_End_State is
      begin
         return In_End;
      end Get_In;

      function Get_Out return Out_End_State is
      begin
         return Out_End;
      end Get_Out;

      function Is_Empty return Boolean is
      begin
         case Out_End is
            when Closed | Empty =>
               return True;
            when Readable =>
               return False;
         end case;
      end Is_Empty;

      function Is_Full return Boolean is
      begin
         case In_End is
            when Closed | Full =>
               return True;
            when Writable | Sink =>
               return False;
         end case;
      end Is_Full;

      procedure Erase is
      begin
         In_End    := Writable;
         Out_End   := Empty;
         Out_Index := 1;
         In_Index  := 1;
      end Erase;

      procedure Sink_Write is
      begin
         In_End := Sink;
      end Sink_Write;

      entry Read
            (  Data : in out Stream_Element_Array;
               Last : out Stream_Element_Offset
            )  when Out_End /= Empty is
         Length : Stream_Element_Offset;
      begin
         case Out_End is
            when Empty =>
               raise End_Error;
            when Closed =>
               if In_Index = Out_Index and then In_End /= Full then
                  raise End_Error;
               end if;
            when Readable =>
               null;
         end case;
         if In_Index <= Out_Index then
            --
            -- [XX         XXXX]
            --    | In     | Out
            --
            Length := Buffer'Last - Out_Index + 1;
            if Length <= Data'Length then
               Data (Data'First..Data'First + Length - 1) :=
                  Buffer (Out_Index..Buffer'Last);
               Out_Index := Buffer'First;
               if Out_Index = In_Index then
                  if Out_End = Readable then
                     Out_End := Empty;
                  end if;
               end if;
               Last := Data'First + Length - 1;
            else
               Data :=
                  Buffer (Out_Index..Out_Index + Data'Length - 1);
               Out_Index := Out_Index + Data'Length;
               Last      := Data'Last;
            end if;
         else
            --
            -- [  XXXXXXXXX    ]
            --    | Out    | In
            --
            Length := In_Index - Out_Index;
            if Length <= Data'Length then
               Data (Data'First..Data'First + Length - 1) :=
                  Buffer (Out_Index..In_Index - 1);
               Out_Index := In_Index;
               if Out_End = Readable then
                  Out_End := Empty;
               end if;
               Last := Data'First + Length - 1;
            else
               Data :=
                  Buffer (Out_Index..Out_Index + Data'Length - 1);
               Out_Index := Out_Index + Data'Length;
               Last      := Data'Last;
            end if;
         end if;
         if In_End = Full then
            In_End := Writable;
         end if;
      end Read;

      entry Write
            (  Data : Stream_Element_Array;
               Last : out Stream_Element_Offset
            )  when In_End /= Full is
         Length : Stream_Element_Offset;
      begin
         case In_End is
            when Closed | Full =>
               raise End_Error;
            when Writable =>
               if In_Index < Out_Index then
                  --
                  -- [XX         XXXX]
                  --    | In     | Out
                  --
                  Length := Out_Index - In_Index;
                  if Length <= Data'Length then
                     Buffer (In_Index..Out_Index - 1) :=
                        Data (Data'First..Data'First + Length - 1);
                     In_Index := Out_Index;
                     In_End   := Full;
                     Last     := Data'First + Length - 1;
                  else
                     Buffer (In_Index..In_Index + Data'Length - 1) :=
                        Data;
                     In_Index := In_Index + Data'Length;
                     Last     := Data'Last;
                  end if;
               else
                  --
                  -- [  XXXXXXXXX    ]
                  --    | Out    | In
                  --
                  Length := Buffer'Last - In_Index + 1;
                  if Length <= Data'Length then
                     Buffer (In_Index..Buffer'Last) :=
                        Data (Data'First..Data'First + Length - 1);
                     In_Index := Buffer'First;
                     if In_Index = Out_Index then
                        In_End := Full;
                     end if;
                     Last := Data'First + Length - 1;
                  else
                     Buffer (In_Index..In_Index + Data'Length - 1) :=
                        Data;
                     In_Index := In_Index + Data'Length;
                     Last     := Data'Last;
                  end if;
               end if;
               if Out_End = Empty then
                  Out_End := Readable;
               end if;
            when Sink =>
               Last := Data'Last;
         end case;
      end Write;
   end Pipe_Object;

end Pipe_Streams;
