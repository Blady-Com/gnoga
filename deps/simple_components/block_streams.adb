--                                                                    --
--  package Block_Streams           Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Autumn, 2011       --
--                                                                    --
--                                Last revision :  22:06 23 Jul 2014  --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body Block_Streams is

   procedure Read_Block
             (  Stream : in out Input_Block_Stream;
                No     : out Unsigned_32
             );
   pragma Inline (Read_Block);

   procedure Write_Block (Stream : in out Output_Block_Stream);
   pragma Inline (Write_Block);

   procedure Finalize (Object : in out Controlled) is
   begin
      Flush (Object.Output.all);
   end Finalize;

   function Fletcher_16
            (  Data : Stream_Element_Array;
               Last : Stream_Element_Offset
            )  return Stream_Element_Array is
      Sum_1 : Unsigned_16 := 16#FF#;
      Sum_2 : Unsigned_16 := 16#FF#;
   begin
      for Major in 1..(Last - Data'First + 1) / 21 loop
         for Index in Major * 21 - 20..Major * 21 loop
            Sum_1 := Sum_1 + Unsigned_16 (Data (Index));
            Sum_2 := Sum_2 + Sum_1;
         end loop;
         Sum_1 := (Sum_1 and 16#FF#) + Sum_1 / 2**8;
         Sum_2 := (Sum_2 and 16#FF#) + Sum_2 / 2**8;
      end loop;
      Sum_1 := (Sum_1 and 16#FF#) + Sum_1 / 2**8;
      Sum_2 := (Sum_2 and 16#FF#) + Sum_2 / 2**8;
      return
      (  1 => Stream_Element (Sum_1 and 16#FF#),
         2 => Stream_Element (Sum_2 and 16#FF#)
      );
   end Fletcher_16;

   procedure Flush (Stream : in out Output_Block_Stream) is
   begin
      if Stream.Index > Stream.Buffer'First then
         for Index in Stream.Index..Stream.Buffer'Last - 6 loop
            Stream.Buffer (Index) := 0;
         end loop;
         Write_Block (Stream);
      end if;
      Stream.No := 0;
   end Flush;

   function Get_Block_No (Stream : Input_Block_Stream)
      return Unsigned_32 is
   begin
      if Stream.Index > Stream.Buffer'Last - 6 then
         return Stream.No + 1;
      else
         return Stream.No;
      end if;
   end Get_Block_No;

   function Get_Block_No (Stream : Output_Block_Stream)
      return Unsigned_32 is
   begin
      return Stream.No + 1;
   end Get_Block_No;

   function Get_Element_No (Stream : Input_Block_Stream)
      return Stream_Element_Count is
   begin
      if Stream.Index > Stream.Buffer'Last - 6 then
         return 1;
      else
         return Stream.Index;
      end if;
   end Get_Element_No;

   function Get_Element_No (Stream : Output_Block_Stream)
      return Stream_Element_Count is
   begin
      return Stream.Index;
   end Get_Element_No;

   procedure Read_Block
             (  Stream : in out Input_Block_Stream;
                No     : out Unsigned_32
             )  is
      Last : Stream_Element_Offset;
   begin
      Read (Stream.Transport.all, Stream.Buffer, Last);
      if Last < Stream.Buffer'First then
         Raise_Exception (End_Error'Identity, "End of stream");
      elsif Last /= Stream.Buffer'Last then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Malformed block"
            &  Stream_Element_Offset'Image
               (  Last
               +  1
               -  Stream.Buffer'First
               )
            &  " <"
            &  Integer'Image (Stream.Buffer'Length)
         )  );
      end if;
      if (  Fletcher_16 (Stream.Buffer, Stream.Buffer'Last - 2)
         /= Stream.Buffer (Stream.Buffer'Last - 1..Stream.Buffer'Last)
         )
      then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Checksum error in block"
            &  Unsigned_32'Image (Stream.No + 1)
         )  );
      end if;
      No :=
         (  Unsigned_32 (Stream.Buffer (Stream.Buffer'Last - 5)) * 2**24
         +  Unsigned_32 (Stream.Buffer (Stream.Buffer'Last - 4)) * 2**16
         +  Unsigned_32 (Stream.Buffer (Stream.Buffer'Last - 3)) * 2**16
         +  Unsigned_32 (Stream.Buffer (Stream.Buffer'Last - 2))
         );
      Stream.Index := 1;
   end Read_Block;

   procedure Write_Block (Stream : in out Output_Block_Stream) is
   begin
      Stream.Buffer (Stream.Buffer'Last - 5) :=
         Stream_Element (Stream.No / 2**24);
      Stream.Buffer (Stream.Buffer'Last - 4) :=
         Stream_Element ((Stream.No / 2**16) mod 2**8);
      Stream.Buffer (Stream.Buffer'Last - 3) :=
         Stream_Element ((Stream.No / 2**8) mod 2**8);
      Stream.Buffer (Stream.Buffer'Last - 2) :=
         Stream_Element (Stream.No mod 2**8);
      Stream.No := Stream.No + 1;
      Stream.Buffer (Stream.Buffer'Last - 1..Stream.Buffer'Last) :=
         Fletcher_16 (Stream.Buffer, Stream.Buffer'Last - 2);
      Write (Stream.Transport.all, Stream.Buffer);
      Stream.Index := 1;
   end Write_Block;

   procedure Read
             (  Stream : in out Input_Block_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
   begin
      Last := Item'First - 1;
      while Last < Item'Last loop
         if Stream.Index > Stream.Buffer'Last - 6 then
            declare
               No : Unsigned_32;
            begin
               Read_Block (Stream, No);
               if No /= Stream.No then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Sequence number"
                     &  Unsigned_32'Image (No)
                     &  " error, expected"
                     &  Unsigned_32'Image (Stream.No)
                  )  );
               end if;
               Stream.No := Stream.No + 1;
            end;
         end if;
         Last         := Last + 1;
         Item (Last)  := Stream.Buffer (Stream.Index);
         Stream.Index := Stream.Index + 1;
      end loop;
   end Read;

   procedure Read
             (  Stream : in out Output_Block_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
   begin
      Raise_Exception
      (  Use_Error'Identity,
         "Output stream cannot be read"
      );
   end Read;

   procedure Skip (Stream : in out Input_Block_Stream) is
      No : Unsigned_32;
   begin
      loop
         Read_Block (Stream, No);
         exit when No = 0;
      end loop;
      Stream.No := 1;
   end Skip;

   procedure Write
             (  Stream : in out Input_Block_Stream;
                Item   : Stream_Element_Array
             )  is
   begin
      Raise_Exception
      (  Use_Error'Identity,
         "Input stream cannot be written"
      );
   end Write;

   procedure Write
             (  Stream : in out Output_Block_Stream;
                Item   : Stream_Element_Array
             )  is
   begin
      for Index in Item'Range loop
         if Stream.Index > Stream.Buffer'Last - 6 then
            Write_Block (Stream);
         end if;
         Stream.Buffer (Stream.Index) := Item (Index);
         Stream.Index := Stream.Index + 1;
      end loop;
   end Write;

end Block_Streams;
