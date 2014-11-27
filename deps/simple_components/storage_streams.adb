--                                                                    --
--  package Storage_Streams           Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Autumn, 2011       --
--                                                                    --
--                                Last revision :  17:24 01 Jun 2012  --
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

package body Storage_Streams is

   procedure Erase (Stream : in out Storage_Stream) is
   begin
      Merge (Stream.Free, Stream.First);
      Stream.Size := 0;
      Stream.Out_Count := 0;
   end Erase;

   procedure Finalize (Object : in out Controlled) is
   begin
      Erase (Object.Stream.Free);
      Erase (Object.Stream.First);
   end Finalize;

   function Get_Size (Stream : Storage_Stream)
      return Stream_Element_Count is
   begin
      return Stream.Size;
   end Get_Size;

   procedure Read
             (  Stream : in out Storage_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
      Count : Stream_Element_Count := Item'Length;
      Rest  : Stream_Element_Count;
      This  : Doubly_Linked.Item :=  Doubly_Linked.Item (Stream.First);
   begin
      if This = null then
         Last := Item'First - 1;
         return;
      end if;
      loop
         if Next (This) = This then
            Rest := Stream.In_Count - Stream.Out_Count;
         else
            Rest := This'Length - Stream.Out_Count;
         end if;
         exit when Rest >= Count;
         Item (Item'Last - (Count - 1)..Item'Last - (Count - Rest)) :=
            This (This'First + Stream.Out_Count..This'Last);
         Append (Stream.Free, This, Stream.First);
         Count := Count - Rest;
         Stream.Size := Stream.Size - Rest;
         Stream.Out_Count := 0;
         This := Doubly_Linked.Item (Stream.First);
         if This = null then
            Last := Item'Last - Count;
            return;
         end if;
      end loop;
      Item (Item'Last - (Count - 1)..Item'Last) :=
         This
         (  This'First + Stream.Out_Count
         .. This'First + Stream.Out_Count + (Count - 1)
         );
      Stream.Out_Count := Stream.Out_Count + Count;
      Stream.Size := Stream.Size - Count;
      Last := Item'Last;
   end Read;

   procedure Write
             (  Stream : in out Storage_Stream;
                Item   : Stream_Element_Array
             )  is
      Count : Stream_Element_Count := Item'Length;
      This  : Doubly_Linked.Item   := Doubly_Linked.Item (Stream.First);
   begin
      if Count = 0 then
         return;
      end if;
      if This /= null then
         This := Previous (This);
      end if;
      loop
         if This = null then
            Take (Stream.Free, This);
            if This = null then
               This := new Stream_Element_Array (1..Stream.Block_Size);
            end if;
            Append (Stream.First, This);
            Stream.In_Count := 0;
         end if;
         declare
            Space : constant Stream_Element_Count :=
                             This'Length - Stream.In_Count;
         begin
            exit when Space >= Count;
            This (This'First + Stream.In_Count..This'Last) :=
               Item
               (  Item'Last - (Count - 1)
               .. Item'Last - (Count - Space)
               );
            Count := Count - Space;
            Stream.Size := Stream.Size + Space;
         end;
         This  := null;
      end loop;
      This
      (  This'First + Stream.In_Count
      .. This'First + Stream.In_Count + (Count - 1)
      )  := Item (Item'Last - (Count - 1)..Item'Last);
      Stream.In_Count := Stream.In_Count + Count;
      Stream.Size := Stream.Size + Count;
   end Write;

end Storage_Streams;
