--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Generic_FIFO                                Spring, 2018       --
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

with Ada.Calendar;       use Ada.Calendar;
with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with Ada.Tags;           use Ada.Tags;

package body Synchronization.Interprocess.Generic_FIFO is

   In_End_Init : constant String :=  "The in-end of the queue is " &
                                     "not initialized or already " &
                                     "finalized";
   Out_End_Init : constant String := "The out-end of the queue is " &
                                     "not initialized or already " &
                                     "finalized";
   Queue_Init : constant String :=   "The queue is " &
                                     "not initialized or already " &
                                     "finalized";

   procedure Delete_Implementation
             (  Queue   : in out FIFO'Class;
                Count   : Natural  := 1;
                Timeout : Duration := Duration'Last
             )  is
   begin
      if Queue.Data = null then
         Raise_Exception (Status_Error'Identity, Out_End_Init);
      end if;
      if Count > 0 then
         declare
            Free   : Positive renames Queue.Data.Free;
            First  : Positive renames Queue.Data.First;
            Buffer : Element_Array renames Queue.Data.Buffer;
            Length : Integer := Free - First;
         begin
            if Length < 0 then
               Length := Length + Queue.Size;
            end if;
            declare
               Next : constant Positive :=
                               First + Integer'Min (Length, Count);
            begin
               if Next > Queue.Size then
                  First := Next - Queue.Size;
               else
                  First := Next;
               end if;
            end;
            if Length = Queue.Size - 1 then
               Signal (Queue.Not_Full.all); -- Queue was full
            end if;
         end;
      end if;
   end Delete_Implementation;

   procedure Delete
             (  Queue   : in out FIFO_Out;
                Count   : Natural  := 1;
                Timeout : Duration := Duration'Last
             )  is
   begin
      Delete_Implementation (Queue, Count, Timeout);
   end Delete;

   procedure Delete
             (  Queue   : in out Universal_FIFO;
                Count   : Natural  := 1;
                Timeout : Duration := Duration'Last
             )  is
   begin
      if Queue.Mode = Out_End then
         Delete_Implementation (Queue, Count, Timeout);
      else
         Raise_Exception
         (  Mode_Error'Identity,
            "An in-end of the queue is expected in Delete"
         );
      end if;
   end Delete;

   function Get_Implementation
            (  Queue   : FIFO'Class;
               Timeout : Duration := Duration'Last
            )  return Element_Type is
      Started : constant Time := Clock;
   begin
      if Queue.Data = null then
         Raise_Exception (Status_Error'Identity, Out_End_Init);
      end if;
      declare
         Free   : Positive renames Queue.Data.Free;
         First  : Positive renames Queue.Data.First;
         Buffer : Element_Array renames Queue.Data.Buffer;
      begin
         loop
            if First /= Free then -- Queue is not empty
               declare
                  Element : constant Element_Type := Buffer (First);
               begin
                  if First = Queue.Size then
                     First := 1;
                  else
                     First := First + 1;
                  end if;
                  Signal (Queue.Not_Full.all);
                  return Element;
               end;
            end if;
            Reset (Queue.Not_Empty.all); -- Prepare to wait
            if First = Free then         -- Still empty, wait
               Wait
               (  Queue.Not_Empty.all,
                  Timeout - (Clock - Started)
               );
            end if;
         end loop;
      end;
   end Get_Implementation;

   function Get
            (  Queue   : FIFO_Out;
               Timeout : Duration := Duration'Last
            )  return Element_Type is
   begin
      return Get_Implementation (Queue, Timeout);
   end Get;

   function Get
            (  Queue   : Universal_FIFO;
               Timeout : Duration := Duration'Last
            )  return Element_Type is
   begin
      if Queue.Mode = Out_End then
         return Get_Implementation (Queue, Timeout);
      else
         Raise_Exception
         (  Mode_Error'Identity,
            "An out-end of the queue is expected in Get"
         );
      end if;
   end Get;

   function Get_Mode (Queue : Universal_FIFO) return FIFO_End is
   begin
      return Queue.Mode;
   end Get_Mode;

   function Get_Signature (Object : FIFO) return Unsigned_16 is
   begin
      return Get_Signature (External_Tag (FIFO'Tag));
   end Get_Signature;

   function Get_Size (Queue : FIFO) return Storage_Count is
      subtype Data is FIFO_Data (Queue.Size);
   begin
      return Round (Data'Max_Size_In_Storage_Elements);
   end Get_Size;

   function Is_Empty (Queue : FIFO) return Boolean is
   begin
      declare
         Data : FIFO_Data renames Queue.Data.all;
      begin
         return Data.First = Data.Free;
      end;
   exception
      when Constraint_Error =>
         if Queue.Data = null then
            Raise_Exception (Status_Error'Identity, Queue_Init);
         else
            raise;
         end if;
   end Is_Empty;

   function Is_Full (Queue : FIFO) return Boolean is
   begin
      declare
         Data  : FIFO_Data renames Queue.Data.all;
         Count : constant Integer := Data.First - Data.Free;
      begin
         return Count = 1 or else Count = 1 - Data.Size;
      end;
   exception
      when Constraint_Error =>
         if Queue.Data = null then
            Raise_Exception (Status_Error'Identity, Queue_Init);
         else
            raise;
         end if;
   end Is_Full;

   function Is_Preserved (Queue : FIFO; Element : Element_Type)
      return Boolean is
   begin
      return True;
   end Is_Preserved;

   procedure Map
             (  Queue    : in out FIFO;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             )  is
      subtype Data is FIFO_Data (Queue.Size);
      package Mapper is new Generic_Memory_Mapper (Data);
      This : Abstract_Shared_Object_Ptr := Shared.First;
   begin
      while This /= null and then This /= Queue'Unchecked_Access loop
         if This.all in Event'Class then
            Queue.Not_Full  := Queue.Not_Empty;
            Queue.Not_Empty := Event'Class (This.all)'Unchecked_Access;
         end if;
         This := This.Next;
      end loop;
      if Queue.Not_Full = null then
         Raise_Exception
         (  Mode_Error'Identity,
            (  "The environment contains no not-full event "
            &  "record member appearing before the FIFO object"
         )  );
      end if;
      Queue.Data := Mapper.Map (Location, Owner).all'Unchecked_Access;
   end Map;

   procedure Map
             (  Queue    : in out FIFO_In;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             )  is
   begin
      Map (FIFO (Queue), Shared, Location, Size, Owner);
      if not Compare_And_Swap
             (  Target  => Queue.Data.Has_In'Access,
                Old_Val => 0,
                New_Val => 1
             )
      then
         Raise_Exception
         (  Mode_Error'Identity,
            "The queue already has the in-end"
         );
      end if;
   end Map;

   procedure Map
             (  Queue    : in out FIFO_Multiplexed_In;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             )  is
      This : Abstract_Shared_Object_Ptr := Shared.First;
   begin
      Map (FIFO (Queue), Shared, Location, Size, Owner);
      while This /= null and then This /= Queue'Unchecked_Access loop
         if This.all in Mutex'Class then
            Queue.Lock := Mutex'Class (This.all)'Unchecked_Access;
         end if;
         This := This.Next;
      end loop;
      if Queue.Lock = null then
         Raise_Exception
         (  Data_Error'Identity,
            (  "The environment contains no mutex "
            &  "record member appearing before the FIFO object"
         )  );
      end if;
      if (  not Compare_And_Swap
                (  Target  => Queue.Data.Has_In'Access,
                   Old_Val => 0,
                   New_Val => 2
                )
         and then
            not Compare_And_Swap
                (  Target  => Queue.Data.Has_In'Access,
                   Old_Val => 2,
                   New_Val => 2
         )      )
      then
         Raise_Exception
         (  Mode_Error'Identity,
            "The queue already has an non-multiplexed in-end"
         );
      end if;
   end Map;

   procedure Map
             (  Queue    : in out FIFO_Out;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             )  is
   begin
      Map (FIFO (Queue), Shared, Location, Size, Owner);
      if not Compare_And_Swap
             (  Target  => Queue.Data.Has_Out'Access,
                Old_Val => 0,
                New_Val => 1
             )
      then
         Raise_Exception
         (  Mode_Error'Identity,
            "The queue already has the out-end"
         );
      end if;
   end Map;

   procedure Map
             (  Queue    : in out Universal_FIFO;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             )  is
   begin
      Map (FIFO (Queue), Shared, Location, Size, Owner);
      case Queue.Mode is
         when In_End =>
            if not Compare_And_Swap
                   (  Target  => Queue.Data.Has_In'Access,
                      Old_Val => 0,
                      New_Val => 1
                   )
            then
               Raise_Exception
               (  Mode_Error'Identity,
                  "The queue already has an in-end"
               );
            end if;
         when Multiplexed_In_End =>
            declare
               This : Abstract_Shared_Object_Ptr := Shared.First;
            begin
               while (  This /= null
                     and then
                        This /= Queue'Unchecked_Access
                     )  loop
                  if This.all in Mutex'Class then
                     Queue.Lock :=
                        Mutex'Class (This.all)'Unchecked_Access;
                  end if;
                  This := This.Next;
               end loop;
               if Queue.Lock = null then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "The environment contains no mutex "
                     &  "record member appearing before the FIFO object"
                  )  );
               end if;
               if (  not Compare_And_Swap
                         (  Target  => Queue.Data.Has_In'Access,
                            Old_Val => 0,
                            New_Val => 2
                         )
                  and then
                     not Compare_And_Swap
                         (  Target  => Queue.Data.Has_In'Access,
                            Old_Val => 2,
                            New_Val => 2
                  )      )
               then
                  Raise_Exception
                  (  Mode_Error'Identity,
                     "The queue already has a non-multiplexed in-end"
                  );
               end if;
            end;
         when Out_End =>
            if not Compare_And_Swap
                   (  Target  => Queue.Data.Has_Out'Access,
                      Old_Val => 0,
                      New_Val => 1
                   )
            then
               Raise_Exception
               (  Mode_Error'Identity,
                  "The queue already has an out-end"
              );
            end if;
      end case;
   end Map;

   procedure Peek_Implementation
             (  Queue   : FIFO'Class;
                Element : out Element_Type;
                Empty   : out Boolean;
                Timeout : Duration := Duration'Last
             )  is
   begin
      if Queue.Data = null then
         Raise_Exception (Status_Error'Identity, Out_End_Init);
      end if;
      declare
         Data : FIFO_Data renames Queue.Data.all;
      begin
         Empty := Data.First = Data.Free;
         if not Empty then
            Element := Data.Buffer (Data.First);
         end if;
      end;
   end Peek_Implementation;

   procedure Peek
             (  Queue   : FIFO_Out;
                Element : out Element_Type;
                Empty   : out Boolean;
                Timeout : Duration := Duration'Last
             )  is
   begin
      Peek_Implementation (Queue, Element, Empty, Timeout);
   end Peek;

   procedure Peek
             (  Queue   : Universal_FIFO;
                Element : out Element_Type;
                Empty   : out Boolean;
                Timeout : Duration := Duration'Last
             )  is
   begin
      if Queue.Mode = Out_End then
         Peek_Implementation (Queue, Element, Empty, Timeout);
      else
         Raise_Exception
         (  Mode_Error'Identity,
            "An out-end of the queue is expected in Peek"
         );
      end if;
   end Peek;

   procedure Purge_Implementation
             (  Queue        : in out FIFO'Class;
                Is_Preserved : Is_Preserved_Ptr;
                Purged       : out Natural;
                Timeout      : Duration := Duration'Last
             )  is
   begin
      if Queue.Data = null then
         Raise_Exception (Status_Error'Identity, Out_End_Init);
      end if;
      declare
         Free   : Positive renames Queue.Data.Free;
         First  : Positive renames Queue.Data.First;
         Buffer : Element_Array renames Queue.Data.Buffer;
         From   : Positive := Free;
         To     : Positive := From;
      begin
         Purged := 0;
         while From /= First loop
            if From = 1 then
               From := Queue.Size;
            else
               From := From - 1;
            end if;
            if Is_Preserved (Buffer (From)) then
               -- Preserve this element
               if To = 1 then
                  To := Queue.Size;
               else
                  To := To - 1;
               end if;
               if To /= From then
                  Buffer (To) := Buffer (From);
               end if;
            else
               -- Remove this one
               Purged := Purged + 1;
            end if;
         end loop;
         if Purged > 0 then
            First := To;
            Signal (Queue.Not_Full.all);
         end if;
      end;
   end Purge_Implementation;

   procedure Purge
             (  Queue        : in out FIFO_Out;
                Is_Preserved : Is_Preserved_Ptr;
                Purged       : out Natural;
                Timeout      : Duration := Duration'Last
             )  is
   begin
      Purge_Implementation (Queue, Is_Preserved, Purged, Timeout);
   end Purge;

   procedure Purge
             (  Queue        : in out Universal_FIFO;
                Is_Preserved : Is_Preserved_Ptr;
                Purged       : out Natural;
                Timeout      : Duration := Duration'Last
             )  is
   begin
      if Queue.Mode = Out_End then
         Purge_Implementation (Queue, Is_Preserved, Purged, Timeout);
      else
         Raise_Exception
         (  Mode_Error'Identity,
            "An out-end of the queue is expected in Purge"
         );
      end if;
   end Purge;

   procedure Purge_Implementation
             (  Queue   : in out FIFO'Class;
                Purged  : out Natural;
                Timeout : Duration := Duration'Last
             )  is
   begin
      if Queue.Data = null then
         Raise_Exception (Status_Error'Identity, Out_End_Init);
      end if;
      declare
         Free   : Positive renames Queue.Data.Free;
         First  : Positive renames Queue.Data.First;
         Buffer : Element_Array renames Queue.Data.Buffer;
         From   : Positive := Free;
         To     : Positive := From;
      begin
         Purged := 0;
         while From /= First loop
            if From = 1 then
               From := Queue.Size;
            else
               From := From - 1;
            end if;
            if Is_Preserved
               (  Queue   => FIFO_Out'Class (Queue),
                  Element => Buffer (From)
               )
            then
               -- Preserve this element
               if To = 1 then
                  To := Queue.Size;
               else
                  To := To - 1;
               end if;
               if To /= From then
                  Buffer (To) := Buffer (From);
               end if;
            else
               -- Remove this one
               Purged := Purged + 1;
            end if;
         end loop;
         if Purged > 0 then
            First := To;
            Signal (Queue.Not_Full.all);
         end if;
      end;
   end Purge_Implementation;

   procedure Purge
             (  Queue   : in out FIFO_Out;
                Purged  : out Natural;
                Timeout : Duration := Duration'Last
             )  is
   begin
      Purge_Implementation (Queue, Purged, Timeout);
   end Purge;

   procedure Purge
             (  Queue   : in out Universal_FIFO;
                Purged  : out Natural;
                Timeout : Duration := Duration'Last
             )  is
   begin
      if Queue.Mode = Out_End then
         Purge_Implementation (Queue, Purged, Timeout);
      else
         Raise_Exception
         (  Mode_Error'Identity,
            "An out-end of the queue is expected in Purge"
         );
      end if;
   end Purge;

   procedure Put_Lock_Free
             (  Queue   : in out FIFO'Class;
                Element : Element_Type;
                Timeout : Duration := Duration'Last
             )  is
      Started : constant Time := Clock;
   begin
      if Queue.Data = null then
         Raise_Exception (Status_Error'Identity, In_End_Init);
      end if;
      declare
         Free   : Positive renames Queue.Data.Free;
         First  : Positive renames Queue.Data.First;
         Buffer : Element_Array renames Queue.Data.Buffer;
      begin
         loop
            declare
               Count : Integer := First - Free;
            begin
               if Count /= 1 and then Count /= 1 - Queue.Size then
                  Buffer (Free) := Element;
                  if Free = Queue.Size then
                     Free := 1;
                  else
                     Free := Free + 1;
                  end if;
                  Signal (Queue.Not_Empty.all);
                  return;
               end if;
               Reset (Queue.Not_Full.all); -- Prepare to wait
               Count := First - Free;
               if Count = 1 or else Count = 1 - Queue.Size then
                  Wait
                  (  Queue.Not_Full.all,
                     Timeout - (Clock - Started)
                  );
               end if;
            end;
         end loop;
      end;
   end Put_Lock_Free;

   procedure Put
             (  Queue   : in out FIFO_In;
                Element : Element_Type;
                Timeout : Duration := Duration'Last
             )  is
   begin
      Put_Lock_Free (Queue, Element, Timeout);
   end Put;

   procedure Put_Interlocked
             (  Queue   : in out FIFO'Class;
                Element : Element_Type;
                Lock    : in out Mutex'Class;
                Timeout : Duration := Duration'Last
             )  is
      Started : constant Time := Clock;
   begin
      if Queue.Data = null then
         Raise_Exception (Status_Error'Identity, In_End_Init);
      end if;
      declare
         Free   : Positive renames Queue.Data.Free;
         First  : Positive renames Queue.Data.First;
         Buffer : Element_Array renames Queue.Data.Buffer;
      begin
         Seize (Lock, Timeout);
         loop
            declare
               Count : Integer := First - Free;
            begin
               if Count /= 1 and then Count /= 1 - Queue.Size then
                  Buffer (Free) := Element;
                  if Free = Queue.Size then
                     Free := 1;
                  else
                     Free := Free + 1;
                  end if;
                  Release (Lock);
                  Signal (Queue.Not_Empty.all);
                  return;
               end if;
               Reset (Queue.Not_Full.all); -- Prepare to wait
               Count := First - Free;
               if Count = 1 or else Count = 1 - Queue.Size then
                  Wait
                  (  Queue.Not_Full.all,
                     Timeout - (Clock - Started)
                  );
               end if;
            end;
         end loop;
      exception
         when others =>
            Release (Lock);
            raise;
      end;
   end Put_Interlocked;

   procedure Put
             (  Queue   : in out FIFO_Multiplexed_In;
                Element : Element_Type;
                Timeout : Duration := Duration'Last
             )  is
   begin
      Put_Interlocked (Queue, Element, Queue.Lock.all, Timeout);
   end Put;

   procedure Put
             (  Queue   : in out Universal_FIFO;
                Element : Element_Type;
                Timeout : Duration := Duration'Last
             )  is
   begin
      case Queue.Mode is
         when In_End =>
            Put_Lock_Free (Queue, Element, Timeout);
         when Multiplexed_In_End =>
            Put_Interlocked (Queue, Element, Queue.Lock.all, Timeout);
         when Out_End =>
            Raise_Exception
            (  Mode_Error'Identity,
               "An in-end of the queue is expected in Put"
            );
      end case;
   end Put;

   procedure Set_Mode
             (  Queue : in out Universal_FIFO;
                Mode  : FIFO_End
             )  is
   begin
      if Queue.Data /= null then
         Raise_Exception
         (  Status_Error'Identity,
            "The queue is already initialized"
         );
      end if;
      Queue.Mode := Mode;
   end Set_Mode;

   procedure Wait_For_Not_Empty
             (  Queue   : in out FIFO;
                Timeout : Duration
             )  is
   begin
      if Is_Empty (Queue) then
         Reset (Queue.Not_Empty.all);
         if Is_Empty (Queue) then -- Still empty, enter wait
            Wait (Queue.Not_Empty.all, Timeout);
         end if;
      end if;
   end Wait_For_Not_Empty;

   procedure Wait_For_Not_Empty
             (  Queue   : in out FIFO;
                Timeout : Duration;
                Empty   : out Boolean
             )  is
      Signaled : Boolean;
   begin
      if Is_Empty (Queue) then
         Reset (Queue.Not_Empty.all);
         if Is_Empty (Queue) then -- Still empty, enter wait
            Wait (Queue.Not_Empty.all, Timeout, Signaled);
         end if;
         Empty := Is_Empty (Queue);
      else
         Empty := False;
      end if;
   end Wait_For_Not_Empty;

   procedure Wait_For_Not_Full
             (  Queue   : in out FIFO;
                Timeout : Duration
             )  is
   begin
      if Is_Full (Queue) then
         Reset (Queue.Not_Full.all);
         if Is_Full (Queue) then -- Still full, enter wait
            Wait (Queue.Not_Full.all, Timeout);
         end if;
      end if;
   end Wait_For_Not_Full;

   procedure Wait_For_Not_Full
             (  Queue   : in out FIFO;
                Timeout : Duration;
                Full    : out Boolean
             )  is
      Signaled : Boolean;
   begin
      if Is_Full (Queue) then
         Reset (Queue.Not_Full.all);
         if Is_Full (Queue) then -- Still full, enter wait
            Wait (Queue.Not_Full.all, Timeout, Signaled);
         end if;
         Full := Is_Full (Queue);
      else
         Full := False;
      end if;
   end Wait_For_Not_Full;

end Synchronization.Interprocess.Generic_FIFO;
