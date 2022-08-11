--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_FIFO.Generic_Signaled               Luebeck            --
--  Implementation                                 Autumn, 2007       --
--                                                                    --
--                                Last revision :  11:30 01 May 2021  --
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

package body Generic_FIFO.Generic_Signaled is

   protected body Signal is
      entry Break when Wait_To_Get'Count + Wait_To_Put'Count = 0 is
      begin
         null;
      end Break;

      procedure Release_Get is
      begin
         Released_Get := True;
      end Release_Get;

      procedure Release_Put is
      begin
         Released_Put := True;
      end Release_Put;

      entry Wait_To_Get when Released_Get or else Break'Count > 0 is
      begin
         if Break'Count > 0 then
            raise End_Error;
         end if;
         Released_Get := False;
         if Is_Empty (Queue.all) then
            requeue Wait_To_Get with abort;
         end if;
      end Wait_To_Get;

      entry Wait_To_Put when Released_Put or else Break'Count > 0 is
      begin
         if Break'Count > 0 then
            raise End_Error;
         end if;
         Released_Put := False;
         if Is_Full (Queue.all) then
            requeue Wait_To_Put with abort;
         end if;
      end Wait_To_Put;

   end Signal;

   procedure Cancel (Queue : in out Signaled_FIFO) is
   begin
      Queue.Event.Break;
   end Cancel;

   procedure Delete
             (  Queue : in out Signaled_FIFO;
                Count : Natural := 1
             )  is
   begin
      if Count > 0 then
         Delete (FIFO (Queue), Count);
         if Queue.Put_Blocked then
            Queue.Event.Release_Put;
         end if;
      end if;
   end Delete;

   procedure Get
             (  Queue   : in out Signaled_FIFO;
                Element : out Element_Type;
                Empty   : out Boolean
             )  is
   begin
      Get (FIFO (Queue), Element, Empty);
      if Queue.Put_Blocked then
         Queue.Event.Release_Put;
      end if;
   end Get;

   function Get (Queue : Signaled_FIFO) return Element_Type is
      This    : Signaled_FIFO renames Queue.Event.Queue.all;
      Element : Element_Type;
      Empty   : Boolean;
   begin
      Get (This, Element, Empty);
      if Empty then
         This.Get_Blocked := True; -- Going to block
         Get (This, Element, Empty);            -- Try again
         if Empty then                          -- Have to block
            This.Event.Wait_To_Get;
            Get (This, Element, Empty);
         end if;
         This.Get_Blocked := False;
      end if;
      return Element;
   end Get;

   function Get (Queue : Signaled_FIFO; Timeout : Duration)
      return Element_Type is
      This    : Signaled_FIFO renames Queue.Event.Queue.all;
      Element : Element_Type;
      Empty   : Boolean;
   begin
      Get (This, Element, Empty);
      if Empty then
         This.Get_Blocked := True;
         Get (This, Element, Empty);
         if Empty then
            select
               This.Event.Wait_To_Get;
            or delay Timeout;
               This.Get_Blocked := False;
               raise Constraint_Error;
            end select;
            Get (This, Element, Empty);
         end if;
         This.Get_Blocked := False;
      end if;
      return Element;
   end Get;

   function Peek (Queue : Signaled_FIFO) return Element_Type is
      Element : Element_Type;
      Empty   : Boolean;
   begin
      Peek (Queue, Element, Empty);
      if Empty then
         Queue.Event.Queue.Get_Blocked := True;
         Peek (Queue, Element, Empty);
         if Empty then
            Queue.Event.Queue.Event.Wait_To_Get;
            Peek (Queue, Element, Empty);
         end if;
         Queue.Event.Queue.Get_Blocked := False;
      end if;
      return Element;
   end Peek;

   procedure Purge
             (  Queue  : in out Signaled_FIFO;
                Purged : out Natural
             )  is
   begin
      Purge (FIFO (Queue), Purged);
      if Purged > 0 and then Queue.Put_Blocked then
         Queue.Event.Release_Put;
      end if;
   end Purge;

   procedure Purge
             (  Queue        : in out Signaled_FIFO;
                Is_Preserved : Is_Preserved_Ptr;
                Purged       : out Natural
             )  is
   begin
      Purge (FIFO (Queue), Is_Preserved, Purged);
      if Purged > 0 and then Queue.Put_Blocked then
         Queue.Event.Release_Put;
      end if;
   end Purge;

   function Peek (Queue : Signaled_FIFO; Timeout : Duration)
      return Element_Type is
      Element : Element_Type;
      Empty   : Boolean;
   begin
      Peek (Queue, Element, Empty);
      if Empty then
         Queue.Event.Queue.Get_Blocked := True;
         Peek (Queue, Element, Empty);
         if Empty then
            select
               Queue.Event.Queue.Event.Wait_To_Get;
            or delay Timeout;
               Queue.Event.Queue.Get_Blocked := False;
               raise Constraint_Error;
            end select;
            Peek (Queue, Element, Empty);
         end if;
         Queue.Event.Queue.Get_Blocked := False;
      end if;
      return Element;
   end Peek;

   procedure Put
             (  Queue   : in out Signaled_FIFO;
                Element : Element_Type;
                Full    : out Boolean
             )  is
   begin
      Put (FIFO (Queue), Element, Full);
      if Queue.Get_Blocked then
         Queue.Event.Release_Get;
      end if;
   end Put;

   procedure Put
             (  Queue   : in out Signaled_FIFO;
                Element : Element_Type
             )  is
      Full : Boolean;
   begin
      Put (Queue, Element, Full);
      if Full then
         Queue.Put_Blocked := True;
         Put (Queue, Element, Full);
         if Full then
            Queue.Event.Wait_To_Put;
            Put (Queue, Element, Full);
         end if;
         Queue.Put_Blocked := False;
      end if;
   end Put;

   procedure Put
             (  Queue   : in out Signaled_FIFO;
                Element : Element_Type;
                Timeout : Duration
             )  is
      Full : Boolean;
   begin
      Put (Queue, Element, Full);
      if Full then
         Queue.Put_Blocked := True;
         Put (Queue, Element, Full);
         if Full then
            select
               Queue.Event.Wait_To_Put;
            or delay Timeout;
               Queue.Put_Blocked := False;
               raise Constraint_Error;
            end select;
            Put (Queue, Element, Full);
         end if;
         Queue.Put_Blocked := False;
      end if;
   end Put;

end Generic_FIFO.Generic_Signaled;
