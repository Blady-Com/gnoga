--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Indefinite_FIFO                     Luebeck            --
--        Generic_Signaled                         Summer, 2008       --
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

package body Generic_Indefinite_FIFO.Generic_Signaled is

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
         Released_Get := False;
         if Break'Count > 0 then
            raise End_Error;
         end if;
         if Is_Empty (Queue.all) then
            requeue Wait_To_Get with abort;
         end if;
      end Wait_To_Get;

      entry Wait_To_Put (Element : Element_Type)
         when Released_Put or else Break'Count > 0 is
      begin
         Released_Put := False;
         if Break'Count > 0 then
            raise End_Error;
         end if;
         Put (FIFO (Queue.all), Element);
         Released_Get := True;
      exception
         when Constraint_Error =>
            requeue Wait_To_Put with abort;
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

   function Get (Queue : Signaled_FIFO) return Element_Type is
   begin
      if Is_Empty (Queue) then
         Queue.Event.Queue.Get_Blocked := True; -- Going to block
         if Is_Empty (Queue) then
            Queue.Event.Queue.Event.Wait_To_Get; -- Wait
         end if;
         Queue.Event.Queue.Get_Blocked := False; -- No blocking
      end if;
      declare
         Result : constant Element_Type := Get (FIFO (Queue));
      begin
         if Queue.Put_Blocked then
            Queue.Event.Queue.Event.Release_Put;
         end if;
         return Result;
      end;
   end Get;

   function Get (Queue : Signaled_FIFO; Timeout : Duration)
      return Element_Type is
   begin
      if Is_Empty (Queue) then
         Queue.Event.Queue.Get_Blocked := True; -- Going to block
         if Is_Empty (Queue) then
            select
               Queue.Event.Queue.Event.Wait_To_Get; -- Wait
            or delay Timeout;
               Queue.Event.Queue.Get_Blocked := False;
               raise Constraint_Error;
            end select;
         end if;
         Queue.Event.Queue.Get_Blocked := False; -- No blocking
      end if;
      declare
         Result : constant Element_Type := Get (FIFO (Queue));
      begin
         if Queue.Put_Blocked then
            Queue.Event.Queue.Event.Release_Put;
         end if;
         return Result;
      end;
   end Get;

   procedure Lock_Free_Put
             (  Queue   : in out Signaled_FIFO;
                Element : Element_Type
             )  is
   begin
      Put (FIFO (Queue), Element);
      if Queue.Get_Blocked then
         Queue.Event.Release_Get;
      end if;
   end Lock_Free_Put;

   function Peek (Queue : Signaled_FIFO) return Element_Type is
   begin
      if Is_Empty (Queue) then
         Queue.Event.Queue.Get_Blocked := True;
         if Is_Empty (Queue) then
            Queue.Event.Queue.Event.Wait_To_Get;
         end if;
         Queue.Event.Queue.Get_Blocked := False;
      end if;
      declare
         Result : constant Element_Type := Peek (FIFO (Queue));
      begin
         if Queue.Put_Blocked then
            Queue.Event.Queue.Event.Release_Put;
         end if;
         return Result;
      end;
   end Peek;

   function Peek (Queue : Signaled_FIFO; Timeout : Duration)
      return Element_Type is
   begin
      if Is_Empty (Queue) then
         Queue.Event.Queue.Get_Blocked := True;
         if Is_Empty (Queue) then
            select
               Queue.Event.Queue.Event.Wait_To_Get;
            or delay Timeout;
               Queue.Event.Queue.Get_Blocked := False;
               raise Constraint_Error;
            end select;
         end if;
         Queue.Event.Queue.Get_Blocked := False;
      end if;
      declare
         Result : constant Element_Type := Peek (FIFO (Queue));
      begin
         if Queue.Put_Blocked then
            Queue.Event.Queue.Event.Release_Put;
         end if;
         return Result;
      end;
   end Peek;

   procedure Put
             (  Queue   : in out Signaled_FIFO;
                Element : Element_Type
             )  is
   begin
      Lock_Free_Put (Queue, Element);
   exception
      when Constraint_Error =>
         Queue.Put_Blocked := True;
         begin
            Lock_Free_Put (Queue, Element);
         exception
            when Constraint_Error =>
               Queue.Event.Wait_To_Put (Element);
         end;
         Queue.Put_Blocked := False;
   end Put;

   procedure Put
             (  Queue   : in out Signaled_FIFO;
                Element : Element_Type;
                Timeout : Duration
             )  is
   begin
      Lock_Free_Put (Queue, Element);
   exception
      when Constraint_Error =>
         Queue.Put_Blocked := True;
         begin
            Lock_Free_Put (Queue, Element);
         exception
            when Constraint_Error =>
               select
                  Queue.Event.Wait_To_Put (Element);
               or delay Timeout;
                  Queue.Put_Blocked := False;
                  raise Constraint_Error;
               end select;
         end;
         Queue.Put_Blocked := False;
   end Put;

end Generic_Indefinite_FIFO.Generic_Signaled;
