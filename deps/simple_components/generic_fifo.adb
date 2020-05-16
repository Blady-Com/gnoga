--                                                                    --
--  package Generic_FIFO            Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Autumn, 2007       --
--                                                                    --
--                                Last revision :  08:25 05 May 2020  --
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

package body Generic_FIFO is

   procedure Delete
             (  Queue : in out FIFO;
                Count : Natural := 1
             )  is
   begin
      if Count > 0 then
         declare
            Length : Integer := Queue.Free - Queue.First;
         begin
            if Length < 0 then
               Length := Length + Queue.Size;
            end if;
            declare
               First : constant Positive :=
                       Queue.First + Integer'Min (Length, Count);
            begin
               if First > Queue.Size then
                  Queue.First := First - Queue.Size;
               else
                  Queue.First := First;
               end if;
            end;
         end;
      end if;
   end Delete;

   function Free_Space (Queue : FIFO) return Natural is
      Count : constant Integer := Queue.First - Queue.Free;
   begin
      if Count <= 0 then
         return Queue.Size + Count - 1;
      else
         return Count - 1;
      end if;
   end Free_Space;

   procedure Get
             (  Queue   : in out FIFO;
                Element : out Element_Type;
                Empty   : out Boolean
             )  is
   begin
      Empty := Queue.First = Queue.Free;
      if not Empty  then
         Element := Queue.Buffer (Queue.First);
         if Queue.First = Queue.Size then
            Queue.First := 1;
         else
            Queue.First := Queue.First + 1;
         end if;
      end if;
   end Get;

   function Get (Queue : FIFO) return Element_Type is
      Element : Element_Type;
      Empty   : Boolean;
   begin
      Get (Queue.Self.all, Element, Empty);
      if Empty then
         raise Constraint_Error;
      else
         return Element;
      end if;
   end Get;

   function Is_Empty (Queue : FIFO) return Boolean is
   begin
      return Queue.First = Queue.Free;
   end Is_Empty;

   function Is_Full (Queue : FIFO) return Boolean is
      Count : constant Integer := Queue.First - Queue.Free;
   begin
      return Count = 1 or else Count = 1 - Queue.Size;
   end Is_Full;

   function Is_Preserved (Queue : FIFO; Element : Element_Type)
      return Boolean is
   begin
      return True;
   end Is_Preserved;

   procedure Peek
             (  Queue   : FIFO;
                Element : out Element_Type;
                Empty   : out Boolean
             )  is
   begin
      Empty := Queue.First = Queue.Free;
      if not Empty then
         Element := Queue.Buffer (Queue.First);
      end if;
   end Peek;

   function Peek (Queue : FIFO) return Element_Type is
   begin
      if Queue.First = Queue.Free then
         raise Constraint_Error;
      else
         return Queue.Buffer (Queue.First);
      end if;
   end Peek;

   procedure Purge (Queue : in out FIFO; Purged : out Natural) is
      Container : FIFO'Class renames Queue.Self.all;
      From      : Positive := Queue.Free;
      To        : Positive := From;
   begin
      Purged := 0;
      while From /= Queue.First loop
         if From = 1 then
            From := Queue.Size;
         else
            From := From - 1;
         end if;
         if Is_Preserved (Container, Queue.Buffer (From)) then
            -- Preserve this element
            if To = 1 then
               To := Queue.Size;
            else
               To := To - 1;
            end if;
            if To /= From then
               Queue.Buffer (To) := Queue.Buffer (From);
            end if;
         else
            -- Remove this one
            Purged := Purged + 1;
         end if;
      end loop;
      Queue.First := To;
   end Purge;

   procedure Purge
             (  Queue        : in out FIFO;
                Is_Preserved : Is_Preserved_Ptr;
                Purged       : out Natural
             )  is
      From : Positive := Queue.Free;
      To   : Positive := From;
   begin
      Purged := 0;
      while From /= Queue.First loop
         if From = 1 then
            From := Queue.Size;
         else
            From := From - 1;
         end if;
         if Is_Preserved (Queue.Buffer (From)) then
            -- Preserve this element
            if To = 1 then
               To := Queue.Size;
            else
               To := To - 1;
            end if;
            if To /= From then
               Queue.Buffer (To) := Queue.Buffer (From);
            end if;
         else
            -- Remove this one
            Purged := Purged + 1;
         end if;
      end loop;
      Queue.First := To;
   end Purge;

   procedure Put
             (  Queue   : in out FIFO;
                Element : Element_Type;
                Full    : out Boolean
             )  is
      Count : constant Integer := Queue.First - Queue.Free;
   begin
      if Count /= 1 and then Count /= 1 - Queue.Size then
         Full := False;
         Queue.Buffer (Queue.Free) := Element;
         if Queue.Free = Queue.Size then
            Queue.Free := 1;
         else
            Queue.Free := Queue.Free + 1;
         end if;
      else
         Full := True;
      end if;
   end Put;

   procedure Put
             (  Queue   : in out FIFO;
                Element : Element_Type
             )  is
      Full : Boolean;
   begin
      Put (Queue, Element, Full);
      if Full then
         raise Constraint_Error;
      end if;
   end Put;

end Generic_FIFO;
