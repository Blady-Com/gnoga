--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Random_Sequence                     Luebeck            --
--  Implementation                                 Winter, 2008       --
--                                                                    --
--                                Last revision :  18:59 10 Feb 2008  --
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

package body Generic_Random_Sequence is

   function Length (Sequencer : Sequence) return Element is
   begin
      return Sequencer.Size;
   end Length;

   function Next (Sequencer : Sequence; Dice : Element)
      return Element is
      Result : Element;
   begin
      Next (Sequencer.Self.all, Dice, Result);
      return Result;
   end Next;

   procedure Next
             (  Sequencer : in out Sequence;
                Dice      : Element;
                Item      : out Element
             )  is
      Position : Element;
      Size     : Element := Sequencer.Size;
   begin
      if Sequencer.Init then
         -- Uninitialized sequence
         Start (Sequencer);
         Sequencer.Init := False;
         Size     := 0;
         Position := Dice;
      elsif Size = 0 then
         -- The first item of the sequence
         Position := Dice;
      else
         -- The following item of the sequence
         Position := Size + Dice mod (Element'Last - Size + 1);
      end if;
      Item := Sequencer.Order (Position);
      Sequencer.Order (Position) := Sequencer.Order (Size);
      Sequencer.Order (Size)     := Item;
      Sequencer.Size             := Size + 1;
   end Next;

   procedure Start (Sequencer : in out Sequence) is
   begin
      for Item in Sequencer.Order'Range loop
         Sequencer.Order (Item) := Item;
      end loop;
      Sequencer.Size := 0;
   end Start;

end Generic_Random_Sequence;
