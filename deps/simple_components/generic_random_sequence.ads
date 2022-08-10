--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Random_Sequence                     Luebeck            --
--  Interface                                      Winter, 2008       --
--                                                                    --
--                                Last revision :  10:13 29 Nov 2020  --
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
--
--  This package provides a  generator  of  non-repeating  sequences  of
--  items.  The sequence generated can be made random when the output of
--  a  random  generator  is  used.  The sequence items are of a modular
--  type.  It  is  not  recommened  to  use  large  types  because   the
--  implementation requires O(n) space where n  is the type modulo.  The
--  package is generic with the formal parameter:
--
--      Item_Type - The sequence items type
--
--  Note  that  the  sequences  are  generated  from  Item_Type'Base.  A
--  sequence  contains  all  values  of  Item_Type'Base.  Items start to
--  repeat if more than Item_Type'Base'Modulus generated.
--
--  The sequence items can be used in cipher-block chaining and feedback
--  modes in order to improve cryptographic strength.
--
generic
   type Item_Type is mod <>;
package Generic_Random_Sequence is
--
-- Sequence -- The sequence type
--
   type Sequence is limited private;
   subtype Element is Item_Type'Base;
--
-- Length -- The number of items in the sequence
--
--    Sequencer - The sequence generator
--
-- Returns :
--
--    How many unique items were already generated
--
   function Length (Sequencer : Sequence) return Element;
--
-- Next -- Get the next item of the sequence
--
--    Sequencer - The sequence generator
--    Dice      - Randomizing input
--
-- This  function  returns the next sequence item. The parameter Dice is
-- used to modify the sequence. When set 0, the sequence will always  be
-- 0, 1, 2,... In order to get a random sequence Dice has to be  random.
-- Typically it is an output of a random generator of Item_Type'Base.
--
-- Returns :
--
--    The sequence item
--
   function Next (Sequencer : Sequence; Dice : Element) return Element;
--
-- Next -- Get the next item of the sequence
--
--    Sequencer - The sequence generator
--    Dice      - Randomizing input
--    Item      - The result
--
-- This  is  an  equivalent  of  the  function  Next but interfaced as a
-- procedure.
--
   procedure Next
             (  Sequencer : in out Sequence;
                Dice      : Element;
                Item      : out Element
             );
--
-- Next_Unbiased -- Get the next unbiased item of the sequence
--
--    Sequencer - The sequence generator
--    Dice      - Randomizing input
--    Item      - The result
--    Success   - False if unsuccessful
--
-- This variant is used to get an unbiased sequence. When Dice is random
-- the sequence obtained  by Next will be biased  towards first elements
-- of  the sequence.  This variant generates  an unbiased  sequence. The
-- procedure must be called repeatedly when Success is false.
--
   procedure Next_Unbiased
             (  Sequencer : in out Sequence;
                Dice      : Element;
                Item      : out Element;
                Success   : out Boolean
             );
--
-- Start -- Begin a sequence
--
--    Sequencer - To reset
--
-- This procedure is called to repeat a sequence. Note that  to  get  an
-- exactly the same sequence again, the Dice parameter values used  with
-- Next have to repeat.
--
   procedure Start (Sequencer : in out Sequence);

private
   type Permutation is array (Element) of Element;

   type Sequence_Ptr is access all Sequence;

   type Sequence is limited record
      Self  : Sequence_Ptr := Sequence'Unchecked_Access;
      Size  : Element      := 0;
      Init  : Boolean      := True;
      Order : Permutation;
   end record;

end Generic_Random_Sequence;
