--                                                                    --
--  procedure Array_1D              Copyright (c)  Dmitry A. Kazakov  --
--                                                 Luebeck            --
--                                                 Winter, 2019       --
--                                                                    --
--                                Last revision :  20:47 23 Jun 2010  --
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

with Ada.Text_IO;   use Ada.Text_IO;
with Interfaces.C;  use Interfaces.C;
with Julia;         use Julia;

with Julia.Generic_1D_Array;

procedure Array_1D is
   Bin : constant String := "D:\Julia-1.2.0\bin";

   type Words_Array is
      array (Positive range <>) of Interfaces.Integer_16;
   package Words_Arrays is
      new Julia.Generic_1D_Array
          (  Index_Type         => Positive,
             Element_Type       => Interfaces.Integer_16,
             Element_Array_Type => Words_Array,
             Julia_Type         => Int16_Type
          );
begin
   Load (Bin & "\libjulia.dll");  -- Load library
   Init_With_Image (Bin);         -- Initialize environment

   declare
      use Words_Arrays;
      A : value_t;
   begin
      A := Eval_String ("a = [Int16(1), Int16(2), Int16(3)]");
      Put ("A =");
      for Index in 1..Length (A) loop
         Put (Interfaces.Integer_16'Image (Get (A, Index)));
      end loop;
      New_Line;
      Set (A, 2, 10);
      Eval_String ("println(a)");
   end;

   AtExit_Hook;                   -- Finalize environment
end Array_1D;
