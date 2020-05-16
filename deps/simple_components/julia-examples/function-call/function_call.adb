--                                                                    --
--  procedure Function_Call         Copyright (c)  Dmitry A. Kazakov  --
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

procedure Function_Call is
   Bin : constant String := "D:\Julia-1.2.0\bin";
begin
   Load (Bin & "\libjulia.dll");  -- Load library
   Init_With_Image (Bin);         -- Initialize environment

   declare
      Sqrt : constant function_t := Get_Function (Base_Module, "sqrt");
      X, Y : Double;
   begin
      X := 4.0;
      Y := Value (Call (Sqrt, (1 => To_Julia (X))));
      Put_Line ("Y =" & Double'Image (Y));
   end;
   AtExit_Hook;                  -- Finalize environment
end Function_Call;
