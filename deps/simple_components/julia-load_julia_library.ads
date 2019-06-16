--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Julia.Load_Julia_Library                    Luebeck            --
--  Interface                                      Winter, 2019       --
--                                                                    --
--                                Last revision :  11:37 20 Jan 2019  --
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

package Julia.Load_Julia_Library is

   function Get_Default_Name return String;
   function Get_Default_Relative_Path return String;
   function Get_Extension return String;
   function Is_Loaded return Boolean;
   procedure Load (Name : String := "");

end Julia.Load_Julia_Library;
