--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Record_Blackboards                     Luebeck            --
--  Test instantiation                             Winter, 2008       --
--                                                                    --
--                                Last revision :  21:31 21 Dec 2011  --
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

with System.Storage_Elements;  use System.Storage_Elements;

with Generic_Blackboard;

package Test_Record_Blackboards is
   use System;
   type Element is array (1..20) of Storage_Element;
   package Boards is new Generic_Blackboard (Element);
   Element_Size : constant Storage_Count;
   Head_Size    : constant Storage_Count;
private
   Storage_Offset_Size : constant Storage_Count :=
      (Storage_Count'Size + Storage_Unit - 1) / Storage_Unit;
   Head_Size : constant Storage_Count :=
      (  Storage_Offset_Size
      -  (Storage_Offset_Size mod (-Element'Alignment))
      );
   Length : constant Storage_Count :=
      (Element'Size + Storage_Unit - 1) / Storage_Unit;
   Element_Size : constant Storage_Count :=
      Length - (Length mod (-Element'Alignment));

end Test_Record_Blackboards;
