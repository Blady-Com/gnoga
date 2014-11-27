--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Integer_B_Trees                        Luebeck            --
--  Instantiation                                  Spring, 2014       --
--                                                                    --
--                                Last revision :  10:05 22 Nov 2014  --
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

with Generic_B_Tree;
with Persistent.Memory_Pools.Streams.Generic_External_B_Tree;
with Persistent.Memory_Pools.Streams.Generic_External_Ptr_B_Tree;
with Persistent.Memory_Pools.Streams.External_B_Tree.Generic_Table;

package Test_Integer_B_Trees is
   package Internal is new Generic_B_Tree (Integer, Integer, 3);
   package External is
      new Persistent.Memory_Pools.Streams.Generic_External_B_Tree
          (  Integer,
             Integer,
             Integer'Input,
             Integer'Input,
             Integer'Output,
             Integer'Output
          );
   package External_Ptr is
      new Persistent.Memory_Pools.Streams.Generic_External_Ptr_B_Tree
          (  Integer,
             Integer'Input,
             Integer'Output
          );
   type Keys is (Primary, Secondary);
   type Values is range 1..3;
   package Tables is
      new Persistent.Memory_Pools.Streams.External_B_Tree.
          Generic_Table (Keys, Values);

end Test_Integer_B_Trees;
