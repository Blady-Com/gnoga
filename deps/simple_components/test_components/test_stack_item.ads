--                                                                    --
--  package Test_Stack_Item         Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Autumn, 2003       --
--                                                                    --
--                                Last revision :  20:47 23 Jun 2010  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of  the  GNU  Library  General  Public  --
--  License  as  published  by  the Free Software Foundation; either  --
--  version 2 of the License, or (at your option) any later version.  --
--                                                                    --
--  This  library is distributed in the hope that it will be useful,  --
--  but WITHOUT ANY WARRANTY; without even the implied  warranty  of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  Library General Public License for more details.                  --
--                                                                    --
--  You  should  have  received  a  copy  of the GNU Library General  --
--  Public  License  along  with  this library; if not, write to the  --
--  Free Software Foundation, Inc.,  675  Mass  Ave,  Cambridge,  MA  --
--  02139, USA.                                                       --
--____________________________________________________________________--
--
with Stack_Storage.Mark_And_Release; 

package Test_Stack_Item is
   Stack : Stack_Storage.Pool
           (  Initial_Size => 10,
              Items_Number => 2
           );
   package Stack_Objects is
      new Stack_Storage.Mark_And_Release
          (  Stack_Storage.Pool'Class (Stack)
          );
   subtype Stack_Mark is Stack_Objects.Pool_Mark;
   subtype Stack_Object is Stack_Objects.Pool_Object;
   subtype Stack_Object_Ptr is Stack_Objects.Pool_Object_Ptr;

   type Item is new Stack_Object with record
      No : Natural;
   end record;
   procedure Finalize (Object : in out Item);
   procedure Initialize (Object : in out Item);
   function Is_Allocated (No : Positive) return Boolean;
   type Item_Ptr is access Item;
   for Item_Ptr'Storage_Pool use Stack;

end Test_Stack_Item;
