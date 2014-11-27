--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Segmented_Stack                     Luebeck            --
--  Interface                                      Winter, 2004       --
--                                                                    --
--                                Last revision :  11:37 13 Oct 2007  --
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

package body Generic_Segmented_Stack is
   use Segment_Arrays;
   use Segment_Arrays.Object_Ptr_Array;

   procedure Split
             (  Index : Index_Type;
                High  : out Natural;
                Low   : out Natural
             );
   pragma Inline (Split);

   procedure Split
             (  Index : Index_Type;
                High  : out Natural;
                Low   : out Natural
             )  is
   begin
      High :=
         (  Index_Type'Pos (Index)
         -  Index_Type'Pos (Index_Type'First)
         -  1
         )  / Segment_Size;      
      Low :=
         (  Index_Type'Pos (Index)
         -  Index_Type'Pos (Index_Type'First)
         -  1
         )  mod Segment_Size;
   end Split;

   function Get
            (  Container : Unbounded_Ptr_Array;
               Index     : Index_Type
            )  return Object_Type is
      High : Natural;
      Low  : Natural;
   begin
      Split (Index, High, Low);
      return Get (Container, High) (Low);
    end Get;

   procedure Put
             (  Container : in out Unbounded_Ptr_Array;
                Index     : Index_Type;
                Element   : Object_Type
             )  is
      High : Natural;
      Low  : Natural;
      Ptr  : Segment_Ptr;
   begin
      Split (Index, High, Low);
      if (  Container.Vector /= null
         and then
            High in Container.Vector'Range
         )
      then
         Ptr := Get (Container, High);
      end if;
      if Ptr = null then
         Ptr := new Segment;
         Put (Container, High, Ptr);
      end if;
      Ptr (Low) := Element;
   end Put;

end Generic_Segmented_Stack;
