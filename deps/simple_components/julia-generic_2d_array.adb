--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Julia.Generic_2D_Array                      Luebeck            --
--  Implementation                                 Winter, 2019       --
--                                                                    --
--                                Last revision :  18:40 23 Oct 2021  --
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

with System.Address_To_Access_Conversions;

package body Julia.Generic_2D_Array is

   type Flat_Array is array (size_t) of aliased Element_Type;
   pragma Convention (C, Flat_Array);

   package Conversions is
      new System.Address_To_Access_Conversions (Flat_Array);
   use Conversions;

   function Get_Data (Object : value_t) return Object_Pointer is
      Ptr : Address;
      for Ptr'Address use Address (Object);
      pragma Import (Ada, Ptr);
   begin
      return To_Pointer (Ptr);
   end Get_Data;

   procedure Check_Type
             (  Object  : value_t;
                Rows    : out size_t;
                Columns : out size_t
             )  is
      Len : constant value_t := Links.ArrayLen (Object);
   begin
      Check_Error;
      if Links.Array_Rank (Object) /= 2 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Invalid array rank"
         );
      end if;
      Rows    := Links.Array_Size (Object, 0);
      Columns := Links.Array_Size (Object, 1);
   end Check_Type;

   function Columns (Object : value_t) return Natural is
      Columns : size_t;
      Rows    : size_t;
   begin
      Check_Type (Object, Rows, Columns);
      return Natural (Columns);
   end Columns;

   function Get
            (  Object : value_t;
               Row    : Positive;
               Column : Positive
            )  return Element_Type is
      Columns : size_t;
      Rows    : size_t;
   begin
      Check_Type (Object, Rows, Columns);
      if size_t (Column) > Columns or else size_t (Row) > Rows then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Subscript error"
         );
      end if;
      return Get_Data (Object)
             (  Rows * size_t (Column - 1) + size_t (Row - 1)
             );
   end Get;

   function Rows (Object : value_t) return Natural is
      Columns : size_t;
      Rows    : size_t;
   begin
      Check_Type (Object, Rows, Columns);
      return Natural (Rows);
   end Rows;

   procedure Set
             (  Object  : value_t;
                Row     : Positive;
                Column  : Positive;
                Element : Element_Type
             )  is
      Columns : size_t;
      Rows    : size_t;
   begin
      Check_Type (Object, Rows, Columns);
      if size_t (Column) > Columns or else size_t (Row) > Rows then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Subscript error"
         );
      end if;
      Get_Data (Object)
      (  Rows * size_t (Column - 1) + size_t (Row - 1)
      )  := Element;
   end Set;

   function To_Julia (Value : Element_Array_Type) return value_t is
      Result  : value_t;
      Kind_Of : constant datatype_t :=
                         Links.Apply_Array_Type (Julia_Type, 2);
   begin
      Result := Links.Alloc_Array_2D
                (  Kind_Of,
                   Value'Length (1),
                   Value'Length (2)
                );
      if Value'Length (1) > 0 and then Value'Length (2) > 0 then
         declare
            Data  : Flat_Array renames Get_Data (Result).all;
            Count : size_t := 0;
         begin
            for Column in Value'Range (2) loop
               for Row in Value'Range (1) loop
                  Data (Count) := Value (Row, Column);
                  Count := Count + 1;
               end loop;
            end loop;
         end;
      end if;
      return Result;
   end To_Julia;

   function Value (Object : value_t) return Element_Array_Type is
      Columns : size_t;
      Rows    : size_t;
   begin
      Check_Type (Object, Rows, Columns);
      declare
         Value : Element_Array_Type
                 (  1..Row_Index_Type'Base (Rows),
                    1..Column_Index_Type'Base (Columns)
                 );
      begin
         if Value'Length (1) > 0 and then Value'Length (2) > 0 then
            declare
               Data  : Flat_Array renames Get_Data (Object).all;
               Count : size_t := 0;
            begin
               for Column in Value'Range (2) loop
                  for Row in Value'Range (1) loop
                     Value (Row, Column) := Data (Count);
                     Count := Count + 1;
                  end loop;
               end loop;
            end;
         end if;
         return Value;
      end;
   end Value;

end Julia.Generic_2D_Array;
