--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Julia.Generic_1D_Array                      Luebeck            --
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

package body Julia.Generic_1D_Array is

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

   function Get
            (  Object   : value_t;
               Position : Positive
            )  return Element_Type is
      Size : constant Natural := Length (Object);
   begin
      if Position > Size then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Subscript error"
         );
      end if;
      return Get_Data (Object) (size_t (Position - 1));
   end Get;

   function Length (Object : value_t) return Natural is
      Len : constant value_t := Links.ArrayLen (Object);
   begin
      Check_Error;
      if Address'Size > 32 then
         return Natural (Integer_64'(Value (Len)));
      else
         return Natural (Integer_32'(Value (Len)));
      end if;
   end Length;

   procedure Set
             (  Object   : value_t;
                Position : Positive;
                Element  : Element_Type
             )  is
      Size : constant Natural := Length (Object);
   begin
      if Position > Size then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Subscript error"
         );
      end if;
      Get_Data (Object) (size_t (Position - 1)) := Element;
   end Set;

   function To_Julia (Value : Element_Array_Type) return value_t is
      Result  : value_t;
      Kind_Of : constant datatype_t :=
                         Links.Apply_Array_Type (Julia_Type, 1);
   begin
      Result := Links.Alloc_Array_1D (Kind_Of, Value'Length);
      if Value'Length > 0 then
         declare
            Data  : Flat_Array renames Get_Data (Result).all;
            Count : size_t := 0;
         begin
            for Index in Value'Range loop
               Data (Count) := Value (Index);
               Count := Count + 1;
            end loop;
         end;
      end if;
      return Result;
   end To_Julia;

   function Value (Object : value_t) return Element_Array_Type is
      Size  : constant Natural := Length (Object);
      Value : Element_Array_Type (1..Index_Type'Base (Size));
   begin
      if Value'Length > 0 then
         declare
            Data  : Flat_Array renames Get_Data (Object).all;
            Count : size_t := 0;
         begin
            for Index in Value'Range loop
               Value (Index) := Data (Count);
               Count := Count + 1;
            end loop;
         end;
      end if;
      return Value;
   end Value;

end Julia.Generic_1D_Array;
