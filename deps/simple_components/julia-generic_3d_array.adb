--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Julia.Generic_3D_Array                      Luebeck            --
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

package body Julia.Generic_3D_Array is
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
             (  Object   : value_t;
                X_Length : out size_t;
                Y_Length : out size_t;
                Z_Length : out size_t
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
      X_Length := Links.Array_Size (Object, 0);
      Y_Length := Links.Array_Size (Object, 1);
      Z_Length := Links.Array_Size (Object, 2);
   end Check_Type;

   function Get
            (  Object  : value_t;
               X, Y, Z : Positive
            )  return Element_Type is
      X_Size : size_t;
      Y_Size : size_t;
      Z_Size : size_t;
   begin
      Check_Type (Object, X_Size, Y_Size, Z_Size);
      if (  size_t (X) > X_Size
         or else
            size_t (Y) > Y_Size
         or else size_t (Z) > Z_Size
         )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Subscript error"
         );
      end if;
      return Get_Data (Object)
             (  (  size_t (Z - 1) * Y_Size
                +  size_t (Y - 1)
                )
             *  X_Size
             +  size_t (X - 1)
             );
   end Get;

   procedure Set
             (  Object  : value_t;
                X, Y, Z : Positive;
                Element : Element_Type
             )  is
      X_Size : size_t;
      Y_Size : size_t;
      Z_Size : size_t;
   begin
      Check_Type (Object, X_Size, Y_Size, Z_Size);
      if (  size_t (X) > X_Size
         or else
            size_t (Y) > Y_Size
         or else size_t (Z) > Z_Size
         )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Subscript error"
         );
      end if;
      Get_Data (Object)
      (  (  size_t (Z - 1) * Y_Size
         +  size_t (Y - 1)
         )
      *  X_Size
      +  size_t (X - 1)
      )  := Element;
   end Set;

   function To_Julia (Value : Element_Array_Type) return value_t is
      Result  : value_t;
      Kind_Of : constant datatype_t :=
                         Links.Apply_Array_Type (Julia_Type, 3);
   begin
      Result := Links.Alloc_Array_3D
                (  Kind_Of,
                   Value'Length (1),
                   Value'Length (2),
                   Value'Length (3)
                );
      if (  Value'Length (1) > 0
         and then
            Value'Length (2) > 0
         and then
            Value'Length (3) > 0
         )
      then
         declare
            Data  : Flat_Array renames Get_Data (Result).all;
            Count : size_t := 0;
         begin
            for Z in Value'Range (3) loop
               for Y in Value'Range (2) loop
                  for X in Value'Range (1) loop
                     Data (Count) := Value (X, Y, Z);
                     Count := Count + 1;
                  end loop;
               end loop;
            end loop;
         end;
      end if;
      return Result;
   end To_Julia;

   function Value (Object : value_t) return Element_Array_Type is
      X_Size : size_t;
      Y_Size : size_t;
      Z_Size : size_t;
   begin
      Check_Type (Object, X_Size, Y_Size, Z_Size);
      declare
         Value : Element_Array_Type
                 (  1..X_Index_Type'Base (X_Size),
                    1..Y_Index_Type'Base (Y_Size),
                    1..Z_Index_Type'Base (Y_Size)
                 );
      begin
         if (  Value'Length (1) > 0
            and then
               Value'Length (2) > 0
            and then
               Value'Length (3) > 0
            )
         then
            declare
               Data  : Flat_Array renames Get_Data (Object).all;
               Count : size_t := 0;
            begin
               for Z in Value'Range (3) loop
                  for Y in Value'Range (2) loop
                     for X in Value'Range (1) loop
                        Value (X, Y, Z) := Data (Count);
                        Count := Count + 1;
                     end loop;
                  end loop;
               end loop;
            end;
         end if;
         return Value;
      end;
   end Value;

   function X_Length (Object : value_t) return Natural is
      X_Size : size_t;
      Y_Size : size_t;
      Z_Size : size_t;
   begin
      Check_Type (Object, X_Size, Y_Size, Z_Size);
      return Natural (X_Size);
   end X_Length;

   function Y_Length (Object : value_t) return Natural is
      X_Size : size_t;
      Y_Size : size_t;
      Z_Size : size_t;
   begin
      Check_Type (Object, X_Size, Y_Size, Z_Size);
      return Natural (Y_Size);
   end Y_Length;

   function Z_Length (Object : value_t) return Natural is
      X_Size : size_t;
      Y_Size : size_t;
      Z_Size : size_t;
   begin
      Check_Type (Object, X_Size, Y_Size, Z_Size);
      return Natural (Z_Size);
   end Z_Length;

end Julia.Generic_3D_Array;
