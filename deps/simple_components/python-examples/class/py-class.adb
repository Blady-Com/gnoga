--                                                                    --
--  package Py.Class                Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2022       --
--                                                                    --
--                                Last revision :  20:47 23 Jun 2022  --
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

with Ada.Numerics.Elementary_Functions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body Py.Class is

   function Get_X
            (  Self    : Object;
               Closure : System.Address
            )  return Object;
   pragma Convention (C, Get_X);

   function Get_Y
            (  Self    : Object;
               Closure : System.Address
            )  return Object;
   pragma Convention (C, Get_Y);

   function Length (Self : Object; Arg : Object) return Object;
   pragma Convention (C, Length);

   function New_Instance
            (  Class    : Object;
               Args     : Object;
               Keywords : Object
            )  return Object;
   pragma Convention (C, New_Instance);

   function Set_X
            (  Self  : Object;
               Value : Object;
               Closure : System.Address
            )  return int;
   pragma Convention (C, Set_X);

   function Set_Y
            (  Self  : Object;
               Value : Object;
               Closure : System.Address
            )  return int;
   pragma Convention (C, Set_Y);

   function Get (Object : Handle) return Point is
   begin
      Check_Handle (Object); -- Not checking the object type because we
      declare                -- do not keep it anywhere
         This : Point;
         pragma Import (Ada, This);
         for This'Address use System.Address (Object.Ptr)
                            + Storage_Offset (Object_HeadSize);
      begin
         return This;
      end;
   end Get;

   function Get_X
            (  Self    : Object;
               Closure : System.Address
            )  return Object is
      This : Point;
      pragma Import (Ada, This);
      for This'Address use System.Address (Self)
                         + Storage_Offset (Object_HeadSize);
   begin
      return Links.Float_FromDouble (double (This.X));
   exception
      when Python_Error =>
         return Null_Object;
      when others =>
         Throw_ValueError ("Invalid x");
         return Null_Object;
   end Get_X;

   function Get_Y
            (  Self    : Object;
               Closure : System.Address
            )  return Object is
      This : Point;
      pragma Import (Ada, This);
      for This'Address use System.Address (Self)
                         + Storage_Offset (Object_HeadSize);
   begin
      return Links.Float_FromDouble (double (This.Y));
   exception
      when Python_Error =>
         return Null_Object;
      when others =>
         Throw_ValueError ("Invalid y");
         return Null_Object;
   end Get_Y;

   function Length (Self : Object; Arg : Object) return Object is
      use Ada.Numerics.Elementary_Functions;
      This : Point;
      pragma Import (Ada, This);
      for This'Address use System.Address (Self)
                         + Storage_Offset (Object_HeadSize);
   begin
      return
         Links.Float_FromDouble (double (Sqrt (This.X**2 +This.Y**2 )));
   exception
      when Python_Error =>
         return Null_Object;
      when others =>
         Throw_ValueError ("Error in length");
         return Null_Object;
   end Length;

   New_List : constant Argument_List := - "x" - "y";

   function New_Instance
            (  Class    : Object;
               Args     : Object;
               Keywords : Object
            )  return Object is
      Result : Object;
   begin
      declare
         List : constant Object_Array :=
                         Parse (Args, Keywords, New_List);
         X, Y : Float := 0.0;
      begin
         if List (1) /= Null_Object then
            X := Float (Links.Float_AsDouble (List (1)));
            Check_Error;
         end if;
         if List (2) /= Null_Object then
            Y := Float (Links.Float_AsDouble (List (2)));
            Check_Error;
         end if;
         Result := -- Allocate the object
            Links.Type_GetSlot (Class, tp_alloc).tp_alloc (Class, 0);
         declare
            This : Point;
            pragma Import (Ada, This);
            for This'Address use System.Address (Result)
                               + Storage_Offset (Object_HeadSize);
         begin
            This.X := X;
            This.Y := Y;
         end;
      end;
      return Result;
   exception
      when Constraint_Error =>
         Throw_TypeError ("Argument is out of range");
         return Null_Object;
      when Python_Error =>
         return Null_Object;
   end New_Instance;

   procedure Set (Object : Handle; Value : Point) is
   begin
      Check_Handle (Object); -- Not checking the object type because we
      declare                -- do not keep it anywhere
         This : Point;
         pragma Import (Ada, This);
         for This'Address use System.Address (Object.Ptr)
                            + Storage_Offset (Object_HeadSize);
      begin
         This := Value;
      end;
   end Set;

   function Set_X
            (  Self  : Object;
               Value : Object;
               Closure : System.Address
            )  return int is
      This : Point;
      pragma Import (Ada, This);
      for This'Address use System.Address (Self)
                         + Storage_Offset (Object_HeadSize);
   begin
      This.X := Float (Links.Float_AsDouble (Value));
      return 0;
   exception
      when Python_Error =>
         return -1;
      when others =>
         Throw_TypeError ("Error setting x");
         return -1;
   end Set_X;

   function Set_Y
            (  Self  : Object;
               Value : Object;
               Closure : System.Address
            )  return int is
      This : Point;
      pragma Import (Ada, This);
      for This'Address use System.Address (Self)
                         + Storage_Offset (Object_HeadSize);
   begin
      This.Y := Float (Links.Float_AsDouble (Value));
      return 0;
   exception
      when Python_Error =>
         return -1;
      when others =>
         Throw_TypeError ("Error setting y");
         return -1;
   end Set_Y;

   GetSets : array (1..3) of aliased GetSetDef := -- Must be exist when
             (  (  Name    => New_String ("x"),   -- the type does
                   Get     => Get_X'Access,
                   Set     => Set_X'Access,
                   Doc     => New_String ("x-coordinate"),
                   Closure => System.Null_Address
                ),
                (  Name    => New_String ("y"),
                   Get     => Get_Y'Access,
                   Set     => Set_Y'Access,
                   Doc     => New_String ("y-coordinate"),
                   Closure => System.Null_Address
                ),
                End_GetSet
             );
   Methods : array (1..2) of aliased MethodDef :=
             (  (  Name  => New_String ("length"),
                   Meth  => (False, Length'Access),
                   Flags => METH_NOARGS,
                   Doc   => New_String ("vector length")
                ),
                End_Method
             );

   function Create_Point_Type return Handle is
      Result  : Handle;
      Module  : Handle;
      Doc     : aliased char_array := "point example" & NUL;
      Slots   : array (1..5) of aliased Type_Slot :=
         (  (tp_doc,     (tp_doc, To_Chars_Ptr (Doc'Unchecked_Access))),
            (tp_new,     (tp_new,     New_Instance'Access)),
            (tp_getset,  (tp_getset,  GetSets (1)'Unchecked_Access)),
            (tp_methods, (tp_methods, Methods (1)'Unchecked_Access)),
            End_Slot
         );
   begin
      Result.Ptr :=
         Links.Type_FromSpec
         (  (  Name       => New_String ("point"),
               Basic_Size =>
                  int (Object_HeadSize + (Point'Size + 7) / 8),
               Item_Size  => 0,
               Flags      => TPFLAGS_DEFAULT + TPFLAGS_HEAPTYPE,
               Slots      => Slots (Slots'First)'Unchecked_Access
         )  );
      if Result.Ptr = Null_Object then
         Check_Error;
      end if;
      Module := Import_AddModule ("point");
      Module_AddObject (Module, "point", Result);
      return Result;
   end Create_Point_Type;

end Py.Class;
