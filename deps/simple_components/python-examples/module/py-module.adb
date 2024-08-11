--                                                                    --
--  package Py.Module               Copyright (c)  Dmitry A. Kazakov  --
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

with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Text_IO;              use Ada.Text_IO;
with System.Storage_Elements;  use System.Storage_Elements;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body Py.Module is

   Module_Name : aliased char_array := "sample_module"        & Nul;
   Module_Doc  : aliased char_array := "sample Python module" & Nul;

   Operation_Name : aliased char_array := "operation"         & Nul;
   Operation_Doc  : aliased char_array := "Test operation"    & Nul;

   function Operation
            (  Self     : Object;
               Args     : Object;
               Keywords : Object
            )  return Object;
   pragma Convention (C, Operation);

   Operation_Arguments : constant Argument_List := "a" - "b" - "c";

   function Operation
            (  Self     : Object;
               Args     : Object;
               Keywords : Object
            )  return Object is
   begin
      declare
         Result : Handle;
         List   : constant Object_Array :=
                           Parse (Args, Keywords, Operation_Arguments);
      begin
         Put_Line ("A =" & long'image (Links.Long_AsLong (List (1))));
         if List (2) /= Null_Object then
            Put_Line ("B = " & As_String (List (2)));
         end if;
         if List (3) /= Null_Object then
            Put_Line
            (  "C ="
            &  double'Image (Links.Float_AsDouble (List (3)))
            );
         end if;
         Result := Unicode_FromString ("OK");
         Links.IncRef (Result.Ptr); -- New reference
         return Result.Ptr;
      end;
   exception
      when Python_Error =>
         return Null_Object;
      when Error : others =>
         Throw_SystemError (Error);
         return Null_Object;
   end Operation;

   Methods : array (1..2) of aliased MethodDef :=
             (  (  Name  => To_Chars_Ptr (Operation_Name'Access),
                   Meth  => (True, Operation'Access),
                   Flags => METH_VARARGS + METH_KEYWORDS,
                   Doc   => To_Chars_Ptr (Operation_Doc'Access)
                ),
                End_Method
             );

   ModuleDef_HEAD_INIT : constant ModuleDef_Base :=
                                  (  Base  => (1, Null_Object),
                                     Init  => null,
                                     Index => 0,
                                     Copy  => Null_Object
                                  );

   Module : constant ModuleDef :=
                     (  Base     => ModuleDef_HEAD_INIT,
                        Name     => To_Chars_Ptr (Module_Name'Access),
                        Doc      => To_Chars_Ptr (Module_Doc'Access),
                        Size     => -1,
                        Methods  => Methods (1)'Access,
                        Slots    => null,
                        Traverse => null,
                        Clear    => null,
                        Free     => null
                     );

   function Module_Init return Object;
   pragma Convention (C, Module_Init);

   function Module_Init return Object is
      Result : Object;
   begin
      Result := Module_Create (Module);
      return Result;
   end Module_Init;

   procedure Create is
   begin
      if 0 > Import_AppendInittab
             (  Module_Name,
                Module_Init'Access
             )
      then
         Raise_Exception
         (  Python_Error'Identity,
            "Cannot append test module"
         );
      end if;
   end Create;

end Py.Module;
