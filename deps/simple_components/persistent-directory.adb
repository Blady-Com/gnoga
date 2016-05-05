--                                                                    --
--  package Persistent.Directory    Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2007       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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

package body Persistent.Directory is

   procedure Create
             (  Storage   : in out Storage_Handle;
                Directory : out Deposit_Handle;
                Name      : String;
                Parent    : Deposit_Handle := Root_Directory
             )  is
      Ptr : constant Deposit_Ptr := new Directory_Object;
   begin
      Set (Directory, Ptr);
      Put (Storage, Directory, Name, Parent);
   end Create;

   function Get_Class (Object : Directory_Object) return String is
   begin
      return Directory_Class;
   end Get_Class;

   function Is_Directory (Object : Deposit_Handle) return Boolean is
      This : constant Deposit_Ptr := Ptr (Object);
   begin
      return This /= null and then This.all in Directory_Object'Class;
   end Is_Directory;

   function Is_Modified (Directory : Directory_Object) return Boolean is
   begin
      return False;
   end Is_Modified;

   procedure Reset_Modified (Directory : in out Directory_Object) is
   begin
      null;
   end Reset_Modified;

   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Object  : out Deposit_Ptr
             )  is
   begin
      Object := new Directory_Object;
   end Restore;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Object      : Directory_Object
             )  is
   begin
      null;
   end Store;

   function To_Catalogue_Ptr is
      new Ada.Unchecked_Conversion
          (  Deposit_Ptr,
             Directory_Object_Ptr
          );

   function To_Directory_Object_Ptr (Ptr : Deposit_Ptr)
      return Directory_Object_Ptr is
   begin
      if Ptr.all in Directory_Object'Class then
         return To_Catalogue_Ptr (Ptr);
      else
         raise Constraint_Error;
      end if;
   end To_Directory_Object_Ptr;

begin
   Register_Class (Directory_Class, Restore'Access);
end Persistent.Directory;
