--                                                                    --
--  package Persistent.Handle       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2002       --
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

with Strings_Edit.UTF8.Handling;  use Strings_Edit.UTF8.Handling;

package body Persistent.Handle is

   function Get
            (  Storage : Storage_Handle;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Deposit_Handle is
   begin
      return Get (Ptr (Storage), Name, Parent);
   end Get;

   function Get
            (  Storage : Storage_Handle;
               Name    : Wide_String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Deposit_Handle is
   begin
      return Get (Ptr (Storage), To_UTF8 (Name), Parent);
   end Get;

   function Get_Class
            (  Storage : Storage_Handle;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return String is
   begin
      return Get_Class (Ptr (Storage), Name, Parent);
   end Get_Class;

   function Get_Class
            (  Storage : Storage_Handle;
               Name    : Wide_String;
               Parent  : Deposit_Handle := Root_Directory
            )  return String is
   begin
      return Get_Class (Ptr (Storage), To_UTF8 (Name), Parent);
   end Get_Class;

   function Get_Creation_Time
            (  Storage : Storage_Handle;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Time is
   begin
      return Get_Creation_Time (Ptr (Storage), Name, Parent);
   end Get_Creation_Time;

   function Get_Creation_Time
            (  Storage : Storage_Handle;
               Name    : Wide_String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Time is
   begin
      return Get_Creation_Time (Ptr (Storage), To_UTF8 (Name), Parent);
   end Get_Creation_Time;

   function Get_List
            (  Storage     : Storage_Handle;
               Prefix      : String := "";
               Suffix      : String := "";
               Equivalence : Unicode_Mapping_Function := null;
               Parent      : Deposit_Handle := Root_Directory
            )  return Catalogue.Set is
      Result : Catalogue.Set;
   begin
      Result :=
         Get_List
         (  Ptr (Storage),
            Prefix,
            Suffix,
            Equivalence,
            Parent
         );
      return Result;
   end Get_List;

   function Get_List
            (  Storage     : Storage_Handle;
               Prefix      : Wide_String;
               Suffix      : Wide_String;
               Equivalence : Unicode_Mapping_Function := null;
               Parent      : Deposit_Handle := Root_Directory
            )  return Catalogue.Set is
   begin
      return
         Get_List
         (  Ptr (Storage),
            To_UTF8 (Prefix),
            To_UTF8 (Suffix),
            Equivalence,
            Parent
         );
   end Get_List;

   function Get_Name
            (  Storage : Storage_Handle;
               Object  : Deposit_Handle
            )  return String is
   begin
      return Get_Name (Ptr (Storage), Object);
   end Get_Name;

   function Get_Parent
            (  Storage : Storage_Handle;
               Object  : Deposit_Handle
            )  return Deposit_Handle is
   begin
      return Get_Parent (Ptr (Storage), Object);
   end Get_Parent;

   procedure Invalidate (Storage : in out Storage_Handle) is
   begin
      Handles.Invalidate (Handles.Handle (Storage));
   end Invalidate;

   function Is_Descendant
            (  Storage : Storage_Handle;
               Object  : Deposit_Handle;
               Parent  : Deposit_Handle
            )  return Boolean is
   begin
      return Is_Descendant (Ptr (Storage), Object, Parent);
   end Is_Descendant;

   function Is_In
            (  Storage : Storage_Handle;
               Object  : Deposit_Handle
            )  return Boolean is
   begin
      return Is_In (Ptr (Storage), Object);
   end Is_In;

   function Is_In
            (  Storage : Storage_Handle;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Boolean is
   begin
      return Is_In (Ptr (Storage), Name, Parent);
   end Is_In;

   function Is_In
            (  Storage : Storage_Handle;
               Name    : Wide_String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Boolean is
   begin
      return Is_In (Ptr (Storage), To_UTF8 (Name), Parent);
   end Is_In;

   function Is_Named
            (  Storage : Storage_Handle;
               Object  : Deposit_Handle
            )  return Boolean is
   begin
      return Is_Named (Ptr (Storage), Object);
   end Is_Named;

   function Is_Valid (Storage : Storage_Handle) return Boolean is
   begin
      return Handles.Is_Valid (Handles.Handle (Storage));
   end Is_Valid;

   function Ptr (Storage : Storage_Handle) return Storage_Object_Ptr is
   begin
      return Handles.Ptr (Handles.Handle (Storage));
   end Ptr;

   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Deposit_Handle;
                Name    : String;
                Parent  : Deposit_Handle := Root_Directory
             )  is
   begin
      Put (Ptr (Storage).all, Object, Name, Parent);
   end Put;

   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Deposit_Handle;
                Name    : Wide_String;
                Parent  : Deposit_Handle := Root_Directory
             )  is
   begin
      Put (Ptr (Storage).all, Object, To_UTF8 (Name), Parent);
   end Put;

   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Deposit_Handle
             )  is
   begin
      Put (Ptr (Storage).all, Object);
   end Put;

   function Ref (Storage: Storage_Object_Ptr) return Storage_Handle is
   begin
      return (Handles.Ref (Storage) with null record);
   end Ref;

   procedure Rename
             (  Storage    : in out Storage_Handle;
                Old_Name   : String;
                Old_Parent : Deposit_Handle := Root_Directory;
                New_Name   : String;
                New_Parent : Deposit_Handle := Root_Directory
             )  is
   begin
      Rename
      (  Ptr (Storage).all,
         Old_Name,
         Old_Parent,
         New_Name,
         New_Parent
      );
   end Rename;

   procedure Rename
             (  Storage    : in out Storage_Handle;
                Old_Name   : Wide_String;
                Old_Parent : Deposit_Handle := Root_Directory;
                New_Name   : Wide_String;
                New_Parent : Deposit_Handle := Root_Directory
             )  is
   begin
      Rename
      (  Ptr (Storage).all,
         To_UTF8 (Old_Name),
         Old_Parent,
         To_UTF8 (New_Name),
         New_Parent
      );
   end Rename;

   procedure Rename
             (  Storage    : in out Storage_Handle;
                Object     : in out Deposit_Handle;
                New_Name   : String;
                New_Parent : Deposit_Handle := Root_Directory
             )  is
   begin
      Rename (Ptr (Storage).all, Object, New_Name, New_Parent);
   end Rename;

   procedure Rename
             (  Storage    : in out Storage_Handle;
                Object     : in out Deposit_Handle;
                New_Name   : Wide_String;
                New_Parent : Deposit_Handle := Root_Directory
             )  is
   begin
      Rename
      (  Ptr (Storage).all,
         Object,
         To_UTF8 (New_Name),
         New_Parent
      );
   end Rename;

   procedure Set
             (  Storage : in out Storage_Handle;
                Object  : Storage_Object_Ptr
             )  is
   begin
      Handles.Set (Handles.Handle (Storage), Object);
   end Set;

   procedure Unname
             (  Storage : in out Storage_Handle;
                Name    : String;
                Parent  : Deposit_Handle := Root_Directory
             )  is
   begin
      Unname (Ptr (Storage).all, Name, Parent);
   end Unname;

   procedure Unname
             (  Storage : in out Storage_Handle;
                Name    : Wide_String;
                Parent  : Deposit_Handle := Root_Directory
             )  is
   begin
      Unname (Ptr (Storage).all, To_UTF8 (Name), Parent);
   end Unname;

   procedure Unname
             (  Storage : in out Storage_Handle;
                Object  : in out Deposit_Handle
             )  is
   begin
      Unname (Ptr (Storage).all, Object);
   end Unname;

   function "=" (Left, Right : Storage_Handle) return Boolean is
   begin
      return
         Handles."=" (Handles.Handle (Left), Handles.Handle (Right));
   end "=";

end Persistent.Handle;
