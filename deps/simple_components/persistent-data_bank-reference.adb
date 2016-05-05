--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Data_Bank.Reference              Luebeck            --
--  Implementation                                 Autumn, 2004       --
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

package body Persistent.Data_Bank.Reference is
   use Class_Map;

   Map : Table;

   procedure Finalize (Reference : in out Self_Reference) is
   begin
      Close (Reference);
      Free (Reference.Key);
      Finalize (Deposit (Reference));
   end Finalize;

   function Get_Class (Object : Self_Reference) return String is
   begin
      return Class;
   end Get_Class;

   function Is_Modified (Reference : Self_Reference) return Boolean is
   begin
      return False;
   end Is_Modified;

   procedure Add
             (  List    : in out Deposit_Container'Class;
                Storage : Storage_Handle;
                Key     : Persistent_Key'Class
             )  is
      Result : Deposit_Ptr := new Self_Reference;
      This   : Self_Reference renames Self_Reference (Result.all);
   begin
      This.Storage := Storage;
      This.Key     := new Persistent_Key'Class'(Key);
      Add (List, Result, False);
   exception
      when others =>
         Free (Result);
         raise;
   end Add;

   function Image (Object : Self_Reference) return String is
   begin
      return "";
   end Image;

   procedure Register (Class : String; Constructor : Create) is
   begin
      Add (Map, Class, Constructor);
   end Register;

   procedure Reset_Modified (Reference : in out Self_Reference) is
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
      Object := null;
   end Restore;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Object      : Self_Reference
             )  is
   begin
      null;
   end Store;

   function Value
            (  Source  : String;
               Class   : String;
               Storage : Storage_Handle;
               Key     : Persistent_Key'Class
            )  return Deposit_Handle is
      Index : constant Integer := Locate (Map, Class);
   begin
      if Index > 0 then
         return GetTag (Map, Index) (Source, Storage, Key);
      else
         declare
            Result : constant Deposit_Handle :=
                     Ref (new Self_Reference);
            This   : Self_Reference renames
                     Self_Reference (Ptr (Result).all);
         begin
            This.Storage := Storage;
            This.Key     := new Persistent_Key'Class'(Key);
            return Result;
         end;
      end if;
   end Value;

begin
   Register_Class (Class, Restore'Access);
end Persistent.Data_Bank.Reference;
