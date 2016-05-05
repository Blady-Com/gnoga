--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Persistent_File_Storage                Luebeck            --
--  Test                                           Spring, 2014       --
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

with Object.Archived.Lists;  use Object.Archived.Lists;
with Strings_Edit;           use Strings_Edit;

package body Test_Persistent_File_Storage is

   function "<" (Left, Right : Deposit_Ptr) return Boolean is
   begin
      if Right = null then
         return False;
      elsif Left = null then
         return True;
      else
         return Less (Left.all, Right.all);
      end if;
   end "<";

   procedure Clean_Up is
      File : File_Type;
   begin
      Create (File, Out_File, "test.dat");
      Close (File);
   end Clean_Up;

   procedure Write
             (  Storage : in out File_Storage;
                Object  : Deposit'Class;
                ID      : Key
             )  is
      References  : Deposit_List;
      Data_Record : File_Record;
      Pointer     : Integer := Data_Record.Descriptor'First;
   begin
      Get_Referents (Object, References);
      Data_Record.Count := Get_Size (References);
      for Item in 1..Data_Record.Count loop
         Data_Record.References (Item) :=
            Store (Storage'Access, Ref (References, Item));
      end loop;
      Put (Data_Record.Descriptor, Pointer, Get_Class (Object));
      Put (Data_Record.Descriptor, Pointer, ":");
      Store (Data_Record.Descriptor, Pointer, Object);
      Data_Record.Length := Pointer;
      Write (Storage.File, Data_Record, Count (ID));
   end Write;

   procedure Initialize (Storage : in out File_Storage) is
   begin
      Open (Storage.File, Inout_File, "test.dat");
      Storage.Last_ID := Key (Size (Storage.File));
   end Initialize;

   procedure Finalize (Storage : in out File_Storage) is
   begin
      while not Is_Empty (Storage.Key_To_Record) loop
         declare
            Index_Item : Index_Record'Class renames
               Ptr (Get (Storage.Key_To_Record, Integer'(1))).all;
         begin
            Write (Storage, This (Index_Item).all, Index_Item.ID);
         end;
         Remove (Storage.Key_To_Record, Integer'(1));
         Remove (Storage.Object_To_Record, 1);
      end loop;
      Close (Storage.File);
   end Finalize;

   procedure Bind
             (  Storage : access File_Storage;
                Object  : Deposit_Handle;
                ID      : Key
             )  is
      Link_Ptr : constant Backward_Link_Ptr :=-- No accessibility checks
                 new Index_Record (Storage.all'Unchecked_Access);
      Index_Item : Index_Record renames Index_Record (Link_Ptr.all);
   begin
      Index_Item.ID := ID;
      Attach (Link_Ptr, Ptr (Object));
      Add
      (  Storage.Object_To_Record,
         Ptr (Object),
         Ref (Index_Item'Unchecked_Access)
      );
      Add
      (  Storage.Key_To_Record,
         ID,
         Ref (Index_Item'Unchecked_Access)
      );
   end Bind;

   function Store
            (  Storage : access File_Storage;
               Object  : Deposit_Handle
            )  return Key is
      This : constant Deposit_Ptr := Ptr (Object);
   begin
      if This = null or else not Is_In (Storage.Object_To_Record, This)
      then
         Storage.Last_ID := Storage.Last_ID + 1;
         Bind (Storage, Object, Storage.Last_ID);
         return Storage.Last_ID;
      else
         return Ptr (Get (Storage.Object_To_Record, This)).ID;
      end if;
   end Store;

   function Restore (Storage : access File_Storage; ID : Key)
      return Deposit_Handle is
   begin
      if Is_In (Storage.Key_To_Record, ID) then
         return Ref (This (Ptr (Get (Storage.Key_To_Record, ID)).all));
      else
         --
         -- Read the object from the file
         --
         declare
            Data    : File_Record;
            List    : Deposit_List;
            Object  : Deposit_Ptr;
            Result  : Deposit_Handle;
            Pointer : Positive;
         begin
            Read (Storage.File, Data, Count (ID));
            for No in 1..Data.Count loop
               Add (List, Restore (Storage, Data.References (No)));
            end loop;
            Pointer := Data.Descriptor'First;
            while Data.Descriptor (Pointer) /= ':' loop
               Pointer := Pointer + 1;
            end loop;
            Pointer := Pointer + 1;
            Create
            (  Data.Descriptor,
               Pointer,
               Data.Descriptor (Data.Descriptor'First..Pointer - 2),
               List,
               Object
            );
            Result := Ref (Object);
            Bind (Storage, Result, ID);
            return Result;
         end;
      end if;
   end Restore;

   procedure Deleted
             (  Link  : in out Index_Record;
                Temps : in out Deposit_Container'Class
             )  is
   begin
      null;
   end Deleted;

   procedure Destroyed (Link : in out Index_Record) is
   begin
      Write (Link.Storage.all, This (Link).all, Link.ID);
      Remove (Link.Storage.Object_To_Record, This (Link));
      Remove (Link.Storage.Key_To_Record, Link.ID);
   end Destroyed;

end Test_Persistent_File_Storage;
