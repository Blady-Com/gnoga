--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Data_Bank.Index                  Luebeck            --
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

with Ada.IO_Exceptions;    use Ada.IO_Exceptions;
with Ada.Tags;             use Ada.Tags;
with Strings_Edit.Quoted;  use Strings_Edit.Quoted;
with Object;               use Object;

package body Persistent.Data_Bank.Index is
   use Ptr_Map;
   use Key_Map;
   use Name_Map;
--
-- Create -- New catalogue record
--
--    List     - The catalogue
--    Object   - The object
--    External - The object's key
--    Name     - The object's name
--    Parent   - The object's parent
--
   procedure Create
             (  List      : in out Catalogue;
                Object    : Deposit_Ptr;
                External  : Key;
                Name      : String;
                Parent    : Catalogue_Record_Ptr
             );
--
-- Delete -- A catalogue record
--
--    List - The catalogue
--    Item - The record to be deleted (a pointer to)
--
   procedure Delete
             (  List : in out Catalogue;
                Item : Catalogue_Record_Ptr
             );
--
-- Notify_Deletion_By_Key -- Notify objects holding backward links
--
--    List   - The catalogue
--    Object - The key of the object being deleted
--
   procedure Notify_Deletion_By_Key
             (  List   : in out Catalogue;
                Object : Key
             );

   function "<" (Left, Right : By_Name_Ptr) return Boolean is
   begin
      if Equal (Right, null) then
         return False;
      elsif Equal (Left, null) then
         return True;
      elsif Left.Parent = Right.Parent then
         return Left.Name < Right.Name;
      else
         return By_Key_Ptr (Left.Parent) < By_Key_Ptr (Right.Parent);
      end if;
   end "<";

   function "<" (Left, Right : By_Entity_Ptr) return Boolean is
   begin
      if Equal (Right, null) then
         return False;
      elsif Equal (Left, null) then
         return True;
      else
         if Left.all in Catalogue_Record'Class then
            if Right.all in Catalogue_Record'Class then
               return
                  Less
                  (  This (Catalogue_Record'Class (Left.all )).all,
                     This (Catalogue_Record'Class (Right.all)).all
                  );
            else
               return
                  Less
                  (  This (Catalogue_Record'Class (Left.all)).all,
                     Right.all
                  );
            end if;
         else
            if Right.all in Catalogue_Record'Class then
               return
                  Less
                  (  Left.all,
                     This (Catalogue_Record'Class (Right.all)).all
                  );
            else
               return Less (Left.all, Right.all);
            end if;
         end if;
      end if;
   end "<";

   function "<" (Left, Right : By_Key_Ptr) return Boolean is
   begin
      if Equal (Right, null) then
         return False;
      elsif Equal (Left, null) then
         return True;
      else
         return Left.External < Right.External;
      end if;
   end "<";

   function "=" (Left, Right : By_Name_Ptr) return Boolean is
   begin
      if Equal (Left, null) then
         return Equal (Right, null);
      elsif Equal (Right, null) then
         return False;
      else
         return
         (  Left.Parent = Right.Parent
         and then
            Left.Name = Right.Name
         );
      end if;
   end "=";

   function "=" (Left, Right : By_Entity_Ptr) return Boolean is
   begin
      if Equal (Left, null) then
         return Equal (Right, null);
      elsif Equal (Right, null) then
         return False;
      else
         if Left.all in Catalogue_Record'Class then
            if Right.all in Catalogue_Record'Class then
               return
                  Equal
                  (  This (Catalogue_Record'Class (Left.all )).all,
                     This (Catalogue_Record'Class (Right.all)).all
                  );
            else
               return
                  Equal
                  (  This (Catalogue_Record'Class (Left.all)).all,
                     Right.all
                  );
            end if;
         else
            if Right.all in Catalogue_Record'Class then
               return
                  Object.Equal
                  (  Left.all,
                     This (Catalogue_Record'Class (Right.all)).all
                  );
            else
               return Object.Equal (Left.all, Right.all);
            end if;
         end if;
      end if;
   end "=";

   function "=" (Left, Right : By_Key_Ptr) return Boolean is
   begin
      if Equal (Left, null) then
         return Equal (Right, null);
      elsif Equal (Right, null) then
         return False;
      else
         return Left.External = Right.External;
      end if;
   end "=";

   procedure Add
             (  List      : in out Catalogue;
                Object    : Deposit_Ptr;
                External  : Key;
                Name      : String;
                Parent    : Key
             )  is
   begin
      if Name'Length = 0 then
         Raise_Exception
         (  Name_Error'Identity,
            "No empty names allowed"
         );
      end if;
      if Parent = Null_Key then
         Create (List, Object, External, Name, null);
      else
         declare
            Index : constant Integer := Find (List, Parent);
         begin
            if Index < 0 then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Parent "
                  &  Image (List.Storage.all, Parent)
                  &  " of "
                  &  Image (List.Storage.all, External)
                  &  " does not persist"
               )  );
            end if;
            declare
               Parent_Ptr : constant Catalogue_Record_Ptr :=
                   Catalogue_Record_Ptr (Get (List.By_Key, Index));
            begin
               if Parent_Ptr.Name_Length = 0 then
                  Raise_Exception
                  (  Name_Error'Identity,
                     (  "Parent "
                     &  Image (List.Storage.all, Parent)
                     &  " of "
                     &  Image (List.Storage.all, External)
                     &  " is anonymous"
                  )  );
               end if;
               Create (List, Object, External, Name, Parent_Ptr);
            end;
         end;
      end if;
   end Add;

   procedure Add
             (  List     : in out Catalogue;
                Object   : Deposit_Ptr;
                External : Key
             )  is
   begin
      Create (List, Object, External, "", null);
   end Add;

   procedure Create
             (  List      : in out Catalogue;
                Object    : Deposit_Ptr;
                External  : Key;
                Name      : String;
                Parent    : Catalogue_Record_Ptr
             )  is
      Index : Integer;
   begin
      Index := Find (List.By_Ptr, By_Entity_Ptr (Object));
      if Index > 0 then
         --
         -- The object already bound. We check if it has the  properties
         -- requested.
         --
         declare
            Item : Catalogue_Record renames
                      Catalogue_Record (Get (List.By_Ptr, Index).all);
         begin
            if Name /= Item.Name then
               Raise_Exception
               (  Name_Error'Identity,
                  (  "Object "
                  &  Image (List.Storage.all, External)
                  &  " is already bound as "
                  &  Quote (Item.Name)
               )  );
            elsif Parent /= Item.Parent then
               Raise_Exception
               (  Name_Error'Identity,
                  (  "Object "
                  &  Image (List.Storage.all, External)
                  &  ", child of "
                  &  Image (List.Storage.all, Item.Parent.External)
                  &  " is already child of "
                  &  Image (List.Storage.all, Parent.External)
               )  );
            elsif External /= Item.External then
               Raise_Exception
               (  Name_Error'Identity,
                  (  "Object "
                  &  Image (List.Storage.all, External)
                  &  " is bound under other key "
                  &  Image (List.Storage.all, Item.External)
               )  );
            end if;
         end;
      else
         --
         -- The object is not bound, so we create a record for it
         --
         declare
            Link : constant Backward_Link_Ptr :=
                      new Catalogue_Record (Name'Length);
            Item : Catalogue_Record renames Catalogue_Record (Link.all);
         begin
            Increment_Count (Item);
            Attach (Link, Object);
            Item.List     := List'Unchecked_Access;
            Item.External := External;
            Item.Name     := Name;
            if Parent /= null then
               Item.Parent := Parent;
               Increment_Count (This (Parent.all).all);
            end if;
            Add (List.By_Ptr, Item'Unchecked_Access);
            Add (List.By_Key, Item'Unchecked_Access);
            if Name'Length /= 0 then
               Add (List.By_Name, Item'Unchecked_Access);
            end if;
         exception
            when others =>
               Delete (List, Item'Unchecked_Access);
               raise;
         end;
      end if;
   end Create;

   procedure Delete
             (  List : in out Catalogue;
                Item : Catalogue_Record_Ptr
             )  is
   begin
      if Item /= null then
         Remove (List.By_Ptr,  By_Entity_Ptr (Item));
         Remove (List.By_Key,  By_Key_Ptr    (Item));
         Remove (List.By_Name, By_Name_Ptr   (Item));
         if Item.Parent /= null then
            declare
               Ptr : Object.Entity_Ptr :=
                  Object.Entity_Ptr (This (Item.Parent.all));
            begin
               Item.Parent := null;
               Object.Release (Ptr);
            end;
         end if;
         Detach (Item.all);
         declare
            Ptr : Object.Entity_Ptr := Object.Entity_Ptr (Item);
         begin
            Object.Release (Ptr);
         end;
      end if;
   end Delete;

   procedure Delete (List : in out Catalogue; ID : Deposit_Ptr) is
      Index : constant Integer := Find (List, ID);
   begin
      if Index > 0 then
         Delete
         (  List,
            Catalogue_Record
            (  Get (List.By_Ptr, Index).all
            ) 'Unchecked_Access
         );
      end if;
   end Delete;

   procedure Delete (List : in out Catalogue; ID : Key) is
      Index : constant Integer := Find (List, ID);
   begin
      if Index > 0 then
         Delete
         (  List,
            Catalogue_Record_Ptr (Get (List.By_Key, Index))
         );
      end if;
   end Delete;

   procedure Delete
             (  List   : in out Catalogue;
                Name   : String;
                Parent : Deposit_Ptr
             )  is
      Index : constant Integer :=
                 Find (List, Name, Get_As (List, Parent, As_Old));
   begin
      if Index > 0 then
         Delete
         (  List,
            Catalogue_Record_Ptr (Get (List.By_Name, Index))
         );
      end if;
   end Delete;

   procedure Delete
             (  List   : in out Catalogue;
                Name   : String;
                Parent : Key
             )  is
      Index : constant Integer :=
                 Find (List, Name, Get_As (List, Parent, As_Old));
   begin
      if Index > 0 then
         Delete
         (  List,
            Catalogue_Record_Ptr (Get (List.By_Name, Index))
         );
      end if;
   end Delete;

   procedure Deleted
             (  Item  : in out Catalogue_Record;
                Temps : in out Deposit_Container'Class
             )  is
   begin
      null;
   end Deleted;

   procedure Destroyed (Item : in out Catalogue_Record) is
      Object : Deposit'Class renames This (Item).all;
   begin
      if Item.Name_Length = 0 then
         --
         -- The  object  is  anonymous. It is a candidate for collection
         -- when no other object uses it.
         --
         case Get_Access_Mode (Item.List.Storage.all) is
            when Read_Only =>
               Raise_Exception
               (  Use_Error'Identity,
                  "Nested transaction requires access upgrade"
               );
            when Read_Write =>
               if Has_Dependants
                  (  Item.List.Storage,
                     Item.External,
                     True
                  )
               then
                  if Is_Modified (Object) then
                     Put (Item.List.Storage.all, Item.External, Object);
                     Reset_Modified (Object);
                  end if;
               else
                  Delete (Item.List.Storage.all, Item.External);
               end if;
            when Fully =>
               declare
                  Mutex : Write_Mutex (Item.List.Storage);
               begin
                  if Has_Dependants
                     (  Item.List.Storage,
                        Item.External,
                        True
                     )
                  then
                     if Is_Modified (Object) then
                        Put
                        (  Item.List.Storage.all,
                           Item.External,
                           Object
                        );
                        Reset_Modified (Object);
                     end if;
                  else
                     Delete (Item.List.Storage.all, Item.External);
                  end if;
                  Commit (Mutex);
               end;
         end case;
      elsif Is_Modified (Object) then
         case Get_Access_Mode (Item.List.Storage.all) is
            when Read_Only =>
               Raise_Exception
               (  Use_Error'Identity,
                  "Nested transaction requires access upgrade"
               );
            when Read_Write =>
               Put (Item.List.Storage.all, Item.External, Object);
            when Fully =>
               declare
                  Mutex : Write_Mutex (Item.List.Storage);
               begin
                  Put (Item.List.Storage.all, Item.External, Object);
                  Commit (Mutex);
               end;
         end case;
         Reset_Modified (Object);
      end if;
      Delete (Item.List.all, Item'Unchecked_Access);
   exception
      when Use_Error =>
         --
         -- Nested transaction, this is an idicator that the  object  is
         -- being  destroyed  upon error recovery which will be followed
         -- by  the  pending transaction roll-back. So the database will
         -- not be changed anyway. The object is silently  removed  from
         -- the index an that is.
         --
         Delete (Item.List.all, Item'Unchecked_Access);
      when Error : others =>
         Delete (Item.List.all, Item'Unchecked_Access);
         On_Error
         (  Item.List.Storage.all,
            "Destroying " & Expanded_Name (Object'Tag),
            Error
         );
   end Destroyed;

   procedure Erase (List : in out Catalogue) is
   begin
      while not Is_Empty (List.By_Key) loop
         Delete
         (  List,
            Catalogue_Record_Ptr (Get (List.By_Key, 1))
         );
      end loop;
   end Erase;

   procedure Finalize (List : in out Catalogue) is
   begin
      Erase (List);
   exception
      when Error : others =>
         On_Error
         (  List.Storage.all,
            "Finalizing catalogue",
            Error
         );
   end Finalize;

   function Find
            (  List   : Catalogue;
               ID     : String;
               Parent : Catalogue_Record_Ptr
            )  return Integer is
      Item : aliased Catalogue_Record (ID'Length);
   begin
      Item.Name   := ID;
      Item.Parent := Parent;
      return Find (List.By_Name, Item'Unchecked_Access);
   end Find;

   function Find (List : Catalogue; ID : Deposit_Ptr) return Integer is
   begin
      return Find (List.By_Ptr, ID.all'Unchecked_Access);
   end Find;

   function Find (List : Catalogue; ID : Key) return Integer is
      Item : aliased Catalogue_Record (0);
   begin
      Item.External := ID;
      return Find (List.By_Key, Item'Unchecked_Access);
   end Find;

   function Get (List : Catalogue; Name : String; Parent : Deposit_Ptr)
      return Key is
      Index : constant Integer :=
                 Find (List, Name, Get_As (List, Parent, As_Old));
   begin
      if Index > 0 then
         return Get (List.By_Name, Index).External;
      end if;
      raise End_Error;
   end Get;

   function Get (List : Catalogue; Name : String; Parent : Key)
      return Key is
      Index : constant Integer :=
                 Find (List, Name, Get_As (List, Parent, As_Old));
   begin
      if Index > 0 then
         return Get (List.By_Name, Index).External;
      end if;
      raise End_Error;
   end Get;

   function Get (List : Catalogue; Name : String; Parent : Deposit_Ptr)
      return Deposit_Ptr is
      Index : constant Integer :=
                 Find (List, Name, Get_As (List, Parent, As_Old));
   begin
      if Index > 0 then
         return This (Get (List.By_Name, Index).all);
      end if;
      raise End_Error;
   end Get;

   function Get (List : Catalogue; Name : String; Parent : Key)
      return Deposit_Ptr is
      Index : constant Integer :=
                 Find (List, Name, Get_As (List, Parent, As_Old));
   begin
      if Index > 0 then
         return This (Get (List.By_Name, Index).all);
      end if;
      raise End_Error;
   end Get;

   function Get (List : Catalogue; ID : Deposit_Ptr) return Key is
      Index : constant Integer := Find (List, ID);
   begin
      if Index > 0 then
         return
            Catalogue_Record
            (  Get (List.By_Ptr, Index).all
            ) .External;
      end if;
      raise End_Error;
   end Get;

   function Get (List : Catalogue; ID : Deposit_Ptr) return String is
      Index : constant Integer := Find (List, ID);
   begin
      if Index > 0 then
         declare
            Item : Catalogue_Record renames
                      Catalogue_Record (Get (List.By_Ptr, Index).all);
         begin
            if Item.Name'Length > 0 then
               return Item.Name;
            end if;
         end;
      end if;
      raise End_Error;
   end Get;

   function Get (List : Catalogue; ID : Key) return String is
      Index : constant Integer := Find (List, ID);
   begin
      if (  Index > 0
         and then
            Get (List.By_Key, Index).Name'Length > 0
         )
      then
         return Get (List.By_Key, Index).Name;
      end if;
      raise End_Error;
   end Get;

   function Get (List : Catalogue; ID : Key) return Deposit_Ptr is
      Index : constant Integer := Find (List, ID);
   begin
      if Index > 0 then
         return This (Get (List.By_Key, Index).all);
      end if;
      raise End_Error;
   end Get;

   function Get_As
            (  List : Catalogue;
               ID   : Deposit_Ptr;
               As   : Query_Mode
            )  return Catalogue_Record_Ptr is
   begin
      if ID = null then
         return null;
      end if;
      declare
         Index : constant Integer := Find (List, ID);
      begin
         if Index < 0 then
            case As is
               when As_New =>
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Parent does not persist"
                  );
               when As_Old =>
                  Raise_Exception
                  (  Data_Error'Identity,
                     "Parent does not persist"
                  );
            end case;
         end if;
         declare
            Result : constant Catalogue_Record_Ptr :=
                     Catalogue_Record_Ptr (Get (List.By_Ptr, Index));
         begin
            if Result.Name_Length = 0 then
               case As is
                  when As_New =>
                     Raise_Exception
                     (  Name_Error'Identity,
                        "Parent is anonymous"
                     );
                  when As_Old =>
                     Raise_Exception
                     (  Data_Error'Identity,
                        "Parent is anonymous"
                     );
               end case;
            end if;
            return Result;
         end;
      end;
   end Get_As;

   function Get_As
            (  List : Catalogue;
               ID   : Key;
               As   : Query_Mode
            )  return Catalogue_Record_Ptr is
   begin
      if ID = Null_Key then
         return null;
      end if;
      declare
         Index : constant Integer := Find (List, ID);
      begin
         if Index < 0 then
            case As is
               when As_New =>
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Parent does not persist"
                  );
               when As_Old =>
                  Raise_Exception
                  (  Data_Error'Identity,
                     "Parent does not persist"
                  );
            end case;
         end if;
         declare
            Result : constant Catalogue_Record_Ptr :=
                     Catalogue_Record_Ptr (Get (List.By_Key, Index));
         begin
            if Result.Name_Length = 0 then
               case As is
                  when As_New =>
                     Raise_Exception
                     (  Name_Error'Identity,
                        "Parent is anonymous"
                     );
                  when As_Old =>
                     Raise_Exception
                     (  Data_Error'Identity,
                        "Parent is anonymous"
                     );
               end case;
            end if;
            return Result;
         end;
      end;
   end Get_As;

   procedure Get_Descendants
             (  List   : Catalogue;
                Item   : Catalogue_Record_Ptr;
                Result : in out Ptr_Map.Set
             )  is
      Child : aliased Catalogue_Record (0);
      Index : Integer;
   begin
      Child.Parent := Item;
      Index := Find (List.By_Name, Child'Unchecked_Access);
      if Index < 0 then
         Index := -Index;
      end if;
      for Position in Index..Get_Size (List.By_Name) loop
         exit when Get (List.By_Name, Position).Parent /= Item;
         Add (Result, By_Entity_Ptr (Get (List.By_Name, Position)));
      end loop;
   end Get_Descendants;

   function Get_New_Parent_Key
            (  List   : Catalogue;
               Parent : Deposit_Ptr
            )  return Key is
   begin
      if Parent = null then
         return Null_Key;
      else
         return Get_As (List, Parent, As_New).External;
      end if;
   end Get_New_Parent_Key;

   function Get_Key (List : Catalogue; ID : Deposit_Ptr) return Key is
   begin
      if ID = null then
         return Null_Key;
      else
         return Get_As (List, ID, As_Old).External;
      end if;
   end Get_Key;

   function Get_Parent (List : Catalogue; ID : Deposit_Ptr)
      return Deposit_Ptr is
      Index : constant Integer := Find (List, ID);
   begin
      if Index > 0 then
         declare
            Item : Catalogue_Record renames
                   Catalogue_Record (Get (List.By_Ptr, Index).all);
         begin
            if Item.Parent = null then
               return null;
            else
               return This (Item.Parent.all);
            end if;
         end;
      end if;
      raise End_Error;
   end Get_Parent;

   function Get_Parent (List : Catalogue; ID : Deposit_Ptr)
      return Key is
      Index : constant Integer := Find (List, ID);
   begin
      if Index > 0 then
         declare
            Item : Catalogue_Record renames
                   Catalogue_Record (Get (List.By_Ptr, Index).all);
         begin
            if Item.Parent = null then
               return Null_Key;
            else
               return Item.Parent.External;
            end if;
         end;
      end if;
      raise End_Error;
   end Get_Parent;

   function Get_Parent (List : Catalogue; ID : Key)
      return Deposit_Ptr is
      Index : constant Integer := Find (List, ID);
   begin
      if Index > 0 then
         declare
            Item : Catalogue_Record'Class renames
                   Catalogue_Record'Class
                   (  Get (List.By_Key, Index).all
                   );
         begin
            if Item.Parent = null then
               return null;
            else
               return This (Item.Parent.all);
            end if;
         end;
      end if;
      raise End_Error;
   end Get_Parent;

   function Get_Parent (List : Catalogue; ID : Key) return Key is
      Index : constant Integer := Find (List, ID);
   begin
      if Index > 0 then
         declare
            Item : Catalogue_Record'Class renames
                   Catalogue_Record'Class
                   (  Get (List.By_Key, Index).all
                   );
         begin
            if Item.Parent = null then
               return Null_Key;
            else
               return Item.Parent.External;
            end if;
         end;
      end if;
      raise End_Error;
   end Get_Parent;

   function Is_Descendant
            (  List   : Catalogue;
               Object : Deposit_Ptr;
               Parent : Deposit_Ptr
            )  return Boolean is
   begin
      if Object = Parent then
         return False;
      elsif Parent = null then
         return Is_In (List, Object);
      elsif Object = null then
         return False;
      end if;
      declare
         Object_Index : Integer;
         Parent_Index : Integer;
         That         : Catalogue_Record_Ptr;
         This         : Catalogue_Record_Ptr;
      begin
         Object_Index := Find (List, Object);
         if Object_Index < 0 then
            return False;
         end if;
         Parent_Index := Find (List, Parent);
         if Object_Index < 0 then
            return False;
         end if;
         That :=
            Catalogue_Record
            (  Get (List.By_Ptr, Object_Index).all
            ) 'Unchecked_Access;
         This :=
            Catalogue_Record
            (  Get (List.By_Ptr, Parent_Index).all
            ) 'Unchecked_Access.Parent;
         while This /= null loop
            if This = That then
               return True;
            end if;
            This := This.Parent;
         end loop;
         return False;
      end;
   end Is_Descendant;

   function Is_Descendant_Or_Same
            (  List   : Catalogue;
               Object : Catalogue_Record_Ptr;
               Parent : Catalogue_Record_Ptr
            )  return Boolean is
   begin
      if Parent = null or else Object = Parent then
         return True;
      end if;
      if Object = null then
         return False;
      end if;
      declare
         This : Catalogue_Record_Ptr := Object.Parent;
      begin
         while This /= null loop
            if This = Parent then
               return True;
            end if;
            This := This.Parent;
         end loop;
         return False;
      end;
   end Is_Descendant_Or_Same;

   function Is_In (List : Catalogue; ID : Key)
      return Boolean is
   begin
      return Find (List, ID) > 0;
   end Is_In;

   function Is_In (List : Catalogue; ID : Deposit_Ptr)
      return Boolean is
   begin
      return Find (List, ID) > 0;
   end Is_In;

   function Is_In (List : Catalogue; ID : String; Parent : Deposit_Ptr)
      return Boolean is
   begin
      return Find (List, ID, Get_As (List, Parent, As_Old)) > 0;
   end Is_In;

   function Is_In (List : Catalogue; ID : String; Parent : Key)
      return Boolean is
   begin
      return Find (List, ID, Get_As (List, Parent, As_Old)) > 0;
   end Is_In;

   procedure Notify_Deletion_By_Key
             (  List   : in out Catalogue;
                Object : Key
             )  is
      Dependants : Deposit_List;
      This       : Deposit_Handle;
   begin
      for Index in 1..Positive'Last loop
         -- For each backward link to the object
         declare
            Link_Key : constant Persistent_Key'Class :=
                       Get_Dependant (List.Storage, Object, Index);
         begin
            if not Is_Valid (This) then
               -- The object is not resident
               This := Get (List.Storage, Object);
            end if;
            Add (Dependants, Get (List.Storage, Key (Link_Key)));
         end;
      end loop;
   exception
      when End_Error =>
         if Is_Valid (This) then
            --
            -- No  more  backward  links.  Now List contains all objects
            -- dependant on this one. Next we notify them about possible
            -- further deletion of the Object.
            --
            Delete (Ptr (This).all);
            Invalidate (This);
         end if;
      when Error : others =>
         On_Error
         (  List.Storage.all,
            "Notifying deletion by key",
            Error
         );
   end Notify_Deletion_By_Key;

   procedure Rename
             (  Store      : in out Data_Bank'Class;
                List       : in out Catalogue;
                Old        : Catalogue_Record_Ptr;
                New_Name   : String;
                New_Parent : Catalogue_Record_Ptr
             )  is
      procedure Rename_Unchecked
                (  Old    : Catalogue_Record_Ptr;
                   Name   : String;
                   Parent : Catalogue_Record_Ptr
                )  is
         Link : constant Backward_Link_Ptr :=
                new Catalogue_Record (Name'Length);
         Item : Catalogue_Record renames Catalogue_Record (Link.all);
      begin
         Increment_Count (Item);
         Item.List     := Old.List;
         Item.External := Old.External;
         Item.Name     := Name;
         if Parent /= null then
            Item.Parent := Parent;
            Increment_Count (This (Parent.all).all);
         end if;
         Attach (Link, This (Old.all));
         Delete (List, Old);
         Add (List.By_Ptr, Item'Unchecked_Access);
         Add (List.By_Key, Item'Unchecked_Access);
         if Name'Length = 0 then
            declare
               Reference : Deposit_Handle;
            begin
               Deposit_Handles.Set (Reference, This (Link.all));
               Delete (Ptr (Reference).all);
            end;
         else
            Add (List.By_Name, Item'Unchecked_Access);
         end if;
      exception
         when others =>
            Delete (List, Item'Unchecked_Access);
            raise;
      end Rename_Unchecked;
   begin
      if Old.Parent = New_Parent and then Old.Name = New_Name then
         return;
      end if;
      if New_Name'Length > 0 then
         --
         -- Renaming
         --
         if Find (List, New_Name, New_Parent) > 0 then
            Raise_Exception
            (  Name_Error'Identity,
               (  "Object "
               &  Quote (New_Name)
               &  " already exists"
            )  );
         end if;
         if Is_Descendant_Or_Same (List, New_Parent, Old) then
            Raise_Exception
            (  Name_Error'Identity,
               (  "Object "
               &  Quote (New_Name)
               &  " is renamed to a self-descendant"
            )  );
         end if;
         if New_Parent = null then
            -- Renamed to root-level
            Rename
            (  Store,
               Old.External,
               New_Name,
               Persistent.Data_Bank.Index.Null_Key
            );
         else
            -- Renamed to have a parent
            Rename (Store, Old.External, New_Name, New_Parent.External);
         end if;
      else
         --
         -- Recursively unnaming all indexed descendants
         --
         declare
            Descendants : Ptr_Map.Set;
         begin
            Get_Descendants (List, Old, Descendants);
            for Index in 1..Get_Size (Descendants) loop
               Rename_Unchecked
               (  Catalogue_Record_Ptr (Get (Descendants, Index)),
                  "",
                  null
               );
            end loop;
         end;
         Unname_By_Key (Store, List, Old.External);
      end if;
      --
      -- Renaming / unnaming in the index
      --
      Rename_Unchecked (Old, New_Name, New_Parent);
   end Rename;

   procedure Rename
             (  Store      : in out Data_Bank'Class;
                List       : in out Catalogue;
                Old_Name   : String;
                Old_Parent : Deposit_Ptr;
                New_Name   : String;
                New_Parent : Deposit_Ptr
             )  is
      Where : constant Catalogue_Record_Ptr :=
              Get_As (List, Old_Parent, As_Old);
      Index : constant Integer := Find (List, Old_Name, Where);
   begin
      if Index > 0 then
         -- The object being renamed is in the index
         Rename
         (  Store,
            List,
            Catalogue_Record_Ptr (Get (List.By_Name, Index)),
            New_Name,
            Get_As (List, New_Parent, As_New)
         );
      elsif Where = null then
         -- The object is not in the index and it is a root-level one
         declare
            Object_Key : constant Key :=
                         Key
                         (  Find
                            (  Store'Unchecked_Access,
                               Old_Name,
                               Persistent.Data_Bank.Index.Null_Key
                         )  );
         begin
            if Null_Key = Object_Key then
               Raise_Exception
               (  End_Error'Identity,
                  "Object " & Old_Name & " does not exist"
               );
            else
               Rename
               (  Store,
                  Object_Key,
                  New_Name,
                  Get_New_Parent_Key (List, New_Parent)
               );
            end if;
         end;
      else
         -- The object is not in the index
         declare
            Object_Key : constant Key :=
                         Key
                         (  Find
                            (  Store'Unchecked_Access,
                               Old_Name,
                               Where.External
                         )  );
         begin
            if Null_Key = Object_Key then
               Raise_Exception
               (  End_Error'Identity,
                  "Object " & Old_Name & " does not exist"
               );
            else
               Rename
               (  Store,
                  Object_Key,
                  New_Name,
                  Get_New_Parent_Key (List, New_Parent)
               );
            end if;
         end;
      end if;
   end Rename;

   procedure Rename
             (  Store      : in out Data_Bank'Class;
                List       : in out Catalogue;
                Object     : Deposit_Ptr;
                New_Name   : String;
                New_Parent : Deposit_Ptr
             )  is
      Index : constant Integer := Find (List, Object);
   begin
      if Index <= 0 then
         raise Constraint_Error;
      end if;
      Rename
      (  Store,
         List,
         Catalogue_Record_Ptr (Get (List.By_Ptr, Index)),
         New_Name,
         Get_As (List, New_Parent, As_New)
      );
   end Rename;

   procedure Unname
             (  Store  : in out Data_Bank'Class;
                List   : in out Catalogue;
                Name   : String;
                Parent : Deposit_Ptr
             )  is
      Where : constant Catalogue_Record_Ptr :=
              Get_As (List, Parent, As_Old);
      Index : constant Integer := Find (List, Name, Where);
   begin
      if Index > 0 then
         -- The object is in the index
         declare
            Item : constant Catalogue_Record_Ptr :=
                      Catalogue_Record_Ptr (Get (List.By_Name, Index));
            Object : Deposit_Handle := Ref (This (Item.all));
         begin
            --
            -- Ensure that the object will not be deleted in the  course
            -- of the notification operation
            --
            Notify_Deletion_By_Key (List, Item.External);
            Rename (Store, List, Item, "", null);
         end;
      else
         -- The object is not in the index
         declare
            function Parent_Key return Key is
            begin
               if Where = null then
                  return Persistent.Data_Bank.Index.Null_Key;
               else
                  return Where.External;
               end if;
            end Parent_Key;
         begin
            declare
               Object_Key : constant Key :=
                            Key (Find (Store'Access, Name, Parent_Key));
            begin
               if Null_Key /= Object_Key then
                  Notify_Deletion_By_Key (List, Object_Key);
                  Unname_By_Key (Store, List, Object_Key);
               end if;
            end;
         exception
            when End_Error =>
               null;
         end;
      end if;
   end Unname;

   procedure Unname
             (  Store  : in out Data_Bank'Class;
                List   : in out Catalogue;
                Object : Deposit_Ptr
             )  is
      Index : constant Integer := Find (List, Object);
   begin
      if Index > 0 then
         Rename
         (  Store,
            List,
            Catalogue_Record_Ptr (Get (List.By_Ptr, Index)),
            "",
            null
         );
      end if;
   end Unname;

   procedure Unname_By_Key
             (  Store : in out Data_Bank'Class;
                List  : in out Catalogue;
                ID    : Key
             )  is
      function Is_Named (This : Key) return Boolean is
         Parent : aliased Key;
      begin
         declare
            Name : String :=
                      Get_Name (Store'Access, This, Parent'Access);
         begin
            return True;
         end;
      exception
         when Name_Error | End_Error =>
            return False;
      end Is_Named;
      Unnamed : Key_Array;
      Removed : Key_Array;
      This    : Key     := ID;
      From    : Integer := Integer'First;
      To      : Integer := Integer'First;
   begin
      loop
         Get_Children (Store, This, Unnamed, To);
         --
         -- Because  the  parent  object is not in the index none of its
         -- successors can be there. For this reason there is no need to
         -- check if  a  child  is  in  the  index  and  handle  it  any
         -- different.
         --
         if Has_Dependants (Store'Access, This, False) then
            --
            -- There are other objects using this one
            --
            Unname (Store, This);
         else
            --
            -- The  object  is  collected. All the objects it refers are
            -- checked for collection
            --
            declare
               From : Integer := Integer'First;
               To   : Integer := Integer'First;
            begin
               Get_References (Store, This, Removed, To);
               Notify_Deletion_By_Key (List, This);
               Delete (Store, This);
               while From < To loop
                  This := Key (Get (Removed, From));
                  From := From + 1;
                  --
                  -- Only the object outside the index should be checked
                  -- for collection.  Memory-resident  objects  will  be
                  -- collected later on destruction.
                  --
                  if not
                     (  Is_In (List, This)
                     or else
                        Is_Named (This)
                     or else
                        Has_Dependants
                        (  Store'Unchecked_Access,
                           This,
                           False
                     )  )
                  then
                     Get_References (Store, This, Removed, To);
                     Notify_Deletion_By_Key (List, This);
                     Delete (Store, This);
                  end if;
               end loop;
            end;
         end if;
         exit when From = To;
         This := Key (Get (Unnamed, From));
         From := From + 1;
      end loop;
   end Unname_By_Key;

end Persistent.Data_Bank.Index;
