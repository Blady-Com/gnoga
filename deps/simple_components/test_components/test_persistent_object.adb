--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Persistent_Object                      Luebeck            --
--  Implementation                                 Spring, 2009       --
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

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Strings_Edit;           use Strings_Edit;

package body Test_Persistent_Object is

   Term_Class :          constant String := "Term";
   List_Of_Terms_Class : constant String := "List of terms";

   procedure Add (List : in out List_Of_Terms; Element : Deposit_Ptr) is
   begin
      if not Is_In (List.Set, Element) then
         Add (List.Set, Element);
         declare
            Link : constant Backward_Link_Ptr :=
                      new List_Link (List'Unchecked_Access);
         begin
            Add (List.Links, Link);
            Attach (Link, Element);
         end;
      end if;
   end Add;

   procedure Add
             (  List    : Deposit_Handles.Handle;
                Element : Deposit_Handles.Handle
             )  is
   begin
      if Ptr (List).all in List_Of_Terms'Class then
         Add (List_Of_Terms'Class (Ptr (List).all), Ptr (Element));
      end if;
   end Add;

   function Create_List_Of_Terms
            (  Name : String
            )  return Deposit_Handles.Handle is
      List_Ptr : constant Deposit_Ptr :=
                 new List_Of_Terms (Name'Length);
   begin
      List_Of_Terms (List_Ptr.all).Name := Name;
      return Ref (List_Ptr);
   end Create_List_Of_Terms;

   function Create_Term (Name : String) return Deposit_Handles.Handle is
      Term_Ptr : constant Deposit_Ptr := new Term (Name'Length);
   begin
      Term (Term_Ptr.all).Name := Name;
      return Ref (Term_Ptr);
   end Create_Term;

   procedure Deleted
             (  Link  : in out List_Link;
                Temps : in out Deposit_Container'Class
             )  is
   begin
      Remove (Link.Parent.Set, This (Link));
      Backward_Link_Sets.Remove (Link.Parent.Links, Self (Link));
   end Deleted;

   procedure Destroyed (Link : in out List_Link) is
   begin
      null;
   end Destroyed;

   function Get_Class (Object : Term) return String is
   begin
      return Term_Class;
   end Get_Class;

   function Get_Class (Object : List_Of_Terms) return String is
   begin
      return List_Of_Terms_Class;
   end Get_Class;

   procedure Get_Referents
             (  Object    : List_Of_Terms;
                Container : in out Deposit_Container'Class
             )  is
   begin
      for Index in 1..Get_Size (Object.Set) loop
         Add (Container, Get (Object.Set, Index), True);
      end loop;
   end Get_Referents;

   procedure Get_Referents
             (  Object    : Term;
                Container : in out Deposit_Container'Class
             )  is
   begin
      null;
   end Get_Referents;

   function Image (Item : Deposit_Handles.Handle) return String is
      This : Deposit'Class renames Ptr (Item).all;
   begin
      if This in Term'Class then
         return Term'Class (This).Name;
      elsif This in List_Of_Terms'Class then
         declare
            List : List_Of_Terms'Class renames
                      List_Of_Terms'Class (This);
            Result : Unbounded_String;
         begin
            Append (Result, List.Name);
            for Index in 1..Get_Size (List.Set) loop
               if Index = 1 then
                  Append (Result, " (");
               else
                  Append (Result, " ");
               end if;
               Append (Result, Image (Ref (List.Set, Index)));
            end loop;
            Append (Result, ")");
            return To_String (Result);
         end;
      else
         return "Unknown";
      end if;
   end Image;

   function Is_Modified (Object : List_Of_Terms) return Boolean is
   begin
      return True;
   end Is_Modified;

   function Is_Modified (Object : Term) return Boolean is
   begin
      return True;
   end Is_Modified;

   procedure Reset_Modified (Object : in out List_Of_Terms) is
   begin
      null;
   end Reset_Modified;

   procedure Reset_Modified (Object : in out Term) is
   begin
      null;
   end Reset_Modified;

   procedure Restore_List_Of_Terms
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Object  : out Deposit_Ptr
             )  is
   begin
      Object := new List_Of_Terms (Source'Last - Pointer + 1);
      List_Of_Terms (Object.all).Name := Source (Pointer..Source'Last);
      for Index in 1..Get_Size (List) loop
         Add (List_Of_Terms (Object.all), Get (List, Index));
      end loop;
      Pointer := Source'Last + 1;
   end Restore_List_Of_Terms;

   procedure Restore_Term
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Object  : out Deposit_Ptr
             )  is
   begin
      Object := new Term (Source'Last - Pointer + 1);
      Term (Object.all).Name := Source (Pointer..Source'Last);
      Pointer := Source'Last + 1;
   end Restore_Term;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Object      : Term
             )  is
   begin
      Put (Destination, Pointer, Object.Name);
   end Store;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Object      : List_Of_Terms
             )  is
   begin
      Put (Destination, Pointer, Object.Name);
   end Store;

begin
   Register_Class (Term_Class, Restore_Term'Access);
   Register_Class (List_Of_Terms_Class, Restore_List_Of_Terms'Access);
end Test_Persistent_Object;
