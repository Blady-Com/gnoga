--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Persistent_Tree                        Luebeck            --
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

with Ada.Text_IO;            use Ada.Text_IO;
with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

package body Test_Persistent_Tree is
   Class : constant String := "Node"; -- The class of

   function Nothing return Handle is
      None : Handle;
   begin
      return None;
   end Nothing;

   function Create_Node
            (  Field : Integer;
               Left  : Handle := Nothing;
               Right : Handle := Nothing
            )  return Handle is
      Node_Ptr : constant Deposit_Ptr := new Node;
      Object   : Node renames Node (Node_Ptr.all);
   begin
      Object.Field := Field;
      Object.Left  := Left;
      Object.Right := Right;
      return Ref (Node_Ptr);
   end Create_Node;

   function Get_Class (Object : Node) return String is
   begin
      return Class;
   end Get_Class;

   procedure Get_Referents
             (  Object    : Node;
                Container : in out Deposit_Container'Class
             )  is
   begin
      if Is_Valid (Object.Left) then
         Add (Container, Object.Left);
      end if;
      if Is_Valid (Object.Right) then
         Add (Container, Object.Right);
      end if;
   end Get_Referents;

   function Is_Modified (Object : Node) return Boolean is
   begin
      return True; -- Save it always, do not care about performance
   end Is_Modified;

   procedure Reset_Modified (Object : in out Node) is
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
      Field : Integer;
      Left  : Handle;
      Right : Handle;
   begin
      if Source (Pointer) = '<' then
         Left := Ref (List, 1);
         if Source (Pointer + 1) = '>' then
            Right := Ref (List, 2);
         end if;
      elsif Source (Pointer + 1) = '>' then
         Right := Ref (List, 1);
      end if;
      Pointer := Pointer + 2;
      Get (Source, Pointer, Field);
      Object := new Node;
      declare
         Item : Node renames Node (Object.all);
      begin
         Item.Field := Field;
         Item.Left  := Left;
         Item.Right := Right;
      end;
   exception
      when others =>
         raise Data_Error;
   end Restore;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Object      : Node
             )  is
   begin
      if Is_Valid (Object.Left) then
         Put (Destination, Pointer, "<");
      else
         Put (Destination, Pointer, "-");
      end if;
      if Is_Valid (Object.Right) then
         Put (Destination, Pointer, ">");
      else
         Put (Destination, Pointer, "-");
      end if;
      Put (Destination, Pointer, Object.Field);
   end Store;

   procedure Print (Root : Handle; Indentation : String := "") is
   begin
      if Is_Valid (Root) then
         declare
            The_Node : Node renames Node (Ptr (Root).all);
         begin
            Put_Line (Indentation & "\_" & Image (The_Node.Field));
            Print (The_Node.Left,  Indentation & "  |");
            Print (The_Node.Right, Indentation & "   ");
         end;
      else
         Put_Line (Indentation & "\_*");
      end if;
   end Print;

begin
   Register_Class (Class, Restore'Access);
end Test_Persistent_Tree;
