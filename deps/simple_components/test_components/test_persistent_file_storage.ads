--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Persistent_File_Storage                Luebeck            --
--  Interface                                      Autumn, 2004       --
--                                                                    --
--                                Last revision :  20:47 23 Jun 2010  --
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

with Ada.Direct_IO;
with Ada.Finalization;
with Generic_Map;
with Object.Handle;

with Object.Archived;  use Object.Archived;
with Deposit_Handles;  use Deposit_Handles;

package Test_Persistent_File_Storage is
   --
   -- File_Storage -- Direct I/O based storage for persistent objects
   --
   type File_Storage is
      new Ada.Finalization.Limited_Controlled with private;
   --
   -- Key -- To reference stored objects = record number 1..
   --
   type Key is new Integer;
   subtype Deposit_Handle is Deposit_Handles.Handle;

   procedure Initialize (Storage : in out File_Storage);
   procedure Finalize (Storage : in out File_Storage);
   procedure Clean_Up;
   function Store
            (  Storage : access File_Storage;
               Object  : Deposit_Handle
            )  return Key;
   function Restore
            (  Storage : access File_Storage;
               ID      : Key
            )  return Deposit_Handle;

private
   --
   -- Index_Record -- One per bound object
   --
   type Index_Record (Storage : access File_Storage) is
      new Backward_Link with
   record
      ID : Key;   -- Object identifier
   end record;
   type Index_Record_Ptr is access all Index_Record'Class;
   --
   -- Implementation of Backward_Link's operation
   --
   procedure Deleted
             (  Link  : in out Index_Record;
                Temps : in out Deposit_Container'Class
             );
   procedure Destroyed (Link : in out Index_Record);
   --
   -- Record_Handles -- Handles to index records
   --
   package Record_Handles is
      new Object.Handle (Index_Record, Index_Record_Ptr);
   use Record_Handles;
   subtype Record_Handle is Record_Handles.Handle;
   --
   -- Map : object pointer -> record handle
   --
   function "<" (Left, Right : Deposit_Ptr) return Boolean;
   package Object_Maps is
      new Generic_Map
          (  Key_Type    => Deposit_Ptr,
             Object_Type => Record_Handle
          );
   use Object_Maps;
   subtype Object_Map is Object_Maps.Map;
   --
   -- Map : object key -> record handle
   --
   package Key_Maps is
      new Generic_Map
          (  Key_Type    => Key,
             Object_Type => Record_Handle
          );
   use Key_Maps;
   subtype Key_Map is Key_Maps.Map;
   --
   -- File record
   --
   type Reference_List is array (Integer range 1..256) of Key;
   type File_Record is record
      Length     : Natural := 0;
      Count      : Natural := 0;
      References : Reference_List;
      Descriptor : String (1..1024);
   end record;
   package Record_Files is new Ada.Direct_IO (File_Record);
   use Record_Files;
   --
   -- File_Storage -- Implementation
   --
   type File_Storage is
      new Ada.Finalization.Limited_Controlled with
   record
      File             : File_Type;
      Object_To_Record : Object_Map;
      Key_To_Record    : Key_Map;
      Last_ID          : Key := 0; -- Last used object key
   end record;

end Test_Persistent_File_Storage;
