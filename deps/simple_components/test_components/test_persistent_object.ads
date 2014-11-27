--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Persistent_Object                      Luebeck            --
--  Interface                                      Spring, 2009       --
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

with Deposit_Handles;       use Deposit_Handles;
with Object.Archived;       use Object.Archived;
with Object.Archived.Sets;  use Object.Archived.Sets;

with Object.Handle.Generic_Set;

package Test_Persistent_Object is
--
-- Add -- Element to the list
--
--    List    - A handle to
--    Element - A handle to
--
   procedure Add (List : Handle; Element : Handle);
--
-- Create_Term -- This function creates a new term
--
--    Name - Identifies the term
--
   function Create_Term (Name : String) return Handle;
--
-- Create_List_Of_Terms -- This function creates a new term
--
--    Name - Identifies the list
--
   function Create_List_Of_Terms (Name : String) return Handle;
--
-- Image -- Item
--
--    Item - A handle to
--
   function Image (Item : Handle) return String;

private
--
-- Term -- Binary tree node type
--
   type Term (Length : Natural) is new Deposit with record
      Name : String (1..Length);
   end record;
--
-- Implementation of Deposit's operations
--
   function Get_Class (Object : Term) return String;
   procedure Get_Referents
             (  Object    : Term;
                Container : in out Deposit_Container'Class
             );
   function Is_Modified (Object : Term) return Boolean;
   procedure Reset_Modified (Object : in out Term);
   procedure Restore_Term
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Object  : out Deposit_Ptr
             );
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Object      : Term
             );

   type List_Of_Terms;
   type List_Link (Parent : access List_Of_Terms'Class) is
      new Backward_Link with null record;
   procedure Deleted
             (  Link  : in out List_Link;
                Temps : in out Deposit_Container'Class
             );
   procedure Destroyed (Link : in out List_Link);
   package Backward_Link_Handles is
      new Object.Handle (Backward_Link'Class, Backward_Link_Ptr);
   use Backward_Link_Handles;
   package Backward_Link_Sets is new Backward_Link_Handles.Generic_Set;
   use Backward_Link_Sets;
--
-- List_Of_Terms -- List referencing to terms through backward links
--
   type List_Of_Terms (Length : Natural) is new Deposit with record
      Name  : String (1..Length);
      Set   : Deposit_Set;
      Links : Backward_Link_Sets.Set;
   end record;
   procedure Add (List : in out List_Of_Terms; Element : Deposit_Ptr);
   function Get_Class (Object : List_Of_Terms) return String;
   procedure Get_Referents
             (  Object    : List_Of_Terms;
                Container : in out Deposit_Container'Class
             );
   function Is_Modified (Object : List_Of_Terms) return Boolean;
   procedure Reset_Modified (Object : in out List_Of_Terms);
   procedure Restore_List_Of_Terms
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Object  : out Deposit_Ptr
             );
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Object      : List_Of_Terms
             );

end Test_Persistent_Object;
