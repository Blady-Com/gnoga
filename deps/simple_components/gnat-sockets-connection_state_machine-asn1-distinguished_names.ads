--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Distinguished_Names                    Summer, 2019       --
--  Interface                                                         --
--                                Last revision :  13:37 03 Aug 2019  --
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
--
--  ASN.1  encoding  of RFC 4514 distinguished names. The implementation
--  uses ASN.1 strings.
--
with GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Explicit;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Explicit;

with GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Implicit;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Implicit;

with Strings_Edit.Distinguished_Names;
use  Strings_Edit.Distinguished_Names;

package GNAT.Sockets.Connection_State_Machine.ASN1.
        Distinguished_Names is
--
-- [Implicit_]DN_Data_Item -- ASN.1 encoded distinguished name
--
   type DN_Data_Item is new String_Data_Item with private;
   type Implicit_DN_Data_Item is
      new Implicit_String_Data_Item with private;
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out DN_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   procedure Feed
             (  Item    : in out Implicit_DN_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Get_Name -- The current distinguished name
--
--    Item - The item
--
-- Returns :
--
--    The current name
--
-- Exceptions :
--
--    Data_Error - Invalid value
--    End_Error  - No name
--
   function Get_Name
            (  Item : DN_Data_Item
            )  return Distinguished_Name;
   function Get_Name
            (  Item : Implicit_DN_Data_Item
            )  return Distinguished_Name;
--
-- Set_Name -- Set the current distinguished name
--
--    Item - The item
--    Name - The name to set
--
-- Exceptions :
--
--    Constraint_Error - The value is too large to store
--
   procedure Set_Name
             (  Item : in out DN_Data_Item;
                Name : Distinguished_Name
             );
   procedure Set_Name
             (  Item : in out Implicit_DN_Data_Item;
                Name : Distinguished_Name
             );
------------------------------------------------------------------------
--
-- [Implicit_]External_DN_Data_Item -- ASN.1 distinguished name
--
   type External_DN_Data_Item is
      new External_String_Data_Item with private;
   type Implicit_External_DN_Data_Item is
      new Implicit_External_String_Data_Item with private;
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out External_DN_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   procedure Feed
             (  Item    : in out Implicit_External_DN_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Get_Name -- The current distinguished name
--
--    Item - The item
--
-- Returns :
--
--    The current value
--
-- Exceptions :
--
--    End_Error  - No name
--    Data_Error - Invalid value
--
   function Get_Name
            (  Item : External_DN_Data_Item
            )  return Distinguished_Name;
   function Get_Name
            (  Item : Implicit_External_DN_Data_Item
            )  return Distinguished_Name;
--
-- Set_Name -- Set the current distinguished name
--
--    Item - The item
--    Name - The name to set
--
-- Exceptions :
--
--    Storage_Error - The value is too large to store
--    Use_Error     - The object was not initialized
--
   procedure Set_Name
             (  Item : in out External_DN_Data_Item;
                Name : Distinguished_Name
             );
   procedure Set_Name
             (  Item : in out Implicit_External_DN_Data_Item;
                Name : Distinguished_Name
             );
------------------------------------------------------------------------
-- Get -- From stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to read
--    Length  - The encoding length
--
-- Returns :
--
--   The distinguished name
--
-- Exceptions :
--
--    Data_Error   - Syntax error
--    End_Error    - Not enough data
--    Layout_Error - Pointer is outside bounds
--
   function Get
            (  Data    : Stream_Element_Array;
               Pointer : access Stream_Element_Offset;
               Length  : Stream_Element_Offset
            )  return Distinguished_Name;
--
-- Put -- Into stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to write
--    Name    - The name to encode
--
-- The parameter Pointer  is advanced beyond the value output.  The data
-- type is not output, octet/UTF-8 encoding is used.
--
-- Exceptions :
--
--    End_Error    - No room for output
--    Layout_Error - Pointer is outside bounds or no room for output
--
   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Name    : Distinguished_Name
             );
private
   type Distinguished_Name_Ptr is access all Distinguished_Name;
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Distinguished_Name,
             Distinguished_Name_Ptr
          );

   type External_DN_Data_Item is
      new External_String_Data_Item with null record;

   type DN_Data_Item is new String_Data_Item with null record;

   type Implicit_DN_Data_Item is
      new Implicit_String_Data_Item with null record;

   type Implicit_External_DN_Data_Item is
      new Implicit_External_String_Data_Item with null record;

end GNAT.Sockets.Connection_State_Machine.ASN1.Distinguished_Names;
