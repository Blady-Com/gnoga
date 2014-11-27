--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Memory_Pools.Streams.            Luebeck            --
--        External_B_Tree.Generic_Table            Autumn, 2014       --
--  Interface                                                         --
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
--
--  This package provides a table allocated in an external storage.  The
--  table  can  be searched by any of the keys identified by the generic
--  formal discrete type Key_Index.  Thus  each  row  of  the  table  is
--  associated with a tuple of unique keys. Each row contains a tuple of
--  values  identified  by the generic formal discrete type Value_Index.
--  The  implementation  is  raw and untyped. All keys and values are of
--  the type Byte_Index.
--
with Ada.Streams;  use Ada.Streams;
with System;       use System;

with Ada.Finalization;

generic
   type Key_Index is (<>);
   type Value_Index is (<>);
package Persistent.Memory_Pools.Streams.External_B_Tree.Generic_Table is
   type Keys_Tuple   is array (Key_Index)   of Byte_Index;
   type Values_Tuple is array (Value_Index) of Byte_Index;
--
-- Update_Handler -- Base type for user-defined row update
--
   type Update_Handler is abstract
      new Ada.Finalization.Limited_Controlled with null record;
--
-- Update -- User-defined callback to modifiy row values
--
--    Handler - The handler object
--    Keys    - The row keys
--    Values  - The row values
--
   procedure Update
             (  Handler : in out Update_Handler;
                Keys    : Keys_Tuple;
                Values  : in out Values_Tuple
             )  is abstract;
------------------------------------------------------------------------
-- Row_Ptr -- Points to a row in the table
--
-- Row  pointers are volatile, a table update operation can  potentially
-- invalidate any pointer.
--
   type Row_Ptr is private;
   No_Row : constant Row_Ptr;
--
-- Get_Bucket_Address -- The address of the row's bucket
--
--    Row - Pointer to the row
--
-- Returns :
--
--    Index of the bucket
--
   function Get_Bucket_Address (Row : Row_Ptr) return Byte_Index;
--
-- Get_Bucket_Size -- The number used slots in the row's bucket
--
--    Row - Pointer to the row
--
-- Returns :
--
--    Number of simbling rows in the same bucket
--
   function Get_Bucket_Size (Row : Row_Ptr) return Natural;
--
-- Get_Index -- The position of the row in its bucket
--
--    Row - Pointer to the row
--
-- Returns :
--
--    The position 1..Get_Bucket_Size
--
-- Exceptions :
--
--    Constraint_Error - No or illegal row
--
   function Get_Index (Row : Row_Ptr) return Positive;
--
-- Get_Key -- The key corresponding to the row
--
--    Row   - Pointer to the row
--    Index - Indicates the key type
--
-- Returns :
--
--    The key
--
-- Exceptions :
--
--    Constraint_Error - No or illegal row
--
   function Get_Key
            (  Row   : Row_Ptr;
               Index : Key_Index
            )  return Byte_Index;
--
-- Get_Keys -- The keys corresponding to the row
--
--    Row - Pointer to the row
--
-- Returns :
--
--    The keys
--
-- Exceptions :
--
--    Constraint_Error - No or illegal row
--
   function Get_Keys (Row : Row_Ptr) return Keys_Tuple;
--
-- Get_Next -- The next row according to the key
--
--    Row   - Pointer to the row
--    Index - Indicates the key type used to ordering items
--
-- Returns :
--
--    A pointer to the next row or no row
--
   function Get_Next
            (  Row   : Row_Ptr;
               Index : Key_Index
            )  return Row_Ptr;
--
-- Get_Previous -- The previous row according to the key
--
--    Row   - Pointer to the row
--    Index - Indicates the key type used to ordering items
--
-- Returns :
--
--    A pointer to the previous row or no row
--
   function Get_Previous
            (  Row   : Row_Ptr;
               Index : Key_Index
            )  return Row_Ptr;
--
-- Get_Root -- The root row of the table according to the key
--
--    Row   - Pointer to the row
--    Index - Indicates the key type used to ordering items
--
-- Returns :
--
--    The first row in the root bucket or no row
--
   function Get_Root
            (  Row   : Row_Ptr;
               Index : Key_Index
            )  return Row_Ptr;
--
-- Get_Value -- The value corresponding to the row
--
--    Row    - Pointer to the row
--    Column - Indicates the column
--
-- Returns :
--
--    The value
--
-- Exceptions :
--
--    Constraint_Error - No or illegal row
--
   function Get_Value
            (  Row    : Row_Ptr;
               Column : Value_Index
            )  return Byte_Index;
--
-- Get_Values -- The values corresponding to the row
--
--    Row - Pointer to the row
--
-- Returns :
--
--    The values corresponding to the row
--
-- Exceptions :
--
--    Constraint_Error - No or illegal row
--
   function Get_Values (Row : Row_Ptr) return Values_Tuple;
--
-- Remove -- The row
--
--    Row      - Pointer to the row
--  [ Values ] - The old values, or 0 if No_Row
--
-- After removal Row is set to No_Row. Nothing happens if Row is already
-- No_Row.  Values when specified is set to  the values from the removed
-- row or 0 if no row removed.
--
-- Exceptions :
--
--    Constraint_Error - Illegal pointer
--
   procedure Remove (Row : in out Row_Ptr);
   procedure Remove (Row : in out Row_Ptr; Values : out Values_Tuple);
--
-- Replace -- Row value in the table
--
--    Row        - Pointer to the row
--    Column     - Indicates the value type
--    Value      - To replace the old value
--  [ Replaced ] - The old value
--
-- This procedure replaces the row value.
--
-- Exceptions :
--
--    Constraint_Error - No row
--
   procedure Replace
             (  Row    : Row_Ptr;
                Column : Value_Index;
                Value  : Byte_Index
             );
   procedure Replace
             (  Row      : Row_Ptr;
                Column   : Value_Index;
                Value    : Byte_Index;
                Replaced : out Byte_Index
             );
--
-- Replace -- Change row values
--
--    Row        - Pointer to the row
--    Values     - To replace the old values
--  [ Replaced ] - The old value
--
-- This procedure replaces the row value.
--
-- Exceptions :
--
--    Constraint_Error - No row
--
   procedure Replace (Row : Row_Ptr; Values : Values_Tuple);
   procedure Replace
             (  Row      : Row_Ptr;
                Values   : Values_Tuple;
                Replaced : out Values_Tuple
             );
--
-- Update -- Change row values
--
--    Row     - Pointer to the row
--    Handler - Update handler
--
-- Exceptions :
--
--    Constraint_Error - No row
--
   procedure Update
             (  Row     : Row_Ptr;
                Handler : in out Update_Handler'Class
             );
------------------------------------------------------------------------
-- Table -- External table object
--
--    Pool - The parent pool
--
   type Table
        (  Pool : access Persistent_Pool'Class
        )  is new Ada.Finalization.Limited_Controlled with private;
--
-- Add -- A new row
--
--    Container - The table to modify
--    Keys      - Of the row to be added
--    Values    - Of the row
--
-- This procedure adds a new row to the table.
--
-- Exceptions :
--
--    Constraint_Error - There  is already a row with a key equal to any
--                       of Keys.
--
   procedure Add
             (  Container : in out Table;
                Keys      : Keys_Tuple;
                Values    : Values_Tuple
             );
--
-- Compare -- The operation used to compare keys
--
--    Container - The table
--    Index     - Indicates the type type
--    Left      - The first key
--    Right     - The second key
--
-- This function  can be overridden  to provide  a different ordering of
-- keys.
--
-- Returns :
--
--    If keys are equal
--
   type Outcome is (Before, Same, After);
   function Compare
            (  Container : Table;
               Index     : Key_Index;
               Left      : Byte_Index;
               Right     : Byte_Index
            )  return Outcome;
--
-- Erase -- Remove all items from the table
--
--    Container - The table to erase
--
   procedure Erase (Container : in out Table);
--
-- Finalize -- Destructor
--
--    Container - The table
--
   procedure Finalize (Container : in out Table);
--
-- Find -- Find an item in the table
--
--    Container - The table
--    Index     - Indicates the type type
--    Key       - To search for
--
-- Returns :
--
--    The pointer to found row or no row
--
   function Find
            (  Container : Table;
               Index     : Key_Index;
               Key       : Byte_Index
            )  return Row_Ptr;
--
-- Get -- Values by a key
--
--    Container - The table
--    Index     - Indicates the key type
--    Key       - The key
--
-- Returns :
--
--    The values corresponding to the key
--
-- Exceptions :
--
--    Contraint_Error - Row not found
--
   function Get
            (  Container : Table;
               Index     : Key_Index;
               Key       : Byte_Index
            )  return Values_Tuple;
--
-- Get -- Value by a key
--
--    Container - The table
--    Index     - Indicates the key type
--    Key       - The key
--    Column    - Indicates the value's column
--
-- Returns :
--
--    The values corresponding to the key
--
-- Exceptions :
--
--    Contraint_Error - Row not found
--
   function Get
            (  Container : Table;
               Index     : Key_Index;
               Key       : Byte_Index;
               Column    : Value_Index
            )  return Byte_Index;
--
-- Get_First -- The first row in the table
--
--    Container - The table
--    Index     - Indicates the key type
--
-- Returns :
--
--    The row with the least key or else no row
--
   function Get_First
            (  Container : Table;
               Index     : Key_Index
            )  return Row_Ptr;
--
-- Get_Last -- The last row in the table
--
--    Container - The table
--    Index     - Indicates the key type
--
-- Returns :
--
--    The row with the biggest key or else no row
--
   function Get_Last
            (  Container : Table;
               Index     : Key_Index
            )  return Row_Ptr;
--
-- Get_Root_Address -- The byte index of the table root
--
--    Container - The table
--
-- The result of this function is the byte index of the root  bucket  of
-- the table. Note that the index may change as the table gets  updated.
-- The table root is set using Set_Root_Address when  the  table  object
-- has  to  be  restored  from  the persistent storage. Typically before
-- object  finalization its actual root is obtained and stored somewhere
-- in  the  persistent  storage.  When the storage is re-opened the root
-- index is read from the storage, a table object is  created  and  then
-- initialized using Set_Root_Address.
--
-- Returns :
--
--    The table root byte index
--
   function Get_Root_Address (Container : Table) return Byte_Index;
--
-- Inf -- Find an row with the key less than or equal to a key
--
--    Container - The table
--    Index     - Indicates the key type
--    Key       - To search for
--
-- Returns :
--
--    The pointer to found row or no row
--
   function Inf
            (  Container : Table;
               Index     : Key_Index;
               Key       : Byte_Index
            )  return Row_Ptr;
--
-- Initialize -- Constructor
--
--    Container - The table
--
   procedure Initialize (Container : in out Table);
--
-- Is_Empty -- Test if the table is empty
--
--    Container - The table
--
-- Returns :
--
--    True if the table contains no rows
--
   function Is_Empty (Container : Table) return Boolean;
--
-- Is_In -- Test if an row is in the table
--
--    Container - The table
--    Index     - Indicates the key type
--    Key       - To be searched for
--
-- Returns :
--
--    True if the row is in the table
--
   function Is_In
            (  Container : Table;
               Index     : Key_Index;
               Key       : Byte_Index
            )  return Boolean;
--
-- Remove -- Remove row from the table
--
--    Container - The table
--    Index     - Indicates the key type
--    Key       - Of the row to be removed
--  [ Keys ]    - The removed keys, or 0's if table was not changed
--  [ Values ]  - The removed values, or 0's if table was not changed
--
-- Nothing happens if the table does not contain a row with Key.
--
   procedure Remove
             (  Container : in out Table;
                Index     : Key_Index;
                Key       : Byte_Index
             );
   procedure Remove
             (  Container : in out Table;
                Index     : Key_Index;
                Key       : Byte_Index;
                Values    : out Values_Tuple
             );
   procedure Remove
             (  Container : in out Table;
                Index     : Key_Index;
                Key       : Byte_Index;
                Keys      : out Keys_Tuple;
                Values    : out Values_Tuple
             );
--
-- Replace -- Replace or add a row in the table
--
--    Container  - The table to be modified
--    Keys       - Of the row to be added / replaced
--    Values     - To be added / replaced
--  [ Replaced ] - The old values if replaced, 0's if added
--
-- This procedure  adds a new row  to the  table or replaces an existing
-- row.
--
   procedure Replace
             (  Container : in out Table;
                Keys      : Keys_Tuple;
                Values    : Values_Tuple
             );
   procedure Replace
             (  Container : in out Table;
                Keys      : Keys_Tuple;
                Values    : Values_Tuple;
                Replaced  : out Values_Tuple
             );
--
-- Replace -- Replace an item value in the table
--
--    Container  - The table to be modified
--    Index      - Indicates the key type
--    Key        - Of the row to be updated
--    Column     - Indicates the value type
--    Value      - To be set
--  [ Replaced ] - The old value
--
-- Exceptions :
--
--    Constraint_Error - No row
--
   procedure Replace
             (  Container : in out Table;
                Index     : Key_Index;
                Key       : Byte_Index;
                Column    : Value_Index;
                Value     : Byte_Index
             );
   procedure Replace
             (  Container : in out Table;
                Index     : Key_Index;
                Key       : Byte_Index;
                Column    : Value_Index;
                Value     : Byte_Index;
                Replaced  : out Byte_Index
             );
--
-- Set_Root_Address -- Set the byte index of the table root
--
--    Container - The table
--    Root      - The byte index of the table root
--
-- This procedure sets the root of the table. See Get_Root_Address.
--
   procedure Set_Root_Address
             (  Container : in out Table;
                Root      : Byte_Index
             );
--
-- Sup -- Find an item that is greater than or equal to the key
--
--    Container - The table
--    Key       - To search for
--
-- Returns :
--
--    The pointer to found row or no row
--
   function Sup
            (  Container : Table;
               Index     : Key_Index;
               Key       : Byte_Index
            )  return Row_Ptr;
--
-- Update -- Change row values
--
--    Container  - The table to be modified
--    Index      - Indicates the key type
--    Key        - Of the row to be updated
--    Handler    - Update handler
--
-- Exceptions :
--
--    Constraint_Error - No row
--
   procedure Update
             (  Container : in out Table;
                Index     : Key_Index;
                Key       : Byte_Index;
                Handler   : in out Update_Handler'Class
             );
private
   pragma Assert
          (  Block_Byte_Size / 8
          >= (  Key_Index'Pos (Key_Index'Last) + 1
             +  Value_Index'Pos (Value_Index'Last) + 1
          )  );
   use Persistent.Memory_Pools.Streams.External_B_Tree;

   type Table_Ptr  is access all Table'Class;

   type Row_Ptr is record
      Table : Table_Ptr;
      Node  : Byte_Index;
      Index : Positive;
   end record;

   type Tree_Array is array (Key_Index) of B_Tree_Ptr;
   type Table
        (  Pool : access Persistent_Pool'Class
        )  is new Ada.Finalization.Limited_Controlled with
   record
      Self    : Table_Ptr  := Table'Unchecked_Access;
      Address : Byte_Index := 0;
      Roots   : Tree_Array;
   end record;
--
-- Unchecked_Add -- Without mutex
--
   procedure Unchecked_Add
             (  Container : in out Table;
                Keys      : Keys_Tuple;
                Values    : Values_Tuple
             );

   No_Row : constant Row_Ptr := (null, 0, 1);

   procedure Set_Keys
             (  Block  : in out Block_Type;
                Offset : in out Block_Offset;
                Keys   : Keys_Tuple
             );
   procedure Set_Values
             (  Block  : in out Block_Type;
                Offset : in out Block_Offset;
                Values : Values_Tuple
             );

   pragma Inline (Add);
   pragma Inline (Get_First);
   pragma Inline (Get_Key);
   pragma Inline (Get_Last);
   pragma Inline (Get_Next);
   pragma Inline (Get_Previous);
   pragma Inline (Get_Value);
   pragma Inline (Is_In);

end Persistent.Memory_Pools.Streams.External_B_Tree.Generic_Table;
