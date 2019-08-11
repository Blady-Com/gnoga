--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Lengths                                Spring, 2019       --
--  Interface                                                         --
--                                Last revision :  18:41 01 Aug 2019  --
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
--  ASN.1 encoding of length
--
package GNAT.Sockets.Connection_State_Machine.ASN1.Lengths is
--
-- Length_Data_Item -- ASN.1 encoded length
--
   type Length_Data_Item is new Data_Item with record
      Value : Stream_Element_Count;
   end record;
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out Length_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
------------------------------------------------------------------------
--
-- Measured_Data_Item -- ASN.1 encoded length
--
   type Measured_Data_Item is new Data_Item with private;
   function Get_Length
            (  Item : Measured_Data_Item
            )  return Stream_Element_Count;
   procedure Call
             (  Item    : in out Measured_Data_Item;
                Callee  : in out Data_Item'Class;
                Tag     : Tag_Type;
                Pointer : Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- End_Of_Subsequence -- ..Connection_State.State_Machine..
--
   procedure End_Of_Subsequence
             (  Item    : in out Measured_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Enumerate -- Fake stream I/O procedure
--
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Measured_Data_Item
             );
   for Measured_Data_Item'Write use Enumerate;
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out Measured_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Set_Untagged -- Set tagged mode
--
--    Item     - The item
--    Untagged - Set the tagged mode off/on
--
   procedure Set_Untagged
             (  Item     : in out Measured_Data_Item;
                Untagged : Boolean
             );
------------------------------------------------------------------------
--
-- Generic_Put -- Put an definite encoding into a string
--
--    Value_Type - The Ada type
--    Put        - The procedure to output implicitly encoded value
--
-- Parameters :
--
--    Data        - The buffer containing the encoding
--    Pointer     - To advance
--    Tag         - The ASN.1 type
--    Constructed - Constructed flag
--    Value       - The value
--
-- Exceptions :
--
--    End_Error    - No room for output
--    Layout_Error - The pointer is n ot in Data'First..Data'Last + 1 or
--                   Last not in Pointer - 1..Data'Last
--
   generic
      type Value_Type (<>) is limited private;
      with procedure Put
                     (  Data    : in out Stream_Element_Array;
                        Pointer : in out Stream_Element_Offset;
                        Value   : Value_Type
                     )  is <>;
   procedure Generic_Put
             (  Data        : in out Stream_Element_Array;
                Pointer     : in out Stream_Element_Offset;
                Tag         : ASN1_Type;
                Value       : Value_Type;
                Constructed : Boolean := False
             );
--
-- Definite_Encode -- Put an definite encoding into a string
--
--    Item    - The data item
--    Data    - The buffer containing the encoding
--    Pointer - To advance
--
-- The procedure puts length of the following data item encoding and the
-- encoding itself.
--
-- Exceptions :
--
--    End_Error    - No room for output
--    Layout_Error - The pointer is n ot in Data'First..Data'Last + 1 or
--                   Last not in Pointer - 1..Data'Last
--
   procedure Definite_Encode
             (  Item    : Abstract_ASN1_Data_Item'Class;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Explicit_Encode -- Put an definite encoding into a string
--
--    Item    - The data item
--    Data    - The buffer containing the encoding
--    Pointer - To advance
--
-- The procedure puts  the ASN.1 data item tag,  length of the following
-- data item encoding and the encoding itself.
--
-- Exceptions :
--
--    End_Error    - No room for output
--    Layout_Error - The pointer is n ot in Data'First..Data'Last + 1 or
--                   Last not in Pointer - 1..Data'Last
--
   generic
      type Data_Item_Type (<>) is
         abstract new ASN1_Data_Item with private;
      with procedure Encode
                     (  Item    : Data_Item_Type;
                        Data    : in out Stream_Element_Array;
                        Pointer : in out Stream_Element_Offset
                     )  is <>;
   procedure Generic_Explicit_Encode
             (  Item    : Data_Item_Type;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Get -- ASN.1 length from stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to read
--    Value   - The result
--
-- The parameter Pointer is advanced beyond the value obtained
--
-- Exceptions :
--
--    Constraint_Error - The ASN.1 length is too large
--    Data_Error       - Invalid ASN.1 length
--    End_Error        - Not enough data
--    Layout_Error     - Pointer is outside bounds
--
   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Stream_Element_Count
             );
--
-- Put -- ASN.1 length into stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to write
--    Value   - The value to encode
--
-- The parameter Pointer is advanced beyond the value output
--
-- Exceptions :
--
--    End_Error    - No room for output
--    Layout_Error - Pointer is outside bounds
--
   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Stream_Element_Count
             );
------------------------------------------------------------------------
-- Input of ASN.1 length from a custom Feed procedure
--
-- Embedded_Feed -- To be called from Feed to get length
--
--    Data    - The stream element array
--    Pointer - The first element to read
--    State   - The state
--
-- The embedded feed utilizes  negative values of  State for input ASN.1
-- length. The length is input as follows. From custom Feed:
--
--    State := Start_Length;
--    while not Is_Length_Ready (State) ... loop
--       Embedded_Feed (..., State);
--    end loop;
--    Length := Get_Length (State);
--
-- All positive and zero values of State remain free for other uses.
--
   procedure Embedded_Feed
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                State   : in out Stream_Element_Offset
             );
--
-- Is_Indefinite -- If the embedded length is indefinite
--
--    State - The current state
--
-- The result is always False if State is positive or zero.
--
-- Returns :
--
--    True if the length is ready and indefinite
--
   function Is_Indefinite
            (  State : Stream_Element_Offset
            )  return Boolean;
--
-- Is_Length_Ready -- If the embedded length is ready to get
--
--    State - The current state
--
-- The result is always False if State is positive or zero.
--
-- Returns :
--
--    True if the length is ready
--
   function Is_Length_Ready
            (  State : Stream_Element_Offset
            )  return Boolean;
--
-- Get_Length -- If the embedded length is ready to get
--
--    State - The current state
--
-- Returns :
--
--    The length
--
   function Get_Length
            (  State : Stream_Element_Offset
            )  return Stream_Element_Count;
--
-- Start_Length -- Initial value to begin length input
--
-- Returns :
--
--    The initial value
--
   function Start_Length return Stream_Element_Offset;

private
   type Measured_Data_Item is new Data_Item with record
      Definite  : Boolean := False;
      Untagged  : Boolean := False;
      Tag       : Tag_Type;
      Length    : Stream_Element_Offset;
      Total     : Stream_Element_Count;
      Fed_Count : Unsigned_64;
      List      : aliased Sequence (1);
      Self      : aliased Sequence (1);
   end record;

end GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;
