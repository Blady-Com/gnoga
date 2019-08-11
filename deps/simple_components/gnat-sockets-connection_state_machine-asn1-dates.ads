--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Dates                                  Spring, 2019       --
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

with Ada.Calendar.Time_Zones;  use Ada.Calendar;

with GNAT.Sockets.Connection_State_Machine.ASN1.Strings.
     Explicit.Constrained;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Strings.
     Explicit.Constrained;

with GNAT.Sockets.Connection_State_Machine.ASN1.Strings.
     Implicit.Constrained;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Strings.
     Implicit.Constrained;

package GNAT.Sockets.Connection_State_Machine.ASN1.Dates is
--
-- Public_Time_Data_Item -- ASN.1 encoded time, public view
--
   type Public_Time_Data_Item is abstract new ASN1_Data_Item with record
      Value : Time;
   end record;
--
-- Get_Time -- Get value of the data object
--
--    Item - The object
--
-- Returns :
--
--    The current value
--
   function Get_Time
            (  Item : Public_Time_Data_Item
            )  return Time;
--
-- Set_Time -- Set value of the data object
--
--    Item  - The object
--    Value - The value to set
--
   procedure Set_Time
             (  Item  : in out Public_Time_Data_Item;
                Value : Time
             );
--
-- Public_Duration_Data_Item -- ASN.1 encoded duration, public view
--
   type Public_Duration_Data_Item is
      abstract new ASN1_Data_Item with
   record
      Value : Duration;
   end record;
--
-- Get_Duration -- Get value of the data object
--
--    Item - The object
--
-- Returns :
--
--    The current value
--
   function Get_Duration
            (  Item : Public_Duration_Data_Item
            )  return Duration;
--
-- Set_Duration -- Set value of the data object
--
--    Item  - The object
--    Value - The value to set
--
   procedure Set_Duration
             (  Item  : in out Public_Duration_Data_Item;
                Value : Duration
             );
------------------------------------------------------------------------
-- [Implicit_]Date_Data_Item -- ASN.1 encoded date
--
   type Implicit_Date_Data_Item is
      new Public_Time_Data_Item with private;
   type Date_Data_Item is
      new Public_Time_Data_Item with private;
------------------------------------------------------------------------
-- [Implicit_]Date_Time_Data_Item -- ASN.1 encoded date time
--
   type Implicit_Date_Time_Data_Item is
      new Public_Time_Data_Item with private;
   type Date_Time_Data_Item is
      new Public_Time_Data_Item with private;
------------------------------------------------------------------------
-- [Implicit_]Duration_Data_Item -- ASN.1 encoded ISO 8601 duration
--
   type Implicit_Duration_Data_Item is
      new Public_Duration_Data_Item with private;
   type Duration_Data_Item is
      new Public_Duration_Data_Item with private;
------------------------------------------------------------------------
-- [Implicit_]Generalized_Time_Data_Item -- ASN.1  encoded   generalized
--                                          time
--
   type Generalized_Time_Data_Item is
      new Public_Time_Data_Item with private;
   type Implicit_Generalized_Time_Data_Item is
      new Public_Time_Data_Item with private;
------------------------------------------------------------------------
-- [Implicit_]UTC_Data_Item -- ASN.1 encoded UTC time
--
   type Implicit_UTC_Data_Item is
      new Public_Time_Data_Item with private;
   type UTC_Data_Item is
      new Public_Time_Data_Item with private;
------------------------------------------------------------------------
-- [Implicit_]Time_Data_Item -- ASN.1 encoded ISO 8601 time
--
   type Implicit_Time_Data_Item is
      new Public_Time_Data_Item with private;
   type Time_Data_Item is
      new Public_Time_Data_Item with private;
------------------------------------------------------------------------
-- [Implicit_]Time_Of_Day_Data_Item -- ASN.1 encoded time of day
--
   type Implicit_Time_Of_Day_Data_Item is
      new Public_Duration_Data_Item with private;
   type Time_Of_Day_Data_Item is
      new Public_Duration_Data_Item with private;
------------------------------------------------------------------------
--
-- Get -- From stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to read
--    Length  - The encoding length
--    Value   - The result
--
-- The parameter Pointer is advanced beyond the value obtained
--
-- Exceptions :
--
--    End_Error    - Not enough data
--    Layout_Error - Pointer is outside bounds
--    Time_Error   - Invalid time
--
   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Time
             );
--
-- Get -- From stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to read
--    Length  - The encoding length
--    Value   - The result
--
-- The parameter Pointer is advanced beyond the value obtained
--
-- Exceptions :
--
--    End_Error    - Not enough data
--    Layout_Error - Pointer is outside bounds
--    Time_Error   - Invalid time
--
   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Duration
             );
--
-- Get_Date -- From stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to read
--    Length  - The encoding length
--    Value   - The result
--
-- The parameter Pointer is advanced beyond the value obtained
--
-- Exceptions :
--
--    End_Error    - Not enough data
--    Layout_Error - Pointer is outside bounds
--    Time_Error   - Invalid time
--
   procedure Get_Date
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Time
             );
--
-- Get_Date_Time -- From stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to read
--    Length  - The encoding length
--    Value   - The result
--
-- The parameter Pointer is advanced beyond the value obtained
--
-- Exceptions :
--
--    End_Error    - Not enough data
--    Layout_Error - Pointer is outside bounds
--    Time_Error   - Invalid time
--
   procedure Get_Date_Time
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Time
             );
--
-- Get_Duration -- From stream element array ISO 8601
--
--    Data    - The stream element array
--    Pointer - The first element to read
--    Length  - The encoding length
--    Value   - The result
--
-- The parameter Pointer is advanced beyond the value obtained
--
-- Exceptions :
--
--    End_Error    - Not enough data
--    Layout_Error - Pointer is outside bounds
--    Time_Error   - Invalid time
--
   procedure Get_Duration
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Duration
             );
--
-- Get_Time -- From stream element array ISO 8601
--
--    Data    - The stream element array
--    Pointer - The first element to read
--    Length  - The encoding length
--    Value   - The result
--
-- The parameter Pointer is advanced beyond the value obtained
--
-- Exceptions :
--
--    End_Error    - Not enough data
--    Layout_Error - Pointer is outside bounds
--    Time_Error   - Invalid time
--
   procedure Get_Time
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Time
             );
--
-- Get_Time_Of_Day -- From stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to read
--    Length  - The encoding length
--    Value   - The result
--
-- The parameter Pointer is advanced beyond the value obtained
--
-- Exceptions :
--
--    End_Error    - Not enough data
--    Layout_Error - Pointer is outside bounds
--    Time_Error   - Invalid time
--
   procedure Get_Time_Of_Day
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Day_Duration
             );
--
-- Get_UTC_Time -- From stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to read
--    Length  - The encoding length
--    Value   - The result
--
-- The parameter Pointer is advanced beyond the value obtained
--
-- Exceptions :
--
--    End_Error    - Not enough data
--    Layout_Error - Pointer is outside bounds
--    Time_Error   - Invalid time
--
   procedure Get_UTC
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Time
             );
--
-- Put -- Into stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to write
--    Value   - The value to encode
--
-- The parameter Pointer  is advanced beyond the value output.  The data
-- type and length are not output, octet/UTF-8 encoding is used.
--
-- Exceptions :
--
--    End_Error    - No room for output
--    Layout_Error - Pointer is outside bounds
--
   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Time
             );
--
-- Put -- Into stream element array
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
                Value   : Day_Duration
             );
--
-- Put_Date -- Into stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to write
--    Value   - The value to encode
--
-- The parameter Pointer  is advanced beyond the value output.  The data
-- type and length are not output, octet/UTF-8 encoding is used.
--
-- Exceptions :
--
--    End_Error    - No room for output
--    Layout_Error - Pointer is outside bounds
--
   procedure Put_Date
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Time
             );
--
-- Put_Date_Time -- Into stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to write
--    Value   - The value to encode
--
-- The parameter Pointer  is advanced beyond the value output.  The data
-- type and length are not output, octet/UTF-8 encoding is used.
--
-- Exceptions :
--
--    End_Error    - No room for output
--    Layout_Error - Pointer is outside bounds
--
   procedure Put_Date_Time
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Time
             );
--
-- Put_Duration -- Into stream element array ISO 8601
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
   procedure Put_Duration
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Duration
             );
--
-- Put_Time -- Into stream element array ISO 8601
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
   procedure Put_Time
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Time
             );
--
-- Put_Time_Of_Day -- Into stream element array
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
   procedure Put_Time_Of_Day
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Day_Duration
             );
--
-- Put_UTC -- Into stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to write
--    Value   - The value to encode
--
-- The parameter Pointer  is advanced beyond the value output.  The data
-- type and length are not output, octet/UTF-8 encoding is used.
--
-- Exceptions :
--
--    End_Error    - No room for output
--    Layout_Error - Pointer is outside bounds
--
   procedure Put_UTC
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Time
             );
private
   Maximum_Time_Length : constant := 30;
   ---------------------------------------------------------------------
   type Implicit_Date_Data_Item is
      new Public_Time_Data_Item with
   record
      Text : Implicit_Constrained_String_Data_Item (Date_Tag, 10);
   end record;
   procedure Encode
             (  Item    : Implicit_Date_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Date_Data_Item
             );
   for Implicit_Date_Data_Item'Write use Enumerate;
   procedure Feed
             (  Item    : in out Implicit_Date_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   function Get_ASN1_Type
            (  Item : Implicit_Date_Data_Item
            )  return ASN1_Type;
   function Is_Implicit (Item : Implicit_Date_Data_Item)
      return Boolean;
   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Date_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             );

   type Date_Data_Item is
      new Public_Time_Data_Item with
   record
      Text : Constrained_String_Data_Item (Date_Tag, 10);
   end record;
   procedure Encode
             (  Item    : Date_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Date_Data_Item
             );
   for Date_Data_Item'Write use Enumerate;
   procedure Feed
             (  Item    : in out Date_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   function Get_ASN1_Type (Item : Date_Data_Item) return ASN1_Type;
   function Is_Implicit (Item : Date_Data_Item) return Boolean;
   ---------------------------------------------------------------------
   type Implicit_Date_Time_Data_Item is
      new Public_Time_Data_Item with
   record
      Text : Implicit_Constrained_String_Data_Item (Date_Time_Tag, 19);
   end record;
   procedure Encode
             (  Item    : Implicit_Date_Time_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Date_Time_Data_Item
             );
   for Implicit_Date_Time_Data_Item'Write use Enumerate;
   procedure Feed
             (  Item    : in out Implicit_Date_Time_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   function Get_ASN1_Type
            (  Item : Implicit_Date_Time_Data_Item
            )  return ASN1_Type;
   function Is_Implicit (Item : Implicit_Date_Time_Data_Item)
      return Boolean;
   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Date_Time_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             );

   type Date_Time_Data_Item is
      new Public_Time_Data_Item with
   record
      Text : Constrained_String_Data_Item (Date_Time_Tag, 19);
   end record;
   procedure Encode
             (  Item    : Date_Time_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Date_Time_Data_Item
             );
   for Date_Time_Data_Item'Write use Enumerate;
   procedure Feed
             (  Item    : in out Date_Time_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   function Get_ASN1_Type (Item : Date_Time_Data_Item) return ASN1_Type;
   function Is_Implicit (Item : Date_Time_Data_Item) return Boolean;
   ---------------------------------------------------------------------
   type Implicit_Duration_Data_Item is
      new Public_Duration_Data_Item with
   record
      Text : Implicit_Constrained_String_Data_Item
             (  Duration_Tag,
                Maximum_Time_Length
             );
   end record;
   procedure Encode
             (  Item    : Implicit_Duration_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Duration_Data_Item
             );
   for Implicit_Duration_Data_Item'Write use Enumerate;
   procedure Feed
             (  Item    : in out Implicit_Duration_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   function Get_ASN1_Type
            (  Item : Implicit_Duration_Data_Item
            )  return ASN1_Type;
   function Is_Implicit (Item : Implicit_Duration_Data_Item)
      return Boolean;
   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Duration_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             );

   type Duration_Data_Item is
      new Public_Duration_Data_Item with
   record
      Text : Constrained_String_Data_Item
             (  Duration_Tag,
                Maximum_Time_Length
             );
   end record;
   procedure Encode
             (  Item    : Duration_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Duration_Data_Item
             );
   for Duration_Data_Item'Write use Enumerate;
   procedure Feed
             (  Item    : in out Duration_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   function Get_ASN1_Type
            (  Item : Duration_Data_Item
            )  return ASN1_Type;
   function Is_Implicit (Item : Duration_Data_Item) return Boolean;
   ---------------------------------------------------------------------
   type Implicit_Generalized_Time_Data_Item is
      new Public_Time_Data_Item with
   record
      Text : Implicit_Constrained_String_Data_Item
             (  Generalized_Time_Tag,
                Maximum_Time_Length
             );
   end record;
   procedure Encode
             (  Item    : Implicit_Generalized_Time_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Feed
             (  Item    : in out Implicit_Generalized_Time_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Generalized_Time_Data_Item
             );
   for Implicit_Generalized_Time_Data_Item'Write use Enumerate;
   function Get_ASN1_Type
            (  Item : Implicit_Generalized_Time_Data_Item
            )  return ASN1_Type;
   function Is_Implicit (Item : Implicit_Generalized_Time_Data_Item)
      return Boolean;
   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Generalized_Time_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             );

   type Generalized_Time_Data_Item is
      new Public_Time_Data_Item with
   record
      Text : Constrained_String_Data_Item
             (  Generalized_Time_Tag,
                Maximum_Time_Length
             );
   end record;
   procedure Encode
             (  Item    : Generalized_Time_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Generalized_Time_Data_Item
             );
   for Generalized_Time_Data_Item'Write use Enumerate;
   procedure Feed
             (  Item    : in out Generalized_Time_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   function Get_ASN1_Type
            (  Item : Generalized_Time_Data_Item
            )  return ASN1_Type;
   function Is_Implicit (Item : Generalized_Time_Data_Item)
      return Boolean;
   ---------------------------------------------------------------------
   type Implicit_Time_Data_Item is new Public_Time_Data_Item with record
      Text : Implicit_Constrained_String_Data_Item
             (  Time_Tag,
                Maximum_Time_Length
             );
   end record;
   procedure Encode
             (  Item    : Implicit_Time_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Time_Data_Item
             );
   for Implicit_Time_Data_Item'Write use Enumerate;
   procedure Feed
             (  Item    : in out Implicit_Time_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   function Get_ASN1_Type
            (  Item : Implicit_Time_Data_Item
            )  return ASN1_Type;
   function Is_Implicit (Item : Implicit_Time_Data_Item)
      return Boolean;
   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Time_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             );

   type Time_Data_Item is new Public_Time_Data_Item with record
      Text : Constrained_String_Data_Item
             (  Time_Tag,
                Maximum_Time_Length
             );
   end record;
   procedure Encode
             (  Item    : Time_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Time_Data_Item
             );
   for Time_Data_Item'Write use Enumerate;
   procedure Feed
             (  Item    : in out Time_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   function Get_ASN1_Type
            (  Item : Time_Data_Item
            )  return ASN1_Type;
   function Is_Implicit (Item : Time_Data_Item) return Boolean;
   ---------------------------------------------------------------------
   type Implicit_Time_Of_Day_Data_Item is
      new Public_Duration_Data_Item with
   record
      Text : Implicit_Constrained_String_Data_Item (Time_Of_Day_Tag, 8);
   end record;
   procedure Encode
             (  Item    : Implicit_Time_Of_Day_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_Time_Of_Day_Data_Item
             );
   for Implicit_Time_Of_Day_Data_Item'Write use Enumerate;
   procedure Feed
             (  Item    : in out Implicit_Time_Of_Day_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   function Get_ASN1_Type
            (  Item : Implicit_Time_Of_Day_Data_Item
            )  return ASN1_Type;
   function Is_Implicit (Item : Implicit_Time_Of_Day_Data_Item)
      return Boolean;
   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Time_Of_Day_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             );
   procedure Set_Duration
             (  Item  : in out Implicit_Time_Of_Day_Data_Item;
                Value : Duration
             );

   type Time_Of_Day_Data_Item is
      new Public_Duration_Data_Item with
   record
      Text : Constrained_String_Data_Item (Time_Of_Day_Tag, 8);
   end record;
   procedure Encode
             (  Item    : Time_Of_Day_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Time_Of_Day_Data_Item
             );
   for Time_Of_Day_Data_Item'Write use Enumerate;
   procedure Feed
             (  Item    : in out Time_Of_Day_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   function Get_ASN1_Type
            (  Item : Time_Of_Day_Data_Item
            )  return ASN1_Type;
   function Is_Implicit (Item : Time_Of_Day_Data_Item) return Boolean;
   procedure Set_Duration
             (  Item  : in out Time_Of_Day_Data_Item;
                Value : Duration
             );
   ---------------------------------------------------------------------
   type Implicit_UTC_Data_Item is new Public_Time_Data_Item with record
      Text : Implicit_Constrained_String_Data_Item
             (  UTC_Time_Tag,
                Maximum_Time_Length
             );
   end record;
   procedure Encode
             (  Item    : Implicit_UTC_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Implicit_UTC_Data_Item
             );
   for Implicit_UTC_Data_Item'Write use Enumerate;
   procedure Feed
             (  Item    : in out Implicit_UTC_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   function Get_ASN1_Type
            (  Item : Implicit_UTC_Data_Item
            )  return ASN1_Type;
   function Is_Implicit (Item : Implicit_UTC_Data_Item)
      return Boolean;
   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_UTC_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             );

   type UTC_Data_Item is new Public_Time_Data_Item with record
      Text : Constrained_String_Data_Item
             (  UTC_Time_Tag,
                Maximum_Time_Length
             );
   end record;
   procedure Encode
             (  Item    : UTC_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : UTC_Data_Item
             );
   for UTC_Data_Item'Write use Enumerate;
   procedure Feed
             (  Item    : in out UTC_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   function Get_ASN1_Type (Item : UTC_Data_Item) return ASN1_Type;
   function Is_Implicit (Item : UTC_Data_Item) return Boolean;
   ---------------------------------------------------------------------
   function Get (Source : String; Short : Boolean) return Time;
   function Get (Source : String) return Day_Duration;
   function Get_Date (Source : String) return Time;
   function Get_Date_Time (Source : String) return Time;
   function Get_Time_Of_Day (Source : String) return Day_Duration;
   function Get_Zoned_Time
            (  Source  : String;
               Year    : Integer;
               Month   : Integer;
               Day     : Integer;
               Seconds : Duration
            )  return Time;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Short       : Boolean;
                Value       : Time
             );
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Day_Duration
             );
   procedure Put_Date
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Time
             );
   procedure Put_Date_Time
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Time
             );
   procedure Put_Time_Of_Day
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Day_Duration
             );
end GNAT.Sockets.Connection_State_Machine.ASN1.Dates;
