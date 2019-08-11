--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.X509_Certificates                      Summer, 2019       --
--  Implementation                                                    --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with Ada.Tags;           use Ada.Tags;

with GNAT.Sockets.Connection_State_Machine.ASN1.Tagged_Values;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Tagged_Values;

package body GNAT.Sockets.Connection_State_Machine.ASN1.
             X509_Certificates is
   use ASN1.Choices;
   use ASN1.Dates;
   use ASN1.Sequences;
   use ASN1.Strings;

   function "<"
            (  Left, Right : Attribute_Type_And_Value
            )  return Boolean is
   begin
      return
      (  Left.OID < Right.OID
      or else
         (  Left.OID < Right.OID
         and then
            Left.Value < Right.Value
      )  );
   end "<";

   procedure Feed
             (  Item    : in out Extension_Type;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State = 0 then
         ASN1.Booleans.Set_Value (Item.Critical, False); -- The default
      end if;
      Feed
      (  Item    => Tagged_Sequence_Data_Item (Item),
         Data    => Data,
         Pointer => Pointer,
         Client  => Client,
         State   => State
      );
   end Feed;

   function Get
            (  Data : Attribute_Type_And_Value_Data_Item
            )  return Attribute_Type_And_Value is
      OID   : constant Object_Identifier := Get_Value (Data.OID);
      Value : constant String            := Get_Value (Data.Value);
   begin
      return (OID'Length, Value'Length, OID, Value);
   end Get;

   function Get
            (  Data : Attribute_Type_And_Value_Data_Item_Set
            )  return Attribute_Type_And_Value_Sets.Set is
      use Attribute_Type_And_Value_Sets;
      Result : Attribute_Type_And_Value_Sets.Set;
   begin
      for Index in 1..Get_Length (Data) loop
         Add (Result, Get (Data, Index));
      end loop;
      return Result;
   end Get;

   function Get (Data : Extension_Type) return String is
   begin
      return Get_Value (Data.Value);
   end Get;

   procedure Initialized (Algorithm : in out Algorithm_Identifier) is
   begin
      Set_Tag (Algorithm, 1, (Universal_Tag, 0, False));
      Set_Tag
      (  Item      => Algorithm,
         Index     => 2,
         Tag       => (Universal_Tag, Null_Tag, True),
         Universal => True
      );
   end Initialized;

   procedure Initialized (Data : in out Extension_Type) is
   begin
      Set_Tag (Data, 1, (Universal_Tag, 0, False));
      Set_Tag (Data, 2, (Universal_Tag, 0, True));
      Set_Tag (Data, 3, (Universal_Tag, 0, False));
   end Initialized;

   procedure Initialized (Certificate : in out TBS_Certificate) is
   begin
      Set_Tag (Certificate,  1, (Context_Specific_Tag, 0, False));
      Set_Tag (Certificate,  2, (Universal_Tag,        0, False));
      Set_Tag (Certificate,  3, (Universal_Tag,        0, False));
      Set_Tag (Certificate,  4, (Universal_Tag,        0, False));
      Set_Tag (Certificate,  5, (Universal_Tag,        0, False));
      Set_Tag (Certificate,  6, (Universal_Tag,        0, False));
      Set_Tag (Certificate,  7, (Universal_Tag,        0, False));
      Set_Tag (Certificate,  8, (Context_Specific_Tag, 1, True));
      Set_Tag (Certificate,  9, (Context_Specific_Tag, 2, True));
      Set_Tag (Certificate, 10, (Context_Specific_Tag, 3, True));
   end Initialized;

   procedure Initialized (Key_Info : in out Subject_Public_Key_Info) is
   begin
      Set_Tag (Key_Info, 1, (Universal_Tag, 0, False));
      Set_Tag (Key_Info, 2, (Universal_Tag, 0, False));
   end Initialized;

   procedure Initialized (Stamp : in out Time_Type) is
   begin
      Set_Tag
      (  Stamp,
         1,
         (Universal_Tag, UTC_Time_Tag, True)
      );
      Set_Tag
      (  Stamp,
         2,
         (Universal_Tag, Generalized_Time_Tag, True)
      );
   end Initialized;

   procedure Initialized (Certificate : in out X509_Certificate) is
   begin
      Set_Tag (Certificate, 1, (Universal_Tag, 0, False));
      Set_Tag (Certificate, 2, (Universal_Tag, 0, False));
      Set_Tag (Certificate, 3, (Universal_Tag, 0, False));
   end Initialized;

   procedure Set
             (  Data  : in out Attribute_Type_And_Value_Data_Item;
                Value : Attribute_Type_And_Value
             )  is
   begin
      Set_Value (Data.OID,   Value.OID);
      Set_Value (Data.Value, Value.Value);
   end Set;

   procedure Set
             (  Data  : in out Attribute_Type_And_Value_Data_Item_Set;
                Value : Attribute_Type_And_Value_Sets.Set
             )  is
      use Attribute_Type_And_Value_Sets;
   begin
      for Index in 1..Get_Size (Value) loop
         Append (Data, Get (Value, Index));
      end loop;
   end Set;

   procedure Set (Data : in out Extension_Type; Value : String) is
   begin
      Set_Value (Data.Value, Value);
   end Set;

end GNAT.Sockets.Connection_State_Machine.ASN1.X509_Certificates;
