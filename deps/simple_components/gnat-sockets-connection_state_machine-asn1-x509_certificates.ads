--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.X509_Certificate                       Summer, 2019       --
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

with Generic_Indefinite_Set;
with GNAT.Sockets.Connection_State_Machine.ASN1.Bit_Strings.Implicit;
with GNAT.Sockets.Connection_State_Machine.ASN1.Booleans;
with GNAT.Sockets.Connection_State_Machine.ASN1.Choices;
with GNAT.Sockets.Connection_State_Machine.ASN1.Dates;
with GNAT.Sockets.Connection_State_Machine.ASN1.Indefinite_Unsigneds;
with GNAT.Sockets.Connection_State_Machine.ASN1.Object_Identifiers;
with GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.Explicit;
with GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.Implicit;
with GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.
     Generic_Sequence_Of;
with GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Generic_Set_Of;
with GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Explicit.
     Constrained;
with GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Implicit.
     Constrained;
with GNAT.Sockets.Connection_State_Machine.ASN1.Unsigneds_8;
with Strings_Edit.Object_Identifiers;

package GNAT.Sockets.Connection_State_Machine.ASN1.X509_Certificates is
   use ASN1.Bit_Strings.Implicit;
   use ASN1.Indefinite_Unsigneds;
   use ASN1.Object_Identifiers;
   use ASN1.Sequences.Explicit;
   use ASN1.Sequences.Implicit;
   use ASN1.Strings.Explicit;
   use ASN1.Strings.Explicit.Constrained;
   use ASN1.Strings.Implicit;
   use ASN1.Strings.Implicit.Constrained;
   use Strings_Edit.Object_Identifiers;

   type Algorithm_Identifier is
      new ASN1.Sequences.Implicit.
          Implicit_Tagged_Sequence_Data_Item with
   record
      Algorithm  : Implicit_External_OID_Data_Item;
      Parameters : Implicit_External_String_Data_Item;
   end record;
   procedure Initialized (Algorithm : in out Algorithm_Identifier);

   type Attribute_Type_And_Value
        (  OID_Length   : Natural;
           Value_Length : Natural
        )  is
   record
      OID   : Object_Identifier (1..OID_Length);
      Value : String (1..Value_Length);
   end record;
   function "<" (Left, Right : Attribute_Type_And_Value) return Boolean;
   package Attribute_Type_And_Value_Sets is
      new Generic_Indefinite_Set (Attribute_Type_And_Value);

   type Attribute_Type_And_Value_Data_Item is
      new Sequence_Data_Item with
   record
      OID   : ASN1.Object_Identifiers.External_OID_Data_Item;
      Value : External_String_Data_Item;
   end record;
   function Get
            (  Data : Attribute_Type_And_Value_Data_Item
            )  return Attribute_Type_And_Value;
   procedure Set
             (  Data  : in out Attribute_Type_And_Value_Data_Item;
                Value : Attribute_Type_And_Value
             );

   package Attribute_Type_And_Value_Data_Item_Sets is
      new GNAT.Sockets.Connection_State_Machine.ASN1.Sets.
          Generic_Set_Of
          (  Value_Type   => Attribute_Type_And_Value,
             Element_Type => Attribute_Type_And_Value_Data_Item
          );

   type Attribute_Type_And_Value_Data_Item_Set is
      new Attribute_Type_And_Value_Data_Item_Sets.Set_Of with
         null record;
   function Get
            (  Data : Attribute_Type_And_Value_Data_Item_Set
            )  return Attribute_Type_And_Value_Sets.Set;
   procedure Set
             (  Data  : in out Attribute_Type_And_Value_Data_Item_Set;
                Value : Attribute_Type_And_Value_Sets.Set
             );

   package Name_Sequences is
      new GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.
          Generic_Sequence_Of
          (  Value_Type   => Attribute_Type_And_Value_Sets.Set,
             Element_Type => Attribute_Type_And_Value_Data_Item_Set
          );
   type Name_Type is
      new Name_Sequences.Implicit_Sequence_Of with null record;

   type Unique_Identifier is
      new Implicit_External_Bit_String_Data_Item with
         null record;
   type Implicit_Octet_String is
      new Implicit_Constrained_External_String_Data_Item
          (  Octet_String_Tag
          )  with null record;

   type Extension_Type is new Tagged_Sequence_Data_Item with record
      ID       : ASN1.Object_Identifiers.
                 Implicit_External_OID_Data_Item;
      Critical : ASN1.Booleans.Implicit_Boolean_Data_Item;
      Value    : Implicit_Octet_String;
                    -- contains the DER encoding of an ASN.1 value
                    -- corresponding to the extension type identified
                    -- by extnID
   end record;
   procedure Feed
             (  Item    : in out Extension_Type;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   function Get (Data : Extension_Type) return String;
   procedure Initialized (Data : in out Extension_Type);
   procedure Set (Data : in out Extension_Type; Value : String);

   package Extension_Sequences is
      new ASN1.Sequences.Generic_Sequence_Of
          (  Value_Type   => String,
             Element_Type => Extension_Type
          );
   type Extension_Sequence is
      new Extension_Sequences.Sequence_Of with null record;

   type Time_Type is new ASN1.Choices.Choice_Data_Item with record
      UTC_Time     : ASN1.Dates.Implicit_UTC_Data_Item;
      General_Time : ASN1.Dates.Implicit_Generalized_Time_Data_Item;
   end record;
   procedure Initialized (Stamp : in out Time_Type);

   type Validity_Time is new Implicit_Sequence_Data_Item with
   record
      Not_Before : Time_Type;
      Not_After  : Time_Type;
   end record;

   type Subject_Public_Key_Info is
      new Implicit_Tagged_Sequence_Data_Item with
   record
      Algorithm  : Algorithm_Identifier;
      Public_Key : Implicit_External_Bit_String_Data_Item;
   end record;
   procedure Initialized (Key_Info : in out Subject_Public_Key_Info);

   type TBS_Certificate is
      new Implicit_Tagged_Sequence_Data_Item with
   record
      Version           : ASN1.Unsigneds_8.Unsigned_Data_Item;
      Serial_Number     : Implicit_Indefinite_Unsigned_Data_Item;
      Signature         : Algorithm_Identifier;
      Issuer            : Name_Type;
      Validity          : Validity_Time;
      Subject           : Name_Type;
      Key_Info          : Subject_Public_Key_Info;
      Issuer_Unique_ID  : Unique_Identifier;
      Subject_Unique_ID : Unique_Identifier;
      Extensions        : Extension_Sequence;
   end record;
   procedure Initialized (Certificate : in out TBS_Certificate);

   type X509_Certificate is
      new Tagged_Sequence_Data_Item with
   record
      Certificate : TBS_Certificate;
      Algorithm   : Algorithm_Identifier;
      Value       : Implicit_External_Bit_String_Data_Item;
   end record;
   procedure Initialized (Certificate : in out X509_Certificate);

end GNAT.Sockets.Connection_State_Machine.ASN1.X509_Certificates;
