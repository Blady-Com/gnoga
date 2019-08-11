--                                                                    --
--  package Test_Data_Servers       Copyright (c)  Dmitry A. Kazakov  --
--  Test server                                    Luebeck            --
--  Interface                                      Winter, 2012       --
--                                                                    --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Streams;               use Ada.Streams;
with GNAT.Sockets;              use GNAT.Sockets;
with GNAT.Sockets.Server;       use GNAT.Sockets.Server;

with GNAT.Sockets.Connection_State_Machine.ASN1.Bit_Strings.Explicit;
with GNAT.Sockets.Connection_State_Machine.ASN1.Bit_Strings.Implicit;
with GNAT.Sockets.Connection_State_Machine.ASN1.Booleans;
with GNAT.Sockets.Connection_State_Machine.ASN1.Choices;
with GNAT.Sockets.Connection_State_Machine.ASN1.Dates;
with GNAT.Sockets.Connection_State_Machine.ASN1.Generic_Enumeration;
with GNAT.Sockets.Connection_State_Machine.ASN1.Indefinite_Unsigneds;
with GNAT.Sockets.Connection_State_Machine.ASN1.Integers;
with GNAT.Sockets.Connection_State_Machine.ASN1.Integers_32;
with GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;
with GNAT.Sockets.Connection_State_Machine.ASN1.Object_Identifiers;
with GNAT.Sockets.Connection_State_Machine.ASN1.Objects;
with GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.Explicit;
with GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.Implicit;
with GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Explicit;
with GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Implicit;
with GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Explicit;
with GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Implicit;
with GNAT.Sockets.Connection_State_Machine.ASN1.Tagged_Values;
with GNAT.Sockets.Connection_State_Machine.ASN1.X509_Certificates;
with GNAT.Sockets.Connection_State_Machine.Big_Endian.IEEE_754_Floats;
with GNAT.Sockets.Connection_State_Machine.Big_Endian.
                                           IEEE_754_Long_Floats;
with GNAT.Sockets.Connection_State_Machine.Big_Endian.Integers;
with GNAT.Sockets.Connection_State_Machine.Big_Endian.Unsigneds;
with GNAT.Sockets.Connection_State_Machine.Chain_Code.Naturals;
with GNAT.Sockets.Connection_State_Machine.Little_Endian.
                                           IEEE_754_Floats;
with GNAT.Sockets.Connection_State_Machine.Little_Endian.
                                           IEEE_754_Long_Floats;
with GNAT.Sockets.Connection_State_Machine.Chain_Code.Integers;
with GNAT.Sockets.Connection_State_Machine.Little_Endian.Integers;
with GNAT.Sockets.Connection_State_Machine.Little_Endian.Unsigneds;
with GNAT.Sockets.Connection_State_Machine.Terminated_Strings;
with GNAT.Sockets.Connection_State_Machine.Variable_Length_Strings;

with Generic_Unbounded_Array;
with Interfaces;

package Test_Data_Servers is
   use GNAT.Sockets.Connection_State_Machine;

   package Character_ASN1 is
      new GNAT.Sockets.Connection_State_Machine.
          ASN1.Generic_Enumeration (Character);

   type Data_Connection;
   type String_Length_Setter
        (  Parent : access Data_Connection
        )  is new Data_Item with null record;
   procedure Feed
             (  Item    : in out String_Length_Setter;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );

   type Nested is new Data_Block with record
      N1 : Little_Endian.Unsigneds.Unsigned_8_Data_Item;
      N2 : Little_Endian.Unsigneds.Unsigned_16_Data_Item;
      N3 : Little_Endian.Unsigneds.Unsigned_32_Data_Item;
      N4 : Little_Endian.Unsigneds.Unsigned_64_Data_Item;
   end record;

   type Alternative_1 is new Data_Block with record
      N : Little_Endian.Integers.Integer_16_Data_Item;
   end record;

   type Alternative_2 is new Data_Block with record
      L : Chain_Code.Naturals.Unsigned_Data_Item;
      M : Chain_Code.Naturals.Unsigned_Data_Item;
   end record;

   type Variant is new Data_Selector with record
      A1   : Alternative_1;
      A2   : Alternative_2;
      S    : Terminated_Strings.String_Data_Item (9, Character'Val (0));
      None : Data_Null;
   end record;

   type Data_Connection is new State_Machine with record
      From : Sock_Addr_Type;
      Blk  : Nested;
      N5   : Big_Endian.Unsigneds.Unsigned_8_Data_Item;
      N6   : Big_Endian.Unsigneds.Unsigned_16_Data_Item;
      N7   : Big_Endian.Unsigneds.Unsigned_32_Data_Item;
      N8   : Big_Endian.Unsigneds.Unsigned_64_Data_Item;
      F1   : Big_Endian.IEEE_754_Floats.IEEE_754_Data_Item;
      F2   : Big_Endian.IEEE_754_Long_Floats.IEEE_754_Data_Item;
      F3   : Little_Endian.IEEE_754_Floats.IEEE_754_Data_Item;
      F4   : Little_Endian.IEEE_754_Long_Floats.IEEE_754_Data_Item;
      S1   : Terminated_Strings.String_Data_Item (9, Character'Val (0));
      Len  : Big_Endian.Integers.Integer_16_Data_Item;
      Fix  : String_Length_Setter (Data_Connection'Unchecked_Access);
      S2   : Variable_Length_Strings.String_Data_Item (80);
      N9   : Chain_Code.Naturals.Unsigned_Data_Item;
      N10  : Chain_Code.Integers.Integer_Data_Item;
      V    : Variant;
   end record;
   procedure Finalize (Client : in out Data_Connection);
   procedure Process_Packet (Client : in out Data_Connection);

   type Data_Factory is new Connections_Factory with null record;
   function Create
            (  Factory  : access Data_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr;
   procedure Trace
             (  Factory    : in out Data_Factory;
                Context    : String;
                Occurrence : Exception_Occurrence
             );
   ---------------------------------------------------------------------
   type Null_Machine is new State_Machine (100, 100) with record
      Completed : Boolean := False;
   end record;
   procedure Feed
             (  Client : in out Null_Machine;
                Title  : String;
                Data   : Stream_Element_Array
             );
   procedure Feed_Flat
             (  Client : in out Null_Machine;
                Title  : String;
                Data   : Stream_Element_Array
             );
   procedure Initialize (Client : in out Null_Machine);
   procedure Process_Packet (Client : in out Null_Machine);
   ---------------------------------------------------------------------
   type Name_And_Check is
      new GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.
          Explicit.Sequence_Data_Item with
   record
      Name  : ASN1.Strings.Explicit.String_Data_Item (200);
      Check : ASN1.Booleans.Boolean_Data_Item;
      Date  : ASN1.Dates.Generalized_Time_Data_Item;
   end record;

   type Name_And_Check_Machine is new Null_Machine with record
      Data : Name_And_Check;
   end record;
   ---------------------------------------------------------------------
   type Integer_32_Array is
      array (Positive range <>) of Interfaces.Integer_32;
   function Image (Value : Integer_32_Array) return String;
   type Integer_Data_Array is
      array (Positive range <>)
         of aliased ASN1.Integers_32.Integer_Data_Item;
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Item   : Integer_Data_Array
             );
   for Integer_Data_Array'Write use Enumerate;

   type Sequence_Of_Integers is
      new ASN1.Sequences.Explicit.Sequence_Of_Data_Item with
   record
      Values : Integer_Data_Array (1..1024);
   end record;
   function Get (Item : Sequence_Of_Integers) return Integer_32_Array;

   type Sequence_Of_Integers_Machine is new Null_Machine with record
      Data : Sequence_Of_Integers;
   end record;
   ---------------------------------------------------------------------
   type Tagged_Name is
      new ASN1.Tagged_Values.Tagged_Data_Item with
   record
      Value : ASN1.Strings.Implicit.Implicit_String_Data_Item (100);
   end record;
   type Tagged_Name_Machine is new Null_Machine with record
      Data : Tagged_Name;
   end record;
   ---------------------------------------------------------------------
   type Four_Integers is
      new ASN1.Sequences.Explicit.Tagged_Sequence_Data_Item with
   record
      A : ASN1.Integers.Integer_Data_Item;
      B : ASN1.Integers.Implicit_Integer_Data_Item;
      C : ASN1.Integers.Implicit_Integer_Data_Item;
      D : ASN1.Integers.Implicit_Integer_Data_Item;
   end record;
   type Four_Integers_Machine is new Null_Machine with
   record
      Data : Four_Integers;
   end record;
   ---------------------------------------------------------------------
   type Assorted_Set is
      new ASN1.Sets.Explicit.Set_Data_Item with
   record
      Name : ASN1.Strings.Implicit.Implicit_String_Data_Item (200);
      A    : ASN1.Integers.Implicit_Integer_Data_Item;
      B    : ASN1.Integers.Implicit_Integer_Data_Item;
   end record;
   type Assorted_Set_Machine is new Null_Machine with record
      Data : Assorted_Set;
   end record;
   ---------------------------------------------------------------------
   type Alternatives_Record is
      new ASN1.Choices.Choice_Data_Item with
   record
      Name : ASN1.Strings.Implicit.Implicit_String_Data_Item (20);
      ID   : ASN1.Integers.Implicit_Integer_Data_Item;
   end record;
   type Alternatives_Record_Machine is new Null_Machine with record
      Data : Alternatives_Record;
   end record;
   ---------------------------------------------------------------------
   type Shared_Bit_Strings_Record_Machine is
      new Null_Machine with
   record
      Buffer : External_String_Buffer (1024 * 10);
      Data   : ASN1.Bit_Strings.Explicit.External_Bit_String_Data_Item;
   end record;
   ---------------------------------------------------------------------
   type Shared_Indefinite_Unsigned_Record_Machine is
      new Null_Machine with
   record
      Buffer : External_String_Buffer (150);
      Data   : ASN1.Indefinite_Unsigneds.Indefinite_Unsigned_Data_Item;
   end record;
   ---------------------------------------------------------------------
   type Shared_Strings_Record is
      new ASN1.Sequences.Implicit.Implicit_Sequence_Data_Item with
   record
      A : ASN1.Strings.Implicit.Implicit_External_String_Data_Item;
      B : ASN1.Strings.Implicit.Implicit_External_String_Data_Item;
      C : ASN1.Strings.Implicit.Implicit_External_String_Data_Item;
   end record;
   type Shared_Strings_Record_Machine is new Null_Machine with record
      Buffer : External_String_Buffer (100);
      Data   : Shared_Strings_Record;
   end record;
   ---------------------------------------------------------------------
   type Integer_Set is
      new ASN1.Sets.Explicit.External_Set_Of_Data_Item with null record;
   function Create
            (  Set : access Integer_Set
            )  return ASN1.Abstract_ASN1_Data_Item_Ptr;
   function Get
            (  Set   : Integer_Set;
               Index : Positive
            )  return Interfaces.Integer_32;
   function Get (Set : Integer_Set) return Integer_32_Array;
   type Set_Of_Machine is new Null_Machine with record
      Buffer : External_String_Buffer (1024 * 20);
      Data   : Integer_Set;
   end record;
   ---------------------------------------------------------------------
   type Integer_Reference is
      new ASN1.Sets.Reference_Data_Item with null record;
   function Create
            (  Reference : access Integer_Reference
            )  return ASN1.Abstract_ASN1_Data_Item_Ptr;
   function Get
            (  Reference : Integer_Reference
            )  return Interfaces.Integer_32;
   type Reference_Machine is new Null_Machine with record
      Buffer : External_String_Buffer (1024);
      Data   : Integer_Reference;
   end record;
   ---------------------------------------------------------------------
   type External_OID_Machine is new Null_Machine with record
      Buffer : External_String_Buffer (1024);
      OID    : ASN1.Object_Identifiers.External_OID_Data_Item;
   end record;
   ---------------------------------------------------------------------
   type X509_Certificate_Machine is new Null_Machine with record
      Buffer      : External_String_Buffer (1024 * 10);
      Certificate : ASN1.X509_Certificates.X509_Certificate;
   end record;
   ---------------------------------------------------------------------
   type Any_Machine is new Null_Machine with record
      Buffer : External_String_Buffer (1024 * 40);
      Any    : ASN1.Objects.Any_Data_Item;
   end record;

   procedure Encode_String is
      new ASN1.Lengths.Generic_Explicit_Encode
          (  ASN1.Strings.Implicit.Implicit_String_Data_Item,
             ASN1.Strings.Implicit.Encode
          );
   procedure Put_String is
      new ASN1.Lengths.Generic_Put (String, ASN1.Strings.Put);

end Test_Data_Servers;
