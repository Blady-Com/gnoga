--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Strings                                Spring, 2019       --
--  Implementation                                                    --
--                                Last revision :  10:13 29 Nov 2020  --
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

with Ada.Exceptions;              use Ada.Exceptions;
with Ada.IO_Exceptions;           use Ada.IO_Exceptions;
with Ada.Tags;                    use Ada.Tags;
with Strings_Edit.Integers;       use Strings_Edit.Integers;
with Strings_Edit.UTF8.Handling;  use Strings_Edit.UTF8.Handling;

with Ada.Unchecked_Conversion;
with Strings_Edit.UTF8.ITU_T61;
with System.Address_To_Access_Conversions;

with GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Lengths;

package body GNAT.Sockets.Connection_State_Machine.ASN1.Strings is

   type Embedded_Length is mod 2**24;
   type Embedding_Mode is
        (  String_Body,
           String_Type,
           End_Of_Content
        );
   type Octet_Count is mod 4;
   type Status_Type is record
      Length : Embedded_Length;
      Octet  : Octet_Count;
      Mode   : Embedding_Mode;
   end record;
   for Status_Type use record
      Length at 0 range  0..23;
      Octet  at 0 range 24..28;
      Mode   at 0 range 29..30;
   end record;
   for Status_Type'Size use Unsigned_32'Size;

   function From_State
            (  State : Stream_Element_Offset
            )  return Status_Type is
      function Convert is
         new Ada.Unchecked_Conversion (Unsigned_32, Status_Type);
   begin
      return Convert (Unsigned_32 (State));
   end From_State;

   function To_State
            (  Status : Status_Type
            )  return Stream_Element_Offset is
      function Convert is
         new Ada.Unchecked_Conversion (Status_Type, Unsigned_32);
   begin
      return Stream_Element_Offset (Convert (Status));
   end To_State;

   procedure Not_A_String (Tag : ASN1_Tag) is
   begin
      Raise_Exception
      (  Data_Error'Identity,
         (  "Not an ASN.1 string tag "
         &  Image (Integer (Tag))
      )  );
   end Not_A_String;

   function Check_Code_Point
            (  Code : UTF8_Code_Point;
               Tag  : ASN1_Type
            )  return Boolean is
   begin
      case Tag is
         when Octet_String_Tag      |
              Object_Descriptor_Tag |
              Generalized_Time_Tag  |
              UTC_Time_Tag          =>
            return True;
         when UTF8_String_Tag =>
            return True;
         when Numeric_String_Tag =>
            case Code is
               when Character'Pos (' ') |
                    Character'Pos ('0') .. Character'Pos ('9') =>
                  return False;
               when others =>
                  null;
            end case;
         when Printable_String_Tag =>
            case Code is
               when Character'Pos ('A') .. Character'Pos ('Z') |
                    Character'Pos ('a') .. Character'Pos ('z') |
                    Character'Pos ('0') .. Character'Pos ('9') |
                    Character'Pos (' ') |
                    Character'Pos (''') |
                    Character'Pos ('(') |
                    Character'Pos (')') |
                    Character'Pos ('+') |
                    Character'Pos (',') |
                    Character'Pos ('-') |
                    Character'Pos ('.') |
                    Character'Pos ('/') |
                    Character'Pos (';') |
                    Character'Pos ('=') |
                    Character'Pos ('?') =>
                  return False;
               when others =>
                  null;
            end case;
         when Teletext_String_Tag =>
            declare
               use Strings_Edit.UTF8.ITU_T61;
            begin
               begin
                  if To_ITU_T61 (Code) = 'a' then
                     null;
                  end if;
                  return False;
               exception
                  when Constraint_Error =>
                     null;
               end;
            end;
         when Videotext_String_Tag =>
            return True;
         when IA5_String_Tag =>
            if Code <= Character'Pos ('~') then
               return False;
            end if;
         when Graphic_String_Tag =>
            return True;
         when ISO646_String_Tag =>
            if Code in Character'Pos (' ') .. Character'Pos ('~') then
               return False;
            end if;
         when General_String_Tag =>
            return True;
         when Universal_String_Tag =>
            return True;
         when Character_String_Tag =>
            if Code < 256 then
               return False;
            end if;
         when BMP_String_Tag =>
            if Code < 2**16 then
               return False;
            end if;
         when others =>
            null;
      end case;
      Raise_Exception (Data_Error'Identity, Unsupported);
   end Check_Code_Point;

   procedure Check_Value (Value : String; Tag : ASN1_Type) is
      Code    : UTF8_Code_Point;
      Pointer : Positive := Value'First;
   begin
      while Pointer <= Value'Last loop
         Get (Value, Pointer, Code);
         exit when Check_Code_Point (Code, Tag);
      end loop;
   end Check_Value;

   procedure Check_Value
             (  Parent : ASN1_Data_Item'Class;
                Item   : Embedded_String_Data;
                Tag    : ASN1_Type
             )  is
   begin
      Check_Value (Item.Parent.Value (1..Item.Parent.Length), Tag);
   end Check_Value;

   procedure Check_Value
             (  Parent : ASN1_Data_Item'Class;
                Item   : External_String_Data;
                Tag    : ASN1_Type
             )  is
   begin
      if Item.Buffer = null then
         Uninitialized (Parent);
      end if;
      declare
         Code    : UTF8_Code_Point;
         Buffer  : String renames Item.Buffer.Buffer;
         Pointer : Positive := Item.Start;
         Last    : constant Positive :=
                            Pointer + Item.Length - 1;
      begin
         while Pointer <= Last loop
            Get (Buffer, Pointer, Code);
            exit when Check_Code_Point (Code, Tag);
         end loop;
      end;
   end Check_Value;

   procedure Do_Feed
             (  Item     : in out String_Data'Class;
                Data     : Stream_Element_Array;
                Pointer  : in out Stream_Element_Offset;
                Expected : ASN1_Type_Array;
                State    : in out Stream_Element_Offset
             )  is
      procedure Check is
      begin
         for Index in Expected'Range loop
            if Item.Tag = Expected (Index) then
               return;
            end if;
         end loop;
         Raise_Exception
         (  Data_Error'Identity,
            (  "ASN.1 type "
            &  Image (Item.Tag)
            &  " does not match expected type "
            &  Image (Expected)
         )  );
      end Check;
   begin
      if State = 0 then
         declare
            This : constant Stream_Element := Data (Pointer);
         begin
            Reset (Item);
            Item.Primitive := 0 = (This and 16#20#);
            Item.Nested    := False;
            if Universal_Class /= ASN1_Tag (This and 2#1100_0000#) then
               if Expected'Length = 1 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "ASN.1 type " & Image (Expected) & " is expected"
                  );
               else
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "One of ASN.1 types "
                     &  Image (Expected)
                     &  " is expected"
                  )  );
               end if;
            end if;
            if (This and 16#1F#) >= 31 then -- Long tag
               State         := 1;
               Item.Long_Tag := True;
               Item.Tag      := 0;
            else
               Item.Tag      := ASN1_Type (This and 16#1F#);
               Item.Long_Tag := False;
               State         := Start_Length;
               Check;
            end if;
         end;
         Pointer := Pointer + 1;
         if Pointer > Data'Last then
            return;
         end if;
      end if;
      if Item.Long_Tag and then State > 0 then
         loop
            declare
               This : constant ASN1_Type := ASN1_Type (Data (Pointer));
            begin
               Pointer := Pointer + 1;
               if This > 127 then
                  Item.Tag := Item.Tag * 128 + This - 128;
               else
                  Item.Tag := Item.Tag * 128 + This;
                  Item.Long_Tag := False;
                  State := Start_Length;
                  Check;
                  return;
               end if;
            end;
            if Pointer > Data'Last then
               return;
            end if;
         end loop;
      end if;
   end Do_Feed;

   procedure Feed
             (  Parent  : ASN1_Data_Item'Class;
                Item    : in out String_Data'Class;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
      Status : Status_Type;

      procedure Encoding_Error is
      begin
         Raise_Exception
         (  Data_Error'Identity,
            (  "Invalid ASN.1 character encoding of "
            &  Image (Integer (Item.Tag))
         )  );
      end Encoding_Error;
   begin
      if State = 0 then
         Reset (Item);
         if Item.Definite then
            declare
               Length : constant Stream_Element_Offset := Item.Last;
            begin
               if Item.Primitive then -- Primitive string
                  if Length = 0 then
                     State := 0;
                     return;
                  else
                     Status.Mode   := String_Body;
                     Status.Length := Embedded_Length (Length);
                     Item.Last     := Pointer + Length - 1;
                  end if;
               else -- The total length is here
                  Item.Last   := Pointer + Length;
                  Status.Mode := String_Type;
               end if;
            end;
         else
            if Item.Primitive then
               Raise_Exception (Data_Error'Identity, Wrong_Length);
            end if;
            Status.Mode   := String_Type;
            Status.Length := 0;
         end if;
         Item.Nested  := True;
         Status.Octet := 0;
         State        := To_State (Status);
      end if;
      while Pointer <= Data'Last loop
         if State < 0 then
            while not Is_Length_Ready (State) loop
               if Pointer > Data'Last then
                  return;
               end if;
               Embedded_Feed (Data, Pointer, State);
            end loop;
            if Is_Implicit (Parent) then
               Item.Primitive := not Is_Indefinite (State);
            end if;
            if Is_Indefinite (State) then
               if Item.Primitive then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "Primitive ASN.1 string has indefinite length"
                  );
               elsif Item.Nested then
                  Raise_Exception (Data_Error'Identity, Nested);
               end if;
               Item.Definite := False;
               Item.Nested   := True;
               Status.Length := 0;
               Status.Mode   := String_Type;
               Status.Octet  := 0;
            else
               declare
                  Length : constant Stream_Element_Offset :=
                                    Get_Length (State);
               begin
                  if Item.Primitive then -- Primitive string
                     if Length = 0 then
                        State := 0;
                        return;
                     else
                        Status.Mode  := String_Body;
                        Status.Octet := 0;
                        Status.Length := Embedded_Length (Length);
                        if (  Integer (Status.Length)
                           >  Get_Available (Item)
                           )  then
                           Raise_Exception
                           (  Data_Error'Identity,
                              Too_Large
                           );
                        end if;
                     end if;
                  elsif Item.Nested then -- Constructor string
                     if Length = 0 then
                        if (  Item.Definite
                           and then
                              Pointer > Item.Last
                           )  then
                           State := 0;
                           return;
                        end if;
                        Status.Mode  := String_Type;
                        Status.Octet := 0;
                     elsif (  Item.Definite
                           and then
                              Pointer > Item.Last
                           )  then
                        Raise_Exception
                        (  Data_Error'Identity,
                           "ASN.1 string data overrun"
                        );
                     else
                        Status.Mode  := String_Body;
                        Status.Octet := 0;
                        Status.Length := Embedded_Length (Length);
                        if (  Integer (Status.Length)
                           >  Get_Available (Item)
                           )  then
                           Raise_Exception
                           (  Data_Error'Identity,
                              Too_Large
                           );
                        end if;
                     end if;
                  else -- Not nested, the total length is here
                     Item.Last     := Pointer + Length - 1;
                     Item.Definite := True;
                     Item.Nested   := True;
                     Status.Mode   := String_Type;
                     Status.Octet  := 0;
                  end if;
               end;
            end if;
            State := To_State (Status);
         else
            Status := From_State (State);
            loop
               case Status.Mode is
                  when String_Body =>
                     loop
                        Status.Length := Status.Length - 1;
                        case Item.Tag is
                           when Embedded_Tag          |
                                Date_Tag              |
                                Date_Time_Tag         |
                                Duration_Tag          |
                                Generalized_Time_Tag  |
                                Object_Descriptor_Tag |
                                Octet_String_Tag      |
                                Time_Of_Day_Tag       |
                                UTC_Time_Tag          =>
                              Store (Item, Data (Pointer));
                           when UTF8_String_Tag =>
                              Store (Item, Data (Pointer));
                           when Numeric_String_Tag =>
                              case Data (Pointer) is
                                 when Character'Pos (' ') |
                                      Character'Pos ('0')
                                   .. Character'Pos ('9') =>
                                    Store (Item, Data (Pointer));
                                 when others =>
                                    Encoding_Error;
                              end case;
                           when Printable_String_Tag =>
                              case Data (Pointer) is
                                 when Character'Pos ('A')
                                   .. Character'Pos ('Z') |
                                      Character'Pos ('a')
                                   .. Character'Pos ('z') |
                                      Character'Pos ('0')
                                   .. Character'Pos ('9') |
                                      Character'Pos (' ') |
                                      Character'Pos (''') |
                                      Character'Pos ('(') |
                                      Character'Pos (')') |
                                      Character'Pos ('+') |
                                      Character'Pos (',') |
                                      Character'Pos ('-') |
                                      Character'Pos ('.') |
                                      Character'Pos ('/') |
                                      Character'Pos (';') |
                                      Character'Pos ('=') |
                                      Character'Pos ('?') =>
                                    Store (Item, Data (Pointer));
                                 when others =>
                                    Encoding_Error;
                              end case;
                           when Teletext_String_Tag =>
                              declare
                                 use Strings_Edit.UTF8.ITU_T61;
                                 Value : Code_Point;
                              begin
                                 begin
                                    Value :=
                                       From_ITU_T61
                                       (  Character'Val
                                          (  Data (Pointer)
                                       )  );
                                 exception
                                    when Constraint_Error =>
                                       Encoding_Error;
                                 end;
                                 Store (Item, Image (Value));
                              end;
                           when Videotext_String_Tag =>
                              Store (Item, Data (Pointer));
                           when IA5_String_Tag =>
                              if Data (Pointer) > 127 then
                                 Encoding_Error;
                              end if;
                              Store (Item, Data (Pointer));
                           when Graphic_String_Tag =>
                              Store (Item, Data (Pointer));
                           when ISO646_String_Tag =>
                              case Data (Pointer) is
                                 when 32..127 =>
                                    Store (Item, Data (Pointer));
                                 when others =>
                                    Encoding_Error;
                              end case;
                           when General_String_Tag =>
                              Store (Item, Data (Pointer));
                           when Universal_String_Tag =>
                              case Status.Octet is
                                 when 0 =>
                                    Item.Accumulator :=
                                       Code_Point (Data (Pointer));
                                    Status.Octet := 1;
                                 when 1 =>
                                    Item.Accumulator :=
                                       (  Item.Accumulator * 256
                                       +  Code_Point (Data (Pointer))
                                       );
                                    Status.Octet := 2;
                                 when 2 =>
                                    Item.Accumulator :=
                                       (  Item.Accumulator * 256
                                       +  Code_Point (Data (Pointer))
                                       );
                                    Status.Octet := 3;
                                 when others =>
                                    begin
                                       Item.Accumulator :=
                                          (  Item.Accumulator * 256
                                          +  Code_Point (Data (Pointer))
                                          );
                                    exception
                                       when Constraint_Error =>
                                          Encoding_Error;
                                    end;
                                    Store
                                    (  Item,
                                       Image (Item.Accumulator)
                                    );
                                    Status.Octet := 0;
                              end case;
                           when Character_String_Tag =>
                              Store
                              (  Item,
                                 To_UTF8
                                 (  Character'Val (Data (Pointer))
                              )  );
                           when BMP_String_Tag =>
                              if Status.Octet = 0 then
                                 Item.Accumulator :=
                                    Code_Point (Data (Pointer));
                                 Status.Octet := 1;
                              else
                                 begin
                                    Item.Accumulator :=
                                       (  Item.Accumulator * 256
                                       +  Code_Point (Data (Pointer))
                                       );
                                 exception
                                    when Constraint_Error =>
                                       Encoding_Error;
                                 end;
                                 Store (Item, Image (Item.Accumulator));
                                 Status.Octet := 0;
                              end if;
                           when others =>
                              Store (Item, Data (Pointer));
                        end case;
                        Pointer := Pointer + 1;
                        if Status.Length = 0 then
                           if Status.Octet /= 0 then
                              Encoding_Error;
                           end if;
                           if (  Item.Primitive
                              or else
                                 (  Item.Definite
                                 and then
                                    Pointer > Item.Last
                              )  )  then
                              State := 0;
                              return;
                           else
                              Status.Mode  := String_Type;
                              Status.Octet := 0;
                              exit;
                           end if;
                        end if;
                        if Pointer > Data'Last then
                           State := To_State (Status);
                           return;
                        end if;
                     end loop;
                  when String_Type =>
                     case Status.Octet is
                        when 0 =>
                           if (  Item.Definite
                              and then
                                 Pointer > Item.Last
                              )  then
                              State := 0;
                              return;
                           end if;
                           if (Data (Pointer) and 16#1F#) >= 31 then
                              Status.Length := 0;
                              Status.Octet  := 1;
                              Pointer       := Pointer + 1;
                              if (  Item.Definite
                                 and then
                                    Pointer > Item.Last
                                 )  then
                                 Raise_Exception
                                 (  Data_Error'Identity,
                                    Invalid_Length
                                 );
                              end if;
                           else -- Check the tag
                              Status.Octet  := 2;
                              Status.Length :=
                                 Embedded_Length
                                 (  Data (Pointer) and 16#1F#
                                 );
                              Pointer := Pointer + 1;
                           end if;
                        when 1 =>
                           if (  Item.Definite
                              and then
                                 Pointer > Item.Last
                              )  then
                              Raise_Exception
                              (  Data_Error'Identity,
                                 Invalid_Length
                              );
                           end if;
                           begin
                              if Data (Pointer) > 127 then
                                 Status.Length :=
                                    (  Status.Length * 128
                                    +  Embedded_Length
                                       (  Data (Pointer)
                                       -  128
                                    )  );
                                 Pointer := Pointer + 1;
                              else
                                 Status.Length :=
                                    (  Status.Length * 128
                                    +  Embedded_Length
                                       (  Data (Pointer)
                                    )  );
                                 Pointer      := Pointer + 1;
                                 Status.Octet := 2;
                              end if;
                           exception
                              when Constraint_Error =>
                                 Raise_Exception
                                 (  Data_Error'Identity,
                                    Tag_Too_Large
                                 );
                           end;
                        when others =>
                           Item.Tag := ASN1_Type (Status.Length);
                           case Item.Tag is
                              when BMP_String_Tag        |
                                   Character_String_Tag  |
                                   Date_Tag              |
                                   Date_Time_Tag         |
                                   Duration_Tag          |
                                   General_String_Tag    |
                                   Generalized_Time_Tag  |
                                   Graphic_String_Tag    |
                                   Embedded_Tag          |
                                   IA5_String_Tag        |
                                   ISO646_String_Tag     |
                                   Numeric_String_Tag    |
                                   Object_Descriptor_Tag |
                                   Octet_String_Tag      |
                                   Printable_String_Tag  |
                                   Teletext_String_Tag   |
                                   Time_Of_Day_Tag       |
                                   Time_Tag              |
                                   Universal_String_Tag  |
                                   UTC_Time_Tag          |
                                   UTF8_String_Tag       |
                                   Videotext_String_Tag  =>
                                 if (  Item.Constraint /= 0
                                    and then
                                       Item.Constraint /= Item.Tag
                                    )  then
                                    Raise_Exception
                                    (  Data_Error'Identity,
                                       String_Type_Error
                                    );
                                 end if;
                                 State := Start_Length;
                                 exit;
                              when 0 =>
                                 Status.Mode := End_Of_Content;
                              when others =>
                                 Not_A_String (ASN1_Tag (Status.Length));
                           end case;
                     end case;
                  when End_Of_Content =>
                     if Data (Pointer) /= 0 then
                        Raise_Exception
                        (  Data_Error'Identity,
                           String_Invalid_EOC
                        );
                     end if;
                     Pointer := Pointer + 1;
                     State   := 0;
                     return;
               end case;
               if Pointer > Data'Last then
                  State := To_State (Status);
                  return;
               end if;
            end loop;
         end if;
      end loop;
   end Feed;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out String
             )  is
      Index : Stream_Element_Offset := Pointer;
   begin
      for Octet in Value'Range loop
         if Index > Data'Last then
            Raise_Exception (End_Error'Identity, Non_Terminated);
         end if;
         Value (Octet) := Character'Val (Data (Index));
         Index := Index + 1;
      end loop;
      Pointer := Index;
   end Get;

   function Get_Available
            (  Item  : Embedded_String_Data
            )  return Natural is
   begin
      return Item.Parent.Size - Item.Parent.Length;
   end Get_Available;

   function Get_Available
            (  Item  : External_String_Data
            )  return Natural is
   begin
      if Item.Buffer = null then
         return 0;
      else
         return Item.Buffer.Size - Item.Buffer.Length;
      end if;
   end Get_Available;

   function Get_ASN1_Type
            (  Item : Public_String_Data_Item
            )  return ASN1_Type is
   begin
      return UTF8_String_Tag;
   end Get_ASN1_Type;

   function Get_Length
            (  Item : Public_String_Data_Item
            )  return Natural is
   begin
      return Item.Length;
   end Get_Length;

   function Get_Value (Item : Public_String_Data_Item) return String is
   begin
      return Item.Value (1..Item.Length);
   end Get_Value;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : String
             )  is
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - Data'Last /= 1
         )  )  then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      elsif Data'Last - Pointer < Value'Length - 1 then
         Raise_Exception (End_Error'Identity, No_Room);
      end if;
      for Index in Value'Range loop
         Data (Pointer) := Character'Pos (Value (Index));
         Pointer := Pointer + 1;
      end loop;
   end Put;

   procedure Put_BMP
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : String
             )  is
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - Data'Last /= 1
         )  )  then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      end if;
      declare
         Count    : constant Stream_Element_Count :=
                             Stream_Element_Offset (Length (Value));
         Code     : UTF8_Code_Point;
         Index    : Stream_Element_Offset := Pointer;
         Position : Integer := Value'First;
      begin
         if Data'Last - Pointer < Count * 2 - 1 then
            Raise_Exception (End_Error'Identity, No_Room);
         end if;
         for Item in 1..Count loop
            Get (Value, Position, Code);
            if Code >= 2**16 then
               Raise_Exception (Constraint_Error'Identity, Unsupported);
            end if;
            Data (Index + 1) := Stream_Element (Code and 16#FF#);
            Code := Code / 256;
            Data (Index) := Stream_Element (Code and 16#FF#);
            Index := Index + 2;
         end loop;
         Pointer := Index;
      end;
   end Put_BMP;

   procedure Put_ITU_T61
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : String
             )  is
      use Strings_Edit.UTF8.ITU_T61;
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - Data'Last /= 1
         )  )  then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      end if;
      declare
         Count    : constant Stream_Element_Count :=
                             Stream_Element_Offset (Length (Value));
         Code     : UTF8_Code_Point;
         Index    : Stream_Element_Offset := Pointer;
         Position : Integer := Value'First;
      begin
         if Data'Last - Pointer < Count - 1 then
            Raise_Exception (End_Error'Identity, No_Room);
         end if;
         for Item in 1..Count loop
            Get (Value, Position, Code);
            Data (Index) := Character'Pos (To_ITU_T61 (Code));
            Index := Index + 1;
         end loop;
         Pointer := Index;
      end;
   end Put_ITU_T61;

   procedure Put_Latin1
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : String
             )  is
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - Data'Last /= 1
         )  )  then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      end if;
      declare
         Count    : constant Stream_Element_Count :=
                             Stream_Element_Offset (Length (Value));
         Code     : UTF8_Code_Point;
         Position : Integer := Value'First;
         Index    : Stream_Element_Offset := Pointer;
      begin
         if Data'Last - Pointer < Count - 1 then
            Raise_Exception (End_Error'Identity, No_Room);
         end if;
         for Item in 1..Count loop
            Get (Value, Position, Code);
            if Code >= 2**8 then
               Raise_Exception (Constraint_Error'Identity, Unsupported);
            end if;
            Data (Index) := Stream_Element (Code);
            Index := Index + 1;
         end loop;
         Pointer := Index;
      end;
   end Put_Latin1;

   procedure Put_Universal
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : String
             )  is
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - Data'Last /= 1
         )  )  then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      end if;
      declare
         Count    : constant Stream_Element_Count :=
                             Stream_Element_Offset (Length (Value));
         Code     : UTF8_Code_Point;
         Position : Integer := Value'First;
         Index    : Stream_Element_Offset := Pointer;
      begin
         if Data'Last - Pointer < Count * 4 - 1 then
            Raise_Exception (End_Error'Identity, No_Room);
         end if;
         for Item in 1..Count loop
            Get (Value, Position, Code);
            Data (Index + 3) := Stream_Element (Code and 16#FF#);
            Code := Code / 256;
            Data (Index + 2) := Stream_Element (Code and 16#FF#);
            Code := Code / 256;
            Data (Index + 1) := Stream_Element (Code and 16#FF#);
            Code := Code / 256;
            Data (Index) := Stream_Element (Code and 16#FF#);
            Index := Index + 4;
         end loop;
         Pointer := Index;
      end;
   end Put_Universal;

   procedure Reset (Item : in out Embedded_String_Data) is
   begin
      Item.Parent.Length := 0;
   end Reset;

   procedure Reset (Item : in out External_String_Data) is
   begin
      if Item.Buffer = null then
         Item.Start := 1;
      else
         Item.Start  := Item.Buffer.Length + 1;
      end if;
      Item.Length := 0;
   end Reset;

   function Self
            (  Item : External_String_Buffer'Class
            )  return External_String_Buffer_Ptr is
      package From_Address is
         new System.Address_To_Access_Conversions
             (  External_String_Buffer'Class
             );
      use From_Address;
   begin
      return To_Pointer (Item'Address).all'Unchecked_Access;
   end Self;

   procedure Set_Value
             (  Item  : in out Public_String_Data_Item;
                Value : String
             )  is
   begin
      if Value'Length > Item.Size then
         Raise_Exception (Constraint_Error'Identity, Too_Large);
      else
         Item.Value (1..Value'Length) := Value;
         Item.Length := Value'Length;
      end if;
   end Set_Value;

   procedure Store
             (  Item    : in out Embedded_String_Data;
                Element : Stream_Element
             )  is
      Length : Natural renames Item.Parent.Length;
      Size   : Natural renames Item.Parent.Size;
   begin
      if Length + 1 > Size then
         Raise_Exception (Constraint_Error'Identity, Too_Large);
      end if;
      Length := Length + 1;
      Item.Parent.Value (Length) := Character'Val (Element);
   end Store;

   procedure Store
             (  Item     : in out Embedded_String_Data;
                Sequence : String
             )  is
      Length : Natural renames Item.Parent.Length;
      Size   : Natural renames Item.Parent.Size;
      Value  : String  renames Item.Parent.Value;
   begin
      if Length + Sequence'Length > Size then
         Raise_Exception (Constraint_Error'Identity, Too_Large);
      end if;
      Value (Length + 1..Length + Sequence'Length) := Sequence;
      Length := Length + Sequence'Length;
   end Store;

   procedure Store
             (  Item    : in out External_String_Data;
                Element : Stream_Element
             )  is
      Length : Natural renames Item.Buffer.Length;
      Size   : Natural renames Item.Buffer.Size;
   begin
      if Length + 1 > Size then
         Raise_Exception (Constraint_Error'Identity, Too_Large);
      end if;
      Length      := Length + 1;
      Item.Length := Item.Length + 1;
      Item.Buffer.Buffer (Length) := Character'Val (Element);
   end Store;

   procedure Store
             (  Item     : in out External_String_Data;
                Sequence : String
             )  is
      Length : Natural renames Item.Buffer.Length;
      Size   : Natural renames Item.Buffer.Size;
      Value  : String  renames Item.Buffer.Buffer;
   begin
      if Length + Sequence'Length > Size then
         Raise_Exception (Constraint_Error'Identity, Too_Large);
      end if;
      Value (Length + 1..Length + Sequence'Length) := Sequence;
      Length      := Length      + Sequence'Length;
      Item.Length := Item.Length + Sequence'Length;
   end Store;

   procedure Uninitialized (Item : ASN1_Data_Item'Class) is
   begin
      Raise_Exception
      (  Use_Error'Identity,
         (  "ASN.1 string "
         &  Expanded_Name (Item'Tag)
         &  " was not properly initialized"
      )  );
   end Uninitialized;

end GNAT.Sockets.Connection_State_Machine.ASN1.Strings;
