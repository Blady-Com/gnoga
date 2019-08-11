--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Distinguished_Names            Luebeck            --
--                                                 Spring, 2019       --
--  Implementation                                                    --
--                                Last revision :  18:40 01 Aug 2019  --
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

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Strings_Edit;             use Strings_Edit;
with Strings_Edit.Fields;      use Strings_Edit.Fields;
with Strings_Edit.Integers;    use Strings_Edit.Integers;
with Strings_Edit.UTF8;        use Strings_Edit.UTF8;

package body Strings_Edit.Distinguished_Names is

   procedure Skip_Spaces (Source : String; Pointer : in out Integer) is
   begin
      while Pointer <= Source'Last and then Source (Pointer) = ' ' loop
         Pointer := Pointer + 1;
      end loop;
   end Skip_Spaces;

   procedure Check
             (  Source : String;
                Index  : in out Integer;
                Depth  : out Natural;
                Pairs  : out Natural;
                Items  : out Natural;
                Size   : out Natural
             )  is
      Spaces : Natural := 0;
   begin
      Depth := 0;
      Pairs := 0;
      Items := 0;
      Size  := 0;
      if (  Index < Source'First
         or else
            (  Index > Source'Last
            and then
               Index > Source'Last + 1
         )  )  then
         raise Layout_Error;
      elsif Index > Source'Last then
         return;
      end if;
      loop
         for Attribute in Positive'Range loop
            declare
               Start : Integer;
            begin
               case Source (Index) is
                  when 'A'..'Z' | 'a'..'z' =>
                     Start := Index;
                     while Index <= Source'Last loop
                        case Source (Index) is
                           when 'A'..'Z' | 'a'..'z'| '0'..'9' | '-' =>
                              Index := Index + 1;
                           when others =>
                              exit;
                        end case;
                     end loop;
                     if Attribute = 1 then
                        Depth := Depth + 1;
                     end if;
                     Size := Size + Index - Start;
                  when '0'..'9' =>
                     Start := Index;
                     Items := Items + 1;
                     while Index <= Source'Last loop
                        case Source (Index) is
                           when '0'..'9' =>
                              Index := Index + 1;
                           when '.' =>
                              Items := Items + 1;
                              Index := Index + 1;
                           when others =>
                              exit;
                        end case;
                     end loop;
                     if Attribute = 1 then
                        Depth := Depth + 1;
                     end if;
                  when others =>
                     if Depth = 0 and then Attribute = 1 then
                        raise End_Error;
                     else
                        Raise_Exception
                        (  Data_Error'Identity,
                           (  "Key does not start with "
                           &  "letter or digit ["
                           &  Source (Start..Source'Last)
                           &  "]"
                        )  );
                     end if;
               end case;
               Pairs := Pairs + 1;
               Skip_Spaces (Source, Index);
               if Index > Source'Last or else Source (Index) /= '=' then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Key ["
                     &  Source (Start..Index - 1)
                     &  "] is not followed by '=' ["
                     &  Source (Index..Source'Last)
                     &  "]"
                  )  );
               end if;
               Index := Index + 1;
               Skip_Spaces (Source, Index);
               if Index > Source'Last then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Missing attribute value after '=' ["
                     &  Source (Index..Source'Last)
                     &  "]"
                  )  );
               elsif Source (Index) = '#' then
                  Index := Index + 1;
                  Start := Index;
                  while Index <= Source'Last loop
                     case Source (Index) is
                        when '0'..'9' | 'A'..'F' | 'a'..'f' =>
                           Index := Index + 1;
                        when others =>
                           exit;
                     end case;
                  end loop;
                  if (Index - Start) mod 2 /= 0 then
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "Odd number of hexadecimal "
                        &  "digits after '#' ["
                        &  Source (Start..Index - 1)
                        &  "]"
                     )  );
                  end if;
                  Size := Size + (Index - Start) / 2;
                  Skip_Spaces (Source, Index);
               else
                  declare
                     Code  : UTF8_Code_Point;
                  begin
                     while Index <= Source'Last loop
                        Start := Index;
                        Get (Source, Index, Code);
                        case Code is
                           when Character'Pos ('\') =>
                              if Index > Source'Last then
                                 Raise_Exception
                                 (  Data_Error'Identity,
                                    (  "Unfinished escape sequence ["
                                    &  Source (Index..Source'Last)
                                    &  "]"
                                 )  );
                              end if;
                              Size   := Size + Spaces;
                              Spaces := 0;
                              case Source (Index) is
                                 when '0'..'9' | 'A'..'F' | 'a'..'f' =>
                                    Index := Index + 1;
                                    if (  Index > Source'Last
                                       or else
                                          not Is_Hexadecimal_Digit
                                              (  Source (Index)
                                       )      )  then
                                       Raise_Exception
                                       (  Data_Error'Identity,
                                          (  "Invalid hexadecimal "
                                          &  "escape pair ["
                                          &  Source (Index..Source'Last)
                                          &  "]"
                                       )  );
                                    end if;
                                    Index := Index + 1;
                                    Size  := Size  + 1;
                                 when ' ' | '"' | '#' | '+' | ',' |
                                      ';' | '<' | '=' | '>' | '\' =>
                                    Index := Index + 1;
                                    Size  := Size  + 1;
                                 when others =>
                                    Raise_Exception
                                    (  Data_Error'Identity,
                                       (  "Invalid escape sequence ["
                                       &  Source (Index..Source'Last)
                                       &  "]"
                                    )  );
                              end case;
                           when Character'Pos ('+') |
                                Character'Pos ('=') |
                                Character'Pos ('#') |
                                Character'Pos (',') |
                                Character'Pos (';') |
                                Character'Pos ('<') |
                                Character'Pos ('>') |
                                Character'Pos ('"') =>
                              Index := Start;
                              exit;
                           when Character'Pos (' ') =>
                              Spaces := Spaces + 1;
                           when others =>
                              Size   := Size + Index - Start + Spaces;
                              Spaces := 0;
                        end case;
                     end loop;
                  end;
               end if;
            end;
            exit when Index > Source'Last or else Source (Index) /= '+';
            Index  := Index + 1;
            Spaces := 0;
            Skip_Spaces (Source, Index);
         end loop;
         exit when Index > Source'Last or else Source (Index) /= ',';
         Index  := Index + 1;
         Spaces := 0;
         Skip_Spaces (Source, Index);
      end loop;
      Index := Index - Spaces;
   end Check;

   function Do_Compare (Shorter, Longer : String) return Precedence is
      Shift : constant Integer := Longer'First - Shorter'First;
      This  : Character;
      That  : Character;
   begin
      for Index in Shorter'Range loop
         This := To_Lower (Shorter (Index));
         That := To_Lower (Longer (Index + Shift));
         if This /= That then
            if This < That then
               return Less;
            else
               return Greater;
            end if;
         end if;
      end loop;
      if Shorter'Length = Longer'Length then
         return Equal;
      else
         return Less;
      end if;
   end Do_Compare;

   function Compare (Left, Right : String) return Precedence is
   begin
      if Left'Length <= Right'Length then
         return Do_Compare (Left, Right);
      else
         case Do_Compare (Right, Left) is
            when Equal =>
               return Equal;
            when Less =>
               return Greater;
            when Greater =>
               return Less;
         end case;
      end if;
   end Compare;

   function Compare (Left, Right : Attribute_Key) return Precedence is
   begin
      if Left.Mode = Right.Mode then
         case Left.Mode is
            when OID_Keyed =>
               if Left.Identifier = Right.Identifier then
                  return Equal;
               elsif Left.Identifier < Right.Identifier then
                  return Less;
               else
                  return Greater;
               end if;
            when Text_Keyed =>
               return Compare (Left.Text, Right.Text);
         end case;
      else
         if Left.Mode < Right.Mode then
            return Less;
         else
            return Greater;
         end if;
      end if;
   end Compare;

   function Compare
            (  Name  : Distinguished_Name;
               Left  : Pair;
               Right : Pair
            )  return Precedence is
   begin
      if Left.Mode = Right.Mode then
         case Left.Mode is
            when OID_Keyed =>
               return Compare
                      (  Name.SubIDs
                         (  Left.Key.Start
                         .. Left.Key.Start + Left.Key.Length - 1
                         ),
                         Name.SubIDs
                         (  Right.Key.Start
                         .. Right.Key.Start + Right.Key.Length - 1
                      )  );
            when Text_Keyed =>
               return Compare
                      (  Name.Buffer
                         (  Left.Key.Start
                         .. Left.Key.Start + Left.Key.Length - 1
                         ),
                         Name.Buffer
                         (  Right.Key.Start
                         .. Right.Key.Start + Right.Key.Length - 1
                      )  );
         end case;
      else
         if Left.Mode < Right.Mode then
            return Less;
         else
            return Greater;
         end if;
      end if;
   end Compare;

   function Compare
            (  Name  : Distinguished_Name;
               Left  : Attribute_Key;
               Right : Pair
            )  return Precedence is
   begin
      if Left.Mode = Right.Mode then
         case Left.Mode is
            when OID_Keyed =>
               declare
                  That : Object_Identifier renames
                         Name.SubIDs
                         (  Right.Key.Start
                         .. Right.Key.Start + Right.Key.Length - 1
                         );
               begin
                  if Left.Identifier = That then
                     return Equal;
                  elsif Left.Identifier < That then
                     return Less;
                  else
                     return Greater;
                  end if;
               end;
            when Text_Keyed =>
               return Compare
                      (  Left.Text,
                         Name.Buffer
                         (  Right.Key.Start
                         .. Right.Key.Start + Right.Key.Length - 1
                      )  );
         end case;
      else
         if Left.Mode < Right.Mode then
            return Less;
         else
            return Greater;
         end if;
      end if;
   end Compare;

   function Find_Attribute
            (  Name      : Distinguished_Name;
               Component : Positive;
               Attribute : Attribute_Key
            )  return Integer is
      From : Integer;
      To   : Integer;
      This : Integer;
   begin
      if Component > Name.Depth then
         raise Constraint_Error;
      end if;
      if Name.Components (Component).Length = 0 then
         return -1;
      end if;
      From := Name.Components (Component).Start;
      To   := From + Name.Components (Component).Length - 1;
      loop
         This := (From + To) / 2;
         case Compare (Name, Attribute, Name.Attributes (This)) is
            when Equal =>
               return This;
            when Greater =>
               if This = From then
                  return -This - 1;
               end if;
               To := This - 1;
            when Less =>
               if This = To then
                  return -This;
               end if;
               From := This + 1;
         end case;
      end loop;
   end Find_Attribute;

   function Find_Attribute
            (  Name      : Distinguished_Name;
               Component : Positive;
               Attribute : Pair
            )  return Integer is
      From : Integer;
      To   : Integer;
      This : Integer;
   begin
      if Component > Name.Depth then
         raise Constraint_Error;
      end if;
      if Name.Components (Component).Length = 0 then
         return -1;
      end if;
      From := Name.Components (Component).Start;
      To   := From + Name.Components (Component).Length - 1;
      loop
         This := (From + To) / 2;
         case Compare (Name, Attribute, Name.Attributes (This)) is
            when Equal =>
               return This;
            when Greater =>
               if This = From then
                  return -This - 1;
               end if;
               To := This - 1;
            when Less =>
               if This = To then
                  return -This;
               end if;
               From := This + 1;
         end case;
      end loop;
   end Find_Attribute;

   function Get
            (  Source  : String;
               Pointer : access Integer
            )  return Distinguished_Name is
      Depth  : Natural;
      Pairs  : Natural;
      Items  : Natural;
      Size   : Natural;
      Index  : Integer := Pointer.all;
      Spaces : Natural := 0;
   begin
      Check (Source, Index, Depth, Pairs, Items, Size);
      Index := Pointer.all;
      declare
         This   : Pair;
         Result : Distinguished_Name
                  (  Depth => Depth,
                     Pairs => Pairs,
                     Items => Items,
                     Size  => Size
                  );
      begin
         Pairs := 1;
         Items := 1;
         Size  := 1;
         for Component in 1..Depth loop
            Result.Components (Component) := (Pairs, 1);
            for Attribute in Positive'Range loop
               if Is_Letter (Source (Index)) then
                  declare
                     Start : constant Integer := Index;
                  begin
                     while Index <= Source'Last loop
                        case Source (Index) is
                           when 'A'..'Z' | 'a'..'z'| '0'..'9' | '-' =>
                              Index := Index + 1;
                           when others =>
                              exit;
                        end case;
                     end loop;
                     This.Mode := Text_Keyed;
                     This.Key  := (Size, Index - Start);
                     Result.Buffer (Size..Size + Index - Start - 1) :=
                        Source (Start..Index - 1);
                     Size := Size + Index - Start;
                  end;
               else
                  declare
                     Last : Integer;
                  begin
                     This.Mode := OID_Keyed;
                     Get
                     (  Source,
                        Index,
                        Result.SubIDs (Items..Result.SubIDs'Last),
                        Last
                     );
                     This.Key := (Items, Last - Items + 1);
                     Items := Last + 1;
                  end;
               end if;
               Skip_Spaces (Source, Index);
               Index := Index + 1; -- Skip =
               Skip_Spaces (Source, Index);
               This.Value.Start := Size;
               if Source (Index) = '#' then
                  Index := Index + 1;
                  while Index <= Source'Last loop
                     exit when
                          not Is_Hexadecimal_Digit (Source (Index));
                     Result.Buffer (Size) :=
                        Character'Val
                        (  Value
                           (  Source => Source (Index..Index + 1),
                              Base   => 16
                        )  );
                     Index := Index + 2;
                     Size  := Size  + 1;
                  end loop;
                  Skip_Spaces (Source, Index);
               else
                  declare
                     Code   : UTF8_Code_Point;
                     Start  : Integer;
                  begin
                     while Index <= Source'Last loop
                        Start := Index;
                        Get (Source, Index, Code);
                        case Code is
                           when Character'Pos ('\') =>
                              while Spaces > 0 loop
                                 Result.Buffer (Size) := ' ';
                                 Size   := Size + 1;
                                 Spaces := Spaces - 1;
                              end loop;
                              if Is_Hexadecimal_Digit
                                 (  Source (Index)
                                 )  then
                                 Result.Buffer (Size) :=
                                    Character'Val
                                    (  Value
                                       (  Source =>
                                             Source (Index..Index + 1),
                                          Base   => 16
                                    )  );
                                 Index := Index + 2;
                                 Size  := Size  + 1;
                              else
                                 Result.Buffer (Size) := Source (Index);
                                 Index := Index + 1;
                                 Size  := Size  + 1;
                              end if;
                           when Character'Pos ('+') |
                                Character'Pos ('=') |
                                Character'Pos ('#') |
                                Character'Pos (',') |
                                Character'Pos (';') |
                                Character'Pos ('<') |
                                Character'Pos ('>') |
                                Character'Pos ('"') =>
                              Index := Start;
                              exit;
                           when Character'Pos (' ') =>
                              Spaces := Spaces + 1;
                           when others =>
                              while Spaces > 0 loop
                                 Result.Buffer (Size) := ' ';
                                 Size   := Size + 1;
                                 Spaces := Spaces - 1;
                              end loop;
                              Result.Buffer
                              (  Size
                              .. Size + Index - Start - 1
                              )  := Source (Start..Index - 1);
                              Size := Size + Index - Start;
                        end case;
                     end loop;
                  end;
               end if;
               This.Value.Length := Size - This.Value.Start;
               if This.Value.Length = 0 then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "Invalid empty attribute value"
                  );
               end if;
               if Attribute = 1 then
                  Result.Attributes (Pairs) := This;
               else
                  declare
                     Offset : Integer :=
                        Find_Attribute (Result, Component, This);
                  begin
                     if Offset > 0 then
                        Raise_Exception
                        (  Data_Error'Identity,
                          "Duplicated attribute key"
                        );
                     end if;
                     Offset := -Offset;
                     Result.Attributes (Offset + 1..Pairs) :=
                        Result.Attributes (Offset..Pairs - 1);
                     Result.Attributes (Offset) := This;
                     Result.Components (Component).Length :=
                        Result.Components (Component).Length + 1;
                  end;
               end if;
               Pairs := Pairs + 1;
               exit when Index > Source'Last
                 or else Source (Index) /= '+';
               Index  := Index + 1;
               Spaces := 0;
               Skip_Spaces (Source, Index);
            end loop;
            exit when Index > Source'Last or else Source (Index) /= ',';
            Index  := Index + 1;
            Spaces := 0;
            Skip_Spaces (Source, Index);
         end loop;
         Pointer.all := Index - Spaces;
         return Result;
      end;
   end Get;

   function Get_Component_Length
            (  Name      : Distinguished_Name;
               Component : Positive
            )  return Positive is
   begin
      if Component > Name.Depth then
         raise Constraint_Error;
      else
         return Name.Components (Component).Length;
      end if;
   end Get_Component_Length;

   function Get_Length (Name : Distinguished_Name) return Natural is
   begin
      return Name.Depth;
   end Get_Length;

   function Get_Attribute
            (  Name      : Distinguished_Name;
               Component : Positive;
               Attribute : Positive := 1
            )  return Name_Attribute is
   begin
      if Component > Name.Depth then
         raise Constraint_Error;
      end if;
      declare
         This : Slice renames Name.Components (Component);
      begin
         if Attribute > This.Length then
            raise Constraint_Error;
         end if;
         declare
            Item : Pair renames
                   Name.Attributes (This.Start + Attribute - 1);
         begin
            case Item.Mode is
               when OID_Keyed =>
                  return
                  (  OID_Keyed,
                     Item.Key.Length,
                     Item.Value.Length,
                     (  OID_Keyed,
                        Item.Key.Length,
                        Name.SubIDs
                        (  Item.Key.Start
                        .. Item.Key.Start + Item.Key.Length - 1
                     )  ),
                     Name.Buffer
                     (  Item.Value.Start
                     .. Item.Value.Start + Item.Value.Length - 1
                  )  );
               when Text_Keyed =>
                  return
                  (  Text_Keyed,
                     Item.Key.Length,
                     Item.Value.Length,
                     (  Text_Keyed,
                        Item.Key.Length,
                        Name.Buffer
                        (  Item.Key.Start
                        .. Item.Key.Start + Item.Key.Length - 1
                     )  ),
                     Name.Buffer
                     (  Item.Value.Start
                     .. Item.Value.Start + Item.Value.Length - 1
                  )  );
            end case;
         end;
      end;
   end Get_Attribute;

   function Get_Key
            (  Name : Distinguished_Name;
               Item : Pair
            )  return Attribute_Key is
   begin
      case Item.Mode is
         when OID_Keyed =>
            return
            (  OID_Keyed,
               Item.Key.Length,
               Name.SubIDs
               (  Item.Key.Start
               .. Item.Key.Start + Item.Key.Length - 1
            )  );
         when Text_Keyed =>
            return
            (  Text_Keyed,
               Item.Key.Length,
               Name.Buffer
               (  Item.Key.Start
               .. Item.Key.Start + Item.Key.Length - 1
            )  );
      end case;
   end Get_Key;

   function Get_Key
            (  Name      : Distinguished_Name;
               Component : Positive;
               Attribute : Positive := 1
            )  return Attribute_Key is
   begin
      if Component > Name.Depth then
         raise Constraint_Error;
      end if;
      declare
         This : Slice renames Name.Components (Component);
      begin
         if Attribute > This.Length then
            raise Constraint_Error;
         end if;
         return Get_Key
                (  Name,
                   Name.Attributes (This.Start + Attribute - 1)
                );
      end;
   end Get_Key;

   function Get_Value
            (  Name : Distinguished_Name;
               Item : Pair
            )  return String is
   begin
      return Name.Buffer
             (  Item.Value.Start
             .. Item.Value.Start + Item.Value.Length - 1
             );
   end Get_Value;

   function Get_Value
            (  Name      : Distinguished_Name;
               Component : Positive;
               Attribute : Positive := 1
            )  return String is
   begin
      if Component > Name.Depth then
         raise Constraint_Error;
      end if;
      declare
         This : Slice renames Name.Components (Component);
      begin
         if Attribute > This.Length then
            raise Constraint_Error;
         end if;
         return Get_Value
                (  Name,
                   Name.Attributes (This.Start + Attribute - 1)
                );
      end;
   end Get_Value;

   function Image (Name : Distinguished_Name) return String is
      Size : Integer := 512;
   begin
      loop
         declare
            Text    : String (1..Size);
            Pointer : Integer := Text'First;
         begin
            Put (Text, Pointer, Name);
            return Text (Text'First..Pointer - 1);
         exception
            when Layout_Error =>
               Size := (Size * 3) / 2;
         end;
      end loop;
   end Image;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Name        : Distinguished_Name;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
      Out_Field : constant Natural :=
         Get_Output_Field (Destination, Pointer, Field);
      subtype Output is String (Pointer..Pointer + Out_Field - 1);
      Index : Integer := Pointer;
      Text  : Output renames
                     Destination (Pointer..Pointer + Out_Field - 1);
   begin
      for Component in 1..Name.Depth loop
         if Component > 1 then
            Put (Text, Index, ",");
         end if;
         for Attribute in 1..Get_Component_Length (Name, Component) loop
            declare
               Code     : UTF8_Code_Point;
               Position : Integer := 1;
               This     : constant Name_Attribute :=
                          Get_Attribute (Name, Component, Attribute);
            begin
               if Attribute > 1 then
                  Put (Text, Index, "+");
               end if;
               case This.Mode is
                  when OID_Keyed =>
                     Put (Text, Index, This.Key.Identifier);
                  when Text_Keyed =>
                     Put (Text, Index, This.Key.Text);
               end case;
               Put (Text, Index, "=");
               while Position <= This.Value'Last loop
                  Get (This.Value, Position, Code);
                  case Code is
                     when 0..31 =>
                        Put (Text, Index, "\");
                        Put
                        (  Destination => Text,
                           Pointer     => Index,
                           Value       => Integer (Code),
                           Base        => 16,
                           Field       => 2,
                           Fill        => '0',
                           Justify     => Right
                        );
                     when Character'Pos ('+') |
                          Character'Pos ('=') |
                          Character'Pos ('#') |
                          Character'Pos (',') |
                          Character'Pos (';') |
                          Character'Pos ('<') |
                          Character'Pos ('>') |
                          Character'Pos ('\') |
                          Character'Pos ('"') =>
                          Put (Text, Index, "\");
                        Put (Text, Index, Code);
                     when others =>
                        Put (Text, Index, Code);
                  end case;
               end loop;
            end;
         end loop;
      end loop;
      Adjust_Output_Field
      (  Destination,
         Pointer,
         Index,
         Out_Field,
         Field,
         Justify,
         Fill
      );
   end Put;

   procedure Skip (Source : String; Pointer : in out Integer) is
      Depth : Natural;
      Pairs : Natural;
      Items : Natural;
      Size  : Natural;
   begin
      Check (Source, Pointer, Depth, Pairs, Items, Size);
   end Skip;

   function Subname
            (  Name : Distinguished_Name;
               From : Positive;
               To   : Positive
            )  return Distinguished_Name is
      Pairs : Natural := 0;
      Items : Natural := 0;
      Size  : Natural := 0;
   begin
      if From > To or else To > Name.Depth then
         raise Constraint_Error;
      end if;
      for Component in From..To loop
         declare
            This : Slice renames Name.Components (Component);
         begin
            Pairs := Pairs + This.Length;
            for Attribute in 0..This.Length - 1 loop
               declare
                  Item : Pair renames
                         Name.Attributes (This.Start + Attribute);
               begin
                  case Item.Mode is
                     when OID_Keyed =>
                        Items := Items + Item.Key.Length;
                     when Text_Keyed =>
                        Size := Size + Item.Key.Length;
                  end case;
                  Size := Size + Item.Value.Length;
               end;
            end loop;
         end;
      end loop;
      declare
         Result : Distinguished_Name
                  (  To - From + 1,
                     Pairs,
                     Items,
                     Size
                  );
      begin
         Pairs := 1;
         Items := 1;
         Size  := 1;
         for Component in From..To loop
            declare
               This : Slice renames Name.Components (Component);
               That : Slice renames
                            Result.Components (Component - From + 1);
            begin
               That.Start  := Pairs;
               That.Length := This.Length;
               for Attribute in 0..This.Length - 1 loop
                  declare
                     Other : Pair renames Result.Attributes (Pairs);
                     Item  : Pair renames
                             Name.Attributes (This.Start + Attribute);
                  begin
                     Other := Item;
                     case Item.Mode is
                        when OID_Keyed =>
                           Other.Key.Start := Items;
                           Result.SubIDs
                           (  Items
                           .. Items + Item.Key.Length - 1
                           )  := Name.SubIDs
                                 (  Item.Key.Start
                                 .. Item.Key.Start + Item.Key.Length - 1
                                 );
                           Items := Items + Item.Key.Length;
                        when Text_Keyed =>
                           Other.Key.Start := Size;
                           Result.Buffer
                           (  Size
                           .. Size + Item.Key.Length - 1
                           )  := Name.Buffer
                                 (  Item.Key.Start
                                 .. Item.Key.Start + Item.Key.Length - 1
                                 );
                           Size := Size + Item.Key.Length;
                     end case;
                     Other.Value.Start := Size;
                     Result.Buffer
                     (  Size
                     .. Size + Item.Value.Length - 1
                     )  := Name.Buffer
                           (  Item.Value.Start
                           .. Item.Value.Start + Item.Value.Length - 1
                           );
                     Size := Size + Item.Value.Length;
                  end;
                  Pairs := Pairs + 1;
               end loop;
            end;
         end loop;
         return Result;
      end;
   end Subname;

   function Value (Source : String) return Distinguished_Name is
      Pointer : aliased Integer := Source'First;
   begin
      Get (Source, Pointer, SpaceAndTab);
      declare
         Result : constant Distinguished_Name :=
                  Get (Source, Pointer'Access);
      begin
         Get (Source, Pointer, SpaceAndTab);
         if Pointer /= Source'Last + 1 then
            raise Data_Error;
         end if;
         return Result;
      end;
   end Value;

   function "=" (Left, Right : Attribute_Key) return Boolean is
   begin
      return Compare (Left, Right) = Equal;
   end "=";

   function "=" (Left, Right : Distinguished_Name) return Boolean is
   begin
      if Left.Depth /= Right.Depth then
         return False;
      end if;
      for Component in Left.Components'Range loop
         declare
            This : Slice renames Left.Components  (Component);
            That : Slice renames Right.Components (Component);
         begin
            if This.Length /= That.Length then
               return False;
            end if;
            for Offset in 0..This.Length - 1 loop
               declare
                  Left_Item  : constant Pair :=
                               Left.Attributes (This.Start + Offset);
                  Right_Item : constant Pair :=
                               Right.Attributes (That.Start + Offset);
               begin
                  if (  (  Get_Key (Left,  Left_Item)
                        /= Get_Key (Right, Right_Item)
                        )
                     or else
                        (  Get_Value (Left,  Left_Item)
                        /= Get_Value (Right, Right_Item)
                     )  )  then
                     return False;
                  end if;
               end;
            end loop;
         end;
      end loop;
      return True;
   end "=";

   function "="
            (  Key   : String;
               Value : String
            )  return Name_Attribute is
   begin
      for Index in Key'Range loop
         case Key (Index) is
            when 'A'..'Z' | 'a'..'z' =>
               null;
            when '0'..'9' | '-' =>
               if Index = Key'First then
                  raise Constraint_Error;
               end if;
            when others =>
               raise Constraint_Error;
         end case;
      end loop;
      return
      (  Text_Keyed,
         Key'Length,
         Value'Length,
         (Text_Keyed, Key'Length, Key),
         Value
      );
   end "=";

   function "="
            (  Key   : String;
               Value : String
            )  return Distinguished_Name is
   begin
      for Index in Key'Range loop
         case Key (Index) is
            when 'A'..'Z' | 'a'..'z' =>
               null;
            when '0'..'9' | '-' =>
               if Index = Key'First then
                  raise Constraint_Error;
               end if;
            when others =>
               raise Constraint_Error;
         end case;
      end loop;
      return
      (  Depth      => 1,
         Pairs      => 1,
         Items      => 0,
         Size       => Key'Length + Value'Length,
         Components => (1 => (1, 1)),
         Attributes => (  1 => (  Text_Keyed,
                                  (1, Key'Length),
                                  (Key'Length + 1, Value'Length)
                       )       ),
         SubIDs     => (1..0 => 0),
         Buffer     => Key & Value
      );
   end "=";

   function "="
            (  Key   : Object_Identifier;
               Value : String
            )  return Name_Attribute is
   begin
      return
      (  OID_Keyed,
         Key'Length,
         Value'Length,
         (OID_Keyed, Key'Length, Key),
         Value
      );
   end "=";

   function "="
            (  Key   : Object_Identifier;
               Value : String
            )  return Distinguished_Name is
   begin
      return
      (  Depth      => 1,
         Pairs      => 1,
         Items      => Key'Length,
         Size       => Value'Length,
         Components => (1 => (1, 1)),
         Attributes => (  1 => (  OID_Keyed,
                                  (1, Key'Length),
                                  (1, Value'Length)
                       )       ),
         SubIDs     => Key,
         Buffer     => Value
      );
   end "=";

   function "<" (Left, Right : Attribute_Key) return Boolean is
   begin
      return Compare (Left, Right) = Less;
   end "<";

   function "<=" (Left, Right : Attribute_Key) return Boolean is
   begin
      return Compare (Left, Right) /= Greater;
   end "<=";

   function ">" (Left, Right : Attribute_Key) return Boolean is
   begin
      return Compare (Left, Right) = Greater;
   end ">";

   function ">=" (Left, Right : Attribute_Key) return Boolean is
   begin
      return Compare (Left, Right) /= Less;
   end ">=";

   function "<" (Left, Right : Distinguished_Name) return Boolean is
   begin
      for Component in Left.Components'Range loop
         if Component > Right.Depth then
            return False;
         end if;
         declare
            This : Slice renames Left.Components  (Component);
            That : Slice renames Right.Components (Component);
         begin
            for Offset in 0..This.Length - 1 loop
               if Offset >= That.Length then
                  return False;
               end if;
               declare
                  Left_Item  : constant Pair :=
                               Left.Attributes (This.Start + Offset);
                  Right_Item : constant Pair :=
                               Right.Attributes (That.Start + Offset);
               begin
                  case Compare
                       (  Get_Key (Left,  Left_Item),
                          Get_Key (Right, Right_Item)
                       )  is
                     when Less =>
                        return True;
                     when Equal =>
                        case Compare
                             (  Get_Value (Left,  Left_Item),
                                Get_Value (Right, Right_Item)
                             )  is
                           when Less =>
                              return True;
                           when Equal =>
                              null;
                           when Greater =>
                              return False;
                        end case;
                     when Greater =>
                        return False;
                  end case;
               end;
            end loop;
         end;
      end loop;
      return False;
   end "<";

   function "<=" (Left, Right : Distinguished_Name) return Boolean is
   begin
      return not (Right < Left);
   end "<=";

   function ">" (Left, Right : Distinguished_Name) return Boolean is
   begin
      return Right < Left;
   end ">";

   function ">=" (Left, Right : Distinguished_Name) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   function "&"
            (  Left  : Distinguished_Name;
               Right : Distinguished_Name
            )  return Distinguished_Name is
      Result : Distinguished_Name
               (  Left.Depth + Right.Depth,
                  Left.Pairs + Right.Pairs,
                  Left.Items + Right.Items,
                  Left.Size  + Right.Size
               );
   begin
      Result.Components := Left.Components & Right.Components;
      Result.Attributes := Left.Attributes & Right.Attributes;
      Result.SubIDs     := Left.SubIDs & Right.SubIDs;
      Result.Buffer     := Left.Buffer & Right.Buffer;
      for Component in Left.Depth + 1..Result.Depth loop
         declare
            Start : Integer renames Result.Components (Component).Start;
         begin
            Start := Start + Left.Pairs;
         end;
      end loop;
      for Attribute in Left.Pairs + 1..Result.Pairs loop
         declare
            Item : Pair renames Result.Attributes (Attribute);
         begin
            case Item.Mode is
               when OID_Keyed =>
                  Item.Key.Start := Item.Key.Start + Left.Items;
               when Text_Keyed =>
                  Item.Key.Start := Item.Key.Start + Left.Size;
            end case;
            Item.Value.Start := Item.Value.Start + Left.Size;
         end;
      end loop;
      return Result;
   end "&";

   function "or"
            (  Left  : Distinguished_Name;
               Right : Name_Attribute
            )  return Distinguished_Name is
      Index : Integer;

      procedure Initialize
                (  Result : in out Distinguished_Name;
                   Item   : Pair
                )  is
      begin
         Result.Components := Left.Components;
         Result.Components (Result.Depth).Length :=
            Result.Components (Result.Depth).Length + 1;
         Result.Attributes (1..Index - 1) :=
            Left.Attributes (1..Index - 1);
         Result.Attributes (Index + 1..Result.Pairs) :=
            Left.Attributes (Index..Left.Pairs);
         Result.Attributes (Index) := Item;
         Result.SubIDs (1..Left.Items) := Left.SubIDs;
         Result.Buffer (1..Left.Size)  := Left.Buffer;
         Result.Buffer
         (  Result.Size - Right.Value_Length + 1
         .. Result.Size
         )  := Right.Value;
      end Initialize;
   begin
      if Left.Depth = 0 then
         raise Constraint_Error;
      end if;
      Index := Find_Attribute (Left, Left.Depth, Right.Key);
      if Index > 0 then
         raise Name_Error;
      end if;
      Index := -Index;
      case Right.Mode is
         when OID_Keyed =>
            declare
               Result : Distinguished_Name
                        (  Left.Depth,
                           Left.Pairs + 1,
                           Left.Items + Right.Key_Length,
                           Left.Size  + Right.Value_Length
                        );
            begin
               Result.SubIDs (Left.Items + 1..Result.Items) :=
                  Right.Key.Identifier;
               Initialize
               (  Result,
                  (  OID_Keyed,
                     (Left.Items + 1, Right.Key_Length),
                     (Left.Size  + 1, Right.Value_Length)
               )  );
               return Result;
            end;
        when Text_Keyed =>
            declare
               Result : Distinguished_Name
                        (  Left.Depth,
                           Left.Pairs + 1,
                           Left.Items,
                           (  Left.Size
                           +  Right.Key_Length
                           +  Right.Value_Length
                        )  );
            begin
               Result.Buffer
               (  Left.Size + 1
               .. Left.Size + Right.Key_Length
               )  := Right.Key.Text;
               Initialize
               (  Result,
                  (  Text_Keyed,
                     (Left.Size + 1, Right.Key_Length),
                     (  Left.Size + 1 + Right.Key_Length,
                        Right.Value_Length
               )  )  );
               return Result;
            end;
      end case;
   end "or";

   function "and"
            (  Left  : Distinguished_Name;
               Right : Name_Attribute
            )  return Distinguished_Name is
      procedure Initialize (Result : in out Distinguished_Name) is
      begin
         Result.Components (1..Left.Depth) := Left.Components;
         Result.Components (Result.Depth)  := (Result.Pairs, 1);
         Result.Attributes (1..Left.Pairs) := Left.Attributes;
         Result.SubIDs     (1..Left.Items) := Left.SubIDs;
         Result.Buffer     (1..Left.Size)  := Left.Buffer;
         Result.Buffer
         (  Result.Size - Right.Value_Length + 1
         .. Result.Size
         )  := Right.Value;
      end Initialize;
   begin
      case Right.Mode is
         when OID_Keyed =>
            declare
               Result : Distinguished_Name
                        (  Left.Depth + 1,
                           Left.Pairs + 1,
                           Left.Items + Right.Key_Length,
                           Left.Size  + Right.Value_Length
                        );
            begin
               Result.Attributes (Result.Pairs) :=
                  (  OID_Keyed,
                     (Left.Items + 1, Right.Key_Length),
                     (Left.Size  + 1, Right.Value_Length)
                  );
               Result.SubIDs (Left.Items + 1..Result.Items) :=
                  Right.Key.Identifier;
               Initialize (Result);
               return Result;
            end;
        when Text_Keyed =>
            declare
               Result : Distinguished_Name
                        (  Left.Depth + 1,
                           Left.Pairs + 1,
                           Left.Items,
                           (  Left.Size
                           +  Right.Key_Length
                           +  Right.Value_Length
                        )  );
            begin
               Result.Attributes (Result.Pairs) :=
                  (  Text_Keyed,
                     (Left.Size + 1, Right.Key_Length),
                     (  Left.Size + 1 + Right.Key_Length,
                        Right.Value_Length
                  )  );
               Result.Buffer
               (  Left.Size + 1
               .. Left.Size + Right.Key_Length
               )  := Right.Key.Text;
               Initialize (Result);
               return Result;
            end;
      end case;
   end "and";

end Strings_Edit.Distinguished_Names;
