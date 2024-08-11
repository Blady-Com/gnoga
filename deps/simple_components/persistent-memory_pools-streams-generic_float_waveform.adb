--                                                                    --
--  package                          Copyright (c) Dmitry A. Kazakov  --
--     Persistent.Memory_Pools.Streams.            Luebeck            --
--        Generic_Float_Waveform                   Summer, 2022       --
--  Implementation                                                    --
--                                Last revision :  18:00 18 Aug 2022  --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body Persistent.Memory_Pools.Streams.Generic_Float_Waveform is

   function "<=" (Left, Right : Y_Type) return Boolean is
      pragma Inline ("<=");
   begin
      return not (Right < Left);
   end "<=";

   function "<=" (Left, Right : X_Type) return Boolean is
      pragma Inline ("<=");
   begin
      return not (Right < Left);
   end "<=";

   function ">=" (Left, Right : Y_Type) return Boolean is
      pragma Inline (">=");
   begin
      return not (Left < Right);
   end ">=";

   function ">=" (Left, Right : X_Type) return Boolean is
      pragma Inline (">=");
   begin
      return not (Left < Right);
   end ">=";

   function Root
            (  Y      : Y_Type;
               X1, X2 : X_Type;
               Y1, Y2 : Y_Type
            )  return X_Type is
      pragma Inline (Root);
   begin
      return X2 + (X1 - X2) * (Y2 - Y) / (Y2 - Y1);
   end Root;

   procedure Add (Tag : in out Min_Max; Value : Y_Type) is
      pragma Inline (Add);
   begin
      if Value < Tag.Min then
         Tag.Min := Value;
      end if;
      if Tag.Max < Value then
         Tag.Max := Value;
      end if;
   end Add;

   procedure Add (Tag : in out Min_Max; Value : Min_Max) is
      pragma Inline (Add);
   begin
      if Value.Min < Tag.Min then
         Tag.Min := Value.Min;
      end if;
      if Tag.Max < Value.Max then
         Tag.Max := Value.Max;
      end if;
   end Add;

   procedure Add
             (  Container : in out Waveform;
                X         : X_Type;
                Y         : Y_Type
             )  is
      Lock : Holder (Container.Pool);
   begin
      Add (Container, From_X (X), From_Y (Y));
      Drop_Tags (Container);
   end Add;

   function Compare
            (  Container : Waveform;
               Left      : Byte_Index;
               Right     : Byte_Index
            )  return Precedence is
      X1 : constant X_Type := To_X (Left);
      X2 : constant X_Type := To_X (Right);
   begin
      if X1 < X2 then
         return Less;
      elsif X1 = X2 then
         return Equal;
      else
         return Greater;
      end if;
   end Compare;

   function Condition
            (  Y1, Y2 : Y_Type;
               Parameter : Comparator
            )  return Boolean is
   begin
      case Parameter.Mode is
         when Above_Threshold =>
            return Parameter.Y1 <= Y1;
         when Below_Threshold =>
            return Parameter.Y1 >= Y2;
         when Inside_Range =>
            return not (Y2 < Parameter.Y1 or else Parameter.Y2 < Y1);
         when Outside_Range =>
            return Y1 < Parameter.Y1 or else Parameter.Y2 < Y2;
      end case;
   end Condition;

   procedure Drop_Tags (Container : in out Waveform) is
      Lock : Holder (Container.Pool);
   begin
      if Container.Last /= 0 then
         Update_Tag_Bucket
         (  Container.Pool.all,
            Container.Last
         ) .Next := Container.Free;
         Container.Last := 0;
      end if;
      Container.Free  := Container.First;
      Container.First := 0;
      Container.Used  := 0;
   end Drop_Tags;

   procedure Erase (Container : in out Waveform) is
      Lock : Holder (Container.Pool);
   begin
      Erase (B_Tree (Container));
      Drop_Tags (Container);
      if Container.Head /= 0 then
         Unchecked_Deallocate (Container.Pool.all, Container.Head);
         Container.Head := 0;
      end if;
   end Erase;

   procedure Finalize (Container : in out Waveform) is
   begin
      Finalize (B_Tree (Container));
      if Is_Writable (Container.Pool.File.all) then
         declare
            Pool : Persistent_Pool'Class renames Container.Pool.all;
            Lock : Holder (Container.Pool);
            This : Byte_Index := Container.First;
            Next : Byte_Index;
         begin
            while This /= 0 loop
               Next := Load_Tag_Bucket (Pool, This).Next;
               Unchecked_Deallocate (Pool, This);
               This := Next;
            end loop;
            This := Container.Free;
            while This /= 0 loop
               Next := Load_Tag_Bucket (Pool, This).Next;
               Unchecked_Deallocate (Pool, This);
               This := Next;
            end loop;
            Drop_Tags (Container);
         end;
      end if;
   end Finalize;

   function Find_Exact
            (  Container  : Waveform;
               X1, X2     : X_Type;
               Parameters : Comparator;
               Autotag    : Boolean
            )  return X_Type is
      Left  : Item_Ptr;
      Right : Item_Ptr;
   begin
      if X2 < X1 or else Is_Empty (Container) then
         Raise_Exception (Constraint_Error'Identity, Empty_Tree);
      elsif X2 = X1 then
         declare
            Value : constant Y_Type := Get (Container, X1, None);
         begin
            if Condition (Value, Value, Parameters) then
               return X1;
            else
               Raise_Exception (End_Error'Identity, Not_Found);
            end if;
         end;
      end if;
      Left := Sup (Container, From_X (X1));
      if Left = No_Item then
         Raise_Exception (End_Error'Identity, Not_Found);
      end if;
      Right := Search (Container, Left, X2, Parameters, Autotag);
      if Right = No_Item then
         Raise_Exception (End_Error'Identity, Not_Found);
      else
         return X_Of (Right);
      end if;
   end Find_Exact;

   function Find_Rightmost
            (  Container  : Waveform;
               X1, X2     : X_Type;
               Parameters : Comparator;
               Autotag    : Boolean
            )  return X_Type is
      Left  : Item_Ptr;
      Right : Item_Ptr;
   begin
      if X2 < X1 or else Is_Empty (Container) then
         Raise_Exception (Constraint_Error'Identity, Empty_Tree);
      elsif X2 = X1 then
         declare
            Value : constant Y_Type := Get (Container, X1, None);
         begin
            if Condition (Value, Value, Parameters) then
               return X1;
            else
               Raise_Exception (End_Error'Identity, Not_Found);
            end if;
         end;
      end if;
      Left := Inf (Container, From_X (X1));
      if Left = No_Item then
         Left := Sup (Container, From_X (X1));
         if Left = No_Item then
            Raise_Exception (End_Error'Identity, Not_Found);
         end if;
      end if;
      Right := Search (Container, Left, X2, Parameters, Autotag);
      if Right = No_Item then
         Raise_Exception (End_Error'Identity, Not_Found);
      else
         return X_Of (Right);
      end if;
   end Find_Rightmost;

   function Find_Linear
            (  Container  : Waveform;
               X1, X2     : X_Type;
               Parameters : Comparator;
               Autotag    : Boolean
            )  return X_Type is
      Left  : Item_Ptr;
      Right : Item_Ptr;
      X     : X_Type;
   begin
      if X2 < X1 or else Is_Empty (Container) then
         Raise_Exception (Constraint_Error'Identity, Empty_Tree);
      elsif X2 = X1 then
         declare
            Value : constant Y_Type := Get (Container, X1, None);
         begin
            if Condition (Value, Value, Parameters) then
               return X1;
            else
               Raise_Exception (End_Error'Identity, Not_Found);
            end if;
         end;
      end if;
      Left := Inf (Container, From_X (X1));
      if Left = No_Item then
         Left := Sup (Container, From_X (X1));
         if Left = No_Item then
            Raise_Exception (End_Error'Identity, Not_Found);
         end if;
      end if;
      Right := Sup (Container, From_X (X2));
      if Right = No_Item then
         Right := Sup (Container, From_X (X1));
         if Left = No_Item then
            Raise_Exception (End_Error'Identity, Not_Found);
         end if;
      end if;
      Right :=
         Search (Container, Left, X_Of (Right), Parameters, Autotag);
      if Right = No_Item then
         Raise_Exception (End_Error'Identity, Not_Found);
      end if;
      X := X_Of (Right);
      if X2 < X then -- After X2
         Left := Get_Previous (Right);
         if Left = No_Item then
            Raise_Exception (End_Error'Identity, Not_Found);
         end if;
         X := Root
              (  Y  => Parameters.Y1,
                 X1 => X_Of (Left),
                 X2 => X,
                 Y1 => Y_Of (Left),
                 Y2 => Y_Of (Right)
              );
         if X2 < X then
            Raise_Exception (End_Error'Identity, Not_Found);
         end if;
         return X;
      elsif X < X1 then -- Before X1
         Left  := Right;
         Right := Get_Next (Left);
         if Right = No_Item then
            Raise_Exception (End_Error'Identity, Not_Found);
         end if;
         X := Root
              (  Y  => Parameters.Y1,
                 X1 => X,
                 X2 => X_Of (Right),
                 Y1 => Y_Of (Right),
                 Y2 => Y_Of (Right)
              );
         if X < X1 then
            Raise_Exception (End_Error'Identity, Not_Found);
         end if;
         return X;
      else
         Left := Get_Previous (Right);
         if Left = No_Item then
            if X < X1 or else X2 < X then
               Raise_Exception (End_Error'Identity, Not_Found);
            end if;
            return X;
         end if;
         case Parameters.Mode is
            when Above_Threshold | Below_Threshold =>
               X := Root
                    (  Y  => Parameters.Y1,
                       X1 => X_Of (Left),
                       X2 => X,
                       Y1 => Y_Of (Left),
                       Y2 => Y_Of (Right)
                    );
            when Inside_Range =>
               if Y_Of (Left) < Parameters.Y1 then
                  X := Root
                       (  Y  => Parameters.Y1,
                          X1 => X_Of (Left),
                          X2 => X,
                          Y1 => Y_Of (Left),
                          Y2 => Y_Of (Right)
                       );
               else
                  X := Root
                       (  Y  => Parameters.Y2,
                          X1 => X_Of (Left),
                          X2 => X,
                          Y1 => Y_Of (Left),
                          Y2 => Y_Of (Right)
                       );
               end if;
            when Outside_Range =>
               if Parameters.Y2 < Y_Of (Right) then
                  X := Root
                       (  Y  => Parameters.Y2,
                          X1 => X_Of (Left),
                          X2 => X,
                          Y1 => Y_Of (Left),
                          Y2 => Y_Of (Right)
                       );
               else
                  X := Root
                       (  Y  => Parameters.Y1,
                          X1 => X_Of (Left),
                          X2 => X,
                          Y1 => Y_Of (Left),
                          Y2 => Y_Of (Right)
                       );
               end if;
         end case;
         if X < X1 then
            return X1;
         elsif X2 < X then
            Raise_Exception (End_Error'Identity, Not_Found);
         else
            return X;
         end if;
      end if;
   end Find_Linear;

   function Find
            (  Container  : Waveform;
               X1, X2     : X_Type;
               Y          : Y_Type;
               Comparison : Threshold_Comparison;
               Mode       : Interpolation_Mode;
               Autotag    : Boolean := True
            )  return X_Type is
   begin
      case Mode is
         when None =>
            case Comparison is
               when Above =>
                  return Find_Exact
                         (  Container,
                            X1, X2,
                            (Above_Threshold, Y, Y),
                            Autotag
                         );
               when Below =>
                  return Find_Exact
                         (  Container,
                            X1, X2,
                            (Below_Threshold, Y, Y),
                            Autotag
                         );
            end case;
         when Rightmost =>
            case Comparison is
               when Above =>
                  return Find_Rightmost
                         (  Container,
                            X1, X2,
                            (Above_Threshold, Y, Y),
                            Autotag
                         );
               when Below =>
                  return Find_Rightmost
                         (  Container,
                            X1, X2,
                            (Below_Threshold, Y, Y),
                            Autotag
                         );
            end case;
         when Linear =>
            case Comparison is
               when Above =>
                  return Find_Linear
                         (  Container,
                            X1, X2,
                            (Above_Threshold, Y, Y),
                            Autotag
                         );
               when Below =>
                  return Find_Linear
                         (  Container,
                            X1, X2,
                            (Below_Threshold, Y, Y),
                            Autotag
                         );
            end case;
      end case;
   end Find;

   function Find
            (  Container  : Waveform;
               X1, X2     : X_Type;
               Y1, Y2     : Y_Type;
               Comparison : Range_Comparison;
               Mode       : Interpolation_Mode;
               Autotag    : Boolean := True
            )  return X_Type is
   begin
      if Y2 < Y1 then
         Raise_Exception (Constraint_Error'Identity, Bad_Interval);
      end if;
      case Mode is
         when None =>
            case Comparison is
               when Inside =>
                  return Find_Exact
                         (  Container,
                            X1, X2,
                            (Inside_Range, Y1, Y2),
                            Autotag
                         );
               when Outside =>
                  return Find_Exact
                         (  Container,
                            X1, X2,
                            (Outside_Range, Y1, Y2),
                            Autotag
                         );
            end case;
         when Rightmost =>
            case Comparison is
               when Inside =>
                  return Find_Rightmost
                         (  Container,
                            X1, X2,
                            (Inside_Range, Y1, Y2),
                            Autotag
                         );
               when Outside =>
                  return Find_Rightmost
                         (  Container,
                            X1, X2,
                            (Outside_Range, Y1, Y2),
                            Autotag
                         );
            end case;
         when Linear =>
            case Comparison is
               when Inside =>
                  return Find_Linear
                         (  Container,
                            X1, X2,
                            (Inside_Range, Y1, Y2),
                            Autotag
                         );
               when Outside =>
                  return Find_Linear
                         (  Container,
                            X1, X2,
                            (Outside_Range, Y1, Y2),
                            Autotag
                         );
            end case;
      end case;
   end Find;

   function Find
            (  Container : Waveform;
               Iterator  : access Abstract_Visitor'Class;
               X1, X2    : X_Type;
               Autotag   : Boolean := True
            )  return Search_Outcome is
      Derived : Waveform'Class renames Waveform'Class (Container);
      Right   : Item_Ptr;

      function Visit_Item
               (  Container : B_Tree;
                  Key       : Byte_Index;
                  Item      : Item_Ptr
               )  return Boolean is
         Value : constant Y_Type := Y_Of (Item);
      begin
         if Condition (Iterator, Derived, Value, Value) then
            Right := Item;
            return False;
         else
            return True;
         end if;
      end Visit_Item;

      function Visit_Range
               (  Container : B_Tree;
                  Item      : Item_Ptr
               )  return Bucket_Traversal is
          Tag : constant Min_Max :=
                         Load_Tag
                         (  Container.Pool.all,
                            Get_Bucket_Address (Item)
                         );
      begin
         if Condition (Iterator, Derived, Tag.Min, Tag.Max) then
            return Step_In;
         else
            return Step_Over;
         end if;
      end Visit_Range;

      procedure Traverse is new Generic_Traverse;

      Lock : Holder (Container.Pool);
      Left : Item_Ptr;
   begin
      if X2 < X1 or else Is_Empty (Container) then
         return (Kind_Of => Empty);
      end if;
      Left := Sup (Container, From_X (X1));
      if Left = No_Item or else X2 < X_Of (Left) then
         return (Kind_Of => Greater);
      end if;
      if Container.Used = 0 then
         if not Autotag then
            Raise_Exception (Status_Error'Identity, Tag_Error);
         end if;
         declare
            Result : Min_Max;
         begin
            Tag_Unlocked
            (  Waveform'Class (Get_Self (Container).all),
               Get_Root (Container),
               0.0, 1.0, null,
               Result
            );
         end;
      end if;
      Right := No_Item;
      Traverse (B_Tree (Container), Left, From_X (X2));
      if Right = No_Item then
         return (Kind_Of => Greater);
      end if;
      Left := Get_Previous (Right);
      if Left = No_Item or else X_Of (Left) < X1 then
         Left := Right;
      end if;
      return
      (  Kind_Of => Inside,
         X1      => X_Of (Left),
         X2      => X_Of (Right),
         Y1      => Y_Of (Left),
         Y2      => Y_Of (Right)
      );
   end Find;

   function Generic_Find
            (  Container : Waveform;
               X1, X2    : X_Type;
               Autotag   : Boolean := True
            )  return Search_Outcome is
      Right : Item_Ptr;

      function Visit_Item
               (  Container : B_Tree;
                  Key       : Byte_Index;
                  Item      : Item_Ptr
               )  return Boolean is
         Value : constant Y_Type := Y_Of (Item);
      begin
         if Condition (Value, Value) then
            Right := Item;
            return False;
         else
            return True;
         end if;
      end Visit_Item;

      function Visit_Range
               (  Container : B_Tree;
                  Item      : Item_Ptr
               )  return Bucket_Traversal is
          Tag : constant Min_Max :=
                         Load_Tag
                         (  Container.Pool.all,
                            Get_Bucket_Address (Item)
                         );
      begin
         if Condition (Tag.Min, Tag.Max) then
            return Step_In;
         else
            return Step_Over;
         end if;
      end Visit_Range;

      procedure Traverse is new Generic_Traverse;

      Lock : Holder (Container.Pool);
      Left : Item_Ptr;
   begin
      if X2 < X1 or else Is_Empty (Container) then
         return (Kind_Of => Empty);
      end if;
      Left := Sup (Container, From_X (X1));
      if Left = No_Item or else X2 < X_Of (Left) then
         return (Kind_Of => Greater);
      end if;
      if Container.Used = 0 then
         if not Autotag then
            Raise_Exception (Status_Error'Identity, Tag_Error);
         end if;
         declare
            Result : Min_Max;
         begin
            Tag_Unlocked
            (  Waveform'Class (Get_Self (Container).all),
               Get_Root (Container),
               0.0, 1.0, null,
               Result
            );
         end;
      end if;
      Right := No_Item;
      Traverse (B_Tree (Container), Left, From_X (X2));
      if Right = No_Item then
         return (Kind_Of => Greater);
      end if;
      Left := Get_Previous (Right);
      if Left = No_Item or else X_Of (Left) < X1 then
         Left := Right;
      end if;
      return
      (  Kind_Of => Inside,
         X1      => X_Of (Left),
         X2      => X_Of (Right),
         Y1      => Y_Of (Left),
         Y2      => Y_Of (Right)
      );
   end Generic_Find;

   function Get
            (  Container : Waveform;
               X         : X_Type;
               Mode      : Interpolation_Mode
            )  return Y_Type is
      Lock : Holder (Container.Pool);
   begin
      if Is_Empty (Container) then
         Raise_Exception (Constraint_Error'Identity, Empty_Tree);
      end if;
      declare
         Key  : constant Byte_Index := From_X (X);
         Left : constant Item_Ptr   := Inf (Container, Key);
      begin
         if Left = No_Item then
            Raise_Exception (End_Error'Identity, Not_Found);
         elsif Get_Key (Left) = Key then
            return Y_Of (Left);
         end if;
         case Mode is
            when None =>
               Raise_Exception (End_Error'Identity, Not_Found);
            when Rightmost =>
               return Y_Of (Left);
            when Linear =>
               declare
                  Right : constant Item_Ptr := Get_Next (Left);
               begin
                  if Left = No_Item then
                     Raise_Exception (End_Error'Identity, No_Right);
                  end if;
                  return Interpolate
                         (  X,
                            X_Of (Left),
                            X_Of (Right),
                            Y_Of (Left),
                            Y_Of (Right)
                         );
               end;
         end case;
      end;
   end Get;

   function Get
            (  Container : Waveform;
               X         : X_Type
            )  return Search_Outcome is
      Key   : constant Byte_Index := From_X (X);
      Lock  : Holder (Container.Pool);
      Left  : Item_Ptr;
      Right : Item_Ptr;
   begin
      if Is_Empty (Container) then
         return (Kind_Of => Empty);
      end if;
      Left := Inf (Container, Key);
      if Left = No_Item then
         return (Kind_Of => Less);
      end if;
      Right := Sup (Container, Key);
      if Right = No_Item then
         return (Kind_Of => Greater);
      end if;
      return
      (  Kind_Of => Inside,
         X1      => X_Of (Left),
         X2      => X_Of (Right),
         Y1      => Y_Of (Left),
         Y2      => Y_Of (Right)
      );
   end Get;

   procedure Get_Convex
             (  Container : Waveform;
                X1, X2    : X_Type;
                Mode      : Interpolation_Mode;
                Y1, Y2    : out Y_Type;
                Autotag   : Boolean := True
             )  is
      Result : Min_Max;

      function Visit_Item
               (  Container : B_Tree;
                  Key       : Byte_Index;
                  Item      : Item_Ptr
               )  return Boolean is
      begin
         Add (Result, Y_Of (Item));
         return True;
      end Visit_Item;

      function Visit_Range
               (  Container : B_Tree;
                  Item      : Item_Ptr
               )  return Bucket_Traversal is
      begin
         Add (Result, Load_Tag (Container.Pool.all, Get_Tag (Item)));
         return Step_Over;
      end Visit_Range;

      procedure Traverse is new Generic_Traverse;

      First : Item_Ptr;
      Lock  : Holder (Container.Pool);
   begin
      if X2 < X1 then
         Raise_Exception (Constraint_Error'Identity, Bad_Interval);
      elsif Is_Empty (Container) then
         Raise_Exception (Constraint_Error'Identity, Empty_Tree);
      elsif X1 = X2 then
         Y1 := Get (Container, X1, Mode);
         Y2 := Y1;
         return;
      end if;
      if Container.Used = 0 then
         if not Autotag then
            Raise_Exception (Status_Error'Identity, Tag_Error);
         end if;
         Tag_Unlocked
         (  Waveform'Class (Get_Self (Container).all),
            Get_Root (Container),
            0.0, 1.0, null,
            Result
         );
      end if;
      Result.Min := Y_Type'Last;
      Result.Max := Y_Type'First;
      if Mode /= None then
         Add (Result, Get (Container, X1, Mode));
      end if;
      First := Sup (Container, From_X (X1));
      if First /= No_Item then
         Traverse (B_Tree (Container), First, From_X (X2));
      end if;
      if Mode = Linear then
         Add (Result, Get (Container, X2, Mode));
      end if;
      if Result.Max < Result.Min then
         Raise_Exception (Constraint_Error'Identity, Not_Found);
      end if;
      Y1 := Result.Min;
      Y2 := Result.Max;
   end Get_Convex;

   function Get_First_X (Container : Waveform) return X_Type is
      First : Item_Ptr;
      Lock  : Holder (Container.Pool);
   begin
      First := Get_First (B_Tree (Container));
      if First = No_Item then
         Raise_Exception (Constraint_Error'Identity, Empty_Tree);
      else
         return X_Of (First);
      end if;
   end Get_First_X;

   function Get_First_Y (Container : Waveform) return Y_Type is
      First : Item_Ptr;
      Lock  : Holder (Container.Pool);
   begin
      First := Get_First (B_Tree (Container));
      if First = No_Item then
         Raise_Exception (Constraint_Error'Identity, Empty_Tree);
      else
         return Y_Of (First);
      end if;
   end Get_First_Y;

   function Get_Last_X (Container : Waveform) return X_Type is
      Last : Item_Ptr;
      Lock : Holder (Container.Pool);
   begin
      Last := Get_Last (B_Tree (Container));
      if Last = No_Item then
         Raise_Exception (Constraint_Error'Identity, Empty_Tree);
      else
         return X_Of (Last);
      end if;
   end Get_Last_X;

   function Get_Last_Y (Container : Waveform) return Y_Type is
      Last : Item_Ptr;
      Lock : Holder (Container.Pool);
   begin
      Last := Get_Last (B_Tree (Container));
      if Last = No_Item then
         Raise_Exception (Constraint_Error'Identity, Empty_Tree);
      else
         return Y_Of (Last);
      end if;
   end Get_Last_Y;

   function Get_Next
            (  Container : Waveform;
               X         : X_Type
            )  return Point is
      Lock  : Holder (Container.Pool);
      Right : Item_Ptr := Sup (Container, From_X (X));
   begin
      if Right = No_Item then
         Raise_Exception (End_Error'Identity, Not_Found);
      end if;
      if not (X < X_Of (Right)) then
         Right := Get_Next (Right);
         if Right = No_Item then
            Raise_Exception (End_Error'Identity, Not_Found);
         end if;
      end if;
      return (X_Of (Right), Y_Of (Right));
   end Get_Next;

   function Get_Point (Item : Item_Ptr) return Point is
   begin
      if Item = No_Item then
         Raise_Exception (Constraint_Error'Identity, Invalid_Item);
      end if;
      declare
         Lock : Holder (Get_Self (Item).Pool);
      begin
         return (X_Of (Item), Y_Of (Item));
      end;
   end Get_Point;

   function Get_Previous
            (  Container : Waveform;
               X         : X_Type
            )  return Point is
      Lock : Holder (Container.Pool);
      Left : Item_Ptr := Inf (Container, From_X (X));
   begin
      if Left = No_Item then
         Raise_Exception (End_Error'Identity, Not_Found);
      end if;
      if not (X < X_Of (Left)) then
         Left := Get_Previous (Left);
         if Left = No_Item then
            Raise_Exception (End_Error'Identity, Not_Found);
         end if;
      end if;
      return (X_Of (Left), Y_Of (Left));
   end Get_Previous;

   procedure Get_Tag
             (  Item : Item_Ptr;
                Min  : out Y_Type;
                Max  : out Y_Type
             )  is
   begin
      if Item = No_Item then
         Raise_Exception (Constraint_Error'Identity, Invalid_Item);
      end if;
      declare
         Container : Waveform renames Waveform (Get_Self (Item).all);
         Tag       : Min_Max;
         Lock      : Holder (Container.Pool);
      begin
         if Container.Used = 0 then
            Raise_Exception (Status_Error'Identity, Tag_Error);
         end if;
         Tag := Load_Tag (Container.Pool.all, Get_Tag (Item));
         Min := Tag.Min;
         Max := Tag.Max;
      end;
   end Get_Tag;

   function Get_X (Item : Item_Ptr) return X_Type is
   begin
      if Item = No_Item then
         Raise_Exception (Constraint_Error'Identity, Invalid_Item);
      end if;
      declare
         Container : Waveform renames Waveform (Get_Self (Item).all);
         Lock      : Holder (Container.Pool);
      begin
         return X_Of (Item);
      end;
   end Get_X;

   function Get_Y (Item : Item_Ptr) return Y_Type is
   begin
      if Item = No_Item then
         Raise_Exception (Constraint_Error'Identity, Invalid_Item);
      end if;
      declare
         Container : Waveform renames Waveform (Get_Self (Item).all);
         Lock      : Holder (Container.Pool);
      begin
         return Y_Of (Item);
      end;
   end Get_Y;

   function X_Of (Item : Item_Ptr) return X_Type is
   begin
      return To_X (Get_Key (Item));
   end X_Of;

   function Y_Of (Item : Item_Ptr) return Y_Type is
   begin
      return To_Y (Get_Value (Item));
   end Y_Of;

   function Interpolate
            (  X, X1, X2 : X_Type;
                  Y1, Y2 : Y_Type
            )  return Y_Type is
      T : constant Y_Type := (X - X1) / (X2 - X1);
   begin
      return Y1 * (1.0 - T) + Y2 * T;
   end Interpolate;

   function Load_Tag
            (  Pool    : Persistent_Pool'Class;
               Pointer : Byte_Index
            )  return Min_Max is
      Block : Block_Type renames Load (Pool.File, Pointer).all;
      Tag   : Min_Max;
      for Tag'Address use Block (Get_Offset (Pointer))'Address;
      pragma Import (Ada, Tag);
   begin
      return Tag;
   end Load_Tag;

   function Load_Tag_Bucket
            (  Pool    : Persistent_Pool'Class;
               Pointer : Byte_Index
            )  return Tag_Bucket_Ref is
      Block  : Block_Type renames Load (Pool.File, Pointer).all;
      Bucket : aliased Tag_Bucket;
      for Bucket'Address use Block (Get_Offset (Pointer))'Address;
      pragma Import (Ada, Bucket);
   begin
      return Bucket'Unchecked_Access;
   end Load_Tag_Bucket;

   function Update_Tag
            (  Pool    : Persistent_Pool'Class;
               Pointer : Byte_Index
            )  return Min_Max_Ptr is
      Block : Block_Type renames Update (Pool.File, Pointer).all;
      Tag   : aliased Min_Max;
      for Tag'Address use Block (Get_Offset (Pointer))'Address;
      pragma Import (Ada, Tag);
   begin
      return Tag'Unchecked_Access;
   end Update_Tag;

   function Update_Tag_Bucket
            (  Pool    : Persistent_Pool'Class;
               Pointer : Byte_Index
            )  return Tag_Bucket_Ptr is
      Block  : Block_Type renames Update (Pool.File, Pointer).all;
      Bucket : aliased Tag_Bucket;
      for Bucket'Address use Block (Get_Offset (Pointer))'Address;
      pragma Import (Ada, Bucket);
   begin
      return Bucket'Unchecked_Access;
   end Update_Tag_Bucket;

   function Inf
            (  Container : Waveform;
               X         : X_Type
            )  return Point is
      Lock  : Holder (Container.Pool);
      Right : constant Item_Ptr := Inf (Container, From_X (X));
   begin
      if Right = No_Item then
         Raise_Exception (End_Error'Identity, Not_Found);
      end if;
      return (X_Of (Right), Y_Of (Right));
   end Inf;

   function Inf
            (  Container : Waveform;
               X         : X_Type
            )  return Item_Ptr is
   begin
      return Inf (Container, From_X (X));
   end Inf;

   function Is_Empty (Container : Waveform) return Boolean is
   begin
      return Is_Empty (B_Tree (Container));
   end Is_Empty;

   function Is_Tagged (Container : Waveform) return Boolean is
      Lock : Holder (Container.Pool);
   begin
      return Is_Empty (Container) or else Container.Used > 0;
   end Is_Tagged;

   procedure Replace
             (  Container : in out Waveform;
                X         : X_Type;
                Y         : Y_Type
             )  is
      Lock : Holder (Container.Pool);
   begin
      Replace (Container, From_X (X), From_Y (Y));
      Drop_Tags (Container);
   end Replace;

   procedure Remove
             (  Container : in out Waveform;
                X1, X2    : X_Type
             )  is
      Lock : Holder (Container.Pool);
      This : Item_Ptr;
      Next : Item_Ptr;
   begin
      if X2 < X1 then
         return;
      end if;
      This := Sup (Container, From_X (X1));
      while This /= No_Item loop
         Next := Get_Next (This);
         Remove (This);
         exit when X2 < X_Of (Next);
         This := Next;
      end loop;
   end Remove;

   procedure Restore
             (  Container : in out Waveform;
                Reference : Byte_Index
             )  is
      Lock   : Holder (Container.Pool);
      Block  : Block_Type renames
               Load (Container.Pool.File, Reference).all;
      Offset : constant Block_Offset := Get_Offset (Reference);
   begin
      Restore (B_Tree (Container), Get (Block, Offset));
      Container.First := Get (Block, Offset +  8);
      Container.Last  := Get (Block, Offset + 16);
      Container.Free  := Get (Block, Offset + 24);
      Container.Used  :=
         Natural (Unsigned_16'(Get (Block, Offset + 32)));
   end Restore;

   procedure Store
             (  Container : in out Waveform;
                Reference : out Byte_Index
             )  is
      Lock : Holder (Container.Pool);
   begin
      if Container.Head = 0 then
         Reference := Unchecked_Allocate
                      (  Container.Pool.all,
                         8 + 8 + 8 + 8 + 2
                      );
      else
         Reference := Container.Head;
      end if;
      declare
         Block  : Block_Type renames
                  Update (Container.Pool.File, Reference).all;
         Offset : constant Block_Offset := Get_Offset (Reference);
         Root   : Byte_Index;
      begin
         Store (B_Tree (Container), Root);
         Put (Block, Offset,      Root);
         Put (Block, Offset +  8, Container.First);
         Put (Block, Offset + 16, Container.Last);
         Put (Block, Offset + 24, Container.Free);
         Put (Block, Offset + 32, Unsigned_16 (Container.Used));
         Container.First := 0;
         Container.Last  := 0;
         Container.Free  := 0;
         Container.Used  := 0;
         Container.Head  := 0;
      end;
   end Store;

   function Search
            (  Container  : Waveform;
               From       : Item_Ptr;
               To         : X_Type;
               Parameters : Comparator;
               Autotag    : Boolean
            )  return Item_Ptr is
      Result : Item_Ptr := No_Item;

      function Visit_Item
               (  Container : B_Tree;
                  Key       : Byte_Index;
                  Item      : Item_Ptr
               )  return Boolean is
         Value : constant Y_Type := Y_Of (Item);
      begin
         if Condition (Value, Value, Parameters) then
            Result  := Item;
            return False;
         else
            return True;
         end if;
      end Visit_Item;

      function Visit_Range
               (  Container : B_Tree;
                  Item      : Item_Ptr
               )  return Bucket_Traversal is
          Tag : constant Min_Max :=
                         Load_Tag
                         (  Container.Pool.all,
                            Get_Bucket_Address (Item)
                         );
      begin
         if Condition (Tag.Min, Tag.Max, Parameters) then
            return Step_In;
         else
            return Step_Over;
         end if;
      end Visit_Range;

      procedure Traverse is new Generic_Traverse;

      Lock : Holder (Container.Pool);
   begin
      if From = No_Item or else To < X_Of (From) then
         return No_Item;
      elsif Container.Used = 0 then
         if not Autotag then
            Raise_Exception (Status_Error'Identity, Tag_Error);
         end if;
         declare
            Result : Min_Max;
         begin
            Tag_Unlocked
            (  Waveform'Class (Get_Self (Container).all),
               Get_Root (Container),
               0.0, 1.0, null,
               Result
            );
         end;
      end if;
      Traverse (B_Tree (Container), From, From_X (To));
      return Result;
   end Search;

   function Sup
            (  Container : Waveform;
               X         : X_Type
            )  return Item_Ptr is
   begin
      return Sup (Container, From_X (X));
   end Sup;

   function Sup
            (  Container : Waveform;
               X         : X_Type
            )  return Point is
      Lock  : Holder (Container.Pool);
      Right : constant Item_Ptr := Sup (Container, From_X (X));
   begin
      if Right = No_Item then
         Raise_Exception (End_Error'Identity, Not_Found);
      end if;
      return (X_Of (Right), Y_Of (Right));
   end Sup;

   procedure Tag_Unlocked
             (  Container : in out Waveform;
                Root      : Item_Ptr;
                Finished  : Float;
                Part      : Float;
                Progress  : Tagging_Progress_Ptr;
                Result    : out Min_Max
             )  is
      Outcome  : Min_Max;
      Length   : constant Natural := Get_Bucket_Size (Root);
      Child    : Item_Ptr;
   begin
      Result.Min := Y_Type'Last;
      Result.Max := Y_Type'First;
      if Length > 0 then
         declare
            Total    : Float := Finished;
            Fraction : constant Float := Part / Float (Length + 1);
         begin
            for Item in 1..Length loop
               Child := Get_Left_Child (Get_Item (Root, Item));
               if Child /= No_Item then
                  Tag_Unlocked
                  (  Container,
                     Child,
                     Total,
                     Fraction,
                     Progress,
                     Outcome
                  );
                  Add (Result, Outcome);
                  if Progress /= null then
                     Total := Total + Fraction;
                     Complete (Progress.all, Float'Min (Total, 1.0));
                  end if;
               end if;
               Add (Result, Y_Of (Get_Item (Root, Item)));
            end loop;
            Child := Get_Right_Child (Get_Item (Root, Length));
            if Child /= No_Item then
               Tag_Unlocked
               (  Container,
                  Child,
                  Total,
                  Fraction,
                  Progress,
                  Outcome
               );
               Add (Result, Outcome);
               if Progress /= null then
                  Total := Total + Fraction;
                  Complete (Progress.all, Float'Min (Total, 1.0));
               end if;
            end if;
         end;
         if (  Container.Last /= 0
            and then
               Container.Used < Tags_Array_Length
            )  then
            Container.Used := Container.Used + 1;
            Update_Tag_Bucket
            (  Container.Pool.all,
               Container.Last
            ). Tags (Container.Used) := Result;
         else
            declare
               Bucket : Byte_Index;
            begin
               if Container.Free = 0 then
                  Bucket :=
                     Unchecked_Allocate
                     (  Container.Pool.all,
                        Tag_Bucket_Size
                     );
               else
                  Bucket := Container.Free;
                  Container.Free :=
                     Load_Tag_Bucket (Container.Pool.all, Bucket).Next;
               end if;
               if Container.First = 0 then
                  Container.First := Bucket;
               elsif Container.Last /= 0 then
                  Update_Tag_Bucket
                  (  Container.Pool.all,
                     Container.Last
                  ) .Next := Bucket;
               end if;
               Container.Last := Bucket;
            end;
            declare
               Bucket : Tag_Bucket renames
                        Update_Tag_Bucket
                        (  Container.Pool.all,
                           Container.Last
                        ) .all;
            begin
               Bucket.Next     := 0;
               Bucket.Tags (1) := Result;
               Container.Used  := 1;
            end;
         end if;
         Set_Tag
         (  Root,
            (  Container.Last
            +  Byte_Index (Container.Used - 1)
            *  Min_Max_Array'Size / (8 * Tags_Array_Length)
         )  );
      end if;
   end Tag_Unlocked;

   procedure Tag (Container : in out Waveform) is
      Result : Min_Max;
      Lock   : Holder (Container.Pool);
      Root   : constant Item_Ptr := Get_Root (Container);
   begin
      if Root = No_Item then
         return;
      end if;
      if Container.Used /= 0 then
         Drop_Tags (Container);
      end if;
      Tag_Unlocked (Container, Root, 0.0, 1.0, null, Result);
   end Tag;

   procedure Tag
             (  Container : in out Waveform;
                Progress  : in out Tagging_Progress'Class
             )  is
      Result : Min_Max;
      Lock   : Holder (Container.Pool);
      Root   : constant Item_Ptr := Get_Root (Container);
   begin
      if Root = No_Item then
         return;
      end if;
      if Container.Used /= 0 then
         Drop_Tags (Container);
      end if;
      Start (Progress);
      Tag_Unlocked
      (  Container,
         Root,
         0.0,
         1.0,
         Progress'Unchecked_Access,
         Result
      );
      Stop (Progress);
   end Tag;

end Persistent.Memory_Pools.Streams.Generic_Float_Waveform;
