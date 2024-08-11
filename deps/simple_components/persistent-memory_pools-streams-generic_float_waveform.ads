--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Memory_Pools.Streams.            Luebeck            --
--        Generic_Float_Waveform                   Summer, 2022       --
--  Interface                                                         --
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
--
--  This generic package  provides a waveform  allocated  in an external
--  storage.  It supports effective search  for various  conditions like
--  greater than a threshold over  large intervals  as  well as  getting
--  value ranges of such intervals. The generic formal parameters:
--
--     X_Type - The  type  of  x-axis.  It  must  fit  into  64-bits and
--              comparison operations = and <.
--     Y_Type - The type  of Y-axis.  It must be  floating-point and fit
--              into 64-bits.
--
--  Cross operations X + Y -> X and X - X -> Y must be also provided.
--
with Persistent.Memory_Pools.Streams.External_B_Tree;

generic
   type X_Type is private;
   type Y_Type is digits <>;
   with function "=" (Left, Right : X_Type) return Boolean is <>;
   with function "<" (Left, Right : X_Type) return Boolean is <>;
   with function "+" (Left : X_Type; Right : Y_Type)
      return X_Type  is <>;
   with function "-" (Left, Right : X_Type)  return Y_Type     is <>;
   with function From_X (Value : X_Type)     return Byte_Index is <>;
   with function From_Y (Value : Y_Type)     return Byte_Index is <>;
   with function To_X   (Value : Byte_Index) return X_Type     is <>;
   with function To_Y   (Value : Byte_Index) return Y_Type     is <>;
package Persistent.Memory_Pools.Streams.Generic_Float_Waveform is
--
-- Interpolation_Mode
--
--    None      - No interpolation, the x-axis value must match exactly
--    Rightmost - The values are extrapolated to the right
--    Linear    - The value interpolated linearly
--
   type Interpolation_Mode is (None, Rightmost, Linear);
--
-- Interpolate -- Linear interpolation or extrapolation
--
--    X      - The value to linearly interpolate or extrapolate
--    X1, X2 - The interval
--    Y1, Y2 - The values at the interval margins
--
   function Interpolate
            (  X, X1, X2 : X_Type;
                  Y1, Y2 : Y_Type
            )  return Y_Type;
--
-- Outcome -- Search
--
   type Location_Type is (Empty, Less, Inside, Greater);
   type Search_Outcome (Kind_Of : Location_Type := Empty) is record
      case Kind_Of is
         when Empty | Less | Greater =>
            null;
         when Inside =>
            X1, X2 : X_Type;
            Y1, Y2 : Y_Type;
      end case;
   end record;
--
-- Point - A waveform item
--
   type Point is record
      X : X_Type;
      Y : Y_Type;
   end record;
--
-- Threshold_Comparison -- Search condition
--
--    Above - The values greater than ro equal to the threshold
--    Below - The values less than ro equal to the threshold
--
   type Threshold_Comparison is (Above, Below);
--
-- Range_Comparison -- Search condition
--
--    Inside  - The values inside given interval
--    Outside - The values outside given interval
--
   type Range_Comparison is (Inside, Outside);
--
-- Waveform -- A container of x-y pairs
--
   type Waveform
        (  Pool : access Persistent_Pool'Class
        )  is new Persistent.Memory_Pools.Streams.External_B_Tree.B_Tree
              with private;
--
-- Add -- Add a new point to the waveform
--
--    Container - The waveform to modify
--    X         - The x-axis value
--    Y         - The y-axis value
--
-- Exceptions :
--
--    Constraint_Error - There is already a point with this x-axis value
--
   procedure Add (Container : in out Waveform; X : X_Type; Y : Y_Type);
--
-- Erase -- Remove all points from the waveform
--
--    Container - The waveform to erase
--
   procedure Erase (Container : in out Waveform);
--
-- Find -- By threshold
--
--    Container  - The waveform
--    X1, X2     - The interval to search in
--    Y          - The threshold
--    Comparison - Above or under threshold
--    Mode       - Interpolation mode
--    Autotag    - Tag if necessary
--
-- This function searches  for the first x-axis point where the value on
-- the y-axis  is equal  or else  above  or  under  the  threshold  Y as
-- specified  by  the  parameter  Comparison.   The  interpolation  mode
-- specifies how gaps between items in the waveform are treated. It tags
-- the waveform if necessary when Autotag is True otherwise Status_Error
-- is propagated.
--
-- Returns :
--
--    The first x-axis value from X1..X2 that meets the condition
--
-- Exceptions :
--
--    Constraint_Error - The waveform is empty or not found
--    End_Error        - Not found
--    Status_Error     - Tagging required
--
   function Find
            (  Container  : Waveform;
               X1, X2     : X_Type;
               Y          : Y_Type;
               Comparison : Threshold_Comparison;
               Mode       : Interpolation_Mode;
               Autotag    : Boolean := True
            )  return X_Type;
--
-- Find -- By range
--
--    Container  - The waveform
--    X1, X2     - The interval to search in
--    Y1, Y2     - The range of values
--    Comparison - Inside or outside the range
--    Mode       - Interpolation mode
--    Autotag    - Tag if necessary
--
-- This function  searches for the first x-axis value  that is  equal or
-- else above  or under  the threshold  Y as specified  by the parameter
-- Comparison. The interpolation  mode specifies  how gaps between items
-- are treated.  It tags the waveform if necessary  when Autotag is True
-- otherwise Status_Error is propagated.
--
-- Returns :
--
--    The first x-axis value from X1..X2 that meets the condition
--
-- Exceptions :
--
--    Constraint_Error - The waveform is empty or bad interval
--    End_Error        - Not found
--    Status_Error     - Tagging required
--
   function Find
            (  Container  : Waveform;
               X1, X2     : X_Type;
               Y1, Y2     : Y_Type;
               Comparison : Range_Comparison;
               Mode       : Interpolation_Mode;
               Autotag    : Boolean := True
            )  return X_Type;
--
-- Generic_Find -- Find the least interval on which a condition is met
--
--    Container - The waveform
--    X1, X2    - The interval to search in
--    Autotag   - Tag if necessary
--
-- This function  tags the waveform  if necessary  when Autotag  is True
-- otherwise Status_Error is propagated.
--
-- Returns :
--
--    The first x-axis value from X1..X2 that meets the condition
--
-- Exceptions :
--
--    Status_Error - Tagging required
--
   generic
      with function Condition (Y1, Y2 : Y_Type) return Boolean is <>;
   function Generic_Find
            (  Container : Waveform;
               X1, X2    : X_Type;
               Autotag   : Boolean := True
            )  return Search_Outcome;
--
-- Abstract_Visitor -- Abstract visitor base type
--
   type Abstract_Visitor is abstract
      new Ada.Finalization.Limited_Controlled with null record;
--
-- Condition - Checking if the search condition is met
--
--    Iterator  - The visitor object
--    Container - The waveform
--    Y1, Y2    - The range of value to check
--
-- Returns :
--
--    True if the range may contain the values searched for
--
   function Condition
            (  Iterator  : access Abstract_Visitor;
               Container : Waveform'Class;
               Y1, Y2    : Y_Type
            )  return Boolean is abstract;
--
-- Find -- Find the least interval on which a condition is met
--
--    Container - The waveform
--    Iterator  - The iterator object
--    X1, X2    - The interval to search in
--    Autotag   - Tag if necessary
--
-- This function  tags the waveform  if necessary  when Autotag  is True
-- otherwise Status_Error is propagated.
--
-- Returns :
--
--    The first x-axis value from X1..X2 that meets the condition
--
-- Exceptions :
--
--    Status_Error - Tagging required
--
   function Find
            (  Container : Waveform;
               Iterator  : access Abstract_Visitor'Class;
               X1, X2    : X_Type;
               Autotag   : Boolean := True
            )  return Search_Outcome;
--
-- Get -- Get value by its x-axis
--
--    Container - The waveform
--    X         - The x-axis value
--    Mode      - Interpolation mode
--
-- Returns :
--
--    The y-axis value
--
-- Exceptions :
--
--    Constraint_Error - Empty waveform
--    End_Error        - The value is not in the waveform
--
   function Get
            (  Container : Waveform;
               X         : X_Type;
               Mode      : Interpolation_Mode
            )  return Y_Type;
--
-- Get -- The least interval containing the x-axis point
--
--    Container - The waveform
--    X         - The x-axis value
--
-- Returns :
--
--    Empty   - Empty waveform
--    Less    - X is less than the least item in waveform
--    Greater - X is greater than the greatest item in waveform
--    Inside  - The least interval containing X
--
   function Get
            (  Container : Waveform;
               X         : X_Type
            )  return Search_Outcome;
--
-- Get_Convex -- Get minimum and maximim on an interval
--
--    Container - The waveform
--    X1, X2    - The interval
--    Mode      - Interpolation mode at the interval ends
--    Y1, Y2    - The inverval of y-values
--
-- This function  tags the waveform  if necessary  when Autotag  is True
-- otherwise Status_Error is propagated.
--
-- Exceptions :
--
--    Constraint_Error - Empty waveform or invalid interval
--    End_Error        - No items found
--    Status_Error     - Tagging required
--
   procedure Get_Convex
             (  Container : Waveform;
                X1, X2    : X_Type;
                Mode      : Interpolation_Mode;
                Y1, Y2    : out Y_Type;
                Autotag   : Boolean := True
             );
--
-- Get_First_X -- Get the least x-axis value
--
--    Container - The waveform
--
-- Returns :
--
--    The least x-axis value
--
-- Exceptions :
--
--    Contraint_Error - The wavefom is empty
--
   function Get_First_X (Container : Waveform) return X_Type;
--
-- Get_First_Y -- Get value at the beginning
--
--    Container - The waveform
--
-- Returns :
--
--    The value at the beginning
--
-- Exceptions :
--
--    Contraint_Error - The wavefom is empty
--
   function Get_First_Y (Container : Waveform) return Y_Type;
--
-- Get_Next -- Get the next point
--
--    Container - The waveform
--    X         - The x-axis
--
-- Returns :
--
--    The point with x-axis greater than X
--
-- Exceptions :
--
--    End_Error - No such item
--
   function Get_Next
            (  Container : Waveform;
               X         : X_Type
            )  return Point;
--
-- Get_Last_X -- Get the greatest x-axis value
--
--    Container - The waveform
--
-- Returns :
--
--    The greatest x-axis value
--
-- Exceptions :
--
--    Contraint_Error - The wavefom is empty
--
   function Get_Last_X (Container : Waveform) return X_Type;
--
-- Get_Last_Y -- Get value at the end
--
--    Container - The waveform
--
-- Returns :
--
--    The value at the end
--
-- Exceptions :
--
--    Contraint_Error - The wavefom is empty
--
   function Get_Last_Y (Container : Waveform) return Y_Type;
--
-- Get_Point -- Get the point
--
--    Item - An item of B-tree
--
-- Returns :
--
--    The point corresponding to the item
--
-- Exceptions :
--
--    Constraint_Error - No item
--
   function Get_Point (Item : External_B_Tree.Item_Ptr) return Point;
--
-- Get_Previous -- Get the previous point
--
--    Container - The waveform
--    X         - The x-axis
--
-- Returns :
--
--    The point with x-axis less than X
--
-- Exceptions :
--
--    End_Error - No such item
--
   function Get_Previous
            (  Container : Waveform;
               X         : X_Type
            )  return Point;
--
-- Get_Tag -- Get the tag associated with the B-tree bucket
--
--    Item - An item of B-tree
--    Min  - The least y-axis value of the bucket and its children
--    Max  - The greatest y-axis value of the bucket and its children
--
-- Exceptions :
--
--    Constraint_Error - No item
--    Status_Error     - The waveform is not tagged
--
   procedure Get_Tag
             (  Item : External_B_Tree.Item_Ptr;
                Min  : out Y_Type;
                Max  : out Y_Type
             );
--
-- Get_X -- Get the x-axis value of an item
--
--    Item - The waveform item indicating a pair (x,y)
--
-- Returns :
--
--    The x-axis value
--
-- Exceptions :
--
--    Contraint_Error - Invalid item
--
   function Get_X (Item : External_B_Tree.Item_Ptr) return X_Type;
--
-- Get_Y -- Get the y-axis value of an item
--
--    Item - The waveform item indicating a pair (x,y)
--
-- Returns :
--
--    The x-axis value
--
-- Exceptions :
--
--    Contraint_Error - Invalid item
--
   function Get_Y (Item : External_B_Tree.Item_Ptr) return Y_Type;
--
-- Inf -- Get the point with lesser or equal x-axis
--
--    Container - The waveform
--    X         - The x-axis
--
-- Returns :
--
--    The point with x-axis less than or equal to X
--
-- Exceptions :
--
--    End_Error - No such item
--
   function Inf
            (  Container : Waveform;
               X         : X_Type
            )  return Point;
--
-- Inf -- Get the point with lesser or equal x-axis
--
--    Container - The waveform
--    X         - The x-axis
--
-- Returns :
--
--    The item of the B-tree with x-axis less than or equal to X
--
   function Inf
            (  Container : Waveform;
               X         : X_Type
            )  return External_B_Tree.Item_Ptr;
--
-- Is_Empty -- Test if the waveform is empty
--
--    Container - The waveform
--
-- Returns :
--
--    True if waveform contains no items
--
   function Is_Empty (Container : Waveform) return Boolean;
--
-- Is_Tagged -- Test if the waveform has ben tagged
--
--    Container - The waveform
--
-- Returns :
--
--    True if waveform is searchable (needs no prior tagging)
--
   function Is_Tagged (Container : Waveform) return Boolean;
--
-- Remove -- Remove an interval from the waveform
--
--    Container - The waveform to modify
--    X1, X2    - The interval to remove
--
   procedure Remove
             (  Container : in out Waveform;
                X1, X2    : X_Type
             );
--
-- Replace -- Add or replace a new point to the waveform
--
--    Container - The waveform to modify
--    X         - The x-axis value
--    Y         - The y-axis value
--
   procedure Replace
             (  Container : in out Waveform;
                X         : X_Type;
                Y         : Y_Type
             );
-- Restore -- Restore waveform stored using Store
--
--    Container - The tree
--    Reference - Obtained by Store
--
-- Exceptions :
--
--    Status_Error - The tree is not empty
--
   procedure Restore
             (  Container : in out Waveform;
                Reference : Byte_Index
             );
--
-- Store -- Store the waveform
--
--    Container - The waveform
--    Reference - To the waveform
--
-- This  procedure  stores  the  waveform.  It  must  be  called  before
-- finalization  if the  waveform  must  persist.  Otherwise  it will be
-- erased. The waveform can be restored using Restore:
--
--    declare
--       Graph : Waveform (...); -- A new waveform
--    begin
--       ... -- Work with the waveform
--       Store (Graph, Reference);
--    end;
--    ...
--    declare
--       Graph : Waveform (...); -- A waveform
--    begin
--       Restore (Graph, Reference);
--       ... -- Continue to work with the waveform
--    end;
--
   procedure Store
             (  Container : in out Waveform;
                Reference : out Byte_Index
             );
--
-- Sup -- Get the point with greater or equal x-axis
--
--    Container - The waveform
--    X         - The x-axis
--
-- Returns :
--
--    The point with x-axis greater than or equal to X
--
-- Exceptions :
--
--    End_Error - No such item
--
   function Sup
            (  Container : Waveform;
               X         : X_Type
            )  return Point;
--
-- Sup -- Get the point with greater or equal x-axis
--
--    Container - The waveform
--    X         - The x-axis
--
-- Returns :
--
--    The item of the B-tree with x-axis greater than or equal to X
--
   function Sup
            (  Container : Waveform;
               X         : X_Type
            )  return External_B_Tree.Item_Ptr;
--
-- Tag -- Tag buckets of the waveform with min-max values
--
--    Container - The waveform to tag
--
-- This procedure tags  buckets of the waveform tree with min-max values
-- to speed  up interval operations.  It can take potentially  very long
-- time.
--
   procedure Tag (Container : in out Waveform);
   procedure Tag
             (  Container : in out Waveform;
                Progress  : in out Tagging_Progress'Class
             );

private
   use Persistent.Memory_Pools.Streams.External_B_Tree;

   pragma Assert (X_Type'Size <= Byte_Index'Size);
   pragma Assert (Y_Type'Size <= Byte_Index'Size);

   pragma Inline (Interpolate);

   type Min_Max is record
      Min : Y_Type;
      Max : Y_Type;
   end record;
   pragma Assert (Min_Max'Size <= 128);
   type Min_Max_Ptr is access all Min_Max;

   procedure Add (Tag : in out Min_Max; Value : Y_Type);
   procedure Add (Tag : in out Min_Max; Value : Min_Max);

   Tags_Array_Length : constant := (Byte_Count'Last - 8) / 16;

   type Min_Max_Array is
      array (1..Tags_Array_Length) of aliased Min_Max;
   type Tag_Bucket is record
      Tags : Min_Max_Array;
      Next : Byte_Index := 0;
   end record;
   Tag_Bucket_Size : constant Byte_Count :=
                     Byte_Count (Integer ((Tag_Bucket'Size + 7) / 8));

   pragma Assert ((Tag_Bucket'Size + 7) / 8 <= Byte_Count'Last);

   type Tag_Bucket_Ref is access constant Tag_Bucket;
   type Tag_Bucket_Ptr is access all Tag_Bucket;

   type Waveform
        (  Pool : access Persistent_Pool'Class
        )  is new B_Tree (Pool) with
   record
      First : Byte_Index := 0; -- First allocated tag bucket
      Last  : Byte_Index := 0; -- Last used tag bucket
      Free  : Byte_Index := 0; -- First free tag bucket
      Head  : Byte_Index := 0; -- Persistent head of the waveform
      Used  : Natural    := 0; -- Last used tag in the last bucket
   end record;

   function Compare
            (  Container : Waveform;
               Left      : Byte_Index;
               Right     : Byte_Index
            )  return Precedence;
   procedure Drop_Tags (Container : in out Waveform);
   procedure Finalize (Container : in out Waveform);

   type Tagging_Progress_Ptr is access all Tagging_Progress'Class;
   procedure Tag_Unlocked
             (  Container : in out Waveform;
                Root      : Item_Ptr;
                Finished  : Float;
                Part      : Float;
                Progress  : Tagging_Progress_Ptr;
                Result    : out Min_Max
             );

   function Load_Tag
            (  Pool    : Persistent_Pool'Class;
               Pointer : Byte_Index
            )  return Min_Max;
   function Load_Tag_Bucket
            (  Pool    : Persistent_Pool'Class;
               Pointer : Byte_Index
            )  return Tag_Bucket_Ref;
   function Update_Tag
            (  Pool    : Persistent_Pool'Class;
               Pointer : Byte_Index
            )  return Min_Max_Ptr;
   function Update_Tag_Bucket
            (  Pool    : Persistent_Pool'Class;
               Pointer : Byte_Index
            )  return Tag_Bucket_Ptr;

   function X_Of (Item : Item_Ptr) return X_Type;
   pragma Inline (X_Of);

   function Y_Of (Item : Item_Ptr) return Y_Type;
   pragma Inline (Y_Of);
--
-- Comparator_Mode -- Linear comparator modes
--
   type Comparator_Mode is
        (  Above_Threshold,
           Below_Threshold,
           Inside_Range,
           Outside_Range
        );
--
-- Comparator -- Linear  comparator  for searching  threshold  and range
--               conditions
--
   type Comparator (Mode : Comparator_Mode) is record
      Y1 : Y_Type;
      Y2 : Y_Type;
   end record;

   function Condition
            (  Y1, Y2 : Y_Type;
               Parameter : Comparator
            )  return Boolean;
   function Search
            (  Container  : Waveform;
               From       : Item_Ptr;
               To         : X_Type;
               Parameters : Comparator;
               Autotag    : Boolean
            )  return Item_Ptr;

end Persistent.Memory_Pools.Streams.Generic_Float_Waveform;
