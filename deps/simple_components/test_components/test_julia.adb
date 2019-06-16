--                                                                    --
--  procedure Test_Julia            Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Winter, 2019       --
--                                                                    --
--                                Last revision :  11:37 20 Jan 2019  --
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
with Ada.Text_IO;        use Ada.Text_IO;
with Interfaces;         use Interfaces;
with Interfaces.C;       use Interfaces.C;
with Julia;              use Julia;

with Julia.Generic_1D_Array;
with Julia.Generic_2D_Array;
with Julia.Generic_3D_Array;

procedure Test_Julia is
   type Double_Array is array (Integer range <>) of Double;
   package Double_Arrays is
      new Generic_1D_Array
          (  Integer,
             Double,
             Double_Array,
             Float64_Type
          );
   type Double_Matrix is
      array (Integer range <>, Integer range <>) of Double;
   package Double_Matrices is
      new Generic_2D_Array
          (  Integer,
             Integer,
             Double,
             Double_Matrix,
             Float64_Type
          );
   type Double_Space is
      array (Integer range <>, Integer range <>, Integer range <>)
         of Double;
   package Double_Spaces is
      new Generic_3D_Array
          (  Integer,
             Integer,
             Integer,
             Double,
             Double_Space,
             Float64_Type
          );

   function Callback (X : Double) return Double;
   pragma Convention (C, Callback);

   function Create_Array return value_t;
   pragma Convention (C, Create_Array);

   function Create_List return value_t;
   pragma Convention (C, Create_List);

   function Callback (X : Double) return Double is
   begin
      Put_Line ("Here in the callback" & Double'Image (X));
      return X / 2.0;
   end Callback;

   function Create_List return value_t is
      Roots  : Holder (4);
      Values : values_array (1..4);
   begin
      Values (1) := To_Julia ("first");
      Set (Roots, 1, Values (1));
      Values (2) := To_Julia (Interfaces.Integer_32'(1));
      Set (Roots, 2, Values (2));
      Values (3) := To_Julia (Double'(2.0));
      Set (Roots, 3, Values (3));
      Values (4) := To_Julia ("last");
      Set (Roots, 4, Values (4));
      return To_Julia (Values);
   end Create_List;

   function Create_Array return value_t is
   begin
      return Double_Arrays.To_Julia ((4.0, 5.0, 6.0));
   end Create_Array;

   function Image (Data : value_t) return String is
   begin
      if TypeOf (Data) = Int64_Type then
         return Integer_64'Image (Value (Data));
      elsif TypeOf (Data) = Int32_Type then
         return Integer_32'Image (Value (Data));
      else
         return Integer_16'Image (Value (Data));
      end if;
   end Image;

begin
--
-- Windows
--
   Load ("D:\Julia-1.0.3\bin\libjulia.dll");
   Init_With_Image ("D:\Julia-1.0.3\bin");
--
-- Debian, package libjulia1
--
-- Load ("/usr/lib/x86_64-linux-gnu/libjulia.so.1");
-- Init_With_Image ("/usr/lib/x86_64-linux-gnu");

-- Load ("/usr/lib/i386-linux-gnu/libjulia.so.1");
-- Init_With_Image ("/usr/lib/i386-linux-gnu");
--
-- Fedora, package julia
--
-- Load ("/usr/lib/libjulia.so.1");
-- Init_With_Image ("/usr/lib");

   Eval_String ("println(sqrt(2.0))");
   begin
      Eval_String ("this_function_does_not_exist()");
   exception
      when Error : Julia_Error => -- This is OK
         Put_Line
         (  "Exception from Julia: "
         &  Exception_Message (Error)
         );
   end;
   declare
      Result : constant value_t := Eval_String ("""some text""");
   begin
      Put_Line ("Text=" & Value (Result));
   end;
   declare
      Result : constant value_t := Eval_String ("sqrt(2.0)");
   begin
      if TypeOf (Result) = Float64_Type then
         Put_Line
         (  "sqrt(2.0) ="
         &  double'Image (Value (Result))
         &  " Type="
         &  TypeName (Float64_Type)
         );
         begin
            Put_Line ("sqrt(2.0)=" & Boolean'Image (Value (Result)));
         exception
            when Error : Constraint_Error =>
               Put_Line (Exception_Message (Error));
         end;
      end if;
   end;
   declare
      Func     : constant function_t := Get_Function
                                        (  Base_Module,
                                           "sqrt"
                                        );
      Argument : constant value_t    := To_Julia (double'(2.0));
      Result   : constant value_t    := Call (Func, Argument);
   begin
      Put_Line
      (  "sqrt(2.0) ="
      &  double'Image (Value (Result))
      );
   end;
   declare
      Result : constant value_t :=
               Eval_String
               (  "errorcode = ccall("
               &  CCall_Address (Callback'Address)
               &  ",Cdouble,(Cdouble,),10.0)"
               );
   begin
      if TypeOf (Result) = Float64_Type then
         Put_Line
         (  "evaluated ="
         &  double'Image (Value (Result))
         );
      end if;
   end;
   begin
      Eval_String
      (  "println(ccall("
      &  CCall_Address (Create_List'Address)
      &  ",Any,()))"
      );
   end;
   declare
      File   : File_Type;
      Result : value_t;
   begin
      Create (File, Out_File, "test.jl");
      Put_Line (File, "sqrt(2.0)");
      Close (File);
      Result := Load (Base_Module, "test.jl");
      Put_Line
      (  "evaluated from file sqrt(2.0) ="
      &  double'Image (Value (Result))
      );
   end;
   declare
      List : constant value_t := Eval_String ("(1,2,3)");
      Result : value_t;
   begin
      Put_Line ("Tuple size: " & Integer'Image (N_Fields (List)));
      Put_Line ("     Tuple: " & Boolean'Image (Is_Tuple (List)));
      Put_Line ("NamedTuple: " & Boolean'Image (Is_NamedTuple (List)));
      for Index in 1..N_Fields (List) loop
         Put_Line
         (  " "
         &  Integer'Image (Index)
         &  " =>"
         &  Image (Get_Field (List, Index))
         );
      end loop;
   end;
   declare
      List : constant value_t := Eval_String ("(a = 1, b = 2, c = 3)");
   begin
      Put_Line ("Named tuple size:"  & Integer'Image (N_Fields (List)));
      Put_Line ("         Type of: " & TypeName (TypeOf (List)));
      Put_Line ("           Tuple: " & Boolean'Image (Is_Tuple (List)));
      Put_Line ("      NamedTuple: " &
                                  Boolean'Image (Is_NamedTuple (List)));
      Put_Line ("         Mutable: " &
                            Boolean'Image (Is_Mutable (TypeOf (List))));
      for Index in 1..N_Fields (List) loop
         Put_Line
         (  "  "
         &  Get_Name (List, Index)
         &  " =>"
         &  Image (Get_Field (List, Index))
         );
      end loop;
      Put_Line ("Accessing by keys:");
      Put_Line ("  a =>" & Image (Get_Field (List, "a")));
      Put_Line ("  b =>" & Image (Get_Field (List, "b")));
      Put_Line ("  c =>" & Image (Get_Field (List, "c")));
   end;
   declare
      use Double_Arrays;
      List : constant value_t := Eval_String ("[1.0,2.0,3.0]");
   begin
      Put_Line ("Array type:  " & TypeOf_Str (List));
      Put_Line ("Array length:" & Integer'Image (Length (List)));
      for Index in 1..Length (List) loop
         Put_Line
         (  " "
         &  Integer'Image (Index)
         &  " =>"
         &  Double'Image (Get (List, Index))
         );
      end loop;
      Put_Line ("Array copy-----------------");
      declare
         Copy : constant Double_Array := Value (List);
      begin
         for Index in Copy'Range loop
            Put_Line
            (  " "
            &  Integer'Image (Index)
            &  " =>"
            &  Double'Image (Copy (Index))
            );
         end loop;
      end;
   end;
   declare
      use Double_Matrices;
      List : value_t := Eval_String ("[1.0 2.0; 3.0 4.0; 5.0 6.0]");
   begin
      Put_Line ("Array type: " & TypeOf_Str (List));
      Put_Line ("      Rows:"  & Integer'Image (Rows (List)));
      Put_Line ("          ="  & Integer'Image (Length (List, 1)));
      Put_Line ("   Columns:"  & Integer'Image (Columns (List)));
      Put_Line ("          ="  & Integer'Image (Length (List, 2)));
      Put_Line (" Dimension:"  & Integer'Image (Dimension (List)));
      Put_Line ("   Element: " & TypeName (Element_Type (List)));
      Put_Line ("     Array: " & Boolean'Image (Is_Array (List)));
      Put_Line ("     Tuple: " & Boolean'Image (Is_Tuple (List)));
      Put_Line ("NamedTuple: " & Boolean'Image (Is_NamedTuple (List)));
      for Row in 1..Rows (List) loop
         Put
         (  " "
         &  Integer'Image (Row)
         &  " =>"
         );
         for Column in 1..Columns (List) loop
            Put (Double'Image (Get (List, Row, Column)) & " ");
         end loop;
         New_Line;
      end loop;
      Put_Line ("Array copy-----------------");
      declare
         Copy : constant Double_Matrix := Value (List);
      begin
         for Row in Copy'Range (1) loop
            Put
            (  " "
            &  Integer'Image (Row)
            &  " =>"
            );
            for Column in Copy'Range (2) loop
               Put (Double'Image (Copy (Row, Column)) & " ");
            end loop;
            New_Line;
         end loop;
      end;
      List := Call
              (  Get_Function (Base_Module, "*"),
                 To_Julia
                 (  Double_Matrix'
                    (  1 => (1.0, 0.0, 0.0),
                       2 => (0.0, 1.0, 0.0)
                 )  ),
                 List
              );
      Put_Line ("Array multiplication-------");
      for Row in 1..Rows (List) loop
         Put
         (  " "
         &  Integer'Image (Row)
         &  " =>"
         );
         for Column in 1..Columns (List) loop
            Put (Double'Image (Get (List, Row, Column)) & " ");
         end loop;
         New_Line;
      end loop;
   end;
   declare
      use Double_Spaces;
      Result : value_t;
      Matrix : constant Double_Space :=
                        (  (( 1.0,  9.0, 17.0), ( 5.0, 13.0, 21.0)),
                           (( 2.0, 10.0, 18.0), ( 6.0, 14.0, 22.0)),
                           (( 3.0, 11.0, 19.0), ( 7.0, 15.0, 23.0)),
                           (( 4.0, 12.0, 20.0), ( 8.0, 16.0, 24.0))
                        );
   begin
      Result := Call
                (  Get_Function (Base_Module, "println"),
                   To_Julia (Matrix)
                );
   end;
   begin
      Eval_String
      (  "println(ccall("
      &  CCall_Address (Create_Array'Address)
      &  ",Any,()))"
      );
   end;
   declare
      Map : constant value_t :=
                     Eval_String ("Dict(""a"" => 1, ""b"" => 2.0)");
   begin
      Put_Line ("Dictionary type: " & TypeOf_Str (Map));
      Put_Line ("        Mutable: " &
                             Boolean'Image (Is_Mutable (TypeOf (Map))));
      Put_Line ("          Tuple: " &
                               Boolean'Image (Is_Tuple (TypeOf (Map))));
      Put_Line ("     NamedTuple: " &
                                   Boolean'Image (Is_NamedTuple (Map)));
   end;
   begin
      Eval_String
      (  "function controller(arg=0);"
      &  "   arg + 1;"
      &  "end"
      );
      declare
         Func   : constant function_t := Get_Function
                                         (  Main_Module,
                                            "controller"
                                         );
         Result : value_t;
      begin
         Result := Call (Func);
         Put_Line ("Controller evaluates:"& Image (Result));
         Result := Call (Func, Result);
         Put_Line ("Controller evaluates:"& Image (Result));
      end;
   end;
   declare
      Result : value_t;
      Data   : Tuple;
      Roots  : Holder (2);
   begin
      Add (Data, "a", To_Julia ("abcd"));
      Set (Roots, 1, Get_Value (Data, 1));
      Add (Data, "b", To_Julia (Integer_32'(123)));
      Set (Roots, 2, Get_Value (Data, 1));
      Result := Call
                (  Get_Function (Base_Module, "println"),
                   To_Julia (Data)
                );
   end;
   GC_Collect;
   Put_Line ("Total bytes:" & Integer_64'Image (GC_Total_Bytes));
   Put_Line ("Leftover   :" & Integer_64'Image (GC_Diff_Total_Bytes));
   AtExit_Hook;
exception
   when Error : others =>
      Put_Line ("Fault: " & Exception_Information (Error));
end Test_Julia;
