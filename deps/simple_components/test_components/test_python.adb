--                                                                    --
--  procedure Test_Py               Copyright (c)  Dmitry A. Kazakov  --
--  test                                           Luebeck            --
--                                                 Winter, 2022       --
--                                                                    --
--                                Last revision :  14:35 02 Jul 2024  --
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

with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Ada.Directories;         use Ada.Directories;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Text_IO;             use Ada.Text_IO;
with Interfaces.C;            use Interfaces.C;

with Py.Load_Python_Library;
with System;

procedure Test_Python is

   task Worker is
      entry Engage;
      entry Disengage;
   end Worker;

   task body Worker is
   begin
      loop
         select
            accept Engage;
            declare
               GIL    : Py.Global_Interpreter_Lock;
               Add    : Py.Handle;
               Concat : Py.Handle;
               Text   : Py.Handle;
               Args   : Py.Handle;
               This   : Py.Handle;
            begin
               Put_Line ("Python GIL taken by the thread");
               Put_Line
               (  "HEAD"
               &  Py.ssize_t'Image (Py.Object_HeadSize)
               );
               Add := Py.Compile
                      (  Source    => "def add(n1,n2 ) :" & LF &
                                      "  return n1+n2",
                         File_Name => "test"
                      );
               Args := Py.Tuple_New (2);
               Py.Tuple_SetItem (Args, 0, Py.Long_FromLong (3));
               Py.Tuple_SetItem (Args, 1, Py.Long_FromLong (4));
               This := Py.Object_CallObject (Add, Args, True);
               Put_Line
               (  "Result 3 + 4 ="
               &  Interfaces.Integer_64'Image
                  (  Py.Long_AsInteger64 (This)
               )  );
               Put_Line
               (  "Is Long "
               &  Boolean'Image
                  (  Py."=" (Py.Object_Type (This), Py.Long_Type)
                  )
               &  " "
               &  Py.Object_Str (Py.Object_Type (This))
               );
               Put_Line
               (  "In Long "
               &  Boolean'Image (Py.Is_In (This, Py.Long_Type))
               );

               Text := Py.Compile
                       (  Source   => "def text() :" & LF &
                                      "  return str.encode(""test"")",
                         File_Name => "test1"
                      );
               Args := Py.Tuple_New (0);
               Put_Line
               (  "Result = "
               &  Py.Bytes_AsString
                  (  Py.Object_CallObject (Text, Args, True)
               )  );

               Concat := Py.Compile
                         (  Source   => "def concat(s) :" & LF &
                                        "  return s+s",
                           File_Name => "test2"
                         );
               This := Py.ByteArray_FromString ("A");
               Args := Py.Tuple_New (1);
               Py.Tuple_SetItem (Args, 0, This);
               declare
                  Result : constant String :=
                           Py.ByteArray_AsString
                           (  Py.Object_CallObject (Concat, Args, True)
                           );
               begin
                  Put_Line ("Result A + A = " & Result);
               end;
               select
                  accept Disengage;
               or terminate;
               end select;
            exception
               when Error : others =>
                  Put_Line
                  (  "Python execute fault: "
                  &  Exception_Information (Error)
                  );
                  select
                     accept Disengage;
                  or terminate;
                  end select;
            end;
            Put_Line ("Python GIL released by the thread");
         or terminate;
         end select;
      end loop;
   end Worker;
begin
   Py.Load
   (  Py.Load_Python_Library.Get_Python_Path
   &  Py.Load_Python_Library.Get_Default_Name
   );
   Py.Initialize;
   declare
      use Py;
      GIL        : Global_Interpreter_Lock;
      Bool_False : Handle;
      Bool_True  : Handle;
      Args       : Handle;
      Result     : Handle;
   begin
      Bool_False :=
         Compile
         (  "def get_false():" & LF & "   return 1 < 0",
            "get_false.py"
         );
      Args   := Tuple_New (0);
      Result := Object_CallObject (Bool_False, Args, True);
      Put_Line ("1 < 0 is " & Boolean'Image (To_Ada (Result)));
      if To_Python (False) /= Result then
         Raise_Exception (Data_Error'Identity, "Not equal to False");
      end if;
      Bool_True :=
         Compile
         (  "def get_true():" & LF & "   return 1 > 0",
            "get_true.py"
         );
      Args   := Tuple_New (0);
      Result := Object_CallObject (Bool_True, Args, True);
      Put_Line ("1 > 0 is " & Boolean'Image (To_Ada (Result)));
   end;
   declare
      use Py;
      GIL    : Global_Interpreter_Lock;
      Hello  : Handle;
      Args   : Handle;
      Result : Handle;
   begin
      Hello :=
         Compile
         (  "def Hello(s):" & LF & "   print (""Hello ""+s+'!')",
            "Hello.py"
         );
      Args := Tuple_New (1);
      Tuple_SetItem (Args, 0, Unicode_FromString ("Python"));
      Result := Object_CallObject (Hello, Args, True);
   end;
   declare
      use Py;
      GIL    : Global_Interpreter_Lock;
      Source : File_Type;
      Hello  : Handle;
      Args   : Handle;
      Result : Handle;
   begin
      Create (Source, Out_File, "hello.py");
      Put_Line (Source, "def Hello(s):");
      Put_Line (Source, "   print (""Hello ""+s+'!')");
      Close (Source);
      Hello := Import ("hello.py", "Hello");
      Delete_File ("hello.py");
      Args := Tuple_New (1);
      Tuple_SetItem (Args, 0, Unicode_FromString ("Python"));
      Result := Object_CallObject (Hello, Args, True);
   end;
   declare
      GIL   : Py.Global_Interpreter_Lock;
      State : Py.ThreadState;
   begin
      Put_Line ("Python GIL taken");
      State := Py.Eval_SaveThread;
      Put_Line ("Python GIL released");
      Worker.Engage;
      Worker.Disengage;
      Py.Eval_RestoreThread (State);
      Put_Line ("Python GIL retaken");
   end;
   declare
      use Py;
      GIL    : Global_Interpreter_Lock;
      Module : Handle;
      Result : Handle;
      A, B   : Handle;
      X      : Handle;
      Foo    : Handle;
      Value  : long;
   begin
      Compile
      (  "super_class = ''"                 & LF &
         "class A :"                        & LF &
         "   def __init__ (self):"          & LF &
         "      self.a = 124"               & LF &
         "   def Foo (self):"               & LF &
         "      return self.a"              & LF &
         "class B (A):"                     & LF &
         "   def Foo (self):"               & LF &
         "      return super().Foo () + 2",
         "super_class.py",
         Module,
         Result
      );
      Result := Object_CallNoArgs (Result);
      A := Object_GetAttrString (Module, "A");
      B := Object_GetAttrString (Module, "B");
      X := Object_CallNoArgs (B);
      Foo := Object_GetAttrString (X, "Foo");
      Value := Long_AsLong (Object_CallNoArgs (Foo));
      if Value /= 126 then
         Raise_Exception
         (  Data_Error'Identity,
            "Expected 126 got" & long'Image (Value));
      end if;
      Foo := Object_Super (X, "Foo");
      Value := Long_AsLong (Object_CallNoArgs (Foo));
      if Value /= 124 then
         Raise_Exception
         (  Data_Error'Identity,
            "Expected 124 got" & long'Image (Value));
      end if;
   end;
   declare
      use Py;
      GIL     : Py.Global_Interpreter_Lock;
      X, Y, Z : Handle;
      Value   : long;
   begin
      X := Long_FromLong (1);
      Y := Long_FromLong (2);
      Z := Number_Add (X, Y);
      Value := Long_AsLong (Z);
      if Value /= 3 then
         Raise_Exception
         (  Data_Error'Identity,
            "Expected 3 got" & long'Image (Value));
      end if;
   end;
   if Py.FinalizeEx < 0 then
      Put_Line ("Python finalization error");
   end if;
   Put_Line ("Exiting");
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Python;
