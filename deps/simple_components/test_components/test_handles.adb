--                                                                    --
--  procedure Test_Handles          Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2002       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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
--  This is a test procedure for objects and handles
--
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Text_IO;               use Ada.Text_IO;
with Test_My_String;            use Test_My_String;
with Test_My_String.Handle;     use Test_My_String.Handle;
with Test_Object;               use Test_Object;
with Test_Object.Handle;        use Test_Object.Handle;
with Test_Object.Handle_Array;  use Test_Object.Handle_Array;
with Test_Set;                  use Test_Set;

procedure Test_Handles is
   type My_Object_Array is
      array (Positive range <>) of Test_Object.Handle.Handle;
begin
   Put_Line ("Testing handles and sets ...");
   declare
      A : constant My_Safe_String := Create ("Some text");
   begin
      if Value (A) /= "Some text" then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in create"
         );
      end if;
   end;
   declare
      A  : constant My_Safe_String := Create ("A");
      B  : My_Safe_String;
      S1 : Test_Set.Set;
      S2 : Test_Set.Set;
   begin
      Add (S1, A);
      S2 := S1;
      Add (S2, B); -- Has no effect
      Add (S2, A);
      if 0 > Find (S1, A) or S1 /= S2 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Failed to insert A into the set"
         );
      end if;
      B := Create ("B") ;
      Add (S2, B);
      if 0 > Find (S1, A) or 0 < Find (S1, B) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error in cloning"
         );
      end if;
      if 0 > Find (S2, A) or 0 > Find (S2, B) or S1 = S2 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Failed to insert B into the set"
         );
      end if;
      S1 := S1;
      Remove (S2, B); -- B is finally removed
      if S1 /= S2 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Failed to remove B into the set"
         );
      end if;
      for Index in 1..15 loop
         B := Create (Integer'Image (Index));
         Add (S1, B);
         if 0 > Find (S1, B) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Failed to insert B into the set"
            );
         end if;
      end loop;
   end;
   declare
      X     : Unbounded_Array;
      Y_Ptr : constant My_Object_Ptr := new My_Object;
      Y     : constant Test_Object.Handle.Handle := Ref (Y_Ptr);
   --
   -- The following does not work with GNAT
   --
   --   Y : Test_Object.Handle.Handle := Ref (new My_Object);
   --
   -- which  would  immediately destroy newly created object for unknown
   -- to me reasons.
   --
      Z     : Unbounded_Array;
   begin
      Put (X, 2, new My_Object);
      Put (X, 3, new My_Object);
      Put (X, 4, new My_Object);
      Put (X, 8, Y);
      if 4 /= Set_Of_Objects.Get_Size (Objects) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Wrong objects number"
         );
      end if;
      Z := X;
      if Ptr (Y).Use_Count /= 2 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Failed to copy an unbounded array of handles"
         );
      end if;
      Put (X, 4, null);
      if Ptr (Y).Use_Count /= 3 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Failed to clone an unbounded array of handles"
         );
      end if;
      if Get (Z, 4).Use_Count /= 1 then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Illegal use count"
         );
      end if;
      Put (Z, 4, null);
      if 3 /= Set_Of_Objects.Get_Size (Objects) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "The object 4 lingers"
         );
      end if;
   end;
   if 0 /= Set_Of_Objects.Get_Size (Objects) then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Not all objects gone"
      );
   end if;

   declare
      type My_Object_Record (N : Integer := 1) is record
         Member : Test_Object.Handle.Handle;
      end record;
      function Create return Test_Object.Handle.Handle is
         Result : Test_Object.Handle.Handle;
      begin
         Result := Ref (new My_Object);
         return Result;
      end Create;
      X : constant My_Object_Record := (2, Create);
   begin
      if Ptr (X.Member).Use_Count /= 1 then
         Put_Line ("!!!A compiler bug in controlled types!!!");
         Put_Line ("Program_Error will propagate upon completion");
      end if;
   end;

   declare
      function Create return Test_Object.Handle.Handle is
         Result : Test_Object.Handle.Handle;
      begin
         Result := Ref (new My_Object);
         return Result;
      end Create;
      A : constant My_Object_Array := (Create, Create, Create);
      P : constant My_Object_Ptr := Ptr (A (1));
   begin
      if P.Use_Count /= 1 then
         Put ("!!!A compiler bug detected in arrays of ");
         Put_Line ("controlled types!!!");
         Put_Line ("Program_Error will propagate upon completion");
      end if;
   end;
   Put_Line ("... Done");
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Handles;
