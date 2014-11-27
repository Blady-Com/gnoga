--                                                                    --
--  package Object                  Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2009       --
--                                                                    --
--                                Last revision :  13:51 30 May 2014  --
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
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with Ada.Tags;                    use Ada.Tags;
with GNAT.Most_Recent_Exception;  use GNAT.Most_Recent_Exception;
with GNAT.Traceback.Symbolic;     use GNAT.Traceback.Symbolic;
with System.Storage_Elements;     use System.Storage_Elements;

with Ada.Exceptions.Is_Null_Occurrence;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Generic_Map;
with Generic_Unbounded_Ptr_Array;

package body Object is
   use GNAT.Traceback;
   use type System.Address;

   function To_Address is
      new Ada.Unchecked_Conversion (Entity_Ptr, System.Address);
   function To_Object is
      new Ada.Unchecked_Conversion (System.Address, Entity_Ptr);

   Max_Length    : constant := 200;
   File_Set      : Boolean  := False;
   File_To_Write : Ada.Text_IO.File_Type;

   procedure Put_Line (Text : String) is
   begin
      if File_Set then
         Ada.Text_IO.Put_Line (File_To_Write, Text);
      else
         Ada.Text_IO.Put_Line (Text);
      end if;
   end Put_Line;

   type Operation is (Created, Incremented, Decremented);
   type Location;
   type Location_Ptr is access Location;
   type Location (Length : Natural; Mode : Operation) is record
      Next      : Location_Ptr;
      Traceback : Tracebacks_Array (1..Length);
   end record;

   package Address_To_Location is
      new Generic_Map
          (  Key_Type    => System.Address,
             Object_Type => Location_Ptr
          );
   use Address_To_Location;

   Trace : Map;

   type Map_Ptr is access Map;
   type Map_Ptr_Array is array (Positive range <>) of Map_Ptr;
   package Address_To_Dead_Objects_Maps is
      new Generic_Unbounded_Ptr_Array
          (  Positive,
             Map,
             Map_Ptr,
             Map_Ptr_Array
          );
   use Address_To_Dead_Objects_Maps;

   Dead : Unbounded_Ptr_Array;
   Size : Natural := 0;

   procedure Put_Traceback (Prefix : String; Traceback : String);
   procedure Put_Traceback (Traceback : Location_Ptr);

   procedure Call_Stack is
      Traceback : Tracebacks_Array (1..Max_Length);
      Length    : Natural;
   begin
      Call_Chain (Traceback, Length);
      Put_Line ("Call stack:");
      Put_Traceback
      (  "-- ",
         Symbolic_Traceback (Traceback (3..Length)
      ));
   end Call_Stack;

   procedure Free is
      new Ada.Unchecked_Deallocation (Location, Location_Ptr);

   protected Lock is
      entry Seize;
      procedure Release;
   private
      Owned : Boolean := False;
   end Lock;

   protected body Lock is
      entry Seize when not Owned is
      begin
         Owned := True;
      end Seize;

      procedure Release is
      begin
         Owned := False;
      end Release;
   end Lock;

   function Equal
            (  Left  : Entity;
               Right : Entity'Class;
               Flag  : Boolean := False
            )  return Boolean is
   begin
      if Flag or else Right in Entity then
         return Left'Address = Right'Address;
      else
         return Equal (Right, Left, True);
      end if;
   end Equal;

   function Find (This : System.Address) return Positive is
      pragma Inline (Find);
      Index : Integer := Find (Trace, This);
   begin
      if 0 >= Index then
         Put_Line
         (  "Object at"
         &  Integer_Address'Image (To_Integer (This'Address))
         &  " is not traced"
         );
         Call_Stack;
         Put_Line
         (  "Object type is "
         &  Ada.Tags.Expanded_Name (To_Object (This).all'Tag)
         );
         declare
            Count : Natural := 0;
         begin
            for Item in 1..Size loop
               if Find (Get (Dead, Item).all, This) > 0 then
                  Count := Count + 1;
               end if;
            end loop;
            Put_Line
            (  "Dead objects at this location:"
            &  Natural'Image (Count)
            );
            if Count > 0 then
               Count := 1;
               for Item in 1..Size loop
                  declare
                     Trace : Map renames Get (Dead, Item).all;
                  begin
                     Index := Find (Trace, This);
                     if Index > 0 then
                        Put_Line
                        (  "Dead object"
                        &  Positive'Image (Count)
                        &  " traceback:"
                        );
                        Put_Traceback (Get (Trace, Index));
                        Count := Count + 1;
                     end if;
                  end;
               end loop;
            end if;
         end;
         Lock.Release;
         Raise_Exception
         (  Program_Error'Identity,
            (  Ada.Tags.Expanded_Name (To_Object (This).all'Tag)
            &  " is not traced"
         )  );
      end if;
      return Index;
   end Find;

   procedure Finalize (Object : in out Entity) is
      Index : Integer;
      Ptr   : Location_Ptr;
   begin
      Lock.Seize;
      Index := Find (Object'Address); -- Releases upon exception
      Ptr := Get (Trace, Index);
      if 0 = Object.Use_Count then
         declare
            List : Positive := 1;
         begin
            while (  List <= Size
                  and then
                     Is_In (Get (Dead, List).all, Object'Address)
                  )  loop
               List := List + 1;
            end loop;
            if List > Size then
               Put (Dead, List, new Map);
               Size := List;
            end if;
            Add (Get (Dead, List).all, Object'Address, Ptr);
         end;
         Remove (Trace, Index);
         Lock.Release;
      else
         Put_Line
         (  "Finalized object "
         &  Ada.Tags.Expanded_Name (Entity'Class (Object)'Tag)
         &  " at"
         &  Integer_Address'Image (To_Integer (Object'Address))
         &  " is still in use. Object traceback:"
         );
         Put_Traceback (Ptr);
         declare
            Error : Exception_Occurrence renames Occurrence_Access.all;
         begin
            if Is_Null_Occurrence (Error) then
               Put_Line ("Detected at:");
               Call_Stack;
            else
               Put_Line
               (  "Detected upon exception propagation "
               &  Exception_Information (Error)
               );
               Put_Line ("------ traceback -------");
               Put_Line (Symbolic_Traceback (Error));
               Call_Stack;
               Put_Line ("------------------------");
            end if;
         end;
         Lock.Release;
         Raise_Exception
         (  Program_Error'Identity,
            (  Ada.Tags.Expanded_Name (Entity'Class (Object)'Tag)
            &  " is still in use"
         )  );
      end if;
   end Finalize;

   procedure Increment_Count (Object : in out Entity) is
      Length    : Natural;
      Index     : Integer;
      Ptr       : Location_Ptr;
      Traceback : Tracebacks_Array (1..Max_Length);
   begin
      Lock.Seize;
      Index := Find (Object'Address); -- Releases lock upon an exception
      Ptr   := Get (Trace, Index);
      Call_Chain (Traceback, Length);
      Replace
      (  Trace,
         Index,
         new Location'
             (  Length    => Length - 1,
                Mode      => Incremented,
                Next      => Ptr,
                Traceback => Traceback (2..Length)
      )      );
      Object.Use_Count := Object.Use_Count + 1;
      Lock.Release;
   end Increment_Count;

   procedure Initialize (Object : in out Entity) is
      Traceback : Tracebacks_Array (1..Max_Length);
      Length    : Natural;
   begin
      Call_Chain (Traceback, Length);
      declare
         Ptr : Location_Ptr :=
                  new Location'
                      (  Length    => Length - 1,
                         Mode      => Created,
                         Next      => null,
                         Traceback => Traceback (2..Length)
                      );
      begin
         Lock.Seize;
         Add (Trace, Object'Address, Ptr);
         Lock.Release;
      exception
         when Constraint_Error =>
            Free (Ptr);
            Ptr := Get (Trace, Object'Address);
            Put_Line
            (  Ada.Tags.Expanded_Name (Entity'Class (Object)'Tag)
            &  " collides with another object at"
            &  Integer_Address'Image (To_Integer (Object'Address))
            &  ". Traceback:"
            );
            Put_Traceback (Ptr);
            Call_Stack;
            Lock.Release;
            Raise_Exception
            (  Program_Error'Identity,
               (  Ada.Tags.Expanded_Name (Entity'Class (Object)'Tag)
               &  " collides with another object at"
               &  Integer_Address'Image (To_Integer (Object'Address))
            )  );
         when Error : others =>
            Lock.Release;
            Free (Ptr);
            raise;
      end;
   end Initialize;

   function Less
            (  Left  : Entity;
               Right : Entity'Class;
               Flag  : Boolean := False
            )  return Boolean is
   begin
      if Flag or else Right in Entity then
         return Left'Address < Right'Address;
      else
         return
            not (Less (Right, Left, True) or else Equal (Right, Left));
      end if;
   end Less;

   procedure Decrement_Count (Object : in out Entity) is
      Length    : Natural;
      Index     : Integer;
      Ptr       : Location_Ptr;
      Traceback : Tracebacks_Array (1..Max_Length);
   begin
      Lock.Seize;
      Index := Find (Object'Address); -- Releases lock upon an exception
      Ptr   := Get (Trace, Index);
      if Object.Use_Count = 0 then
         Put_Line
         (  "Object "
         &  Ada.Tags.Expanded_Name (Entity'Class (Object)'Tag)
         &  " at"
         &  Integer_Address'Image (To_Integer (Object'Address))
         &  " has zero count. Use count traceback:"
         );
         Put_Traceback (Ptr);
         Call_Stack;
         Lock.Release;
         Raise_Exception
         (  Program_Error'Identity,
            (  Ada.Tags.Expanded_Name (Entity'Class (Object)'Tag)
            &  " has zero count"
         )  );
      else
         Call_Chain (Traceback, Length);
         Replace
         (  Trace,
            Index,
            new Location'
                (  Length    => Length - 1,
                   Mode      => Decremented,
                   Next      => Ptr,
                   Traceback => Traceback (2..Length)
         )      );
         Object.Use_Count := Object.Use_Count - 1;
         Lock.Release;
      end if;
   end Decrement_Count;

   procedure Put_Traceback (Object : Entity'Class) is
   begin
      Lock.Seize;
      Put_Traceback (Get (Trace, Find (Object'Address)));
      Lock.Release;
   end Put_Traceback;

   procedure Put_Traceback (Traceback : Location_Ptr) is
      Ptr   : Location_Ptr := Traceback;
      First : Location_Ptr;
      Next  : Location_Ptr;
      Count : Natural := 0;
   begin
      while Ptr /= null loop
         Next     := Ptr.Next;
         Ptr.Next := First;
         First    := Ptr;
         Ptr      := Next;
      end loop;
      while First /= null loop
         Next := First.Next;
         case First.Mode is
            when Created =>
               Put_Line ("Created at");
               Put_Traceback
               (  "** ",
                  Symbolic_Traceback (First.Traceback)
               );
            when Incremented =>
               Count := Count + 1;
               Put_Line (Count * "   " & "+1 at");
               Put_Traceback
               (  Count * "   " & ">> ",
                  Symbolic_Traceback (First.Traceback)
               );
            when Decremented =>
               Count := Count - 1;
               Put_Line (Count * "   " & "-1 at");
               Put_Traceback
               (  Count * "   " & "<< ",
                  Symbolic_Traceback (First.Traceback)
               );
         end case;
         Free (First);
         First := Next;
      end loop;
   end Put_Traceback;

   procedure Put_Traceback (Prefix : String; Traceback : String) is
      From : Integer := Traceback'First;
   begin
      for Index in Traceback'Range loop
         if Traceback (Index) = Character'Val (10) then
            Put_Line (Prefix & Traceback (From..Index - 1));
            From := Index + 1;
         elsif Traceback (Index) = Character'Val (13) then
            From := Index + 1;
         end if;
      end loop;
      if From in Traceback'Range then
         Put_Line (Prefix & Traceback (From..Traceback'Last));
      end if;
   end Put_Traceback;

   procedure Release (Ptr : in out Entity_Ptr) is
      procedure Free is new
         Ada.Unchecked_Deallocation (Entity'Class, Entity_Ptr);
      Addr : constant System.Address := To_Address (Ptr);
   begin
      if Ptr /= null then
         declare
            Object : Entity'Class renames Ptr.all;
         begin
            Decrement_Count (Object);
            if Object.Use_Count > 0 then
               return;
            end if;
         end;
         Free (Ptr); -- Finalize will clean up the trace map
      end if;
   exception
      when Error : others =>
         Put_Line
         (  "Object at"
         &  Integer_Address'Image (To_Integer (Addr))
         &  " release fault "
         &  Exception_Information (Error)
         );
         Call_Stack;
         Put_Line
         (  "While releasing the object at"
         &  Integer_Address'Image (To_Integer (Addr))
         &  ". Use count traceback:"
         );
         Lock.Seize;
         Put_Traceback (Get (Trace, Find (Addr)));
         Lock.Release;
         raise;
   end Release;

   procedure Set_Trace_File (File : String) is
   begin
      if File_Set then
         File_Set := False;
         Ada.Text_IO.Close (File_To_Write);
      else
         File_Set := False;
      end if;
      Ada.Text_IO.Create (File_To_Write, Ada.Text_IO.In_File, File);
      File_Set := True;
   end Set_Trace_File;

end Object;
