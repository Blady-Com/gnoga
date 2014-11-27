--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Task_Elements                               Luebeck            --
--  Separate body                                  Autumn, 2006       --
--                                                                    --
--                                Last revision :  20:47 23 Jun 2010  --
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

with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Test_Linked_Lists_Of_Tasks;  use Test_Linked_Lists_Of_Tasks;

separate (Test_Linked_Lists) procedure Task_Elements is
   use Worker_Lists.Doubly_Linked;

   function Image (Container : List) return String is
      This   : Item := Item (Container);
      Result : Unbounded_String;
      Name   : Unbounded_String;
   begin
      if This = null then
         Append (Result, "<>");
      else
         loop
            This.Report (Name);
            Append (Result, Name);
            This := Next (This);
            exit when This = Item (Container);
            Append (Result, "-");
         end loop;
      end if;
      return To_String (Result);
   end Image;

   procedure Check
             (  Name      : String;
                Container : List;
                Expected  : STring
             )  is
   begin
      if Image (Container) /= Expected then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in "
            &  Name
            &  ". Got "
            &  Image (Container)
            &  ", expect "
            &  Expected
         )  );
      end if;
   end Check;

   Workers  : List;
   Managers : List;
begin
   Append (Workers, new Worker); Workers.Start ("A");
   Append (Workers, new Worker); Previous (Workers).Start ("B");
   Append (Workers, new Worker); Previous (Workers).Start ("C");
   Append (Workers, new Worker); Previous (Workers).Start ("D");
   Put_Line
   (  "Task dope size:"
   &  Integer'Image
      (  Integer
         (  Worker_Lists.Doubly_Linked_Webs.Dope_Size
   )  )  );
   Append (Managers, new Worker); Previous (Managers).Start ("E");
   Append (Managers, new Worker); Previous (Managers).Start ("F");
   Append (Managers, new Worker); Previous (Managers).Start ("G");
      Check ("Workers",  Workers,  "A-B-C-D");
      Check ("Managers", Managers, "E-F-G");
   Append (Workers, Item (Managers), Managers);
      Check ("Workers",  Workers,  "A-B-C-D-E");
      Check ("Managers", Managers, "F-G");
   declare
      Guy_To_Fire : Item := Next (Managers);
   begin
      Guy_To_Fire.Stop;
      Delete (Managers, Guy_To_Fire);
         Check ("Managers", Managers, "F");
   end;
   Insert (Next (Next (Workers)), Next (Workers), Workers);
      Check ("Workers",  Workers,  "A-C-B-D-E");
   Insert (Previous (Workers), Item (Workers), Workers);
      Check ("Workers", Workers, "C-B-D-E-A");
   declare
      Guy_To_Fire : Item;
   begin
      while Workers /= null loop
         Guy_To_Fire := Item (Workers);
         Guy_To_Fire.Stop;
         Delete (Workers, Guy_To_Fire);
      end loop;
      while Managers /= null loop
         Guy_To_Fire := Item (Managers);
         Guy_To_Fire.Stop;
         Delete (Managers, Guy_To_Fire);
      end loop;
   end;
end Task_Elements;
