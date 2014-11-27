--                                                                    --
--  package APQ.Links               Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2004       --
--                                                                    --
--                                Last revision :  18:59 10 Feb 2008  --
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

with APQ.Common;   use APQ.Common;

package body APQ.Links is

   procedure Create_Table
             (  Data_Base  : in out APQ_Data_Base;
                Table_Name : String
             )  is
   begin
      Prepare
      (  Data_Base.Query.all,
         (  "CREATE TABLE "
         &  Table_Name
         &  " (dependant "
         &  Ref_Type (Data_Base)
         &  ", referent "
         &  Ref_Type (Data_Base)
         &  ")"
      )  );
      Execute (Data_Base);
   end Create_Table;

   procedure Delete
             (  Data_Base  : in out APQ_Data_Base;
                Table_Name : String;
                Object     : Object_ID
             )  is
      Command : Root_Query_Type'Class renames Data_Base.Query.all;
   begin
      Prepare
      (  Command,
         (  "DELETE FROM "
         &  Table_Name
         &  " WHERE dependant ="
      )  );
      Append (Command, Object, " OR referent =");
      Append (Command, Object);
      Execute (Data_Base);
   end Delete;

   function Depends_On
            (  Data_Base  : APQ_Data_Base;
               Table_Name : String;
               Dependant  : Object_ID;
               Referent   : Object_ID
            )  return Boolean is
      Command : Root_Query_Type'Class renames Data_Base.Query.all;
   begin
      Prepare
      (  Command,
         "SELECT dependant FROM " & Table_Name & " WHERE dependant ="
      );
      Append (Command, Dependant, " AND referent =");
      Append (Command, Referent);
      Execute (Data_Base);
      Fetch (Command);
      Clear (Command);
      return True;
   exception
      when No_Tuple =>
         Clear (Command);
         return False;
   end Depends_On;

   procedure Get_Set
             (  Data_Base  : in out APQ_Data_Base;
                Table_Name : String;
                Key_Name   : String;
                Data_Name  : String;
                Key_Value  : Object_ID;
                Data_Set   : in out Set
             )  is
      Command : Root_Query_Type'Class renames Data_Base.Query.all;
      Data_ID : Object_ID;
   begin
      Prepare
      (  Command,
         (  "SELECT "
         &  Data_Name
         &  " FROM "
         &  Table_Name
         &  " WHERE "
         &  Key_Name
         &  " ="
      )  );
      Append (Command, Key_Value);
      Execute (Data_Base);
      while not End_of_Query (Command) loop
         Fetch (Command);
         Data_ID := Value (Command, 1);
         if Key_Value /= Data_ID then
            Add (Data_Set, Data_ID);
         end if;
      end loop;
      Clear (Command);
   exception
      when No_Tuple | Null_Value =>
         Clear (Command);
   end Get_Set;

   procedure Get_Dependants
             (  Data_Base  : in out APQ_Data_Base;
                Table_Name : String;
                Referent   : Object_ID;
                Dependants : in out Set
             )  is
   begin
      Get_Set
      (  Data_Base  => Data_Base,
         Table_Name => Table_Name,
         Key_Name   => "referent",
         Data_Name  => "dependant",
         Key_Value  => Referent,
         Data_Set   => Dependants
      );
   end Get_Dependants;

   procedure Get_References
             (  Data_Base  : in out APQ_Data_Base;
                Table_Name : String;
                Dependant  : Object_ID;
                Referents  : in out Set
             )  is
   begin
      Get_Set
      (  Data_Base  => Data_Base,
         Table_Name => Table_Name,
         Key_Name   => "dependant",
         Data_Name  => "referent",
         Key_Value  => Dependant,
         Data_Set   => Referents
      );
   end Get_References;

   procedure Get_References
             (  Data_Base  : in out APQ_Data_Base;
                Table_Name : String;
                Dependant  : Object_ID;
                Referents  : in out Unbounded_Array;
                Pointer    : in out Integer
             )  is
      Command : Root_Query_Type'Class renames Data_Base.Query.all;
      Data_ID : Object_ID;
   begin
      Prepare
      (  Command,
         (  "SELECT referent FROM "
         &  Table_Name
         &  " WHERE dependant ="
      )  );
      Append (Command, Dependant);
      Execute (Data_Base);
      while not End_of_Query (Command) loop
         Fetch (Command);
         Data_ID := Value (Command, 1);
         if Dependant /= Data_ID then
            Put (Referents, Pointer, Data_ID);
            Pointer := Pointer + 1;
         end if;
      end loop;
      Clear (Command);
   exception
      when No_Tuple | Null_Value =>
         Clear (Command);
   end Get_References;

   function Has_Dependants
            (  Data_Base  : APQ_Data_Base;
               Table_Name : String;
               Referent   : Object_ID
            )  return Boolean is
      Command : Root_Query_Type'Class renames Data_Base.Query.all;
   begin
      Prepare
      (  Command,
         "SELECT * FROM " & Table_Name & " WHERE referent ="
      );
      Append (Command, Referent);
      Execute (Data_Base);
      loop
         Fetch (Command);
         if Value (Command, 1) /= Referent then
            Clear (Command);
            return True;
         end if;
      end loop;
   exception
      when No_Tuple | Null_Value =>
         Clear (Command);
         return False;
   end Has_Dependants;

   procedure Reference
             (  Data_Base  : in out APQ_Data_Base;
                Table_Name : String;
                Dependant  : Object_ID;
                Referent   : Object_ID
             )  is
      Command : Root_Query_Type'Class renames Data_Base.Query.all;
   begin
      if not Depends_On
             (  Data_Base,
                Table_Name,
                Dependant,
                Referent
             )
      then
         Prepare (Command, "INSERT INTO " &  Table_Name & " VALUES (");
         Append (Command, Dependant, ",");
         Append (Command, Referent,  ")");
         Execute (Data_Base);
      end if;
   end Reference;

   procedure Unreference
             (  Data_Base  : in out APQ_Data_Base;
                Table_Name : String;
                Dependant  : Object_ID;
                Referent   : Object_ID
             )  is
      Command : Root_Query_Type'Class renames Data_Base.Query.all;
   begin
      Prepare
      (  Command,
         "DELETE FROM " & Table_Name & " WHERE dependant ="
      );
      Append (Command, Dependant, " AND Referent = ");
      Append (Command, Referent);
      Execute (Data_Base);
   end Unreference;

end APQ.Links;
