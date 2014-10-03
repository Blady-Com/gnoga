------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--          G N O G A . S E R V E R . T E M P L A T E _ P A R S E R         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2014 David Botton                      --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Directories;

with GNAT.OS_Lib;
with GNAT.Expect; use GNAT;

package body Gnoga.Server.Template_Parser is

   Template_Directory : Ada.Strings.Unbounded.Unbounded_String :=
     Ada.Strings.Unbounded.To_Unbounded_String ("../templates");

   function PHP_Encode_Key (Code : String) return String;
   --  fixes the presence of a ' in a key

   function PHP_Encode_Value (Code : String) return String;
   --  fixes the presence of a "  in a values

   function Execute_PHP (Code : String) return String;
   --  Execute PHP Code and return output
   --  raises X on error

   function String_Data_List (Data     : Gnoga.Types.Data_Map_Type;
                              Var_Name : String;
                              As_Array : Boolean      := False)
                              return String;
   --  Convert key/value pairs in Gnoga.Types.Data_Map_Type to scripting code

   function Map_Data_List (Data     : Gnoga.Types.Map_of_Data_Maps_Type;
                           Var_Name : String)
                           return String;
   --  Convert Map of Maps of key/value pairs in to associative arrays
   --  in scripting code

   --------------------
   -- PHP_Encode_Key --
   --------------------

   function PHP_Encode_Key (Code : String) return String is
   begin
      if Code'Length = 0 then
         return "";
      else
         declare
            New_Char : Character := Code (Code'First);
         begin
            if New_Char = ''' then
               return "\'"
                 & PHP_Encode_Key (Code (Code'First + 1 .. Code'Last));
            else
               return New_Char
                 & PHP_Encode_Key (Code (Code'First + 1 .. Code'Last));
            end if;
         end;
      end if;
   end PHP_Encode_Key;

   ----------------------
   -- PHP_Encode_Value --
   ----------------------

   function PHP_Encode_Value (Code : String) return String is
   begin
      if Code'Length = 0 then
         return "";
      else
         declare
            New_Char : Character := Code (Code'First);
         begin
            if New_Char = '"' then
               return "\"""
                 & PHP_Encode_Value (Code (Code'First + 1 .. Code'Last));
            else
               return New_Char
                 & PHP_Encode_Value (Code (Code'First + 1 .. Code'Last));
            end if;
         end;
      end if;
   end PHP_Encode_Value;

   ----------------
   -- Excute_PHP --
   ----------------

   function Execute_PHP (Code : String) return String is
      Error_Reporting : constant String :=
        "<?php error_reporting (E_ALL ^ E_NOTICE); " &
        "ini_set (""display_errors"", True); ?>";

      Args   : OS_Lib.Argument_List (1 .. 0);
      Result : aliased Integer            := 0;
      RetStr : String := Expect.Get_Command_Output ("php",
                                                    Args,
                                                    Error_Reporting &
                                                       Code,
                                                    Result'Access,
                                                    True);
   begin
      if Result /= 0 then
         raise Parser_Execution_Failure with "Call to php executable failed";
      end if;
      return RetStr;
   end Execute_PHP;

   -----------------------
   --  String_Data_List --
   -----------------------

   function String_Data_List (Data     : Gnoga.Types.Data_Map_Type;
                              Var_Name : String;
                              As_Array : Boolean      := False)
                              return String
   is
      C : Gnoga.Types.Data_Maps.Cursor := Gnoga.Types.Data_Maps.First (Data);

      function Data_List return String;
      --  Recursive function to compile list of parameters to be injected
      --  in to scripting environment

      function Data_List return String is
      begin
         if Gnoga.Types.Data_Maps.Has_Element (C) then
            declare
               Key   : String := Gnoga.Types.Data_Maps.Key (C);
               Value : String := Gnoga.Types.Data_Maps.Element (C);
            begin
               Gnoga.Types.Data_Maps.Next (C);
               if As_Array then
                  return "'" & PHP_Encode_Key (Key)
                    & "' => """ & PHP_Encode_Value (Value)
                    & """, " & Data_List;
               else
                  return "$" & Var_Name & "['" & PHP_Encode_Key (Key)
                    & "'] = """ & PHP_Encode_Value (Value)
                    & """; " & Data_List;
               end if;
            end;
         else
            return "";
         end if;
      end Data_List;
   begin
      if As_Array then
         return "array (" & Data_List & ")";
      else
         return Data_List;
      end if;
   end String_Data_List;

   function Map_Data_List (Data     : Gnoga.Types.Map_of_Data_Maps_Type;
                           Var_Name : String)
                           return String
   is
      C : Gnoga.Types.Maps_of_Data_Maps.Cursor :=
            Gnoga.Types.Maps_of_Data_Maps.First (Data);

      function Data_List return String;
      --  Recursive function to compile list of parameters to be injected
      --  in to scripting environment

      function Data_List return String is
      begin
         if Gnoga.Types.Maps_of_Data_Maps.Has_Element (C) then
            declare
               Key   : String := Gnoga.Types.Maps_of_Data_Maps.Key (C);
               Value : String := String_Data_List
                 (Gnoga.Types.Maps_of_Data_Maps.Element (C),
                  Var_Name,
                  As_Array => True);
            begin
               Gnoga.Types.Maps_of_Data_Maps.Next (C);
               return "$" & Var_Name & "['" & PHP_Encode_Key (Key) & "'] = "
                 & Value & "; " & Data_List;
            end;
         else
            return "";
         end if;
      end Data_List;

   begin
      return Data_List;
   end Map_Data_List;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out View_Data) is
   begin
      Clear (Object);
   end Finalize;

   -------------------
   -- Variable_Name --
   -------------------

   procedure Variable_Name (Data : in out View_Data;
                            Name : String)
   is
   begin
      Data.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Variable_Name;


   ------------
   -- Insert --
   ------------

   procedure Insert (Data  : in out View_Data;
                     Key   : String;
                     Value : String)
   is
   begin
      Data.String_Values.Insert (Key, Value);
   end Insert;

   procedure Insert (Data  : in out View_Data;
                     Key   : String;
                     Value : Integer)
   is
      String_Value : String := Integer'Image (Value);
   begin
      Insert (Data, Key,
              String_Value (String_Value'First + 1 .. String_Value'Last));
   end Insert;

   ------------------
   -- Insert_Array --
   ------------------

   procedure Insert_Array (Data   : in out View_Data;
                           Vector : Gnoga.Types.Data_Array_Type)
   is
   begin
      for I in Vector.First_Index .. Vector.Last_Index loop
         declare
            K : String := I'Img;
         begin
            Data.Insert (K (K'First + 1 .. K'Last), Vector.Element (I));
         end;
      end loop;
   end Insert_Array;

   -----------------------
   -- Insert_Array_Item --
   -----------------------

   procedure Insert_Array_Item (Data   : in out View_Data;
                                Key    : String;
                                Vector : Gnoga.Types.Data_Array_Type)
   is
      Map : Gnoga.Types.Data_Map_Type;
   begin
      for I in Vector.First_Index .. Vector.Last_Index loop
         declare
            K : String := I'Img;
         begin
            Map.Insert (K (K'First + 1 .. K'Last), Vector.Element (I));
         end;
      end loop;

      Data.Insert_Map_Item (Key, Map);
   end Insert_Array_Item;

   ----------------
   -- Insert_Map --
   ----------------

   procedure Insert_Map (Data : in out View_Data;
                         Map  : Gnoga.Types.Data_Map_Type)
   is
      procedure foreach (Cursor : Gnoga.Types.Data_Maps.Cursor);
      --  Iterate through required members

      procedure foreach (Cursor : Gnoga.Types.Data_Maps.Cursor) is
      begin
         Data.Insert (Gnoga.Types.Data_Maps.Key (Cursor),
                      Gnoga.Types.Data_Maps.Element (Cursor));
      end foreach;
   begin
      Map.Iterate (foreach'Access);
   end Insert_Map;

   ---------------------
   -- Insert_Map_Item --
   ---------------------

   procedure Insert_Map_Item (Data : in out View_Data;
                              Key  : String;
                              Map  : Gnoga.Types.Data_Map_Type)
   is
   begin
      Data.Map_Values.Insert (Key, Map);
   end Insert_Map_Item;

   ----------------------
   -- Insert_Recordset --
   ----------------------

   procedure Insert_Recordset
     (Data : in out View_Data;
      RS   : in out Gnoga.Server.Database.Recordset'Class)
   is
      I : Integer := 1;
   begin
      while RS.Next loop
         declare
            Key : String := I'Img;
         begin
            I := I + 1;
            Data.Insert_Map_Item
              (Key (Key'First + 1 .. Key'Last), RS.Field_Values);
         end;
      end loop;
   end Insert_Recordset;

   -------------------
   -- Insert_Record --
   -------------------

   procedure Insert_Record_Item (Data   : in out View_Data;
                            Key    : String;
                            Row    : Gnoga.Server.Model.Active_Record'Class)
   is
   begin
      Data.Insert_Map_Item (Key, Row.Values);
   end Insert_Record_Item;

   ----------------
   -- Inser_Rows --
   ----------------

   procedure Insert_Rows
     (Data   : in out View_Data;
      Row    : Gnoga.Server.Model.Queries.Active_Record_Array.Vector)
   is
   begin
      for I in 1 .. Natural (Row.Length) loop
         declare
            Key : String := I'Img;
         begin
            Data.Insert_Record_Item (Key => Key (Key'First + 1 .. Key'Last),
                                     Row => Row.Element (I));
         end;
      end loop;
   end Insert_Rows;

   -------------------
   --  Insert_Query --
   -------------------

   procedure Insert_Query (Data  : in out View_Data;
                           C     : Gnoga.Server.Database.Connection'Class;
                           Query : String)
   is
      RS : Gnoga.Server.Database.Recordset'Class := C.Query (Query);
   begin
      Insert_Recordset (Data, RS);
   end Insert_Query;

   -----------
   -- Clear --
   -----------

   procedure Clear (Data : in out View_Data) is
   begin
      Data.String_Values.Clear;
      Data.Map_Values.Clear;
   end Clear;

   ----------------
   -- Parse_Name --
   ----------------

   function Parse_Name (Name : String) return String is
      Templates_Dir : constant String :=
                        Ada.Strings.Unbounded.To_String
                          (Template_Directory) & "/";
   begin
      return Templates_Dir & Name;
   end Parse_Name;

   ---------------
   -- Load_View --
   ---------------

   function Load_View (Name : String) return String is
      Empty_Data : View_Data;
   begin
      return Load_View (Name, Empty_Data);
   end Load_View;

   function Load_View (Name     : String;
                       Data_Map : Gnoga.Types.Data_Map_Type;
                       Var_Name : String := "data")
                       return String
   is
      Data : View_Data;
   begin
      Data.Insert_Map (Data_Map);
      Data.Variable_Name (Var_Name);

      return Load_View (Name, Data);
   end Load_View;

   function Load_View (Name : String; Data : View_Data)
                       return String
   is
   begin
      return Load_View (Name, Data_List => (1 => Data));
   end Load_View;

   function Load_View (Name      : String;
                       Data_List : View_Data_Array)
                       return String
   is
      use Ada.Strings.Unbounded;

      Error_Queue_Data : View_Data;
      Info_Queue_Data  : View_Data;

      PHP_Code : String := "include """ & Parse_Name (Name) & """;";

      I : Positive := Data_List'First;

      function Build_List return String;
      --  Build scripting code

      function Build_List return String is
      begin
         if I <= Data_List'Last then
            I := I + 1;
            return Build_List &
            "$" & To_String (Data_List (I - 1).Name) & "=array(); " &
            String_Data_List (Data_List (I - 1).String_Values,
                              To_String (Data_List (I - 1).Name)) &
            Map_Data_List (Data_List (I - 1).Map_Values,
                           To_String (Data_List (I - 1).Name));
         else
            Error_Queue_Data.Insert_Array (Error_Queue);
            Info_Queue_Data.Insert_Array (Info_Queue);

            return
              "$gnoga_errors=array(); " &
              String_Data_List
                (Error_Queue_Data.String_Values, "gnoga_error") &
              "$gnoga_infos=array(); " &
              String_Data_List (Info_Queue_Data.String_Values, "gnoga_info");
         end if;
      end Build_List;
   begin

      return Execute_PHP ("<?php " &
                          Build_List &
                          PHP_Code &
                          "?>");
   end Load_View;

   ----------------------------
   -- Set_Template_Directory --
   ----------------------------

   procedure Set_Template_Directory (Directory : String) is
   begin
      Template_Directory :=
        Ada.Strings.Unbounded.To_Unbounded_String (Directory);
   end Set_Template_Directory;

   -----------------------
   -- Add_Error_Message --
   -----------------------

   procedure Add_Error_Message (Message : String) is
   begin
      Error_Queue.Append (Message);
   end Add_Error_Message;

   -----------------------
   -- Clear_Error_Queue --
   -----------------------

   procedure Clear_Error_Queue is
   begin
      Error_Queue.Clear;
   end Clear_Error_Queue;

   ----------------------
   -- Add_Info_Message --
   ----------------------

   procedure Add_Info_Message (Message : String) is
   begin
      Info_Queue.Append (Message);
   end Add_Info_Message;

   ----------------------
   -- Clear_Info_Queue --
   ----------------------

   procedure Clear_Info_Queue is
   begin
      Info_Queue.Clear;
   end Clear_Info_Queue;

end Gnoga.Server.Template_Parser;
