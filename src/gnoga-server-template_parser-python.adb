------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--   G N O G A . S E R V E R . T E M P L A T E _ P A R S E R . P Y T H O N  --
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

with GNAT.OS_Lib;
with GNAT.Expect; use GNAT;

package body Gnoga.Server.Template_Parser.Python is

   function Python_Encode_Key (Code : String) return String;
   --  fixes the presence of a ' in a key

   function Python_Encode_Value (Code : String) return String;
   --  fixes the presence of a "  in a values

   function Execute_Python (Code : String) return String;
   --  Execute Python Code and return output
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

   -----------------------
   -- Python_Encode_Key --
   -----------------------

   function Python_Encode_Key (Code : String) return String is
   begin
      if Code'Length = 0 then
         return "";
      else
         declare
            New_Char : Character := Code (Code'First);
         begin
            if New_Char = ''' then
               return "\'"
                 & Python_Encode_Key (Code (Code'First + 1 .. Code'Last));
            else
               return New_Char
                 & Python_Encode_Key (Code (Code'First + 1 .. Code'Last));
            end if;
         end;
      end if;
   end Python_Encode_Key;

   -------------------------
   -- Python_Encode_Value --
   -------------------------

   function Python_Encode_Value (Code : String) return String is
   begin
      if Code'Length = 0 then
         return "";
      else
         declare
            New_Char : Character := Code (Code'First);
         begin
            if New_Char = '"' then
               return "\"""
                 & Python_Encode_Value (Code (Code'First + 1 .. Code'Last));
            else
               return New_Char
                 & Python_Encode_Value (Code (Code'First + 1 .. Code'Last));
            end if;
         end;
      end if;
   end Python_Encode_Value;

   -------------------
   -- Excute_Python --
   -------------------

   function Execute_Python (Code : String) return String is
      Args   : OS_Lib.Argument_List (1 .. 0);
      Result : aliased Integer            := 0;
      RetStr : String := Expect.Get_Command_Output ("python",
                                                    Args,
                                                    Code & " quit();",
                                                    Result'Access,
                                                    True);
   begin
      if Result /= 0 then
         raise Parser_Execution_Failure
           with "Call to python executable failed";
      end if;

      return RetStr;
   end Execute_Python;

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
                  return "'" & Python_Encode_Key (Key)
                    & "' : """ & Python_Encode_Value (Value)
                    & """, " & Data_List;
               else
                  return Var_Name & "['" & Python_Encode_Key (Key)
                    & "'] = """ & Python_Encode_Value (Value)
                    & """; " & Data_List;
               end if;
            end;
         else
            return "";
         end if;
      end Data_List;
   begin
      if As_Array then
         return "{" & Data_List & "}";
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
               return Var_Name & "['" & Python_Encode_Key (Key) & "'] = "
                 & Value & "; " & Data_List;
            end;
         else
            return "";
         end if;
      end Data_List;

   begin
      return Data_List;
   end Map_Data_List;

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

      Python_Code : String :=
                   "execfile ('" & Parse_Name (Name) & "', globals());";

      I : Positive := Data_List'First;

      function Build_List return String;
      --  Build scripting code

      function Build_List return String is
      begin
         if I <= Data_List'Last then
            I := I + 1;
            return Build_List &
            To_String (Data_List (I - 1).Name) & "={}; " &
            String_Data_List (Data_List (I - 1).String_Values,
                              To_String (Data_List (I - 1).Name)) &
            Map_Data_List (Data_List (I - 1).Map_Values,
                           To_String (Data_List (I - 1).Name));
         else
            Error_Queue_Data.Insert_Array (Error_Queue);
            Info_Queue_Data.Insert_Array (Info_Queue);

            return
              "gnoga_errors={}; " &
              String_Data_List
                (Error_Queue_Data.String_Values, "gnoga_error") &
              "gnoga_infos={}; " &
              String_Data_List (Info_Queue_Data.String_Values, "gnoga_info");
         end if;
      end Build_List;
   begin
      return Execute_Python (Build_List & Python_Code);
   end Load_View;

end Gnoga.Server.Template_Parser.Python;
