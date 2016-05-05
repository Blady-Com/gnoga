--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Single_File.Text_IO              Luebeck            --
--  Implementation                                 Autumn, 2014       --
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

with Persistent.Single_File_Keys;    use Persistent.Single_File_Keys;
with Strings_Edit;                   use Strings_Edit;
with Strings_Edit.Integers;          use Strings_Edit.Integers;
with Strings_Edit.Quoted;            use Strings_Edit.Quoted;
with Strings_Edit.Streams.Naturals;  use Strings_Edit.Streams.Naturals;

with Persistent.Blocking_Files.Text_IO;
with Persistent.Memory_Pools.Streams;

package body Persistent.Single_File.Text_IO is
   use ID_To_Object_Maps;
   use Name_To_ID_Maps;
   use Persistent.Blocking_Files.Text_IO;
   use Persistent.Memory_Pools.Streams;

   function Image (Date : Time) return String is
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
      Result  : String (1..23); -- yyyy-mm-dd hh:mm:ss.mmm
      Pointer : Integer := 1;   -- 12345678901234567890123
      Count   : Integer;
   begin
      Split (Date, Year, Month, Day, Seconds);
      Count := Integer (Seconds);
      Put
      (  Destination => Result,
         Pointer     => Pointer,
         Value       => Integer (Year),
         Field       => 4,
         Justify     => Right,
         Fill        => '0'
      );
      Put (Result, Pointer, "-");
      Put
      (  Destination => Result,
         Pointer     => Pointer,
         Value       => Integer (Month),
         Field       => 2,
         Justify     => Right,
         Fill        => '0'
      );
      Put (Result, Pointer, "-");
      Put
      (  Destination => Result,
         Pointer     => Pointer,
         Value       => Integer (Day),
         Field       => 2,
         Justify     => Right,
         Fill        => '0'
      );
      Put (Result, Pointer, " ");
      Put
      (  Destination => Result,
         Pointer     => Pointer,
         Value       => Count / 3600,
         Field       => 2,
         Justify     => Right,
         Fill        => '0'
      );
      Put (Result, Pointer, ":");
      Put
      (  Destination => Result,
         Pointer     => Pointer,
         Value       => (Count / 60) mod 60,
         Field       => 2,
         Justify     => Right,
         Fill        => '0'
      );
      Put (Result, Pointer, ":");
      Put
      (  Destination => Result,
         Pointer     => Pointer,
         Value       => Count mod 60,
         Field       => 2,
         Justify     => Right,
         Fill        => '0'
      );
      Put (Result, Pointer, ".");
      Put
      (  Destination => Result,
         Pointer     => Pointer,
         Field       => 3,
         Justify     => Right,
         Fill        => '0',
         Value       => Integer
                        (  (  Seconds
                           -  Day_Duration (Count)
                           )
                           *  1.000
      )                 );
      return Result;
   end Image;

   procedure Put
             (  File    : File_Type;
                Storage : Storage_Handle;
                Flags   : Output_Flags := Output_Catalogue_Index
             )  is
   begin
      Put (File, Data_Base_Object'Class (Ptr (Storage).all), Flags);
   end Put;

   procedure Put
             (  File    : File_Type;
                Storage : Data_Base_Object'Class;
                Index   : Byte_Index;
                Prefix  : String := ""
             )  is
      Stream : aliased Input_Stream (Storage.Pool);
      Object : constant Object_Record := Get_Record (Storage, Index);
   begin
      Put_Line (File, Prefix & "ID         :" & Image (Object.ID));
      if Object.Token > 0 then
         Open (Stream, Object.Token);
         declare
            Token : constant Name_Token := Input (Stream'Access);
         begin
            Put_Line
            (  File,
               (  Prefix
               &  "Key address:"
               &  Image (Object.Token)
               &  ", Parent: "
               &  Image (Token.Parent)
               &  ", Name: "
               &  Quote (Token.Name, ''')
            )  );
         end;
         Close (Stream);
      end if;
      Put_Line (File, Prefix & "Class      :" & Object.Class);
      Open (Stream, Object.Data);
      Put_Line (File, Prefix & "Data       :" & Input (Stream'Access));
      Close (Stream);
      Open (Stream, Object.Parameters);
      Put_Line (File, Prefix & "Parameters :" & Input (Stream'Access));
      Close (Stream);
      if Object.References > 0 then
         Open (Stream, Object.References);
         Put (File, Prefix & "References :");
         for Index in 1..Natural'(Input (Stream'Access)) loop
            declare
               ID : constant Object_ID := Input (Stream'Access);
            begin
               Put (File, Image (ID) & ' ');
            end;
          end loop;
          Close (Stream);
          New_Line (File);
      end if;
      Put_Line (File, Prefix & "Created    :" & Image (Object.Created));
   end Put;

   procedure Put
             (  File    : File_Type;
                Storage : in out Data_Bank_Object'Class;
                Flags   : Output_Flags := Output_Catalogue_Index
             )  is
   begin
      if 0 /= (Flags and Output_Catalogue_Index) then
         Indexed_IO.Put (File, Data_Base_Object'Class (Storage));
      end if;
      if Storage in Data_Base_Object'Class then
         declare
            DB : Data_Base_Object'Class renames
                 Data_Base_Object'Class (Storage);
         begin
            if 0 /= (Flags and Output_File_General_Information) then
               Put_Line ("File   : " & Get_Name (DB.File));
               Put_Line
               (  "Last ID: "
               &  Image
                  (  Object_ID (Get_Root_Index (DB.Pool.all, Last_Index))
               )  );
            end if;
            if 0 /= (Flags and Output_Objects_Map) then
               Put_Line ("Object ID to record map");
               Put_Line ("ID-->>>--Object-Record-------------");
               declare
                  Item : Object_Ptr := Get_First (DB.Map.all);
               begin
                  while Item /= No_Object loop
                     Put_Line
                     (  File,
                        (  Image (Object_ID (Get_Key (Item)))
                        &  " >>> "
                        &  Image (Get_Value (Item))
                     )  );
                     Put (File, DB, Get_Value (Item), "   ");
                     Item := Get_Next (Item);
                  end loop;
               end;
            end if;
            if 0 /= (Flags and Output_Names_Map) then
               Put_Line ("Parent ID+Name to object ID map");
               Put_Line ("ID--Name->>>-Object-ID--Key-address-----");
               declare
                  Item : Name_Ptr := Get_First (DB.Directory.all);
               begin
                  while Item /= No_Name loop
                     declare
                        Key : constant Name_Token :=
                              Get_Key (DB, Get_Key_Address (Item));
                     begin
                        Put_Line
                        (  File,
                           (  Image (Key.Parent)
                           &  " "
                           &  Quote (Key.Name, ''')
                           &  " >>> "
                           &  Image (Object_ID (Get_Pointer (Item)))
                           &  " "
                           &  Image (Get_Key_Address (Item))
                        )  );
                        Item := Get_Next (Item);
                     end;
                  end loop;
               end;
            end if;
            if 0 /= (Flags and Output_Objects_Links) then
               Put_Line ("From-->>>--To---------");
               declare
                  Item : Object_Ptr := Get_First (DB.Links.all);
               begin
                  while Item /= No_Object loop
                     declare
                        Key : constant Byte_Index := Get_Key (Item);
                     begin
                        if Get_Value (Item) = 0 then
                           Put_Line
                           (  File,
                              (  Image (Get_From (Key))
                              &  " >>> "
                              &  Image (Get_To (Key))
                           )  );
                        else
                           Put_Line
                           (  File,
                              (  Image (Get_From (Key))
                              &  " >>> "
                              &  Image (Get_To (Key))
                              &  " weak"
                           )  );
                        end if;
                        Item := Get_Next (Item);
                     end;
                  end loop;
               end;
            end if;
            if 0 /= (Flags and Output_Flags (Dump_All)) then
               Put_Line ("Persistent memory pool:");
               Put
               (  File,
                  DB.Pool.all,
                  Dump_Flags (Flags and Output_Flags (Dump_All))
               );
            end if;
         end;
      end if;
   end Put;

   procedure Put
             (  Storage : Storage_Handle;
                Flags   : Output_Flags := Output_Catalogue_Index
             )  is
   begin
      Put (Standard_Output, Storage, Flags);
   end Put;

   procedure Put
             (  Storage : in out Data_Bank_Object'Class;
                Flags   : Output_Flags := Output_Catalogue_Index
             )  is
   begin
      Put (Standard_Output, Storage, Flags);
   end Put;

   procedure Put
             (  File    : File_Type;
                Storage : Data_Bank_Object'Class;
                ID      : Object_ID;
                Prefix  : String := ""
             )  is
      DB : Data_Base_Object'Class renames
           Data_Base_Object'Class (Storage);
   begin
      Put (File, DB, Get_By_ID (DB, ID), Prefix);
   end Put;

   procedure Put
             (  File    : File_Type;
                Storage : Storage_Handle;
                ID      : Object_ID;
                Prefix  : String := ""
             )  is
   begin
      Put
      (  File,
         Data_Base_Object'Class (Ptr (Storage).all),
         ID,
         Prefix
      );
   end Put;

   procedure Put
             (  Storage : Storage_Handle;
                ID      : Object_ID;
                Prefix  : String := ""
             )  is
   begin
      Put
      (  Standard_Output,
         Data_Base_Object'Class (Ptr (Storage).all),
         ID,
         Prefix
      );
   end Put;

   procedure Put
             (  Storage : Data_Bank_Object'Class;
                ID      : Object_ID;
                Prefix  : String := ""
             )  is
   begin
      Put (Standard_Output, Storage, ID, Prefix);
   end Put;

end Persistent.Single_File.Text_IO;
