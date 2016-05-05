--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--        HTTP server.SQLite_Browser               Winter, 2014       --
--  Implementation                                                    --
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

with Strings_Edit.Integers;  use Strings_Edit.Integers;

package body GNAT.Sockets.Connection_State_Machine.HTTP_Server.
             SQLite_Browser is

   CRLF : constant String := Character'Val (13) & Character'Val (10);

   function Get (Content : access DB_Query_Content) return String is
      function Do_Column return String is
      begin
         case Column_Type (Content.List, Content.Column) is
            when SQLITE_INTEGER =>
               return
               (  Head (Right, Content.Column)
               &  Integer_Image (Content.List, Content.Column)
               &  Tail (Content.Column, Content.Number_Of_Columns)
               );
            when SQLITE_FLOAT =>
               return
               (  Head (Right, Content.Column)
               &  Real_Image (Content.List, Content.Column)
               &  Tail (Content.Column, Content.Number_Of_Columns)
               );
            when SQLITE_BLOB | SQLITE_TEXT =>
               return
               (  Head (Left, Content.Column)
               &  String_Image
                  (  Content.List,
                     Content.Column,
                     Content.Max_String_Column_Width
                  )
               &  Tail (Content.Column, Content.Number_Of_Columns)
               );
            when SQLITE_NULL =>
               return
               (  Head (Center, Content.Column)
               &  "<font color=""#FF0000"">NULL</font>"
               &  Tail (Content.Column, Content.Number_Of_Columns)
               );
         end case;
      end Do_Column;

      function Initial_Value return String is
      begin
         if Is_Valid (Content.Command) then
            return
            (  "value="""
            &  To_HTML (Ptr (Content.Command).Text)
            &  """ "
            );
         else
            return "";
         end if;
      end Initial_Value;
   begin
      if not Is_Valid (Content.Path) then
         return "";
      elsif Content.Count = 0 then
         declare
            Page_Head : constant String :=
                        (  "<html><head><title>Database query</title>"
                        &  "</head><body>" & CRLF
                        &  "<form action="""
                        &  To_HTML
                           (  Get_Query_Action
                              (  DB_Query_Content'Class (Content.all)
                           )  )
                        &  """ method=""post"">" & CRLF
                        &  "<input type=""text"" name=""statement"" "
                        &  Initial_Value
                        &  "style=""width: 100%""><br>" & CRLF
                        &  "<input type=""submit"" "
                        &  "value=""Query"">" & CRLF
                        &  "</form>" & CRLF
                        );
         begin
            if (  Is_Valid (Content.Command)
               and then
                  Ptr (Content.Command).Text'Length > 0
               )
            then
               begin
                  Content.List :=
                     Prepare (Content.DB, Ptr (Content.Command).Text);
               exception
                  when Error : others =>
                     Content.Count := Natural'Last - 1;
                     return
                     (  Page_Head
                     &  "<hr>" & CRLF
                     &  "Error executing the statement: "
                     &  Ada.Exceptions.Exception_Message (Error)
                     );
               end;
               Content.Count  := 1; -- First row of the result set
               Content.Column := 0;
               return Page_Head & "<hr><i>Result set<i><br>" & CRLF;
            else
               Content.Count := Natural'Last - 1;
               return Page_Head;
            end if;
         end;
      elsif Content.Count = 1 then -- First row of the set
         Content.Count := 2;
         if Content.Column in 1..Content.Number_Of_Columns - 1 then
            Content.Column := Content.Column + 1;
            return Do_Column;
         else -- Picking the row
            if Step (Content.List) then -- Here is the first row
               Content.Number_Of_Columns := Column_Count (Content.List);
               Content.Column := 1;
               return
               (  "<table border=""0"" cellspacing=""0"" "
               &  "cellpadding=""0""><tr>" & CRLF
               &  Do_Column
               );
            else
               Content.Count := Natural'Last - 1;
               return "</table><hr>" & CRLF;
            end if;
         end if;
      elsif Content.Count < Natural'Last - 1 then
         if Content.Column in 1..Content.Number_Of_Columns - 1 then
            Content.Column := Content.Column + 1;
            return Do_Column;
         else -- Picking the row
            if Step (Content.List) then -- Here is the first row
               Content.Column := 1;
               return "<tr>" & CRLF & Do_Column;
            else
               Content.Count := Natural'Last - 1;
               return "</table><hr>" & CRLF;
            end if;
         end if;
      elsif Content.Count < Natural'Last then
         Content.Count := Natural'Last;
         return "</body></html>" & CRLF;
      else
         Content.Count := 0;
         return "";
      end if;
   end Get;

   function Get (Content : access DB_Tables_Content) return String is
      function Item (Name : String) return String is
      begin
         return
         (  "<tr><td align=""right"">"
         &  Image (Content.Count - 1)
         &  "&nbsp;</td>"
         &  "<td>&nbsp;<a href="
         &  To_HTML
            (  Get_Content_Page
               (  DB_Tables_Content'Class (Content.all),
                  Name
            )  )
         &  ">"
         &  To_HTML (Name)
         &  "</a>"
         &  " (<a href="
         &  To_HTML
            (  Get_Schema_Page
               (  DB_Tables_Content'Class (Content.all),
                  Name
            )  )
         &  ">schema</a>)"
         &  "</td></tr>"
         &  CRLF
         );
      end Item;
   begin
      if not Is_Valid (Content.Path) then
         return "";
      elsif Content.Count = 0 then
         Content.List :=
            Prepare
            (  Content.DB,
               "SELECT * FROM sqlite_master WHERE type='table'"
            );
         Content.Count := 1;
         return
         (  "<html><head><title>Database tables</title>"
         &  "</head><body>"
         &  CRLF
         &  "<table border=""0"" cellspacing=""0"" cellpadding=""0"">"
         &  CRLF
         &  "<tr>"
         &  "<td bgcolor=""#000000"" align=""right"">"
         &  "<font color=""#FFFFFF"">&nbsp;"
         &  "<b>No.</b>&nbsp;</font></td>"
         &  "<td bgcolor=""#000000""><font color=""#FFFFFF"">"
         &  "&nbsp;<b>Table</b></font></td>"
         &  "</tr>"
         &  CRLF
         );
      elsif Content.Count < Positive'Last then
         if Step (Content.List) then
            Content.Count := Content.Count + 1;
            return Item (Column (Content.List, 2));
         else
            Content.Count := Positive'Last;
            return "</table></body></html>";
         end if;
      else
         Content.Count := 0;
         return "";
      end if;
   end Get;

   function Get (Content : access Schema_Content) return String is
      use type Interfaces.C.int;
      function Item return String is
         function Can_NULL return String is
         begin
            if Interfaces.C.int'(Column (Content.List, 4)) = 0 then
               return "no";
            else
               return "yes";
            end if;
         end Can_NULL;
         function Is_Primary return String is
         begin
            if Interfaces.C.int'(Column (Content.List, 6)) = 0 then
               return "no";
            else
               return "yes";
            end if;
         end Is_Primary;
      begin
         return
         (  "<tr><td align=""right"">"
         &  Image (Content.Count - 1)
         &  "</td>"
         &  "<td>&nbsp;"
         &  To_HTML (Column (Content.List, 2)) -- Name
         &  "&nbsp;</td>"
         &  "<td align=""center"">&nbsp;"
         &  To_HTML (Column (Content.List, 3)) -- Type
         &  "&nbsp;</td>"
         &  "<td align=""center"">&nbsp;"
         &  Can_NULL                           -- NULL
         &  "&nbsp;</td>"
         &  "<td align=""center"">&nbsp;"
         &  Is_Primary                         -- Primary key
         &  "&nbsp;</td>"
         &  "<td>&nbsp;"
         &  To_HTML (Column (Content.List, 5)) -- Default
         &  "</td></tr>"
         &  CRLF
         );
      end Item;
   begin
      if not (  Is_Valid (Content.Path)
             and then
                Is_Valid (Content.Table)
             )
      then
         return "";
      elsif Content.Count = 0 then
         Content.List :=
            Prepare
            (  Content.DB,
               "PRAGMA table_info(" & Get (Content.Table) & ")"
            );
         Content.Count := 1;
         return
         (  "<html><head><title>Schema of "
         &  To_HTML (Get (Content.Table))
         &  "</title></head><body>"
         &  CRLF
         &  "<i>"
         &  To_HTML (Get (Content.Table))
         &  "</i><br>"
         &  CRLF
         &  "<table border=""0"" cellspacing=""0"" cellpadding=""0"">"
         &  CRLF
         &  "<tr>"
         &  "<td bgcolor=""#000000""><font color=""#FFFFFF"">"
         &  "&nbsp;<b>Column</b>&nbsp;</font></td>"
         &  "<td bgcolor=""#000000""><font color=""#FFFFFF"">"
         &  "&nbsp;<b>Name</b>&nbsp;</font></td>"
         &  "<td bgcolor=""#000000"" align=""center"">"
         &  "<font color=""#FFFFFF"">"
         &  "&nbsp;<b>Type</b>&nbsp;</font></td>"
         &  "<td bgcolor=""#000000"" align=""center"">"
         &  "<font color=""#FFFFFF"">"
         &  "&nbsp;<b>NULL</b>&nbsp;</font></td>"
         &  "<td bgcolor=""#000000"" align=""center"">"
         &  "<font color=""#FFFFFF"">"
         &  "&nbsp;<b>Primary</b>&nbsp;</font></td>"
         &  "<td bgcolor=""#000000""><font color=""#FFFFFF"">"
         &  "&nbsp;<b>Default</b>&nbsp;</font></td>"
         &  "</tr>"
         &  CRLF
         );
      elsif Content.Count < Positive'Last then
         if Step (Content.List) then
            Content.Count := Content.Count + 1;
            return Item;
         else
            Content.Count := Positive'Last;
            return "</table></body></html>";
         end if;
      else
         Content.Count := 0;
         return "";
      end if;
   end Get;

   function Get (Content : access Table_Content) return String is
      function Item return String is
         Column : constant Positive := Content.Column - 1;
         Last   : constant Positive := Content.Number_Of_Columns;
      begin
         if Is_Null (Content.List, Column) then
            return
            (  Head (Center, Column)
            &  "<font color=""#FF0000"">NULL</font>"
            &  Tail (Column, Last)
            );
         else
            case Get (Content.Columns_List, Column).Datatype is
               when SQLITE_INTEGER =>
                  return
                  (  Head (Right, Column)
                  &  Integer_Image (Content.List, Column)
                  &  Tail (Column, Last)
                  );
               when SQLITE_FLOAT =>
                  return
                  (  Head (Right, Column)
                  &  Real_Image (Content.List, Column)
                  &  Tail (Column, Last)
                  );
               when SQLITE_BLOB | SQLITE_TEXT =>
                  return
                  (  Head (Left, Column)
                  &  String_Image
                     (  Content.List,
                        Column,
                        Content.Max_String_Column_Width
                     )
                  &  Tail (Column, Last)
                  );
            end case;
         end if;
      end Item;
   begin
      if not (  Is_Valid (Content.Path)
             and then
                Is_Valid (Content.Table)
             )
      then
         return "";
      elsif Content.Count = 0 then
         if Content.Column = 0 then
            Content.List :=
               Prepare
               (  Content.DB,
                  "PRAGMA table_info(" & Get (Content.Table) & ")"
               );
            Content.Number_Of_Columns := 0;
            Content.Column := 1;
            return
            (  "<html><head><title>Content of "
            &  To_HTML (Get (Content.Table))
            &  "</title></head><body>"
            &  CRLF
            &  "<i>"
            &  To_HTML (Get (Content.Table))
            &  "</i><br>"
            &  CRLF
            &  "<table border=""0"" cellspacing=""0"" "
            &  "cellpadding=""0""><tr>"
            &  CRLF
            &  "<td bgcolor=""#000000"" align=""right"">"
            &  "<font color=""#FFFFFF"">&nbsp;"
            &  "<b>No.</b>&nbsp;</font></td>"
            &  CRLF
            );
         else
            if Step (Content.List) then
               Content.Number_Of_Columns :=
                  Content.Number_Of_Columns + 1;
               declare
                  Descriptor : Column_Descriptor;
                  Datatype   : constant String :=
                               Column (Content.List, 3);
               begin
                  if Datatype = "INTEGER" then
                     Descriptor.Datatype := SQLITE_INTEGER;
                  elsif Datatype = "REAL" then
                     Descriptor.Datatype := SQLITE_FLOAT;
                  elsif Datatype = "TEXT" then
                     Descriptor.Datatype := SQLITE_TEXT;
                  else
                     Descriptor.Datatype := SQLITE_BLOB;
                  end if;
                  Put
                  (  Content.Columns_List,
                     Content.Column,
                     Descriptor
                  );
                  Content.Column := Content.Column + 1;
                  return
                  (  "<td bgcolor=""#000000""><font color=""#FFFFFF"">"
                  &  "&nbsp;<b>"
                  &  To_HTML (Column (Content.List, 2))
                  &  "</b>&nbsp;</font></td>"
                  &  CRLF
                  );
               end;
            else
               Content.Count  := 1;
               Content.Column := 0;
               Content.List :=
                  Prepare
                  (  Content.DB,
                     "SELECT * FROM " & Get (Content.Table)
                  );
               return "</tr>" & CRLF;
            end if;
         end if;
      elsif Content.Count < Positive'Last then
         if Content.Column in 1..Content.Number_Of_Columns then
            Content.Column := Content.Column + 1;
            return Item;
         else
            if Step (Content.List) then
               Content.Count  := Content.Count  + 1;
               Content.Column := 1;
               return
               (  "<tr><td align=""right"">"
               &  Image (Content.Count - 1)
               &  "</td>"
               &  CRLF
               );
            else
               Content.Count  := Positive'Last;
               Content.Column := 0;
               return "</table></body></html>";
            end if;
         end if;
      else
         Content.Count  := 0;
         Content.Column := 0;
         return "";
      end if;
   end Get;

   function Get (Object : Handle) return String is
   begin
      if Is_Valid (Object) then
         return Ptr (Object).Text;
      else
         return "";
      end if;
   end Get;

   function Get_Content_Page
            (  Content : DB_Tables_Content;
               Table   : String
            )  return String is
   begin
      return
      (  Get_Database_Path (Abstract_SQLite_Content'Class (Content))
      &  "/"
      &  Table
      &  "/content.htm"
      );
   end Get_Content_Page;

   function Get_Database (Content : Abstract_SQLite_Content)
      return Data_Base is
   begin
      return Content.DB;
   end Get_Database;

   function Get_Database_Path (Content : Abstract_SQLite_Content)
      return String is
   begin
      return Get (Content.Path);
   end Get_Database_Path;

   function Get_Query_Action (Content : DB_Query_Content)
      return String is
   begin
      return
      (  Get_Database_Path (Abstract_SQLite_Content'Class (Content))
      &  "/query.htm"
      );
   end Get_Query_Action;

   function Get_Schema_Page
            (  Content : DB_Tables_Content;
               Table   : String
            )  return String is
   begin
      return
      (  Get_Database_Path (Abstract_SQLite_Content'Class (Content))
      &  "/"
      &  Table
      &  "/schema.htm"
      );
   end Get_Schema_Page;

   function Get_Table (Content : Abstract_Table_Content)
      return String is
   begin
      return Get (Content.Table);
   end Get_Table;

   function Head (Justify : Alignment; Column : Positive)
      return String is
      function Align return String is
      begin
         case Justify is
            when Center =>
               return """center""";
            when Left =>
               return """left""";
            when Right =>
               return """right""";
         end case;
      end Align;
      function Color return String is
      begin
         case Column mod 3 is
            when 0 =>
               return """#FFDDFF""";
            when 1 =>
               return """#DDFFFF""";
            when others =>
               return """#FFFFDD""";
         end case;
      end Color;
   begin
      return "<td bgcolor=" & Color &" align=" & Align & ">&nbsp;";
   end Head;

   function Integer_Image
            (  Result : Statement;
               Column : Positive
            )  return String is
      use Integer_64_Edit;
      Value : constant Integer_64 := SQLite.Column (Result, Column);
   begin
      return Image (Value);
   end Integer_Image;

   function Real_Image
            (  Result : Statement;
               Column : Positive
            )  return String is
      use Double_Edit;
      Value : constant Interfaces.C.double :=
              SQLite.Column (Result, Column);
   begin
      return Image (Value);
   end Real_Image;

   procedure Set (Object : in out Handle; Value : String) is
   begin
      Invalidate (Object);
      Set (Object, new String_Object (Value'Length));
      Ptr (Object).Text := Value;
   end Set;

   procedure Set_Database
             (  Content : in out Abstract_SQLite_Content;
                Source  : Abstract_SQLite_Content'Class
             )  is
   begin
      Content.Count := 0;
      Content.DB    := Source.DB;
      Content.Path  := Source.Path;
   end Set_Database;

   procedure Set_Database
             (  Content   : in out Abstract_SQLite_Content;
                Database  : Data_Base;
                File_Name : String
             )  is
   begin
      Invalidate (Content.Path);
      Content.Count := 0;
      Content.DB    := Database;
      Set (Content.Path, File_Name);
   end Set_Database;

   procedure Set_Database_Path
             (  Content   : in out Abstract_SQLite_Content;
                File_Name : String;
                Flags     : Open_Flags := READONLY or FULLMUTEX
             )  is
   begin
      Invalidate (Content.Path);
      Content.Count := 0;
      Content.DB    := Open (File_Name, Flags);
      Set (Content.Path, File_Name);
   end Set_Database_Path;

   procedure Set_Statement
             (  Content   : in out DB_Query_Content;
                Statement : String
             )  is
   begin
      Set (Content.Command, Statement);
   end Set_Statement;

   procedure Set_Table
             (  Content : in out Abstract_Table_Content;
                Source  : Abstract_Table_Content'Class
             )  is
   begin
      Set_Database (Content, Source);
      Content.Table := Source.Table;
   end Set_Table;

   procedure Set_Table
             (  Content : in out Abstract_Table_Content;
                Table   : String
             )  is
   begin
      Content.Count := 0;
      Set (Content.Table, Table);
   end Set_Table;

   function String_Image
            (  Result    : Statement;
               Column    : Positive;
               Max_Width : Positive
            )  return String is
      Value : constant String := SQLite.Column (Result, Column);
   begin
      if Value'Length > Max_Width then
         return
         (  "<span style=""background-color: #FFFFFF"">"
         &  To_HTML (Value (Value'First .. Value'First + Max_Width / 2))
         &  "</span>..."
         &  "<span style=""background-color: #FFFFFF"">"
         &  To_HTML (Value (Value'Last - Max_Width / 2 .. Value'Last))
         &  "</span>"
         );
      else
         return
         (  "<span style=""background-color: #FFFFFF"">"
         &  To_HTML (Value)
         &  "</span>"
         );
      end if;
   end String_Image;

   function Tail
            (  Column      : Positive;
               Last_Column : Positive
            )  return String is
   begin
      if Column >= Last_Column then
         return "&nbsp;</td></tr>" & CRLF;
      else
         return "&nbsp;</td>" & CRLF;
      end if;
   end Tail;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : DB_Query_Content
             )  is
   begin
      null;
   end Write;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : DB_Tables_Content
             )  is
   begin
      null;
   end Write;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Schema_Content
             )  is
   begin
      null;
   end Write;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Table_Content
             )  is
   begin
      null;
   end Write;

end GNAT.Sockets.Connection_State_Machine.HTTP_Server.SQLite_Browser;
