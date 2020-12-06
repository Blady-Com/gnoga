------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                G N O G A . G U I . E L E M E N T . T A B L E             --
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
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

package body Gnoga.Gui.Element.Table is

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (Table  : in out Table_Type;
      Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      ID     : in     String := "")
   is
   begin
      Table.Create_From_HTML (Parent, "<table />", ID);
   end Create;

   -------------
   -- Caption --
   -------------

   function Caption
     (Table : Table_Type)
      return String
   is
   begin
      return Table.Property ("caption");
   end Caption;

   -----------------
   -- Add_Caption --
   -----------------

   procedure Add_Caption
     (Table : in out Table_Type;
      Value : in     String)
   is
      E : constant Element_Access := new Element_Type;
   begin
      E.Dynamic;
      E.Create_From_HTML (Table, "<caption>" & Escape_Quotes (Value) & "</caption>");
      E.Place_Inside_Top_Of (Table);
   end Add_Caption;

   ------------
   -- Create --
   ------------

   procedure Create
     (Row    : in out Table_Row_Type;
      Parent : in out Element_Type'Class;
      ID     : in     String := "")
   is
   begin
      if not
        (Parent in Table_Type'Class or Parent in Table_Header_Type'Class or Parent in Table_Body_Type'Class or
         Parent in Table_Footer_Type'Class)
      then
         raise Constraint_Error with "Invalid parent of Table_Row_Type";
      end if;

      Row.Create_From_HTML (Parent, "<tr />", ID);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Column      : in out Table_Column_Type;
      Row         : in out Table_Row_Type'Class;
      Content     : in     String   := "";
      Column_Span : in     Positive := 1;
      Row_Span    : in     Positive := 1;
      ID          : in     String   := "")
   is
   begin
      Column.Create_From_HTML
        (Row,
         "<td colspan=" & Image (Column_Span) & " rowspan=" & Image (Row_Span) & ">" & Escape_Quotes (Content) &
         "</td>",
         ID);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Heading     : in out Table_Heading_Type;
      Row         : in out Table_Row_Type'Class;
      Content     : in     String   := "";
      Column_Span : in     Positive := 1;
      Row_Span    : in     Positive := 1;
      ID          : in     String   := "")
   is
   begin
      Heading.Create_From_HTML
        (Row,
         "<th colspan=" & Image (Column_Span) & " rowspan=" & Image (Row_Span) & ">" & Escape_Quotes (Content) &
         "</th>",
         ID);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Header : in out Table_Header_Type;
      Table  : in out Table_Type'Class;
      ID     : in     String := "")
   is
   begin
      Header.Create_From_HTML (Table, "<thead />", ID);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (TBody : in out Table_Body_Type;
      Table : in out Table_Type'Class;
      ID    : in     String := "")
   is
   begin
      TBody.Create_From_HTML (Table, "<tbody />", ID);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Footer : in out Table_Footer_Type;
      Table  : in out Table_Type'Class;
      ID     : in     String := "")
   is
   begin
      Footer.Create_From_HTML (Table, "<tfoot />", ID);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Group : in out Column_Group_Type;
      Table : in out Table_Type'Class;
      ID    : in     String := "")
   is
   begin
      Group.Create_From_HTML (Table, "<colgroup />", ID);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Column      : in out Column_Type;
      Group       : in out Column_Group_Type'Class;
      Content     : in     String   := "";
      Column_Span : in     Positive := 1;
      ID          : in     String   := "")
   is
      pragma Unreferenced (Content);
   begin
      Column.Create_From_HTML (Group, "<col span=" & Image (Column_Span) & ">", ID);
   end Create;

end Gnoga.Gui.Element.Table;
