--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, Michael Rohan <mrohan@zanyblue.com>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--    * Redistributions of source code must retain the above copyright
--      notice, this list of conditions and the following disclaimer.
--
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--
--    * Neither the name of ZanyBlue nor the names of its contributors may
--      be used to endorse or promote products derived from this software
--      without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
--  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

with Ada.Wide_Text_IO.Text_Streams;
with Ada.Strings.Wide_Unbounded;
with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents;
with ZanyBlue.OS;
with ZanyBlue.Text.Formatting;

package body ZBTest.XML_Data is

   use DOM.Core.Nodes;
   use DOM.Core.Elements;
   use DOM.Core.Documents;
   use ZanyBlue.OS;
   use ZanyBlue.Text.Formatting;

   -----------------------------
   -- Add_Text_Node_From_File --
   -----------------------------

   function Add_Text_Node_From_File
      (Document  : in Value_Type'Class;
       Parent    : in Value_Type'Class;
       File_Name : in Wide_String) return Value_Type'Class is
      use Ada.Wide_Text_IO;
      use Ada.Strings.Wide_Unbounded;
      Document_Node : constant Node := XML_Node_Type (Document).Data;
      Parent_Node   : constant Node := XML_Node_Type (Parent).Data;
      Buffer        : Unbounded_Wide_String;
      File          : File_Type;
   begin
      Wide_Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         Append (Buffer, Get_Line (File));
         Append (Buffer, Wide_Character'Val (10));
      end loop;
      Close (File);
      return To_XML_Node_Value (Append_Child (Parent_Node,
                                    Create_Text_Node (Document_Node,
                                         To_UTF8 (To_Wide_String (Buffer)))));
   end Add_Text_Node_From_File;

   ---------------------
   -- Create_XML_Node --
   ---------------------

   function Create_XML_Node
      (Document   : in Value_Type'Class;
       Parent     : in Value_Type'Class;
       Child_Name : in Wide_String) return Value_Type'Class
   is
      use Ada.Wide_Text_IO;
      Document_Node : constant Node := XML_Node_Type (Document).Data;
      Parent_Node   : constant Node := XML_Node_Type (Parent).Data;
   begin
      return To_XML_Node_Value (Append_Child (Parent_Node,
                                       Create_Element (Document_Node,
                                                       To_UTF8 (Child_Name))));
   end Create_XML_Node;

   ------------------
   -- Document_XML --
   ------------------

   function Document_XML return Value_Type'Class is
      Implementation : DOM_Implementation;
   begin
      return To_XML_Node_Value (Create_Document (Implementation));
   end Document_XML;

   -------------------
   -- Set_Attribute --
   -------------------

   procedure Set_Attribute (Parent     : in Value_Type'Class;
                            Name       : in Wide_String;
                            Value      : in Wide_String) is
      Parent_Node : constant Node := XML_Node_Type (Parent).Data;
   begin
      Set_Attribute (Parent_Node, To_UTF8 (Name), To_UTF8 (Value));
   end Set_Attribute;

   -------------
   -- To_List --
   -------------

   function To_List (Value : in XML_Node_Type;
                     Name  : in Wide_String) return List_Type is
      Result : List_Type;
   begin
      Append (Result, Value.To_String (Name));
      return Result;
   end To_List;

   -------------
   -- To_Node --
   -------------

   function To_Node (Value : in XML_Node_Type;
                     Name  : in Wide_String) return Node is
      pragma Unreferenced (Name);
   begin
      return Value.Data;
   end To_Node;

   ---------------
   -- To_String --
   ---------------

   function To_String (Value : in XML_Node_Type;
                       Name  : in Wide_String) return Wide_String is
      pragma Unreferenced (Value);
   begin
      return Format ("<XMLDATA: {0}>", Argument0 => +Name);
   end To_String;

   -----------------------
   -- To_XML_Node_Value --
   -----------------------

   function To_XML_Node_Value (Data : in Node) return Value_Type'Class is
   begin
      return XML_Node_Type'(Value_Type with Data => Data);
   end To_XML_Node_Value;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name (Value : in XML_Node_Type;
                       Name  : in Wide_String) return Wide_String is
      pragma Unreferenced (Value);
      pragma Unreferenced (Name);
   begin
      return "xmldata";
   end Type_Name;

   -----------
   -- Write --
   -----------

   procedure Write (File_Name : in Wide_String;
                    Value     : in XML_Node_Type) is
      use Ada.Wide_Text_IO;
      use Ada.Wide_Text_IO.Text_Streams;
      File : File_Type;
   begin
      Wide_Create (File, File_Name);
      Write (Stream (File), Value.Data, Pretty_Print => True);
      Close (File);
   end Write;

end ZBTest.XML_Data;
