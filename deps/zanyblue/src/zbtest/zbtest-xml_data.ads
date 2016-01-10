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

with DOM.Core;
with ZanyBlue.Parameters.Values;

package ZBTest.XML_Data is

   use DOM.Core;
   use ZanyBlue.Parameters;
   use ZanyBlue.Parameters.Values;

   type XML_Node_Type is new Value_Type with private;

   function To_Node (Value : in XML_Node_Type;
                     Name  : in Wide_String) return Node;
   --  Return the value of an XML Node parameter.  The exception
   --  Not_An_XML_Node_Error can be raised.

   overriding
   function To_List (Value : in XML_Node_Type;
                     Name  : in Wide_String) return List_Type;
   --  Return the value of a list parameter.  This will return the value
   --  converted to a list for if necessary.

   overriding
   function To_String (Value : in XML_Node_Type;
                       Name  : in Wide_String) return Wide_String;
   --  Return the value of a parameter as a string.  This can be used
   --  for any parameter type.

   overriding
   function Type_Name (Value : in XML_Node_Type;
                       Name  : in Wide_String) return Wide_String;
   --  Return the type name for a parameter ("float", "integer", "string" or
   --  "time").

   function Document_XML return Value_Type'Class;
   --  Return an XML document as a boxed Value_Type.  This is the DOM
   --  document.

   function Create_XML_Node
      (Document   : in Value_Type'Class;
       Parent     : in Value_Type'Class;
       Child_Name : in Wide_String) return Value_Type'Class;
   --  Create an XML node in the given XML document and append to the parent
   --  node.  The node created is returned as a boxed Value_Type;

   procedure Set_Attribute (Parent     : in Value_Type'Class;
                            Name       : in Wide_String;
                            Value      : in Wide_String);
   --  Set an XML attribute on a given node.

   function Add_Text_Node_From_File
      (Document  : in Value_Type'Class;
       Parent    : in Value_Type'Class;
       File_Name : in Wide_String) return Value_Type'Class;
   --  Load the contents of the named file and add to the XML document as
   --  a text node.

   function To_XML_Node_Value (Data : in Node) return Value_Type'Class;
   --  Return an XML node as a boxed Value_Type.

   procedure Write (File_Name : in Wide_String;
                    Value     : in XML_Node_Type);
   --  Write the XML document contained in the XML_Node_Type to the named file.

private

   type XML_Node_Type is new Value_Type with
   record
      Data : Node;
   end record;

end ZBTest.XML_Data;
