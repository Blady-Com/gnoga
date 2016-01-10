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

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Wide_Unbounded;
with ZanyBlue.Text.Catalogs;

package ZanyBlue.Text.Message_Maps is

   use Ada.Containers;
   use Ada.Strings.Wide_Unbounded;
   use ZanyBlue.Text.Catalogs;

   type Message_Triple is
      record
         Facility_Index : Facility_Index_Type;
         Key_Index      : Key_Index_Type;
         Locale_Index   : Locale_Index_Type;
      end record;

   type Message_Definition is
      record
         Pool         : Static_Message_Pool_Type;
         First        : Positive;
         Last         : Natural;
         Locale_Index : Locale_Index_Type := 1;
         Count        : Natural := 0;
      end record;

   type Message_Map_Type is tagged private;
   --
   --  Message_Map_Type
   --
   --  Protected type to store mappings from (Facility, Key, Locale)
   --  index triples to message strings.
   --

   function Length (Message_Map : in Message_Map_Type) return Natural;
   --  Return the number of messages currently stored in the map.

   procedure Get (Message_Map : in out Message_Map_Type;
                  Triple      : in Message_Triple;
                  Result      : out Message_Definition);
   --  Get the message associated with a particular index triple.  Raises
   --  the exception No_Such_Item if it does not exist.

   function Get_Pool (Message_Map : in Message_Map_Type) return Wide_String;
   --  Return a copy of a the current pool data.  This is used primarily
   --  by the zbmcompile utility to dump a compiled set of .properties
   --  files.

   function Text (Message_Map : in Message_Map_Type;
                  Message     : in Message_Definition) return Wide_String;
   --  Get the message associated with a particular message.  Raises
   --  the exception No_Such_Item if it does not exist.

   function Pool_Size (Message_Map : in Message_Map_Type) return Natural;
   --  Return the size of the current string pool.

   procedure Add (Message_Map : in out Message_Map_Type;
                  Triple      : in Message_Triple;
                  Message     : in Message_Definition);
   --  Add a new message to the set.

   procedure Add (Message_Map   : in out Message_Map_Type;
                  Triple        : in Message_Triple;
                  Message       : in Wide_String;
                  Source_Locale : in Locale_Index_Type);
   --  Add a new message to the set.

   procedure Adjust_Size (Message_Map    : in out Message_Map_Type;
                          Extra_Messages : in Natural);
   --  When bulk loading (zbmcompile Initialize) adjust the size of the
   --  set to accommodate the new messages.

   procedure Iterate (
      Message_Map : in out Message_Map_Type;
      Handler     : not null
                       access
                          procedure (Facility      : in Facility_Index_Type;
                                     Key           : in Key_Index_Type;
                                     Locale        : in Locale_Index_Type;
                                     Source_Locale : in Locale_Index_Type;
                                     First         : in Positive;
                                     Last          : in Natural;
                                     Count         : in Natural));

   procedure Iterate (
      Message_Map : in out Message_Map_Type;
      Handler     : not null
                       access
                          procedure (Facility      : in Facility_Index_Type;
                                     Key           : in Key_Index_Type;
                                     Locale        : in Locale_Index_Type;
                                     Source_Locale : in Locale_Index_Type;
                                     Message       : in Wide_String;
                                     Count         : in Natural));

private

   function Message_Triple_Hash (Value : in Message_Triple) return Hash_Type;
   --  Ada.Containers hash function for the Message_Triple type.

   package Triple_Maps is
      new Hashed_Maps (Key_Type        => Message_Triple,
                       Element_Type    => Message_Definition,
                       Hash            => Message_Triple_Hash,
                       Equivalent_Keys => "=");

   type Message_Map_Type is tagged
      record
         Messages     : Triple_Maps.Map;
         Pool         : Unbounded_Wide_String;
      end record;

end ZanyBlue.Text.Message_Maps;
