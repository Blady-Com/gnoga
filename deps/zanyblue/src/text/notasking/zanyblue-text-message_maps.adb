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

package body ZanyBlue.Text.Message_Maps is

   use Triple_Maps;

   ---------
   -- Add --
   ---------

   procedure Add (Message_Map : in out Message_Map_Type;
                  Triple      : in Message_Triple;
                  Message     : in Message_Definition) is
      Position : constant Cursor := Find (Message_Map.Messages, Triple);
   begin
      if Position = No_Element then
         Message_Map.Messages.Insert (Triple, Message);
      else
         Message_Map.Messages.Replace_Element (Position, Message);
      end if;
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (Message_Map   : in out Message_Map_Type;
                  Triple        : in Message_Triple;
                  Message       : in Wide_String;
                  Source_Locale : in Locale_Index_Type) is
      New_Message : Message_Definition;
      First       : Natural := 0;
      Last        : Natural;
   begin
      if Message'Length > 0 then
         --  Attempt to locate the message in the existing pool
         First := Index (Message_Map.Pool, Message);
         Last  := First + Message'Length - 1;
      end if;
      if First = 0 then
         --  Failed to find it in the existing pool, add it
         First := Length (Message_Map.Pool) + 1;
         Append (Message_Map.Pool, Message);
         Last := Length (Message_Map.Pool);
      end if;
      New_Message.Pool := null;
      New_Message.First := First;
      New_Message.Last := Last;
      New_Message.Locale_Index := Source_Locale;
      Message_Map.Add (Triple, New_Message);
   end Add;

   -----------------
   -- Adjust_Size --
   -----------------

   procedure Adjust_Size (Message_Map    : in out Message_Map_Type;
                          Extra_Messages : in Natural) is
      Capacity : constant Natural := Natural (Message_Map.Messages.Capacity);
      Size     : constant Natural := Natural (Message_Map.Messages.Length);
      New_Size : constant Natural := Size + Extra_Messages;
   begin
      if New_Size > Capacity then
         --  Extend the size of the messages container, if necessary
         Message_Map.Messages.Reserve_Capacity (Count_Type (New_Size));
      end if;
   end Adjust_Size;

   ---------
   -- Get --
   ---------

   procedure Get (Message_Map    : in out Message_Map_Type;
                  Triple         : in Message_Triple;
                  Result         : out Message_Definition) is

      procedure Increment_Count (Key     : in Message_Triple;
                                 Element : in out Message_Definition);

      procedure Increment_Count (Key     : in Message_Triple;
                                 Element : in out Message_Definition) is
         pragma Unreferenced (Key);
      begin
         if Element.Count < Natural'Last then
            Element.Count := Element.Count + 1;
         end if;
      end Increment_Count;

      Position : constant Cursor := Message_Map.Messages.Find (Triple);

   begin
      if Position /= No_Element then
         Update_Element (Message_Map.Messages, Position,
                         Increment_Count'Access);
      end if;
      Result := Message_Map.Messages.Element (Triple);
   end Get;

   --------------
   -- Get_Pool --
   --------------

   function Get_Pool (Message_Map : in Message_Map_Type) return Wide_String is
   begin
      return To_Wide_String (Message_Map.Pool);
   end Get_Pool;

   -------------
   -- Iterate --
   -------------

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
                                    Count         : in Natural)) is

         procedure Callback (Position : in Cursor);
         --  Ada.Containers callback used to reformat arguments to pass off to
         --  the supplied handler.

         --------------
         -- Callback --
         --------------

         procedure Callback (Position : in Cursor) is
            M : constant Message_Definition := Element (Position);
            T : constant Message_Triple := Key (Position);
         begin
            Handler (T.Facility_Index, T.Key_Index, T.Locale_Index,
                     M.Locale_Index, M.First, M.Last, M.Count);
         end Callback;

   begin
      Message_Map.Messages.Iterate (Callback'Access);
   end Iterate;

   -------------
   -- Iterate --
   -------------

   procedure Iterate (
      Message_Map : in out Message_Map_Type;
      Handler     : not null
                       access
                          procedure (Facility      : in Facility_Index_Type;
                                     Key           : in Key_Index_Type;
                                     Locale        : in Locale_Index_Type;
                                     Source_Locale : in Locale_Index_Type;
                                     Message       : in Wide_String;
                                     Count         : in Natural))
   is

      procedure Callback (Position : in Cursor);
      --  Ada.Containers callback used to reformat arguments to pass off to
      --  the supplied handler.

      --------------
      -- Callback --
      --------------

      procedure Callback (Position : in Cursor) is
         M : constant Message_Definition := Element (Position);
         T : constant Message_Triple := Key (Position);
      begin
         Handler (T.Facility_Index, T.Key_Index, T.Locale_Index,
                  M.Locale_Index, Message_Map.Text (Element (Position)),
                  M.Count);
      end Callback;

   begin
      Message_Map.Messages.Iterate (Callback'Access);
   end Iterate;

   ------------
   -- Length --
   ------------

   function Length (Message_Map : in Message_Map_Type) return Natural is
   begin
      return Natural (Message_Map.Messages.Length);
   end Length;

   -------------------------
   -- Message_Triple_Hash --
   -------------------------

   function Message_Triple_Hash (Value : in Message_Triple) return Hash_Type is
      type M is mod 2**31;
      Result : M := 0;
   begin
      Result := Result xor M'Mod (Value.Facility_Index);
      Result := Result * 10019;
      Result := Result xor M'Mod (Value.Key_Index);
      Result := Result * 10019;
      Result := Result xor M'Mod (Value.Locale_Index);
      Result := Result * 10019;
      return Hash_Type (Result);
   end Message_Triple_Hash;

   ---------------
   -- Pool_Size --
   ---------------

   function Pool_Size (Message_Map : in Message_Map_Type) return Natural is
   begin
      return Length (Message_Map.Pool);
   end Pool_Size;

   ----------
   -- Text --
   ----------

   function Text (Message_Map : in Message_Map_Type;
                  Message     : in Message_Definition) return Wide_String is
   begin
      if Message.Pool /= null then
         return Message.Pool (Message.First .. Message.Last);
      else
         return Slice (Message_Map.Pool, Message.First, Message.Last);
      end if;
   end Text;

end ZanyBlue.Text.Message_Maps;
