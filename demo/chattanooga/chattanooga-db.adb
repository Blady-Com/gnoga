-- Chattanooga: a simple chat program
-- Copyright (C) 2015 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Database: the data stored by the program; currently all in memory
--
-- V1.0B  2015 Jan 05     1st beta release
--
with Ada.Containers.Hashed_Maps;

with Chattanooga.UI;

package body Chattanooga.DB is
   package User_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String, Element_Type => User_Data, Hash => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   protected Control is
      procedure Add
        (User     : in Unbounded_String;
         App_Data : in App_Ptr);
      function Exists
        (User : Unbounded_String)
         return Boolean;
      procedure Remove (User : in Unbounded_String);
      function Send
        (From    : Unbounded_String;
         Message : String)
         return Natural;
      procedure Add_Friend
        (User   : in Unbounded_String;
         Friend : in Unbounded_String);
      procedure Remove_Friend
        (User   : in Unbounded_String;
         Friend : in Unbounded_String);
   private -- Control
      Map : User_Maps.Map;
   end Control;

   procedure Add
     (User     : in Unbounded_String;
      App_Data : in App_Ptr)
   is
      -- Empty declarative part
   begin -- Add
      Control.Add (User => User, App_Data => App_Data);
   end Add;

   function Exists
     (User : Unbounded_String)
      return Boolean
   is
      -- Empty declarative part
   begin -- Exists
      return Control.Exists (User);
   end Exists;

   procedure Remove (User : in Unbounded_String) is
      -- Empty declarative part
   begin -- Remove
      Control.Remove (User => User);
   end Remove;

   function Send
     (From    : Unbounded_String;
      Message : String)
      return Natural
   is
      -- Empty declarative part
   begin -- Send
      return Control.Send (From, Message);
   end Send;

   procedure Add_Friend
     (User   : in Unbounded_String;
      Friend : in Unbounded_String)
   is
      -- Empty declarative part
   begin -- Add_Friend
      Control.Add_Friend (User => User, Friend => Friend);
   end Add_Friend;

   procedure Remove_Friend
     (User   : in Unbounded_String;
      Friend : in Unbounded_String)
   is
      -- Empty declarative part
   begin -- Remove_Friend
      Control.Remove_Friend (User => User, Friend => Friend);
   end Remove_Friend;

   protected body Control is
      procedure Add
        (User     : in Unbounded_String;
         App_Data : in App_Ptr)
      is
         procedure Check_One (Position : in User_Maps.Cursor);
         -- if the Contact set for the user at Position contains User, adds the user at Position to Data.Contact
         -- Changes the display of User for the user at Position to indicate that User is connected

         Data : User_Data;

         procedure Check_One (Position : in User_Maps.Cursor) is
            Key   : constant Unbounded_String := User_Maps.Key (Position);
            Value : constant User_Data        := User_Maps.Element (Position);
         begin -- Check_One
            if Value.Contact.Contains (User) then
               Data.Contact.Include (New_Item => Key);
               UI.New_Friend (Friend => Key, App_Data => App_Data, Connected => True);
               UI.Change_Status (Friend => User, App_Data => Value.App_Data, Connected => True);
            end if;
         end Check_One;
      begin -- Add
         if Map.Contains (User) then
            raise Constraint_Error;
         end if;

         Data.App_Data := App_Data;
         Map.Iterate (Process => Check_One'Access);
         Map.Insert (Key => User, New_Item => Data);
      end Add;

      function Exists
        (User : Unbounded_String)
         return Boolean
      is
         -- Empty declarative part
      begin -- Exists
         return Map.Contains (User);
      end Exists;

      procedure Remove (User : in Unbounded_String) is
         procedure Check_One (Position : in User_Maps.Cursor);
         -- if the Contact set for the user at Position contains User, changes the user at Position's friend list to show User as
         -- not connected

         procedure Check_One (Position : in User_Maps.Cursor) is
            Key   : constant Unbounded_String := User_Maps.Key (Position);
            Value : constant User_Data        := User_Maps.Element (Position);
         begin -- Check_One
            if Value.Contact.Contains (User) then
               UI.Change_Status (Friend => User, App_Data => Value.App_Data, Connected => False);
            end if;
         end Check_One;
      begin -- Remove
         Map.Exclude (Key => User);
         Map.Iterate (Process => Check_One'Access);
      end Remove;

      function Send
        (From    : Unbounded_String;
         Message : String)
         return Natural
      is
         Data : constant User_Data := Map.Element (From);

         procedure Send_One (Position : in Contact_Sets.Cursor);
         -- Sends Message to the user at Position, using the user's App_Data

         Count : Natural := 0;

         procedure Send_One (Position : in Contact_Sets.Cursor) is
            Key : constant Unbounded_String := Contact_Sets.Element (Position);

            Value : User_Data;
         begin -- Send_One
            if Map.Contains (Key) then
               Count := Count + 1;
               Value := Map.Element (Key);
               UI.Show (From => From, Message => Message, App_Data => Value.App_Data);
            end if;
         end Send_One;
      begin -- Send
         Data.Contact.Iterate (Process => Send_One'Access);

         return Count;
      end Send;

      procedure Add_Friend
        (User   : in Unbounded_String;
         Friend : in Unbounded_String)
      is
         Data : User_Data;
      begin -- Add_Friend
         if not Map.Contains (User) then
            return;
         end if;

         Data := Map.Element (User);

         if not Data.Contact.Contains (Friend) then
            Data.Contact.Include (New_Item => Friend);
            Map.Replace (Key => User, New_Item => Data);
            UI.New_Friend (Friend => Friend, App_Data => Data.App_Data, Connected => Map.Contains (Friend));
         end if;

         if Map.Contains (Friend) then
            Data := Map.Element (Friend);

            if not Data.Contact.Contains (User) then
               Data.Contact.Include (New_Item => User);
               Map.Replace (Key => Friend, New_Item => Data);
               UI.New_Friend (Friend => User, App_Data => Data.App_Data, Connected => True);
            end if;
         end if;
      end Add_Friend;

      procedure Remove_Friend
        (User   : in Unbounded_String;
         Friend : in Unbounded_String)
      is
         Data : User_Data;
      begin -- Remove_Friend
         if not Map.Contains (User) then
            return;
         end if;

         Data := Map.Element (User);

         if Data.Contact.Contains (Friend) then
            Data.Contact.Exclude (Item => Friend);
            Map.Replace (Key => User, New_Item => Data);
            UI.Remove_Friend (Friend => Friend, App_Data => Data.App_Data);
         end if;

         if Map.Contains (Friend) then
            Data := Map.Element (Friend);

            if Data.Contact.Contains (User) then
               Data.Contact.Exclude (Item => User);
               Map.Replace (Key => Friend, New_Item => Data);
               UI.Remove_Friend (Friend => User, App_Data => Data.App_Data);
            end if;
         end if;
      end Remove_Friend;
   end Control;
end Chattanooga.DB;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; version 2.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
