-- Chattanooga: a simple chat program
-- Copyright (C) 2015 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Database: the data stored by the program; currently all in memory
--
-- V1.0B  2015 Jan 05     1st beta release
--
with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded.Hash;

package Chattanooga.DB is
   package Contact_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Unbounded_String, Hash => Ada.Strings.Unbounded.Hash, Equivalent_Elements => "=");

   type User_Data is record
      Contact  : Contact_Sets.Set;
      App_Data : App_Ptr;
   end record;

   procedure Add
     (User     : in Unbounded_String;
      App_Data : in App_Ptr);
   -- Adds User to DB with App_Data for its UI updates. User's Contact set will be populated with any existing users who have
   -- User in their Contact sets
   -- If Exists (User), raises Constraint_Error

   function Exists
     (User : Unbounded_String)
      return Boolean;
   -- Returns True if User is in the DB; False otherwise

   procedure Remove (User : in Unbounded_String);
   -- Removes User from the DB, if it is in the DB
   -- No effect if User is not in the DB

   function Send
     (From    : Unbounded_String;
      Message : String)
      return Natural;
   -- Sends Message to From's contacts
   -- Returns the number of contacts Message was sent to

   procedure Add_Friend
     (User   : in Unbounded_String;
      Friend : in Unbounded_String);
   -- Adds Friend to User's contact set, and User to Friend's

   procedure Remove_Friend
     (User   : in Unbounded_String;
      Friend : in Unbounded_String);
   -- Removes Friend from User's contact set, and User from Friend's
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
