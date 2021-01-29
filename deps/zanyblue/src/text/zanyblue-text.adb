--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2018, Michael Rohan <mrohan@zanyblue.com>
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

with Ada.Text_IO.Text_Streams;
with ZanyBlue.OS;
with ZanyBlue.Directories;

package body ZanyBlue.Text is

   use ZanyBlue.OS;

   Update_Extension : constant String := ".zbtmp";

   ----------------------
   -- Close_And_Update --
   ----------------------

   procedure Close_And_Update
     (File    : in out UXStrings.Text_IO.File_Type;
      Updated :    out Boolean)
   is
      use UXStrings.Text_IO;
      use ZanyBlue.Directories;
      Len       : constant Natural := Update_Extension.Length;
      File_Name : constant String  := Name (File);
      Real_Name : constant String  := Head (File_Name, File_Name.Length - Len);
   begin
      Close (File);
      Updated := Files_Differ (File_Name, Real_Name);
      if Updated then
         if Exists (Real_Name) then
            Delete_File (Real_Name);
         end if;
         Rename (File_Name, Real_Name);
      else
         Delete_File (File_Name);
      end if;
   end Close_And_Update;

   ------------------
   -- Files_Differ --
   ------------------

   function Files_Differ
     (Left_File_Name  : String;
      Right_File_Name : String)
      return Boolean
   is
      use Ada.Text_IO;
      use Ada.Text_IO.Text_Streams;

      Result       : Boolean := False;
      Done         : Boolean := False;
      Left_Ch      : Character;
      Right_Ch     : Character;
      Left_Stream  : Stream_Access;
      Right_Stream : Stream_Access;
      Left         : File_Type;
      Right        : File_Type;

   begin
      Open (Left, In_File, To_UTF_8 (Left_File_Name));
      Open (Right, In_File, To_UTF_8 (Right_File_Name));
      Left_Stream  := Stream (Left);
      Right_Stream := Stream (Right);
      while not Done loop
         Done := End_Of_File (Left) or End_Of_File (Right);
         if not Done then
            Character'Read (Left_Stream, Left_Ch);
            Character'Read (Right_Stream, Right_Ch);
            Result := Left_Ch /= Right_Ch;
            Done   := Result;
         end if;
      end loop;
      Result := Result or else (End_Of_File (Left) xor End_Of_File (Right));
      Close (Left);
      Close (Right);
      return Result;
   exception
      when Name_Error =>
         if Is_Open (Left) then
            Close (Left);
         end if;
         if Is_Open (Right) then
            Close (Right);
         end if;
         return True;
   end Files_Differ;

   ----------------------------
   -- Wide_Create_For_Update --
   ----------------------------

   procedure Wide_Create_For_Update
     (File : in out UXStrings.Text_IO.File_Type;
      Name :        String)
   is
      use UXStrings.Text_IO;
   begin
      Create (File, Name & Update_Extension);
   end Wide_Create_For_Update;

   ---------------
   -- Wide_Hash --
   ---------------

   function Wide_Hash
     (Key : String)
      return Ada.Containers.Hash_Type
   is

      use Ada.Containers;

      function Shift_Left
        (Value  : Hash_Type;
         Amount : Natural)
         return Hash_Type;
      pragma Import (Intrinsic, Shift_Left);

      H : Hash_Type;

   begin
      H := 0;
      for J in Key loop
         H :=
           Unicode_Character'Pos (Key (J)) + Shift_Left (H, 6) +
           Shift_Left (H, 16) - H;
      end loop;
      return H;
   end Wide_Hash;

end ZanyBlue.Text;
