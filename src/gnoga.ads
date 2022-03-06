------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                                G N O G A                                 --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Exceptions;
with Ada.Task_Identification;
with UXStrings;
with UXStrings.Conversions;

package Gnoga is
   use UXStrings;

   subtype String is UXString;

   Version        : constant String := "2.2-alpha";
   Version_High   : constant        := 2;
   Version_Low    : constant        := 2;
   Version_Status : constant String := "alpha";

   HTTP_Server_Name : constant String := "gnoga/" & Version;

   function Escape_Quotes
     (S : String)
      return String;
   --  Escape quotes for JavaScript.

   function Unescape_Quotes
     (S : String)
      return String;
   --  Unescape a string quoted for JavaScript

   function URL_Encode
     (S        : String;
      Encoding : String := "")
      return String;
   function URL_Decode
     (S        : String;
      Encoding : String := "")
      return String;
   --  Encode and decode form URL Supported encodings are ISO-8859-1 (default)
   --  and UTF-8 (typically from Input_Encoding function)

   function Left_Trim
     (S : String)
      return String;
   function Right_Trim
     (S : String)
      return String;
   --  Remove extra spaces and tabs

   function Left_Trim_Slashes
     (S : String)
      return String;
   function Right_Trim_Slashes
     (S : String)
      return String;
   --  Remove extra spaces, tabs and '/'s

   procedure String_Replace
     (Source      : in out String;
      Pattern     : in     String;
      Replacement : in     String);
   --  Replace all instances of Pattern with Replacement in Source

   procedure Write_To_Console (Message : in String);
   --  Output message to console

   procedure Log_To_File
     (File_Name  : in String;
      Flush_Auto : in Boolean := False);
   --  Redirect logging to File_Name instead of console with flushing if
   --  specified

   procedure Log (Message : in String);
   --  Output message to log

   procedure Log (Occurrence : in Ada.Exceptions.Exception_Occurrence);
   --  Output exception occurence to log

   procedure Log
     (Message    : in String;
      Occurrence : in Ada.Exceptions.Exception_Occurrence);
   --  Output message with exception occurence to log

   procedure Flush_Log;
   --  Manual flush log file

   procedure Activate_Exception_Handler (Id : Ada.Task_Identification.Task_Id);
   --  Activate exception log for the designated task

   --  Convenient string conversions of usual types
   function Value is new UXStrings.Conversions.Integer_Value (Integer);
   function Value is new UXStrings.Conversions.Floating_Point_Value (Float);
   function Image is new UXStrings.Conversions.Scalar_Image (Boolean);
   function Image is new UXStrings.Conversions.Integer_Image (Integer);
   function Image is new UXStrings.Conversions.Floating_Point_Image (Float);

end Gnoga;
