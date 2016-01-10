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

with Ada.Text_IO;
with Ada.Wide_Text_IO;

with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Filter;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Printer;
with ZanyBlue.Text.Arguments;
with ZanyBlue.Text.Properties_Parser;

package ZanyBlue.Text.Catalogs is

   use ZanyBlue.Text.Pseudo;
   use ZanyBlue.Text.Filter;
   use ZanyBlue.Text.Printer;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Properties_Parser;

   Default_Extension : constant Wide_String := "properties";
   --  Default file extension to use for properties file when loading a
   --  set of localized files for a facility.

   type Catalog_Type is private;
   --  Catalog of messages for an application.  A catalog can contain multiple
   --  Facilities with each Facility containing a set of localized messages
   --  indexed by string keys, e.g.,
   --
   --  Catalog:
   --    Facility => "ojdbc"
   --      Locale => "" (base, English in this case, locale)
   --        ORA-17001 => Internal Error
   --        ...
   --      Locale => "zh_CN" (Simplified Chinese)
   --        ORA-17001 => 内部错误
   --        ...
   --    Facility => "sunorb"
   --      Locale => "" (base, again English in this case, locale)
   --        servertool.serverup => server is already up.
   --        ...
   --      Locale => "fr" (French)
   --        servertool.serverup => le serveur fonctionne déjà.
   --        ...
   --
   --  The Catalog type allows adding localized messges for a Facility and the
   --  lookup of the message text for a (Facility, Key, Locale) triple.  The
   --  message return follows the standard search of locales until a value
   --  associated with a key is found.  E.g., searching ("ojdbc", "ORA-17001",
   --  "fr_CA") will try the locales "fr_CA", "fr" and "" (the base locale)
   --  the message.  The first locale with a message definition is returned.
   --

   subtype Facility_Index_Type is Positive;
   --  Internally facility names are mapped to a sequence index number.  This
   --  is the type used for facility indexes.

   subtype Key_Index_Type is Positive;
   --  Internally key names are mapped to a sequence index number.  This
   --  is the type used for key indexes.

   subtype Locale_Index_Type is Positive;
   --  Internally locale names are mapped to a sequence index number.  This
   --  is the type used for locale indexes.

   type Catalog_Handler_Type is new Parser_Handler_Type with private;
   --  Properties parser handler to add messages to a catalog.

   procedure Add_Key_Value (Handler       : in out Catalog_Handler_Type;
                            Facility      : in Wide_String;
                            Key           : in Wide_String;
                            Value         : in Wide_String;
                            Locale        : in Locale_Type;
                            Source_Locale : in Locale_Type;
                            File_Name     : in Wide_String;
                            Line          : in Natural);
   --  Callback to add a key/value pair when parsing a .properties file.

   procedure Duplicate_Key (Handler       : in out Catalog_Handler_Type;
                            Facility      : in Wide_String;
                            Key           : in Wide_String;
                            Locale        : in Locale_Type;
                            File_Name     : in Wide_String;
                            Current_Line  : in Natural;
                            Previous_Line : in Natural);
   --  Callback to generate an error on a duplicate key in a .properties files.

   procedure Invalid_Character (Handler         : in out Catalog_Handler_Type;
                                Facility        : in Wide_String;
                                File_Name       : in Wide_String;
                                Current_Line    : in Natural;
                                Ch              : in Character);
   --  Call back used to report an invalid character, non-ISO-646, in the
   --  source properties file.

   procedure Invalid_Definition (Handler         : in out Catalog_Handler_Type;
                                 Facility        : in Wide_String;
                                 Locale          : in Locale_Type;
                                 File_Name       : in Wide_String;
                                 Current_Line    : in Natural;
                                 Additional_Info : in String);
   --  Callback to generate an error on an invalid .properties file definition.

   procedure Set_Catalog (Handler   : in out Catalog_Handler_Type;
                          Catalog   : in Catalog_Type);
   --  Set the catalog associated with a parser handler, callback target.

   function Get_Catalog (Handler : in Catalog_Handler_Type)
      return Catalog_Type;
   --  Get the catalog associated with a parser handler.

   function Create return Catalog_Type;
   --  The Catalog type is a simple access type with the internal details
   --  hidden within the package body.  The Create function simply returns
   --  a new Catalog.

   function Is_Valid (Catalog : in Catalog_Type) return Boolean;
   --  Since the Catalog_Type is private, package users cannot simply test
   --  catalog values against null.  This method checks that a catalog has
   --  created, i.e., is not null.

   function Get_Text (Catalog         : in Catalog_Type;
                      Facility        : in Wide_String;
                      Key             : in Wide_String;
                      Locale          : in Locale_Type;
                      Message_Locale  : access Locale_Type := null)
      return Wide_String;
   --  Return the text associated with a (Facility, Key, Locale) triple.
   --  The message returned uses the locale parenting searching to find
   --  a message in set of locales rooted in the argument locale.
   --  The Message_Locale is not null, then the locale associated with
   --  the message found is used.  This can subsequently be used to format
   --  message arguments.

   procedure Set_Filter (Catalog  : in Catalog_Type;
                         Filter   : in Message_Filter_Access);
   --  Set the filter associated with a catalog, can be null.

   function Is_Filtered (Catalog  : in Catalog_Type;
                         Facility : in Wide_String;
                         Key      : in Wide_String) return Boolean;
   --  Should the given message be filtered out, i.e., ignored.

   procedure Add_Facility (Catalog  : in Catalog_Type;
                           Facility : in Wide_String);
   --  Add a facility name to the set of known facilities

   procedure Add_Facility (Catalog  : in Catalog_Type;
                           Facility : in Wide_String;
                           Index    : out Facility_Index_Type);
   --  Add a facility name to the set of known facilities

   procedure Add_Key (Catalog  : in Catalog_Type;
                      Key      : in Wide_String;
                      Index    : out Key_Index_Type);
   --  Add a key name to the set of known keys

   procedure Add_Locale (Catalog  : in Catalog_Type;
                         Locale   : in Locale_Type);
   --  Add a locale to the set of known locales

   procedure Add_Locale (Catalog  : in Catalog_Type;
                         Locale   : in Locale_Type;
                         Index    : out Locale_Index_Type);
   --  Add a locale to the set of known locales

   procedure Add (Catalog          : in Catalog_Type;
                  Facility         : in Wide_String;
                  Key              : in Wide_String;
                  Message          : in Wide_String;
                  Locale           : in Locale_Type);
   --  Add a message for a (Facility, Key, Locale) triple to a Catalog.  The
   --  message text is copied to an internal catatlog buffer.

   procedure Add (Catalog       : in Catalog_Type;
                  Facility      : in Wide_String;
                  Key           : in Wide_String;
                  Message       : in Wide_String;
                  Locale        : in Locale_Type;
                  Source_Locale : in Locale_Type);
   --  Add a message for a (Facility, Key, Locale) triple to a Catalog.  The
   --  message text is copied to an internal catatlog buffer.

   function Get_Facility_Index (Catalog : in Catalog_Type;
                                Name    : in Wide_String)
      return Facility_Index_Type;
   --  Return the index of a facility name, normally only used by zbmcompile.

   function Get_Key_Index (Catalog : in Catalog_Type;
                           Name    : in Wide_String) return Key_Index_Type;
   --  Return the index of a key name, normally only used by zbmcompile.

   function Get_Locale_Index (Catalog : in Catalog_Type;
                              Name    : in Wide_String)
      return Locale_Index_Type;
   --  Return the index of a key name, normally only used by zbmcompile.

   function Get_Locale_Index (Catalog : in Catalog_Type;
                              Locale  : in Locale_Type)
      return Locale_Index_Type;
   --  Return the index of a key name, normally only used by zbmcompile.

   function Load_File (Catalog       : in Catalog_Type;
                       File_Name     : in Wide_String;
                       Facility      : in Wide_String;
                       Locale        : in Locale_Type;
                       Source_Locale : in Locale_Type := Root_Locale)
      return Natural;
   --  Load the messages defined in the properties file File_Name into the
   --  catalog for the given facility and locale.

   procedure Load_File (File_Name     : in Wide_String;
                        Facility      : in Wide_String;
                        Locale        : in Locale_Type;
                        Handler       : in out Catalog_Handler_Type'Class;
                        Source_Locale : in Locale_Type := Root_Locale);
   --  Load the messages defined in the properties file File_Name into the
   --  catalog for the given facility and locale.

   procedure Load_Facility (
      Catalog            : in Catalog_Type;
      Facility           : in Wide_String;
      Source_Name        : in Wide_String;
      N_Locales          : out Natural;
      N_Messages         : out Natural;
      Directory          : in Wide_String := ".";
      Extension          : in Wide_String := Default_Extension;
      Base_Locale_Only   : in Boolean := False;
      Locale_Prefix      : in Wide_String := "";
      Source_Root_Locale : in Locale_Type := Root_Locale);
   --  Load a set of localized files for a facility.  All files matching
   --  The base file name and extension and with locale name matching the
   --  given prefix are loaded.  E.g, "myapp", "fr" would match "myapp_fr"
   --  and "myapp_fr_FR", etc.

   procedure Load_Facility (
      Facility           : in Wide_String;
      Source_Name        : in Wide_String;
      N_Locales          : out Natural;
      N_Messages         : out Natural;
      Handler            : in out Catalog_Handler_Type'Class;
      Directory          : in Wide_String := ".";
      Extension          : in Wide_String := Default_Extension;
      Base_Locale_Only   : in Boolean := False;
      Locale_Prefix      : in Wide_String := "";
      Source_Root_Locale : in Locale_Type := Root_Locale);
   --  Load a set of localized files for a facility as per the previous
   --  routine but load to the catalog handler object.

   procedure Load_Facility (
      Catalog            : in Catalog_Type;
      Facility           : in Wide_String;
      N_Locales          : out Natural;
      N_Messages         : out Natural;
      Directory          : in Wide_String := ".";
      Extension          : in Wide_String := Default_Extension;
      Source_Root_Locale : in Locale_Type := Root_Locale);
   --  Load a set of localized files for a facility.  All files matching
   --  The base file name and extension with interleaved locale names are
   --  loaded.  The facility name and the base name file are the same for
   --  this overloaded call.
   --  If Verbose is enabled, each file loaded generates a message for the
   --  facility V_Facility with the key V_Key.

   procedure Enable_Pseudo_Translations (Catalog        : in Catalog_Type;
                                         Mapping        : in Pseudo_Map_Vector;
                                         Mark_Messages  : in Boolean := True;
                                         Mark_Arguments : in Boolean := True);
   --  Enable pseudo translations for a catalog.

   function Get_Pseudo_Map (Catalog : in Catalog_Type)
      return Pseudo_Map_Access;
   --  Return the pseudo translation mapping associated with a catalog.

   function Get_Mark_Messages (Catalog : in Catalog_Type) return Boolean;
   --  Return whether or not pseudo translated messages should include the
   --  message start/end markers.

   function Get_Mark_Arguments (Catalog : in Catalog_Type) return Boolean;
   --  Return whether or not pseudo translated message arguments should
   --  include the argument start/end markers.

   procedure Enable_Exceptions (Catalog : in Catalog_Type);
   --  Enable exceptions for missing arguments, invalid formats, etc.

   procedure Disable_Exceptions (Catalog : in Catalog_Type);
   --  Disable exceptions for missing arguments, invalid formats, etc.

   function Exceptions_Enabled (Catalog : in Catalog_Type) return Boolean;
   --  Are exceptions enabled for the catalog

   procedure Enable_Source_Locales (Catalog : in Catalog_Type);
   --  Enable usage of source locales for message argument formatting.

   procedure Disable_Source_Locales (Catalog : in Catalog_Type);
   --  Disable usage of source locales for message argument formatting.

   function Source_Locales_Enabled (Catalog : in Catalog_Type)
      return Boolean;
   --  Are source locales for message argument formatting enabled.

   procedure Print (Catalog     : Catalog_Type;
                    Destination : Ada.Text_IO.File_Type;
                    Facility    : Wide_String;
                    Key         : Wide_String;
                    Locale      : Locale_Type;
                    Arguments   : ZanyBlue.Text.Arguments.Argument_List;
                    Message     : Wide_String;
                    With_NL     : Boolean);
   --  Print a message to a Character based file.   This is implemented
   --  via the Printer class associated with the catalog.

   procedure Print (Catalog     : Catalog_Type;
                    Destination : Ada.Wide_Text_IO.File_Type;
                    Facility    : Wide_String;
                    Key         : Wide_String;
                    Locale      : Locale_Type;
                    Arguments   : ZanyBlue.Text.Arguments.Argument_List;
                    Message     : Wide_String;
                    With_NL     : Boolean);
   --  Print a message to a Wide_Character based file.   This is implemented
   --  via the Printer class associated with the catalog.

   procedure Set_Printer (Catalog : Catalog_Type;
                          Printer : Printer_Access);
   --  Set the printer associated with a catalog.

   --------------------------------------------------------------------------
   --  INTERNAL API'S TO SUPPORT GENERATION OF ADA SOURCE VIA ZBMCOMPILE
   --------------------------------------------------------------------------

   procedure Add (Catalog          : in Catalog_Type;
                  Facility         : in Wide_String;
                  Key              : in Wide_String;
                  Pool             : in Static_Message_Pool_Type;
                  First            : in Positive;
                  Last             : in Natural;
                  Locale           : in Locale_Type);
   --  Add a message for a (Facility, Key, Locale) triple to a Catalog.  See
   --  next routine for details.

   procedure Add (Catalog       : in Catalog_Type;
                  Facility      : in Wide_String;
                  Key           : in Wide_String;
                  Pool          : in Static_Message_Pool_Type;
                  First         : in Positive;
                  Last          : in Natural;
                  Locale        : in Locale_Type;
                  Source_Locale : in Locale_Type);
   --  Add a message for a (Facility, Key, Locale) triple to a Catalog.  The
   --  message text a substring (First .. Last) within a static buffer passed
   --  by access (Pool) which is stored in the catalog.  All messages added
   --  via this Add procedure all need to reference the same Pool.  The
   --  exception Static_Pool_Redefinition if different Pool's are used.  This
   --  method is normally used for initializations generated by the zbmcompile
   --  command.

   procedure Use_Single_Pool (Catalog : in Catalog_Type);
   --  Enable indexes to allow access to the various name by index, e.g.,
   --  Facility (I).  The indexes are only maintained if require and must
   --  be explicitly enabled on a per catalog basis via this procedure.

   function Number_Of_Facilities (Catalog : in Catalog_Type) return Natural;
   --  Return the number of facilities defined in a catalog.

   function Get_Facility (Catalog : in Catalog_Type;
                          Index   : in Facility_Index_Type) return Wide_String;
   --  Return the name of facility Index, normally used in an iteration from
   --  1 to Number_Of_Facilities.

   function Number_Of_Keys (Catalog : in Catalog_Type) return Natural;
   --  Return the total number of keys used in a catalog across all
   --  facilities.  This is not specific to a particular set of message
   --  for a locale or facility.  Any key added to any facility within
   --  the catalog is registered as a key in the list enumerated by this
   --  function.

   function Get_Key (Catalog : in Catalog_Type;
                     Index   : in Key_Index_Type) return Wide_String;
   --  Return the name of key Index, again normally used for an iteration
   --  over 1 .. Number_Of_Keys.

   function Number_Of_Locales (Catalog : in Catalog_Type) return Natural;
   --  Return the total number of locales used in a catalog across all
   --  facilities.  Again, this is not specific to a particular facility.
   --  Any locale added to any facility within the catalog is registered
   --  as a locale in the list enumerated by this function.

   function Get_Locale (Catalog : in Catalog_Type;
                        Index   : in Locale_Index_Type) return Locale_Type;
   --  Return the locale numbered Index, again normally used for an iteration
   --  over 1 .. Number_Of_Locales.

   function Get_Locale_Name (Catalog : in Catalog_Type;
                             Index   : in Locale_Index_Type)
      return Wide_String;
   --  Return name for the locale numbered Index, again normally used for
   --  an iteration over 1 .. Number_Of_Locales.

   function Number_Of_Messages (Catalog : in Catalog_Type) return Natural;
   --  Return the total number of messages defined in a catalog across
   --  all facilities, keys and locales.

   function Get_Pool (Catalog : in Catalog_Type) return Wide_String;
   --  Return the pool string data associated with a catalog.  The data
   --  returned includes the contents of both the static and dynamic pools.
   --  The data returned can be used as the static pool data for a new
   --  catalog with messages defined via the Message_Indexes procedure
   --  (this functionality is only used in the zbmcompile application).

   function Pool_Size (Catalog : in Catalog_Type) return Natural;
   --  Return the length of the dynamic pool associated with a catalog.

   function Logical_Pool_Size (Catalog : in Catalog_Type) return Natural;
   --  Return the logical size of the pool, i.e., the total number of
   --  characters stored over all message strings.  This is can be less
   --  than the Pool_Size due to duplicate strings, e.g., storing the
   --  string "Sunday" followed by the string "Sun".  Since "Sun" is
   --  a sub string of the existing pool, it is not stored again.

   procedure Query_Message (Catalog        : in Catalog_Type;
                            Facility_Index : in Facility_Index_Type;
                            Key_Index      : in Key_Index_Type;
                            Locale_Index   : in Locale_Index_Type;
                            First          : out Positive;
                            Last           : out Natural);
   --  Get the First and List indexes for a message with the pool returned
   --  by the Get_Pool function.  The text of associated message is simply
   --
   --     Pool := Get_Pool (Catalog);
   --     Query_Message_Indexes (Catalog, 1, 10, 3,
   --                            First, Last);
   --     Msg := Pool (First .. Last);
   --
   --  All the messages within a catalog can be queried by iterating over
   --  the facilities, keys and locales using the Number_Of_* functions.
   --  Note, this procedure will raise exceptions for non-existant data,
   --  e.g., "myfac1" might not contain data for the locale "fr_FR" while
   --  facility "myfac2" might.

   procedure Iterate (
      Catalog : in Catalog_Type;
      Handler : not null access
                   procedure (Facility      : in Facility_Index_Type;
                              Key           : in Key_Index_Type;
                              Locale        : in Locale_Index_Type;
                              Source_Locale : in Locale_Index_Type;
                              First         : in Positive;
                              Last          : in Natural;
                              Count         : in Natural));
   --  Iterate over the messages defined in a catalog calling the argument
   --  procedure.

   procedure Iterate (
      Catalog : in Catalog_Type;
      Handler : not null access
                   procedure (Facility      : in Facility_Index_Type;
                              Key           : in Key_Index_Type;
                              Locale        : in Locale_Index_Type;
                              Source_Locale : in Locale_Index_Type;
                              Message       : in Wide_String;
                              Count         : in Natural));
   --  Iterate over the messages defined in a catalog calling the argument
   --  procedure.

   function Get_Text (Catalog        : in Catalog_Type;
                      Facility_Index : in Facility_Index_Type;
                      Key_Index      : in Key_Index_Type;
                      Locale_Index   : in Locale_Index_Type)
      return Wide_String;
   --  Return the text of a message given the three index values.  This
   --  will raise one of the No_Such_*_Error exceptions if the triple
   --  does not map to a valid message.

   procedure Reserve (Catalog    : in Catalog_Type;
                      Pool_Size  : in Natural := 0;
                      Messages   : in Natural := 0);
   --  Reserve additional space in a catalog for new messages.  If the
   --  messages are dynamic, the internal string pool used to store text
   --  strings can be increased by giving a Pool_Size.  The Messages
   --  argument is the number of additional messages expected.

   procedure Dump (Catalog   : in Catalog_Type;
                   File_Name : in Wide_String := "");
   --  Debugging utiltiy to dump the contents of a catalog to a named file.
   --  If the file name is the empty string, dump the contents to the standard
   --  output file.

   type ZBMCompile_Definition is
      record
         Facility_Index      : Facility_Index_Type;
         Key_Index           : Key_Index_Type;
         Locale_Index        : Locale_Index_Type;
         First               : Positive;
         Last                : Natural;
         Source_Locale_Index : Locale_Index_Type;
      end record;
   type ZBMCompile_List is array (Positive range <>) of ZBMCompile_Definition;
   --  Record and list data types to represent messages defined by the
   --  zbmcompile command.

   procedure Initialize (Catalog         : in Catalog_Type;
                         Messages        : in ZBMCompile_List;
                         Pool            : in Static_Message_Pool_Type;
                         Facilities      : in Constant_String_List;
                         Keys            : in Constant_String_List;
                         Locales         : in Constant_String_List;
                         Package_Name    : in Wide_String := "";
                         Pool_Length     : in Natural := 0;
                         Expected_Length : in Natural := 0);
   --  This supporting procedure Initialize uses this Message_List and
   --  the list of facilities, keys and locales to initialize the messages
   --  associated with a set of facilities.

private

   type Catalog_Value;

   type Catalog_Access is access Catalog_Value;

   type Catalog_Type is
   record
      C : Catalog_Access;
   end record;

   type Catalog_Handler_Type is new Parser_Handler_Type with
      record
         Catalog  : Catalog_Type;
      end record;

end ZanyBlue.Text.Catalogs;
