with UXStrings.Text_IO;

package Gnoga.Loggings is

   type Root_Logging_Type is tagged limited private;
   type Root_Logging_Access is access all Root_Logging_Type;
   type Root_Logging_Class is access all Root_Logging_Type'Class;
   --  Default root logging type

   procedure Log
     (Object  :    Root_Logging_Type;
      Message : in String) is null;
   --  Default message log (it does nothing)

   procedure Log
     (Object     :    Root_Logging_Type;
      Occurrence : in Ada.Exceptions.Exception_Occurrence) is null;
   --  Default exception occurence log (it does nothing)

   procedure Log
     (Object     :    Root_Logging_Type;
      Message    : in String;
      Occurrence : in Ada.Exceptions.Exception_Occurrence) is null;
   --  Default message with exception occurence log (it does nothing)

   function Create_Root_Logging return Root_Logging_Class;
   --  Create root logging handler

   function Logging return Root_Logging_Class;
   procedure Logging (To : Root_Logging_Class);
   --  Get and set current logging handler

   type Console_Logging_Type is new Root_Logging_Type with private;
   type console_Logging_Access is access all Console_Logging_Type;
   type Console_Logging_Class is access all Console_Logging_Type'Class;
   --  Predefined console logging type

   overriding procedure Log
     (Object  :    Console_Logging_Type;
      Message : in String);
   --  Output message to log

   overriding procedure Log
     (Object     :    Console_Logging_Type;
      Occurrence : in Ada.Exceptions.Exception_Occurrence);
   --  Output exception occurence to log

   overriding procedure Log
     (Object     :    Console_Logging_Type;
      Message    : in String;
      Occurrence : in Ada.Exceptions.Exception_Occurrence);
   --  Output message with exception occurence to log

   function Create_Console_Logging return Root_Logging_Class;
   --  Create console logging handler

   type File_Logging_Type is limited new Root_Logging_Type with private;
   type File_Logging_Access is access all File_Logging_Type;
   type File_Logging_Class is access all File_Logging_Type'Class;
   --  Predefined file logging type

   overriding procedure Log
     (Object  :    File_Logging_Type;
      Message : in String);
   --  Output message to log

   overriding procedure Log
     (Object     :    File_Logging_Type;
      Occurrence : in Ada.Exceptions.Exception_Occurrence);
   --  Output exception occurence to log

   overriding procedure Log
     (Object     :    File_Logging_Type;
      Message    : in String;
      Occurrence : in Ada.Exceptions.Exception_Occurrence);
   --  Output message with exception occurence to log

   procedure Flush (Object : File_Logging_Type);
   --  Manual flush log file

   function Create_File_Logging
     (File_Name  : in String;
      Flush_Auto : in Boolean := False)
      return Root_Logging_Class;
   --  Create file logging handler

private

   type Root_Logging_Type is tagged limited null record;
   type Console_Logging_Type is new Root_Logging_Type with null record;
   type File_Logging_Type is limited new Root_Logging_Type with record
      Automatic_Flush : Boolean := False;
      Log_File        : UXStrings.Text_IO.File_Type;
   end record;

end Gnoga.Loggings;
