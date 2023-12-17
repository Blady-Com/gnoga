with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Unchecked_Deallocation;

package body Gnoga.Loggings is

   Current_Log : Root_Logging_Class := new Root_Logging_Type;

   -------------------------
   -- Create_Root_Logging --
   -------------------------

   function Create_Root_Logging return Root_Logging_Class is
   begin
      return new Root_Logging_Type;
   end Create_Root_Logging;

   -------------
   -- Logging --
   -------------

   function Logging return Root_Logging_Class is
   begin
      return Current_Log;
   end Logging;

   procedure Logging (To : Root_Logging_Class) is
      procedure Free is new Ada.Unchecked_Deallocation (Root_Logging_Type'Class, Root_Logging_Class);
   begin
      Free (Current_Log);
      Current_Log := To;
   end Logging;

   ---------
   -- Log --
   ---------

   overriding procedure Log
     (Object  :    Console_Logging_Type;
      Message : in String)
   is
      T            : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Date_Message : constant String            :=
        From_ASCII
          (Ada.Calendar.Formatting.Image
             (Date => T, Include_Time_Fraction => True, Time_Zone => Ada.Calendar.Time_Zones.UTC_Time_Offset (T)) &
           " : ") &
        Message;
   begin
      Write_To_Console (Date_Message);
   end Log;

   ---------
   -- Log --
   ---------

   overriding procedure Log
     (Object     :    Console_Logging_Type;
      Occurrence : in Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Object.Log (From_UTF_8 (Ada.Exceptions.Exception_Information (Occurrence)));
   end Log;

   ---------
   -- Log --
   ---------

   overriding procedure Log
     (Object     :    Console_Logging_Type;
      Message    : in String;
      Occurrence : in Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Object.Log (Message & From_UTF_8 (Ada.Exceptions.Exception_Information (Occurrence)));
   end Log;

   ----------------------------
   -- Create_Console_Logging --
   ----------------------------

   function Create_Console_Logging return Root_Logging_Class is
   begin
      return new Console_Logging_Type;
   end Create_Console_Logging;

   ---------
   -- Log --
   ---------

   overriding procedure Log
     (Object  :    File_Logging_Type;
      Message : in String)
   is
      T            : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Date_Message : constant String            :=
        From_ASCII
          (Ada.Calendar.Formatting.Image
             (Date => T, Include_Time_Fraction => True, Time_Zone => Ada.Calendar.Time_Zones.UTC_Time_Offset (T)) &
           " : ") &
        Message;
   begin
      UXStrings.Text_IO.Put_Line (Object.Log_File, Date_Message);
      if Object.Automatic_Flush then
         Flush_Log;
      end if;
   end Log;

   ---------
   -- Log --
   ---------

   overriding procedure Log
     (Object     :    File_Logging_Type;
      Occurrence : in Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Object.Log (From_UTF_8 (Ada.Exceptions.Exception_Information (Occurrence)));
   end Log;

   ---------
   -- Log --
   ---------

   overriding procedure Log
     (Object     :    File_Logging_Type;
      Message    : in String;
      Occurrence : in Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Object.Log (Message & From_UTF_8 (Ada.Exceptions.Exception_Information (Occurrence)));
   end Log;

   ---------------
   -- Flush_Log --
   ---------------

   procedure Flush (Object : File_Logging_Type) is
   begin
      UXStrings.Text_IO.Flush (Object.Log_File);
   end Flush;

   -------------------------
   -- Create_File_Logging --
   -------------------------

   function Create_File_Logging
     (File_Name  : in String;
      Flush_Auto : in Boolean := False)
      return Root_Logging_Class
   is
   begin
      return Object : constant Root_Logging_Class := new File_Logging_Type do
         UXStrings.Text_IO.Create
           (File   => File_Logging_Access (Object).Log_File, Mode => UXStrings.Text_IO.Append_File, Name => File_Name,
            Scheme => UTF_8, Ending => UXStrings.Text_IO.LF_Ending);
         File_Logging_Access (Object).Automatic_Flush := Flush_Auto;
      end return;
   end Create_File_Logging;

end Gnoga.Loggings;
