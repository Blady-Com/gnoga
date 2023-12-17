package body User_Defined_Logging is

   ---------
   -- Log --
   ---------

   overriding procedure Log
     (Object  :    User_Defined_Logging_Type;
      Message : in Gnoga.String)
   is
      use all type Gnoga.String;
   begin
      Gnoga.Write_To_Console ("User defined log: " & Message);
   end Log;

   ---------------------------------
   -- Create_User_Defined_Logging --
   ---------------------------------

   function Create_User_Defined_Logging return Gnoga.Loggings.Root_Logging_Class is
   begin
      return new User_Defined_Logging_Type;
   end Create_User_Defined_Logging;

end User_Defined_Logging;
