with Gnoga.Loggings;

package User_Defined_Logging is

   type User_Defined_Logging_Type is new Gnoga.Loggings.Console_Logging_Type with private;

   overriding procedure Log
     (Object  :    User_Defined_Logging_Type;
      Message : in Gnoga.String);

   function Create_User_Defined_Logging return Gnoga.Loggings.Root_Logging_Class;

private

   type User_Defined_Logging_Type is new Gnoga.Loggings.Console_Logging_Type with null record;

end User_Defined_Logging;
