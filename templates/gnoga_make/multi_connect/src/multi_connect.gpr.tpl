with "gnoga";

project @@data.App_Name@@ is
   for Languages use ("Ada");
   for Source_Dirs use (".");
   for Object_Dir use "../obj";
   for Exec_Dir use "../bin";
   for Main use ("@@data.App_Name_Lower@@-main.adb");

   package Compiler is
      for Default_Switches ("Ada") use ("-g", "-gnata", "-gnatq", "-gnatQ");
   end Compiler;

   package Binder is
     for Default_Switches ("Ada") use ("-E");
   end Binder;

   package Builder is
      for Executable ("@@data.App_Name_Lower@@-main.adb") use "@@data.App_Name_Lower@@";
   end Builder;
end @@data.App_Name@@;
