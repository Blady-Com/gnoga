with "gnoga";

project @@data.App_Name@@ is
   for Languages use ("Ada");
   for Source_Dirs use (".");
   for Object_Dir use "../obj";
   for Exec_Dir use "../bin";
   for Main use ("@@data.App_Name_Lower@@-main.adb");

   package Compiler is
      for Default_Switches ("Ada") use ("-gnatwa", "-g", "-gnata", "-gnatq", "-gnatQ", "-gnato", "-gnatf", "-gnatW8");
   end Compiler;

   package Binder is
      for Default_Switches ("Ada") use ("-E");
   end Binder;

   package Builder is
      for Executable ("@@data.App_Name_Lower@@-main.adb") use "@@data.App_Name_Lower@@";
   end Builder;

   package Pretty_Printer is
      for Default_Switches ("Ada") use ("-M120", "-W8", "--par_threshold=1");
   end Pretty_Printer;
end @@data.App_Name@@;
