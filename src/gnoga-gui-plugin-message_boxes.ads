--  Inspired by GWindows and test/jdemo
--  (c) 2015 - Gautier de Montmollin
--  GPLv3 with Runtime Exception

with Gnoga.Gui.Base;

package Gnoga.Gui.Plugin.Message_Boxes is

   type Message_Box_Result is
     (OK, Cancel, Yes, No, Abort_Message, Retry, Ignore, None);

   type Message_Box_Type is (OK_Box,
                             Yes_No_Box,
                             Yes_No_Def_Box,
                             Yes_No_Cancel_Box,
                             Yes_No_Def_Cancel_Box,
                             Yes_No_Cancel_Def_Box);

   function Message_Box
     (Parent          : in out Gnoga.Gui.Base.Base_Type'Class;
      Title, Text     : in     String;
      Style           : in     Message_Box_Type                     := OK_Box)
   return Message_Box_Result;

   --  Same, but ignore message result.

   procedure Message_Box
     (Parent      : in out Gnoga.Gui.Base.Base_Type'Class;
      Title, Text : in     String;
      Style       : in     Message_Box_Type                     := OK_Box);
end Gnoga.Gui.Plugin.Message_Boxes;
