------------------------------------------------------------------------------
--                                                                          --
--                             Snake Game Demo                              --
--                        (c) 2014 - David Botton                           --
--                              License GPLv3                               --
--                                                                          --
--                                                                          --
--       A simple demonstration of using Gnoga for 2D game development.     --
--                                                                          --
------------------------------------------------------------------------------

package Snake is
   Title        : constant String := "Sparky the Snake";

   --  Game Display
   Display_Width   : constant := 400;
   Display_Height  : constant := 400;

   --  Snake Parameters
   Initial_Length : constant := 5;
   Segment_Size   : constant := 10;
end Snake;
