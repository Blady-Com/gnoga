------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                  G N O G A . T Y P E S . K E Y _ C O D E S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2014 Pascal Pignard                    --
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

package Gnoga.Types.Key_Codes is

   --  Key codes from Safari 7.1.4 on MacOS 10.9 and French keyboard
   --  and US keyboard and Firefox

   Key_Shift      : constant := 16;
   Key_Control    : constant := 17;
   Key_Alt        : constant := 18;
   Key_Pause      : constant := 19;
   Key_CapsLock   : constant := 20;
   Key_MumLock    : constant := 144;
   Key_Left_Meta  : constant := 91;
   Key_Right_Meta : constant := 93;

   Key_BackSpace : constant := 8;
   Key_Tab       : constant := 9;
   Key_Enter     : constant := 13;
   Key_Esc       : constant := 27;
   Key_Space     : constant := 32;

   Key_Page_Up   : constant := 33;
   Key_Page_Down : constant := 34;
   Key_End       : constant := 35;
   Key_Home      : constant := 36;
   Key_Left      : constant := 37;
   Key_Up        : constant := 38;
   Key_Right     : constant := 39;
   Key_Down      : constant := 40;
   Key_PrtScr    : constant := 44;
   Key_Insert    : constant := 45;
   Key_Delete    : constant := 46;

   Key_0 : constant := 48;
   Key_1 : constant := 49;
   Key_2 : constant := 50;
   Key_3 : constant := 51;
   Key_4 : constant := 52;
   Key_5 : constant := 53;
   Key_6 : constant := 54;
   Key_7 : constant := 55;
   Key_8 : constant := 56;
   Key_9 : constant := 57;

   Key_Colon          : constant := 58;
   Key_Semicolon      : constant := 59;
   Key_Less_Than_Sign : constant := 60;
   Key_Equals_Sign    : constant := 61;

   Key_A : constant := 65;
   Key_B : constant := 66;
   Key_C : constant := 67;
   Key_D : constant := 68;
   Key_E : constant := 69;
   Key_F : constant := 70;
   Key_G : constant := 71;
   Key_H : constant := 72;
   Key_I : constant := 73;
   Key_J : constant := 74;
   Key_K : constant := 75;
   Key_L : constant := 76;
   Key_M : constant := 77;
   Key_N : constant := 78;
   Key_O : constant := 79;
   Key_P : constant := 80;
   Key_Q : constant := 81;
   Key_R : constant := 82;
   Key_S : constant := 83;
   Key_T : constant := 84;
   Key_U : constant := 85;
   Key_V : constant := 86;
   Key_W : constant := 87;
   Key_X : constant := 88;
   Key_Y : constant := 89;
   Key_Z : constant := 90;

   Key_KP_0 : constant := 96;
   Key_KP_1 : constant := 97;
   Key_KP_2 : constant := 98;
   Key_KP_3 : constant := 99;
   Key_KP_4 : constant := 100;
   Key_KP_5 : constant := 101;
   Key_KP_6 : constant := 102;
   Key_KP_7 : constant := 103;
   Key_KP_8 : constant := 104;
   Key_KP_9 : constant := 105;

   Key_KP_Multiply_Sign     : constant := 106;
   Key_KP_Plus_Sign         : constant := 107;
   Key_KP_Minus_Sign        : constant := 109;
   Key_KP_Decimal_Separator : constant := 110;
   Key_KP_Divide_Sign       : constant := 111;
   Key_KP_Equals_Sign       : constant := 187;
   Key_KP_Cancel_Box        : constant := 12;
   Key_KP_Enter             : constant := 13;

   Key_F1  : constant := 112;
   Key_F2  : constant := 113;
   Key_F3  : constant := 114;
   Key_F4  : constant := 115;
   Key_F5  : constant := 116;
   Key_F6  : constant := 117;
   Key_F7  : constant := 118;
   Key_F8  : constant := 119;
   Key_F9  : constant := 120;
   Key_F10 : constant := 121;
   Key_F11 : constant := 122;
   Key_F12 : constant := 123;
   Key_F13 : constant := 124;
   Key_F14 : constant := 125;
   Key_F15 : constant := 126;
   Key_F16 : constant := 127;
   Key_F17 : constant := 128;
   Key_F18 : constant := 129;
   Key_F19 : constant := 130;

   Key_FR_Commercial_At     : constant := 192; -- French keyboard key "@#"
   Key_FR_Right_Parenthesis : constant := 189; -- French keyboard key ")°"
   Key_FR_Minus_Sign        : constant := 189; -- French keyboard key "-_"
   Key_FR_Circumflex        : constant := 229; -- French keyboard key "^""
   Key_FR_Dollar_Sign       : constant := 221; -- French keyboard key "$*€"
   Key_FR_LC_U_Grave        : constant := 222; -- French keyboard key "ù%"
   Key_FR_Grave             : constant := 229; -- French keyboard key "`£"
   Key_FR_Comma             : constant := 188; -- French keyboard key ",?"
   Key_FR_Semicolon         : constant := 186; -- French keyboard key ";."
   Key_FR_Colon             : constant := 186; -- French keyboard key ":/"
   Key_FR_Equals_Sign       : constant := 187; -- French keyboard key "=+"
   Key_FR_Asterisk          : constant := 170; -- French keyboard key "*µ"
   Key_FR_Exclamation       : constant := 161; -- French keyboard key "!§"
   Key_FR_Superscript_Two   : constant := 178; -- French keyboard key "² "

   Key_US_Grave                : constant := 192; -- US keyboard key "`~"
   Key_US_Minus_Sign           : constant := 173; -- US keyboard key "-_"
   Key_US_Left_Square_Bracket  : constant := 219; -- US keyboard key "[{"
   Key_US_Right_Square_Bracket : constant := 221; -- US keyboard key "]}"
   Key_US_Reverse_Solidus      : constant := 220; -- US keyboard key "\|"
   Key_US_Apostrophe           : constant := 222; -- US keyboard key "'""
   Key_US_Comma                : constant := 188; -- US keyboard key ",<"
   Key_US_Full_Stop            : constant := 190; -- US keyboard key ".>"
   Key_US_Solidus              : constant := 191; -- US keyboard key "/?"

end Gnoga.Types.Key_Codes;
