-------------------------------------------------------------------------------
-- NAME (body)                  : uxstrings-formatting.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : UXString formatting implementation.
-- NOTES                        : Ada 202x
--
-- COPYRIGHT                    : (c) Pascal Pignard 2021
-- LICENCE                      : CeCILL V2.1 (https://www.cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Strings_Edit.Integer_Edit;

package body UXStrings.Formatting is

   --------------------
   -- Integer_Format --
   --------------------

   function Integer_Format
     (Item    :    T; Base : in Number_Base := 10; Put_Plus : in Boolean := False; Field : in Natural := 0;
      Justify : in Alignment := Left; Fill : in Character := ' ') return UXString
   is
      package Strings_Edit_T is new Strings_Edit.Integer_Edit (T);
      Text    : String (1 .. 80);
      Pointer : Integer := Text'First;
   begin
      Strings_Edit_T.Put (Text, Pointer, Item, Base, Put_Plus, Field, Justify, Fill);
      return From_Latin_1 (Text (Text'First .. Pointer - 1));
   end Integer_Format;

end UXStrings.Formatting;
