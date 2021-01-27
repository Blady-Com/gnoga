-------------------------------------------------------------------------------
-- NAME (body)                  : uxstrings-text_io-streams.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : UXString Streams implementation.
-- NOTES                        : Ada 202x
--
-- COPYRIGHT                    : (c) Pascal Pignard 2021
-- LICENCE                      : CeCILL V2.1 (https://www.cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

package body UXStrings.Text_IO.Text_Streams is

   ------------
   -- Stream --
   ------------

   function Stream (File : File_Type) return Stream_Access is
   begin
      pragma Compile_Time_Warning (Standard.True, "Stream unimplemented");
      return raise Program_Error with "Unimplemented function Stream";
   end Stream;

end UXStrings.Text_IO.Text_Streams;
