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

with Ada.Unchecked_Conversion;

package body UXStrings.Text_IO.Text_Streams is

   overriding procedure Read
     (Stream : in out Stream_File; Item : out Stream_Element_Array; Last : out Stream_Element_Offset)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Read unimplemented");
      raise Program_Error with "Unimplemented procedure Read";
   end Read;

   overriding procedure Write (Stream : in out Stream_File; Item : Stream_Element_Array) is
      subtype SEA is Stream_Element_Array (Item'Range);
      subtype UCA is UTF_8_Character_Array (1 .. Natural (Item'Last));
      function Convert is new Ada.Unchecked_Conversion (SEA, UCA);
      Str : constant UXString := From_UTF_8 (UTF_8_Character_Array (Convert (Item)));
   begin
      Put (Stream.File.all, Str);
   end Write;

   ------------
   -- Stream --
   ------------

   function Stream (File : File_Type) return Stream_Access is
   begin
      return new Stream_File'(Root_Stream_Type with File => File'Unrestricted_Access);
   end Stream;

end UXStrings.Text_IO.Text_Streams;
