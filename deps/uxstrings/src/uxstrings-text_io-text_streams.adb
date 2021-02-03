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

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out Stream_File; Item : out Stream_Element_Array; Last : out Stream_Element_Offset)
   is
      subtype SEA is Stream_Element_Array (Item'Range);
      subtype UCA is UTF_8_Character_Array (1 .. Natural (Item'Last));
      function Convert is new Ada.Unchecked_Conversion (UCA, SEA);
      Read_Buffer : UCA;
      Read_Last   : Natural;
   begin
      Read_Stream (Stream.File.all, Read_Buffer, Read_Last);
      Item := Convert (Read_Buffer);
      Last := Stream_Element_Offset (Read_Last);
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write (Stream : in out Stream_File; Item : Stream_Element_Array) is
      subtype SEA is Stream_Element_Array (Item'Range);
      subtype UCA is UTF_8_Character_Array (1 .. Natural (Item'Last));
      function Convert is new Ada.Unchecked_Conversion (SEA, UCA);
   begin
      Write_Stream (Stream.File.all, Convert (Item));
   end Write;

   ------------
   -- Stream --
   ------------

   function Stream (File : File_Type) return Stream_Access is
   begin
      return new Stream_File'(Root_Stream_Type with File => File'Unrestricted_Access);
   end Stream;

end UXStrings.Text_IO.Text_Streams;
