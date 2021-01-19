--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2016, Michael Rohan <mrohan@zanyblue.com>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--    * Redistributions of source code must retain the above copyright
--      notice, this list of conditions and the following disclaimer.
--
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--
--    * Neither the name of ZanyBlue nor the names of its contributors may
--      be used to endorse or promote products derived from this software
--      without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
--  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

--
--  The ZanyBlue.Text.Codecs supports string encoding and decoding for
--  various encoding schemes, e.g., UTF-8, etc.
--

package ZanyBlue.Text.Codecs is

   type Codecs_Type is tagged private;
   --  Representation of a codec, encoding/decoding to/from an external
   --  encoding, e.g., "UTF-8", "ISO8859-1", etc.

   Unsupported_Encoding : exception;
   --  The environment defined or the library requested the usage of an
   --  encoding not supported by the library.  This exception is only
   --  raised if "Set_Unsupported_Encoding_Action" is called with the
   --  "Action" of "Raise_Exception".

   Unicode_Encode_Error : exception;
   --  The encoding of an Unicode string failed as it contained characters
   --  not representable in the encoding associated with the Codecs.  This
   --  exception is only raised if "Set_Unicode_Encode_Action" is called with
   --  the "Action" of "Raise_Exception".  If the "Action" is "Replace", then
   --  Unicode character un-encodeable using the selected encoding are simply
   --  replaced with the ASCII character '?'.

   Unicode_Decode_Error : exception;
   --  The decoding of a string encoding using the selected (Codecs defined)
   --  encoding could failed on encountering a sequence of characters not
   --  defined by the underyling encoding.  This exception is only raised if
   --  "Set_Unicode_Decode_Action" is called with the "Action" of
   --  "Raise_Exception".  If the "Action" is "Replace", then Unicode character
   --  un-decodeable character sequence is simply replaced with the ASCII
   --  character '?'.

   type Unsupported_Encoding_Action_Type is (Use_UTF8, Raise_Exception);
   --  How to handle user selection of an unsupported encoding.  The
   --  default is to simply use "UTF-8" for all unsupported encodings.

   type Unicode_XForm_Action_Type is (Replace, Raise_Exception);
   --  How to handle encoding or decoding errors, e.g., attempting to
   --  encoding an Unicode character not supported by the targe encoding.
   --  The default is to simply replace the character with the ASCII character
   --  '?'.

   type Implementation_Type is (Internal, Data_Driven);
   --  The internal ZanyBlue implementation of the encoding: either internal,
   --  i.e., directly implemented via internal routines, or data driven, i.e.,
   --  via code point mapping tables.

   procedure Set_Unsupported_Encoding_Action
     (Action : Unsupported_Encoding_Action_Type);
   --  Action to take when an unsupported Encoding is selected: either
   --  fallback to UTF-8 (the default action) or raise the exception
   --  "Unsupported_Encoding".

   procedure Set_Unicode_Encode_Action (Action : Unicode_XForm_Action_Type);
   --  Action to take when encoding from Unicode to the target encoding
   --  fails.  The default action is to simply replace the character with
   --  the ASCII character '?" (an "Action" value of "Replace").
   --  Alternatively, the exception "Unicode_Encode_Error" can be raised
   --  (an "Action" value of "Raise_Exception").

   procedure Set_Unicode_Decode_Action (Action : Unicode_XForm_Action_Type);
   --  Action to take when de`oding from a target encoded string to Unicode
   --  fails.  The default action is to simply replace the character(s) with
   --  the ASCII character '?" (an "Action" value of "Replace").
   --  Alternatively, the exception "Unicode_Encode_Error" can be raised
   --  (an "Action" value of "Raise_Exception").

   function Make_Codecs
     (Encoding : Wide_String)
      return Codecs_Type;
   --  Create a Codecs object given an encoding name.   Depending on the
   --  "Action" selected by "Set_Unsupported_Encoding_Action", this routine
   --  could raise "Unsupported_Encoding" for unsupported encodings.  The
   --  default is to simply fallback on "UTF-8" for unsupported encodings.

   function Name
     (Codecs : Codecs_Type)
      return Wide_String;
   --  Return the encoding name associated with a Codecs objects.  If the
   --  "Action" selected by "Set_Unsupported_Encoding_Action" is "Use_UTF8",
   --  (the default) this routine might return a name different from the
   --  string used to create the Codecs objects.

   function Implementation
     (Codecs : Codecs_Type)
      return Implementation_Type;
   --  How is the codecs implemented, is it internally implemented or via
   --  code point data tables.

   function Encode
     (Codecs : Codecs_Type;
      Value  : Wide_String)
      return String;
   --  Encode the Unicode string using the encoding associated with the
   --  Codecs objects.  This could raise the exception "Unicode_Encode_Error"
   --  if requested via the routine "Set_Unicode_Encode_Action".  The default
   --  action is to simply replace un-encodeable character with the ASCII
   --  character '?'.

   function Decode
     (Codecs : Codecs_Type;
      Value  : String)
      return Wide_String;
   --  Decode the encoded argument string using the encoding associated with
   --  the Codecs object.  The exception "Unicode_Decode_Error" can be raised
   --  depending on the setting selected by "Set_Unicode_Decode_Action", the
   --  default is to simply replace the un-decodeable sequence with the ASCII
   --  character '?'.

   function Encode
     (Encoding : Wide_String;
      Value    : Wide_String)
      return String;
   --  Encode the Unicode string using the named encoding.  This could raise
   --  the exception "Unicode_Encode_Error" if requested via the routine
   --  "Set_Unicode_Encode_Action".  The default action is to simply replace
   --  un-encodeable character with the ASCII character '?'.

   function Decode
     (Encoding : Wide_String;
      Value    : String)
      return Wide_String;
   --  Decode the encoded argument string using the named encoding.  The
   --  exception "Unicode_Decode_Error" can be raised depending on the setting
   --  selected by "Set_Unicode_Decode_Action", the default is to simply
   --  replace the un-decodeable sequence with the ASCII character '?'.

   function N_Encodings return Natural;
   --  Return the number of encodings defined in the ZanyBlue library.

   function Name
     (Encoding_Index : Positive)
      return Wide_String;
   --  Return the name of the I'th encoding.  This routine will raise
   --  Constraint_Error if there is no corresponding encoding.

   procedure Iterate_Decodings
     (Codecs  : Codecs_Type;
      Handler : not null access procedure
        (WCh      : Wide_Character;
         Encoding : String));
   --  Iterate over the encoding mappings for a Codecs.  The iteration is
   --  ordered by encoded character sequence.

   procedure Iterate_Encodings
     (Codecs  : Codecs_Type;
      Handler : not null access procedure
        (WCh      : Wide_Character;
         Encoding : String));
   --  Iterate over the encoding mappings for a Codecs.  The iteration is
   --  ordered by Unicode code point associated with the mapping.

private

   function Default_Codecs_Index return Positive;

   type Codecs_Type is tagged record
      Encoding_Index : Positive := Default_Codecs_Index;
   end record;

end ZanyBlue.Text.Codecs;
