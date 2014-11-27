--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_My_Object.Handle                       Luebeck            --
--  Interface                                      Summer, 2002       --
--                                                                    --
--                                Last revision :  20:47 23 Jun 2010  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of  the  GNU  Library  General  Public  --
--  License  as  published  by  the Free Software Foundation; either  --
--  version 2 of the License, or (at your option) any later version.  --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--                                                                    --
--  This  library is distributed in the hope that it will be useful,  --
--  but WITHOUT ANY WARRANTY; without even the implied  warranty  of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  Library General Public License for more details.                  --
--                                                                    --
--  You  should  have  received  a  copy  of the GNU Library General  --
--  Public  License  along  with  this library; if not, write to the  --
--  Free Software Foundation, Inc.,  675  Mass  Ave,  Cambridge,  MA  --
--  02139, USA.                                                       --
--____________________________________________________________________--

with Object.Handle;

package Test_My_String.Handle is
--
-- Though   an   instantiation  of  Object.Handle  provides  handles  to
-- My_String,  we  would  like  to  have  some  additional operations on
-- handles.
--
   package My_String_Handle is
      new Object.Handle (My_String, My_String_Ptr);
--
-- So  we  immediately  derive  from  the  obtained  type.  Note that no
-- additional components needed (with null record).
--
   type My_Safe_String is new My_String_Handle.Handle with null record;
--
-- Now define useful operations on string handles:
--
   function Create (Value : String) return My_Safe_String;
   function Value (Reference : My_Safe_String) return String;
--
-- Note  that Copy takes handle as an inout-parameter. It does not touch
-- the old object it just creates a new one and sets handle to point  to
-- it. The old object is automatically destroyed if no more referenced.
--
   procedure Copy
             (  Reference : in out My_Safe_String;
                New_Value : String
             );
   procedure Copy
             (  Reference : in out My_Safe_String;
                New_Value : My_Safe_String
             );
private
--
-- Note that Ref shall be overridden. This is  a  language  requirement,
-- which ensures that the results are  covariant.  We  make  it  private
-- because there is no need for a user to access it.
--
   function Ref (Pointer : My_String_Ptr) return My_Safe_String;

end Test_My_String.Handle;
