--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Call_Services                          Luebeck            --
--  Call services example test                     Spring, 2018       --
--                                                                    --
--                                Last revision :  19:18 30 Apr 2018  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Ada.Text_IO;  use  Ada.Text_IO;

package body Test_Call_Services is
   use Baton_Message;

   function Add
            (  Left   : Integer;
               Right  : Integer;
               Callee : access Call_Service'Class;
               Caller : access Call_Service'Class
            )  return Integer is
   begin
      return Left + Right;
   end Add;

   function Divide
            (  Dividend : Integer;
               Divisor  : Integer;
               Callee   : access Call_Service'Class;
               Caller   : access Call_Service'Class
            )  return Integer is
   begin
      return Dividend / Divisor;
   end Divide;

   function Greet
            (  Text   : String;
               Callee : access Call_Service'Class;
               Caller : access Call_Service'Class
            )  return String is
   begin
      Put_Line
      (  "Incoming: "
      &  Text
      &  " (from" & Call_Service_ID'Image (Get_ID (Caller.all))
      &  ")"
      );
      return "Here there";
   end Greet;

   procedure Baton
             (  Count  : Positive;
                Text   : String;
                Callee : in out Call_Service'Class;
                Caller : in out Call_Service'Class
             )  is
   begin
      Put_Line
      (  "Baton: "
      &  Text
      &  " ["
      &  " (from" & Call_Service_ID'Image (Get_ID (Caller))
      &  " try"
      &  Integer'Image (Count)
      &  "]"
      );
      if Count < 10 then
         Baton_Message.Call
         (  Method     => Data.Baton,
            Argument_1 => Count + 1,
            Argument_2 => Text & "*",
            Callee     =>
               Get_Service
               (  Data.Baton,
                  (Get_ID (Caller) + 1) mod Data.Services.Size + 1
               ) .all
         );
      end if;
   end Baton;

end Test_Call_Services;
