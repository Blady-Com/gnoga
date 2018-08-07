--                                                                    --
--  package              .          Copyright (c)  Dmitry A. Kazakov  --
--     ...Generic_Dyadic_Function                  Luebeck            --
--                                                 Spring, 2018       --
--  Implementation                                                    --
--                                Last revision :  20:28 27 May 2018  --
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

package body Synchronization.Interprocess.Process_Call_Service.
             Generic_Dyadic_Function is

   function Call
            (  Method     : Remote_Function;
               Callee     : Call_Service'Class;
               Argument_1 : Argument_1_Type;
               Argument_2 : Argument_2_Type;
               Timeout  : Duration := Duration'Last
            )  return Result_Type is
      procedure Send_Parameters
                (  Stream : in out Root_Stream_Type'Class
                )  is
      begin
         Argument_1_Type'Output (Stream'Access, Argument_1);
         Argument_2_Type'Output (Stream'Access, Argument_2);
      end Send_Parameters;

      function Do_Call is new Generic_Function_Call (Result_Type);
   begin
      return Do_Call (Method, Callee, Timeout);
   end Call;

   function Call
            (  Method     : Remote_Function;
               Callee     : Call_Service_ID;
               Argument_1 : Argument_1_Type;
               Argument_2 : Argument_2_Type;
               Timeout  : Duration := Duration'Last
            )  return Result_Type is
      procedure Send_Parameters
                (  Stream : in out Root_Stream_Type'Class
                )  is
      begin
         Argument_1_Type'Output (Stream'Access, Argument_1);
         Argument_2_Type'Output (Stream'Access, Argument_2);
      end Send_Parameters;

      function Do_Call is new Generic_Function_Call (Result_Type);
   begin
      return Do_Call
             (  Method,
                Get_Service (Method, Callee).all,
                Timeout
             );
   end Call;

   procedure Execute
             (  Method     : in out Remote_Function;
                Parameters : in out Root_Stream_Type'Class;
                Results    : in out Root_Stream_Type'Class;
                Caller     : in out Call_Service'Class;
                No         : Sequence_No
             )  is
      Argument_1 : constant Argument_1_Type :=
                            Argument_1_Type'Input (Parameters'Access);
      Argument_2 : constant Argument_2_Type :=
                            Argument_2_Type'Input (Parameters'Access);
   begin
      if Method.Handler = null then
         Raise_Exception (Program_Error'Identity, "Not implemented");
      else
         Result_Type'Output
         (  Results'Access,
            Method.Handler
            (  Argument_1,
               Argument_2,
               Method.Service,
               Caller'Access
         )  );
      end if;
   end Execute;

   procedure Set
             (  Method  : in out Remote_Function;
                Handler : Implementation
             )  is
   begin
      Method.Handler := Handler;
   end Set;

end Synchronization.Interprocess.Process_Call_Service.
    Generic_Dyadic_Function;
