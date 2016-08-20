--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2016, Michael Rohan <mrohan@zanyblue.com>
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
--  Example of the use of the ZanyBlue.Text.Formatting package based on the
--  Java .properties files in Apache Tomcat (rename from LocalStrings).
--

with Ada.Command_Line;
with Apache.Tomcat.Messages;
with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Formatting;
with ZanyBlue.Text.Version_Status_Arguments;

procedure ZBX_Tomcat is

   use ZanyBlue.Text.Formatting;
   use ZanyBlue.Text.Version_Status_Arguments;

   procedure Process_Command_Line;

   Usage_Error, Help_Error : exception;

   procedure Process_Command_Line is
      use Ada.Command_Line;
      use ZanyBlue.Text.Pseudo;
      use ZanyBlue.Text.Locales;
   begin
      for I in 1 .. Argument_Count loop
         declare
            Option : constant String := Argument (I);
         begin
            if Option = "-xh" or Option = "-x" then
               Pseudo_Translate (Halfwidth_Forms_Map);
            elsif Option = "-xe" then
               Pseudo_Translate (Enclosed_Alphanumeric_Map);
            elsif Option = "-xl" then
               Pseudo_Translate (Lowercase_Map);
            elsif Option = "-xu" then
               Pseudo_Translate (Uppercase_Map);
            elsif Option = "-xn" then
               --  No pseudo translation
               null;
            elsif Option = "-h" then
               raise Help_Error;
            elsif Option (1 .. 2) = "-l" then
               Set_Locale (Option (3 .. Option'Last));
            else
               raise Usage_Error;
            end if;
         end;
      end loop;
   end Process_Command_Line;

   Path : constant Wide_String := "../example.dat";
   Mask : constant Wide_String := "mask";
   User : constant Wide_String := "mrohan";
   Resource : constant Wide_String := "resource";
   Component : constant Wide_String := "component";

begin
   Disable_Wide_IO;
   Print_Line ("This is TOMCAT, Version {0}.{1}.{2} - {3}",
               +ZanyBlue.Version_Major, +ZanyBlue.Version_Minor,
               +ZanyBlue.Version_Patch, +ZanyBlue.Version_Status);
   Process_Command_Line;
   Print_Line ("authenticator", "authenticator.notContext");
   Print_Line ("authenticator", "authenticator.check.authorize", +User);
   Print_Line ("connector", "coyoteConnector.protocolHandlerResumeFailed");
   Print_Line ("connector", "coyoteAdapter.accesslogFail");
   Print_Line ("core", "applicationContext.resourcePaths.iae", +Path);
   Print_Line ("core", "applicationContext.attributeEvent");
   Print_Line ("manager", "htmlManagerServlet.deployServer");
   Print_Line ("manager", "htmlManagerServlet.deployUploadInServerXml", +Path);
   Print_Line ("realm", "jaasRealm.loginContextCreated", +User);
   Print_Line ("realm", "jaasRealm.userPrincipalSuccess", +User);
   Print_Line ("security", "SecurityUtil.doAsPrivilege");
   Print_Line ("security", "SecurityListener.checkUmaskParseFail", +Mask);
   Print_Line ("servlets", "webdavservlet.jaxpfailed");
   Print_Line ("servlets", "defaultServlet.missingResource", +Resource);
   Print_Line ("startup", "catalina.serverStartFail");
   Print_Line ("startup", "contextConfig.altDDNotFound", +Path);
   Print_Line ("users", "memoryUserDatabase.readOnly");
   Print_Line ("users", "memoryUserDatabase.nullRole");
   Print_Line ("util", "extensionValidator.web-application-manifest");
   Print_Line ("util", "lifecycleBase.alreadyDestroyed", +Component);
   Print_Line ("valves", "jdbcAccessLogValve.exception");
   Print_Line ("valves", "accessLogValve.openFail", +Path);
exception
when Help_Error =>
   Print_Line ("zbtomcat", "Help");
when Usage_Error =>
   Print_Line ("zbtomcat", "Usage");
end ZBX_Tomcat;
