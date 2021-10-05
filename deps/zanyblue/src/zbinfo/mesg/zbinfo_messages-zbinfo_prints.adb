--  -*- coding: utf-8 -*-
--
--  Ada implementation generated by ZBMCompile, V1.4.0 (r3199).
--  This is a generated file and should not be edited.
--

with ZanyBlue.Text.Formatting;

package body ZBInfo_Messages.ZBInfo_Prints is

   use ZanyBlue.Text.Formatting;

   -----------------
   -- Print_00001 --
   -----------------

   procedure Print_00001 (
      Argument0   : Any_Category_Type'Class;
      Argument1   : Any_Category_Type'Class;
      Argument2   : Any_Category_Type'Class;
      Argument3   : Any_Category_Type'Class;
      Argument4   : Calendar_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Append (Arguments, Argument1);
      Append (Arguments, Argument2);
      Append (Arguments, Argument3);
      Append (Arguments, Argument4);
      Write_Message (Destination, Facility (1), Key (3),
                     Arguments, With_NL, Locale, Catalog);
   end Print_00001;

   -----------------
   -- Print_00002 --
   -----------------

   procedure Print_00002 (
      Argument0   : Any_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Write_Message (Destination, Facility (1), Key (6),
                     Arguments, With_NL, Locale, Catalog);
   end Print_00002;

   -----------------
   -- Print_00003 --
   -----------------

   procedure Print_00003 (
      Argument0   : Calendar_Category_Type'Class;
      Argument1   : Duration_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Append (Arguments, Argument1);
      Write_Message (Destination, Facility (1), Key (4),
                     Arguments, With_NL, Locale, Catalog);
   end Print_00003;

   -----------------
   -- Print_00004 --
   -----------------

   procedure Print_00004 (
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
   begin
      Write_Message (Destination, Facility (1), Key (1),
                     Empty_Argument_List, With_NL, Locale, Catalog);
   end Print_00004;

   -----------------
   -- Print_00005 --
   -----------------

   procedure Print_00005 (
      Argument0   : String_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Write_Message (Destination, Facility (1), Key (13),
                     Arguments, With_NL, Locale, Catalog);
   end Print_00005;

   -----------------
   -- Print_00006 --
   -----------------

   procedure Print_00006 (
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
   begin
      Write_Message (Destination, Facility (1), Key (9),
                     Empty_Argument_List, With_NL, Locale, Catalog);
   end Print_00006;

   -----------------
   -- Print_00007 --
   -----------------

   procedure Print_00007 (
      Argument0   : Any_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Write_Message (Destination, Facility (1), Key (22),
                     Arguments, With_NL, Locale, Catalog);
   end Print_00007;

   -----------------
   -- Print_00008 --
   -----------------

   procedure Print_00008 (
      Argument0   : Any_Category_Type'Class;
      Argument1   : Any_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Append (Arguments, Argument1);
      Write_Message (Destination, Facility (1), Key (34),
                     Arguments, With_NL, Locale, Catalog);
   end Print_00008;

   -----------------
   -- Print_00009 --
   -----------------

   procedure Print_00009 (
      Argument0   : Any_Category_Type'Class;
      Argument1   : Any_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Append (Arguments, Argument1);
      Write_Message (Destination, Facility (1), Key (35),
                     Arguments, With_NL, Locale, Catalog);
   end Print_00009;

   -----------------
   -- Print_00010 --
   -----------------

   procedure Print_00010 (
      Argument0   : Any_Category_Type'Class;
      Argument1   : Any_Category_Type'Class;
      Argument2   : Any_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Append (Arguments, Argument1);
      Append (Arguments, Argument2);
      Write_Message (Destination, Facility (1), Key (14),
                     Arguments, With_NL, Locale, Catalog);
   end Print_00010;

   -----------------
   -- Print_00011 --
   -----------------

   procedure Print_00011 (
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
   begin
      Write_Message (Destination, Facility (1), Key (16),
                     Empty_Argument_List, With_NL, Locale, Catalog);
   end Print_00011;

   -----------------
   -- Print_00012 --
   -----------------

   procedure Print_00012 (
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
   begin
      Write_Message (Destination, Facility (1), Key (2),
                     Empty_Argument_List, With_NL, Locale, Catalog);
   end Print_00012;

   -----------------
   -- Print_00013 --
   -----------------

   procedure Print_00013 (
      Argument0   : Any_Category_Type'Class;
      Argument1   : Any_Category_Type'Class;
      Argument2   : Any_Category_Type'Class;
      Argument3   : Any_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Append (Arguments, Argument1);
      Append (Arguments, Argument2);
      Append (Arguments, Argument3);
      Write_Message (Destination, Facility (1), Key (20),
                     Arguments, With_NL, Locale, Catalog);
   end Print_00013;

   -----------------
   -- Print_00014 --
   -----------------

   procedure Print_00014 (
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
   begin
      Write_Message (Destination, Facility (1), Key (29),
                     Empty_Argument_List, With_NL, Locale, Catalog);
   end Print_00014;

   -----------------
   -- Print_00015 --
   -----------------

   procedure Print_00015 (
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
   begin
      Write_Message (Destination, Facility (1), Key (11),
                     Empty_Argument_List, With_NL, Locale, Catalog);
   end Print_00015;

   -----------------
   -- Print_00016 --
   -----------------

   procedure Print_00016 (
      Argument0   : Any_Category_Type'Class;
      Argument1   : Any_Category_Type'Class;
      Argument2   : Any_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Append (Arguments, Argument1);
      Append (Arguments, Argument2);
      Write_Message (Destination, Facility (1), Key (7),
                     Arguments, With_NL, Locale, Catalog);
   end Print_00016;

   -----------------
   -- Print_00017 --
   -----------------

   procedure Print_00017 (
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
   begin
      Write_Message (Destination, Facility (1), Key (36),
                     Empty_Argument_List, With_NL, Locale, Catalog);
   end Print_00017;

   -----------------
   -- Print_00018 --
   -----------------

   procedure Print_00018 (
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
   begin
      Write_Message (Destination, Facility (1), Key (28),
                     Empty_Argument_List, With_NL, Locale, Catalog);
   end Print_00018;

   -----------------
   -- Print_00019 --
   -----------------

   procedure Print_00019 (
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
   begin
      Write_Message (Destination, Facility (1), Key (8),
                     Empty_Argument_List, With_NL, Locale, Catalog);
   end Print_00019;

   -----------------
   -- Print_00020 --
   -----------------

   procedure Print_00020 (
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
   begin
      Write_Message (Destination, Facility (1), Key (26),
                     Empty_Argument_List, With_NL, Locale, Catalog);
   end Print_00020;

   -----------------
   -- Print_00021 --
   -----------------

   procedure Print_00021 (
      Argument0   : Any_Category_Type'Class;
      Argument1   : Any_Category_Type'Class;
      Argument2   : Any_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Append (Arguments, Argument1);
      Append (Arguments, Argument2);
      Write_Message (Destination, Facility (1), Key (21),
                     Arguments, With_NL, Locale, Catalog);
   end Print_00021;

   -----------------
   -- Print_00022 --
   -----------------

   procedure Print_00022 (
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
   begin
      Write_Message (Destination, Facility (1), Key (18),
                     Empty_Argument_List, With_NL, Locale, Catalog);
   end Print_00022;

   -----------------
   -- Print_00023 --
   -----------------

   procedure Print_00023 (
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
   begin
      Write_Message (Destination, Facility (1), Key (23),
                     Empty_Argument_List, With_NL, Locale, Catalog);
   end Print_00023;

   -----------------
   -- Print_00024 --
   -----------------

   procedure Print_00024 (
      Argument0   : Any_Category_Type'Class;
      Argument1   : Any_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Append (Arguments, Argument1);
      Write_Message (Destination, Facility (1), Key (37),
                     Arguments, With_NL, Locale, Catalog);
   end Print_00024;

   -----------------
   -- Print_00025 --
   -----------------

   procedure Print_00025 (
      Argument0   : Any_Category_Type'Class;
      Argument1   : Any_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Append (Arguments, Argument1);
      Write_Message (Destination, Facility (1), Key (5),
                     Arguments, With_NL, Locale, Catalog);
   end Print_00025;

   -----------------
   -- Print_00026 --
   -----------------

   procedure Print_00026 (
      Argument0   : Any_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Write_Message (Destination, Facility (1), Key (33),
                     Arguments, With_NL, Locale, Catalog);
   end Print_00026;

   -----------------
   -- Print_00027 --
   -----------------

   procedure Print_00027 (
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
   begin
      Write_Message (Destination, Facility (1), Key (12),
                     Empty_Argument_List, With_NL, Locale, Catalog);
   end Print_00027;

   -----------------
   -- Print_00028 --
   -----------------

   procedure Print_00028 (
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
   begin
      Write_Message (Destination, Facility (1), Key (25),
                     Empty_Argument_List, With_NL, Locale, Catalog);
   end Print_00028;

   -----------------
   -- Print_00029 --
   -----------------

   procedure Print_00029 (
      Argument0   : Any_Category_Type'Class;
      Argument1   : Any_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Append (Arguments, Argument1);
      Write_Message (Destination, Facility (1), Key (27),
                     Arguments, With_NL, Locale, Catalog);
   end Print_00029;

   -----------------
   -- Print_00030 --
   -----------------

   procedure Print_00030 (
      Argument0   : Any_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Write_Message (Destination, Facility (1), Key (32),
                     Arguments, With_NL, Locale, Catalog);
   end Print_00030;

   -----------------
   -- Print_00031 --
   -----------------

   procedure Print_00031 (
      Argument0   : Any_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Write_Message (Destination, Facility (1), Key (38),
                     Arguments, With_NL, Locale, Catalog);
   end Print_00031;

   -----------------
   -- Print_00032 --
   -----------------

   procedure Print_00032 (
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
   begin
      Write_Message (Destination, Facility (1), Key (31),
                     Empty_Argument_List, With_NL, Locale, Catalog);
   end Print_00032;

   -----------------
   -- Print_00033 --
   -----------------

   procedure Print_00033 (
      Argument0   : Any_Category_Type'Class;
      Argument1   : Any_Category_Type'Class;
      Argument2   : Any_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Append (Arguments, Argument1);
      Append (Arguments, Argument2);
      Write_Message (Destination, Facility (1), Key (24),
                     Arguments, With_NL, Locale, Catalog);
   end Print_00033;

   -----------------
   -- Print_00034 --
   -----------------

   procedure Print_00034 (
      Argument0   : Any_Category_Type'Class;
      Argument1   : Any_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Append (Arguments, Argument1);
      Write_Message (Destination, Facility (1), Key (30),
                     Arguments, With_NL, Locale, Catalog);
   end Print_00034;

   -----------------
   -- Print_00035 --
   -----------------

   procedure Print_00035 (
      Argument0   : Any_Category_Type'Class;
      Argument1   : Any_Category_Type'Class;
      Argument2   : Any_Category_Type'Class;
      Argument3   : Any_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Append (Arguments, Argument1);
      Append (Arguments, Argument2);
      Append (Arguments, Argument3);
      Write_Message (Destination, Facility (1), Key (17),
                     Arguments, With_NL, Locale, Catalog);
   end Print_00035;

   -----------------
   -- Print_00036 --
   -----------------

   procedure Print_00036 (
      Argument0   : Any_Category_Type'Class;
      Argument1   : Any_Category_Type'Class;
      Argument2   : Any_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Append (Arguments, Argument1);
      Append (Arguments, Argument2);
      Write_Message (Destination, Facility (1), Key (19),
                     Arguments, With_NL, Locale, Catalog);
   end Print_00036;

   -----------------
   -- Print_10001 --
   -----------------

   procedure Print_10001 (
      Argument0   : Any_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Write_Message (Destination, Facility (1), Key (10),
                     Arguments, With_NL, Locale, Catalog);
   end Print_10001;

   -----------------
   -- Print_10002 --
   -----------------

   procedure Print_10002 (
      Argument0   : Any_Category_Type'Class;
      Destination : File_Type    := Current_Output;
      With_NL     : Boolean      := True;
      Locale      : Locale_Type  := Current_Locale;
      Catalog     : Catalog_Type := Standard_Catalog)
   is
      Arguments : Argument_List;
   begin
      Append (Arguments, Argument0);
      Write_Message (Destination, Facility (1), Key (15),
                     Arguments, With_NL, Locale, Catalog);
   end Print_10002;

end ZBInfo_Messages.ZBInfo_Prints;
