-------------------------------------------------------------------------------
-- NAME (specification)         : localize-parser.ads
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : Localization files parser unit.
-- NOTES                        : Ada 2012, GNOGA 1.6 alpha
--
-- COPYRIGHT                    : (c) Pascal Pignard 2020
-- LICENCE                      : CeCILL V2 (http://www.cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Ada.Containers.Indefinite_Vectors;

package Localize.Parser is

   package Lists is new Ada.Containers.Indefinite_Vectors (Positive, String);
   subtype List_Type is Lists.Vector;

   procedure Parse_Master (File_Name : String);
   function Master_Keys return List_Type;
   function Master_Contains (Key : String) return Boolean;
   function Master_Text (Key : String) return String;
   procedure Master_Text (Key : String; Value : String);
   function Master_Comment (Key : String) return String;
   procedure Master_Comment (Key : String; Value : String);

   procedure Parse_Locale (File_Name : String);
   procedure Write_Locale (File_Name : String);
   function Locale_Keys return List_Type;
   function Locale_Contains (Key : String) return Boolean;
   procedure Insert_Locale (Key : String);
   procedure Delete_Locale (Key : String);
   procedure Rename_Locale (From, To : String);
   function Locale_Text (Key : String) return String;
   procedure Locale_Text (Key : String; Value : String);
   function Locale_Comment (Key : String) return String;
   procedure Locale_Comment (Key : String; Value : String);
   function Locale_Modified (Key : String) return Boolean;
   procedure Reset_Locale_Modified_Indicators;

   function Selected_Keys (Pattern : String) return List_Type;

end Localize.Parser;
