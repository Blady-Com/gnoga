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
with Ada.Strings.Wide_Unbounded;
with Ada.Containers.Ordered_Maps;

package Localize.Parser is

   package Lists is new Ada.Containers.Indefinite_Vectors (Positive, String);
   subtype Key_List is Lists.Vector;

   type Property_List is private;

   procedure Read (Properties : out Property_List; File_Name : String);
   procedure Write (Properties : Property_List; File_Name : String);

   function Keys (Properties : Property_List) return Key_List;
   function Selected_Keys
     (Master, Locale : Property_List; Pattern : String) return Key_List;
   function Contains (Properties : Property_List; Key : String) return Boolean;

   function Text (Properties : Property_List; Key : String) return String;
   procedure Text
     (Properties : in out Property_List; Key : String; Value : String);
   function Comment (Properties : Property_List; Key : String) return String;
   procedure Comment
     (Properties : in out Property_List; Key : String; Value : String);

   procedure Insert (Properties : in out Property_List; Key : String);
   procedure Delete (Properties : in out Property_List; Key : String);
   procedure Rename (Properties : in out Property_List; From, To : String);
   function Modified (Properties : Property_List; Key : String) return Boolean;
   procedure Reset_Modified_Indicators (Properties : in out Property_List);

private

   type Property_Type is record
      Comment  : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
      Text     : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
      Modified : Boolean;
   end record;
   use type Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   package Content_Maps is new Ada.Containers.Ordered_Maps
     (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String, Property_Type);
   type Property_List is new Content_Maps.Map with null record;

end Localize.Parser;
