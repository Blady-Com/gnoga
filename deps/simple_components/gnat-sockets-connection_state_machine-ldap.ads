--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     LDAP                                        Summer, 2019       --
--  Interface                                                         --
--                                Last revision :  21:13 03 Aug 2019  --
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

with GNAT.Sockets.Connection_State_Machine.ASN1.Booleans;
with GNAT.Sockets.Connection_State_Machine.ASN1.Choices;
with GNAT.Sockets.Connection_State_Machine.ASN1.Distinguished_Names;
with GNAT.Sockets.Connection_State_Machine.ASN1.Generic_Enumeration;
with GNAT.Sockets.Connection_State_Machine.ASN1.Integers_8;
with GNAT.Sockets.Connection_State_Machine.ASN1.Integers_32;
with GNAT.Sockets.Connection_State_Machine.ASN1.Nulls;
with GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.Explicit;
with GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.
     Generic_Sequence_Of;
with GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.Implicit;
with GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Generic_Reference;
with GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Generic_Set_Of;
with GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Implicit;
with GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Explicit;
with GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Implicit;

with Strings_Edit.Distinguished_Names;
use  Strings_Edit.Distinguished_Names;

with Strings_Edit.Object_Identifiers;
use  Strings_Edit.Object_Identifiers;

with Object.Handle;

package GNAT.Sockets.Connection_State_Machine.LDAP is
   use Interfaces;
   use GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.Explicit;
   use GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.Implicit;
   use GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Implicit;
   use GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Explicit;
   use GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Implicit;

   LDAP_Error   : exception;
   LDAP_Version : constant := 3;

   type Result_Code is mod 2**8;
   Success_Code                        : constant Result_Code :=  0;
   Operations_Error_Code               : constant Result_Code :=  1;
   Protocol_Error_Code                 : constant Result_Code :=  2;
   Time_Limit_Exceeded_Code            : constant Result_Code :=  3;
   Size_Limit_Exceeded_Code            : constant Result_Code :=  4;
   Compare_False_Code                  : constant Result_Code :=  5;
   Compare_True_Code                   : constant Result_Code :=  6;
   Auth_Method_Not_Supported_Code      : constant Result_Code :=  7;
   Stronger_Auth_Required_Code         : constant Result_Code :=  8;
   Referral_Code                       : constant Result_Code := 10;
   Admin_Limit_Exceeded_Code           : constant Result_Code := 11;
   Unavailable_Critical_Extension_Code : constant Result_Code := 12;
   Confidentiality_Required_Code       : constant Result_Code := 13;
   SASL_Bind_In_Progress_Code          : constant Result_Code := 14;
   No_Such_Attribute_Code              : constant Result_Code := 16;
   Undefined_Attribute_Type_Code       : constant Result_Code := 17;
   Inappropriate_Matching_Code         : constant Result_Code := 18;
   Constraint_Violation_Code           : constant Result_Code := 19;
   Attribute_Or_Value_Exists_Code      : constant Result_Code := 20;
   Invalid_Attribute_Syntax_Code       : constant Result_Code := 21;
   No_Such_Object_Code                 : constant Result_Code := 32;
   Alias_Problem_Code                  : constant Result_Code := 33;
   Invalid_DN_Syntax_Code              : constant Result_Code := 34;
   Alias_Dereferencing_Problem_Code    : constant Result_Code := 36;
   Inappropriate_Authentication_Code   : constant Result_Code := 48;
   Invalid_Credentials_Code            : constant Result_Code := 49;
   Insufficient_Access_Rights_Code     : constant Result_Code := 50;
   Busy_Code                           : constant Result_Code := 51;
   Unavailable_Code                    : constant Result_Code := 52;
   Unwilling_To_Perform_Code           : constant Result_Code := 53;
   Loop_Detect_Code                    : constant Result_Code := 54;
   Naming_Violation_Code               : constant Result_Code := 64;
   Object_Class_Violation_Code         : constant Result_Code := 65;
   Not_Allowed_On_Non_Leaf_Code        : constant Result_Code := 66;
   Not_Allowed_On_RDN_Code             : constant Result_Code := 67;
   Entry_Already_Exists_Code           : constant Result_Code := 68;
   Object_Class_Mods_Prohibited_Code   : constant Result_Code := 69;
   Affects_Multiple_DSAs_Code          : constant Result_Code := 71;
   Other_Code                          : constant Result_Code := 80;
--
-- Image -- The textual representation of the result code
--
--    Code - The result code
--
-- Returns :
--
--    The string containing the representation of Code
--
   function Image (Code : Result_Code) return String;
------------------------------------------------------------------------
-- Attributes list construction operations
--
   type Values_List (<>) is private;
   function Get_Length (List : Values_List) return Natural;
   function Get (List : Values_List; Index : Positive) return String;
   function "/" (Left : String;      Right : String) return Values_List;
   function "/" (Left : Values_List; Right : String) return Values_List;
   function Empty return Values_List;
   function "+" (Value : String) return Values_List;

   type Attribute_Definition (<>) is private;
   function "-"
            (  Description : String;
               Value       : String
            )  return Attribute_Definition;
   function "-"
            (  Description : String;
               Values      : Values_List
            )  return Attribute_Definition;

   type Attributes_List (<>) is private;
   function Get_Length (List : Attributes_List) return Natural;
   function Get_Description
            (  List  : Attributes_List;
               Index : Positive
            )  return String;
   function Get_Values
            (  List  : Attributes_List;
               Index : Positive
            )  return Values_List;
   function "-"
            (  Description : String;
               Value       : String
            )  return Attributes_List;
   function "-"
            (  Description : String;
               Values      : Values_List
            )  return Attributes_List;
   function "or" (Left, Right : Attributes_List) return Attributes_List;
------------------------------------------------------------------------
-- Udpate list construction operations
--
   type Updates_List (<>) is private;
   function Add
            (  Attribute : Attribute_Definition
            )  return Updates_List;
   function Delete
            (  Attribute : Attribute_Definition
            )  return Updates_List;
   function Delete_All (Description : String) return Updates_List;
   function Replace
            (  Attribute : Attribute_Definition
            )  return Updates_List;
   function Replace_All (Description : String) return Updates_List;
   function "or" (Left, Right : Updates_List) return Updates_List;
------------------------------------------------------------------------
   package LDAP_Result_Codes is
      new ASN1.Generic_Enumeration (Result_Code);

   type LDAP_Implicit_String is
      new ASN1.Strings.Implicit.Implicit_External_String_Data_Item
         with null record;
   function Get_ASN1_Type
            (  Item : LDAP_Implicit_String
            )  return ASN1.ASN1_Type;

   type LDAP_String is new External_String_Data_Item with null record;
   function Get_ASN1_Type (Item : LDAP_String) return ASN1.ASN1_Type;

   type LDAP_OID is new LDAP_Implicit_String with null record;
   function Get (OID : LDAP_OID) return Object_Identifier;

   type LDAP_DN is
      new GNAT.Sockets.Connection_State_Machine.ASN1.
          Distinguished_Names.External_DN_Data_Item with null record;
   function Get_ASN1_Type (Item : LDAP_DN) return ASN1.ASN1_Type;
   function Get (Name : LDAP_DN) return Distinguished_Name;

   type LDAP_Implicit_DN is
      new GNAT.Sockets.Connection_State_Machine.ASN1.
          Distinguished_Names.Implicit_External_DN_Data_Item with
             null record;
   function Get_ASN1_Type
            (  Item : LDAP_Implicit_DN
            )  return ASN1.ASN1_Type;
   function Get (Name : LDAP_Implicit_DN) return Distinguished_Name;

   subtype Octet_String is LDAP_Implicit_String;
   subtype LDAP_Assertion_Value is Octet_String;
--
-- LDAP_String_Sequence -- Sequence of LDAP_String
--
   package LDAP_String_Sequences is
      new ASN1.Sequences.Generic_Sequence_Of
          (  Value_Type   => String,
             Element_Type => LDAP_String,
             Get          => Get_Value,
             Set          => Set_Value
          );
   type LDAP_Implicit_String_Sequence is
      new LDAP_String_Sequences.Implicit_Sequence_Of with null record;
   procedure Append
             (  List  : in out LDAP_Implicit_String_Sequence;
                Value : String
             );
   function Get
            (  Sequence : LDAP_Implicit_String_Sequence
            )  return Values_List;

   type LDAP_String_Sequence is
      new LDAP_String_Sequences.Sequence_Of with null record;
   procedure Append
             (  List  : in out LDAP_String_Sequence;
                Value : String
             );
--
-- LDAP_String_Set -- Set of LDAP_String
--
   package LDAP_String_Sets is
      new ASN1.Sets.Generic_Set_Of
          (  Value_Type   => String,
             Element_Type => LDAP_String,
             Get          => Get_Value,
             Set          => Set_Value
          );
   type LDAP_String_Set is
      new LDAP_String_Sets.Set_Of with null record;
   procedure Append
             (  List  : in out LDAP_String_Set;
                Value : String
             );

   type LDAP_Attribute is new Sequence_Data_Item with record
      Attribute_Description : LDAP_String;
      Values                : LDAP_String_Set;
   end record;
   procedure Append
             (  Attribute : in out LDAP_Attribute;
                Value     : String
             );
   function Get_Description
            (  Attribute : LDAP_Attribute
            )  return String;
   procedure Set_Description
             (  Attribute : in out LDAP_Attribute;
                Value     : String
             );

   type LDAP_Attribute_Value_Assertion is
      new Tagged_Sequence_Data_Item with
   record
      Attribute_Description : LDAP_Implicit_String;
      Value                 : Octet_String;
   end record;
   procedure Initialized
             (  Sequence : in out LDAP_Attribute_Value_Assertion
             );
   procedure Set
             (  Sequence    : in out LDAP_Attribute_Value_Assertion;
                Description : String;
                Value       : String
             );

   type LDAP_Implicit_Attribute_Value_Assertion is
      new Implicit_Tagged_Sequence_Data_Item with
   record
      Attribute_Description : LDAP_Implicit_String;
      Value                 : Octet_String;
   end record;
   procedure Initialized
             (  Sequence : in out
                           LDAP_Implicit_Attribute_Value_Assertion
             );
   procedure Set
             (  Sequence    : in out
                              LDAP_Implicit_Attribute_Value_Assertion;
                Description : String;
                Value       : String
             );
--
-- LDAP_Attribute_List -- Sequence of LDAP_Attribute
--
   package LDAP_Attribute_Sequences is
      new ASN1.Sequences.Generic_Sequence_Of
          (  Value_Type   => String,
             Element_Type => LDAP_Attribute,
             Get          => Get_Description,
             Set          => Set_Description
          );
   type LDAP_Attribute_List is
      new LDAP_Attribute_Sequences.Sequence_Of with null record;
   procedure Append
             (  List        : in out LDAP_Attribute_List;
                Description : String
             );
   procedure Append
             (  List        : in out LDAP_Attribute_List;
                Description : String;
                Value       : String
             );
   procedure Append_Value
             (  List  : in out LDAP_Attribute_List;
                Value : String
             );
   function Get
            (  List : LDAP_Attribute_List
            )  return Attributes_List;

   type LDAP_Add_Request is new Implicit_Sequence_Data_Item with record
      Entry_Item : LDAP_DN;
      Attributes : LDAP_Attribute_List;
   end record;

   type LDAP_Result is
      new Implicit_Tagged_Sequence_Data_Item with
   record
      Result_Code : LDAP_Result_Codes.Implicit_Enumeration_Data_Item;
      Matched_DN  : LDAP_Implicit_DN;
      Diagnostic_Message : LDAP_Implicit_String;
      Referral           : LDAP_Implicit_String_Sequence;
   end record;
   procedure Initialized (Result : in out LDAP_Result);
   function Get_Code (Result : LDAP_Result) return Result_Code;
   function Get_Matched (Result : LDAP_Result)
      return Distinguished_Name;
   function Get_Message (Result : LDAP_Result) return String;
   function Get_Referral (Result : LDAP_Result) return Values_List;

   type LDAP_SASL_Credentials is
      new Implicit_Tagged_Sequence_Data_Item with
   record
      Mechanism   : LDAP_Implicit_String;
      Credentials : Octet_String;
   end record;
   procedure Initialized (Credentials : in out LDAP_SASL_Credentials);

   type LDAP_Authentication_Choice is
      new ASN1.Choices.Choice_Data_Item with
   record
      Simple : LDAP_Implicit_String;
      SASL   : LDAP_SASL_Credentials;
   end record;
   procedure Initialized (Choice : in out LDAP_Authentication_Choice);

   type LDAP_Bind_Request is new Implicit_Sequence_Data_Item with record
      Version        : ASN1.Integers_8.Integer_Data_Item;
      Name           : LDAP_DN;
      Authentication : LDAP_Authentication_Choice;
   end record;

   type LDAP_Bind_Response is new LDAP_Result with record
      Credentials : Octet_String;
   end record;
   procedure Initialized (Response : in out LDAP_Bind_Response);

   type LDAP_Compare_Request is
      new Implicit_Sequence_Data_Item with
   record
      Entry_Item                : LDAP_DN;
      Attribute_Value_Assertion : LDAP_Attribute_Value_Assertion;
   end record;

   type LDAP_Extended_Request is
      new Implicit_Tagged_Sequence_Data_Item with
   record
      Name  : LDAP_OID;
      Value : Octet_String;
   end record;
   procedure Initialized (Request : in out LDAP_Extended_Request);

   type LDAP_Extended_Response is new LDAP_Result with record
      Name  : LDAP_OID;
      Value : Octet_String;
   end record;
   procedure Initialized (Response : in out LDAP_Extended_Response);

   type Operation_Type is mod 2**8;
   Add_Operation     : constant Operation_Type := 0;
   Delete_Operation  : constant Operation_Type := 1;
   Replace_Operation : constant Operation_Type := 2;
   package LDAP_Operations is
      new ASN1.Generic_Enumeration (Operation_Type);

   type LDAP_Modification is new Sequence_Data_Item with record
      Operation    : LDAP_Operations.Enumeration_Data_Item;
      Modification : LDAP_Attribute;
   end record;
   function Get (Item : LDAP_Modification) return Operation_Type;
   procedure Set
             (  Item      : in out LDAP_Modification;
                Operation : Operation_Type
             );
--
-- LDAP_Operations_List -- Sequence of LDAP_Attribute
--
   package LDAP_Operations_Sequences is
      new ASN1.Sequences.Generic_Sequence_Of
          (  Value_Type   => Operation_Type,
             Element_Type => LDAP_Modification
          );
   type LDAP_Operations_List is
      new LDAP_Operations_Sequences.Sequence_Of with null record;
   procedure Append
             (  List        : in out LDAP_Operations_List;
                Operation   : Operation_Type;
                Description : String
             );
   procedure Append
             (  List        : in out LDAP_Operations_List;
                Operation   : Operation_Type;
                Description : String;
                Value       : String
             );
   procedure Append_Value
             (  List  : in out LDAP_Operations_List;
                Value : String
             );
   type LDAP_Modify_Request is
      new Implicit_Sequence_Data_Item with
   record
      Object  : LDAP_DN;
      Changes : LDAP_Operations_List;
   end record;

   type LDAP_Modify_DN_Request is
      new Implicit_Tagged_Sequence_Data_Item with
   record
      Entry_Item     : LDAP_Implicit_DN;
      New_RDN        : LDAP_Implicit_DN;
      Delete_Old_RDN : ASN1.Booleans.Implicit_Boolean_Data_Item;
      New_Superior   : LDAP_Implicit_DN;
   end record;
   procedure Initialized (Request : in out LDAP_Modify_DN_Request);

   type Scope_Type is mod 2**8;
   Base_Object_Scope   : constant Scope_Type := 0;
   Single_Level_Scope  : constant Scope_Type := 1;
   Whole_Subtree_Scope : constant Scope_Type := 2;
   Subordinate_Subtree : constant Scope_Type := 3;
   package LDAP_Scopes is new ASN1.Generic_Enumeration (Scope_Type);

   type Dereference_Type is mod 2**8;
   Never_Deref_Aliases    : constant Dereference_Type := 0;
   Deref_In_Searching     : constant Dereference_Type := 1;
   Deref_Finding_Base_Obj : constant Dereference_Type := 2;
   Deref_Always           : constant Dereference_Type := 3;
   package LDAP_Derefs is
      new ASN1.Generic_Enumeration (Dereference_Type);

   type LDAP_Matching_Rule_Assertion is
      new Implicit_Tagged_Sequence_Data_Item with
   record
      Rule        : LDAP_Implicit_String;
      Description : LDAP_Implicit_String;
      Value       : LDAP_Assertion_Value;
      Attributes  : ASN1.Booleans.Implicit_Boolean_Data_Item;
   end record;
   procedure Initialized (Rule : in out LDAP_Matching_Rule_Assertion);

   type LDAP_Substring is new ASN1.Choices.Choice_Data_Item with record
      Initial : LDAP_Assertion_Value;
      Any     : LDAP_Assertion_Value;
      Final   : LDAP_Assertion_Value;
   end record;
   procedure Initialized (Choice : in out LDAP_Substring);
   function Get_Value (Item : LDAP_Substring) return String;
   procedure Set_Value (Item : in out LDAP_Substring; Value : String);
--
-- LDAP_Substring_Set -- Set of LDAP_Substring
--
   package LDAP_Substring_Sequences is
      new ASN1.Sequences.Generic_Sequence_Of
          (  Value_Type   => String,
             Element_Type => LDAP_Substring,
             Get          => Get_Value,
             Set          => Set_Value
          );
--
-- Substring_Component_Type -- Substring location in the string
--
   type Substring_Component_Type is
        (  Initial_Component, -- Anchored to the string beginning
           Any_Component,     -- Any string location
           Final_Component    -- Anchored to the string end
        );
--
-- Comparison_Type -- Match operation <=, =, >=
--
   type Comparison_Type is
        (  Less_Or_Equal,
           Equal,
           Approximately_Equal,
           Greater_Or_Equal
        );

   type LDAP_Substring_Sequence is
      new LDAP_Substring_Sequences.Sequence_Of with null record;
   procedure Append
             (  List      : in out LDAP_Substring_Sequence;
                Component : Substring_Component_Type;
                Value     : String
             );

   type LDAP_Substring_Filter is
     new Implicit_Sequence_Data_Item with
   record
      Description : LDAP_String;
      Values      : LDAP_Substring_Sequence;
   end record;

   type LDAP_Filter;
   type LDAP_Filter_Ptr is access all LDAP_Filter;
   type LDAP_Filter_Set is
      new Implicit_External_Set_Of_Data_Item with null record;
   function Create
            (  Item : access LDAP_Filter_Set
            )  return ASN1.Abstract_ASN1_Data_Item_Ptr;
   function Get_Filter
            (  Item  : LDAP_Filter_Set;
               Index : Positive
            )  return LDAP_Filter_Ptr;
   type LDAP_Filter_Reference is
      new ASN1.Sets.Reference_Data_Item with null record;
   function Create
            (  Item : access LDAP_Filter_Reference
            )  return ASN1.Abstract_ASN1_Data_Item_Ptr;
   function Get_Filter
            (  Item : LDAP_Filter_Reference
            )  return LDAP_Filter_Ptr;
   type LDAP_Filter_Item is new Implicit_Sequence_Data_Item with record
      Filter : LDAP_Filter_Reference;
   end record;
   function Get_Filter
            (  Item  : LDAP_Filter_Item
            )  return LDAP_Filter_Ptr;

   And_Filter_Choice       : constant :=  1;
   Or_Filter_Choice        : constant :=  2;
   Not_Filter_Choice       : constant :=  3;
   Equality_Match_Choice   : constant :=  4;
   Substrings_Choice       : constant :=  5;
   Greater_Or_Equal_Choice : constant :=  6;
   Less_Or_Equal_Choice    : constant :=  7;
   Present_Choice          : constant :=  8;
   Approx_Match_Choice     : constant :=  9;
   Extensible_Match_Choice : constant := 10;
------------------------------------------------------------------------
--
-- LDAP_Filter -- Filter used in search requests
--
-- The filters are recuirsive.  A filter may contain other filters bound
-- in logical conjuctive,  disjunctive,  negation forms.  The operations
-- defined  on filters allow building  a complex filter  in the external
-- string buffer.
--
   type LDAP_Filter is new ASN1.Choices.Choice_Data_Item with record
      And_Filter       : LDAP_Filter_Set;
      Or_Filter        : LDAP_Filter_Set;
      Not_Filter       : LDAP_Filter_Item;
      Equality_Match   : LDAP_Implicit_Attribute_Value_Assertion;
      Substrings       : LDAP_Substring_Filter;
      Greater_Or_Equal : LDAP_Implicit_Attribute_Value_Assertion;
      Less_Or_Equal    : LDAP_Implicit_Attribute_Value_Assertion;
      Present          : LDAP_Implicit_String;
      Approx_Match     : LDAP_Implicit_Attribute_Value_Assertion;
      Extensible_Match : LDAP_Matching_Rule_Assertion;
   end record;
--
-- Append -- A new component to the substring match filter
--
--    Filter    - The filter
--    Component - The location of the substring in the string
--    Value     - The substring to match
--
-- Exceptions :
--
--    Use_Error - The filter type is not substring match
--
   procedure Append
             (  Filter    : in out LDAP_Filter;
                Component : Substring_Component_Type;
                Value     : String
             );
--
-- Append_Term -- A new conjunctive or disjunctive term filter
--
--    Filter - The filter
--
-- Returns :
--
--    The new term
--
-- Exceptions :
--
--    Use_Error - The filter type is not conjunction or disjunction
--
   function Append_Term
            (  Filter : access LDAP_Filter
            )  return LDAP_Filter_Ptr;
--
-- Conjunction_Size -- The number of conjunctive terms
--
--    Filter - The filter
--
-- Returns :
--
--    The number of terms
--
-- Exceptions :
--
--    Use_Error - The filter type is not conjunction
--
   function Conjunction_Size (Filter : LDAP_Filter) return Natural;
--
-- Disjunction_Size -- The number of disjunctive terms
--
--    Filter - The filter
--
-- Returns :
--
--    The number of terms
--
-- Exceptions :
--
--    Use_Error - The filter type is not disjunction
--
   function Disjunction_Size (Filter : LDAP_Filter) return Natural;
--
-- Initialized -- Called at the end of enumeration of the components
--
   procedure Initialized (Choice : in out LDAP_Filter);
--
-- Get_Conjunction_Term -- A term
--
--    Filter - The filter
--    Index  - The term number 1..Conjunction_Size
--
-- Returns :
--
--    The term
--
-- Exceptions :
--
--    Constraint_Error - Index in not in range 1..Conjunction_Size
--    Use_Error        - The filter type is not conjunction
--
   function Get_Conjunction_Term
            (  Filter : LDAP_Filter;
               Index  : Positive
            )  return LDAP_Filter_Ptr;
--
-- Get_Disjunction_Term -- A term
--
--    Filter - The filter
--    Index  - The term number 1..Disjunction_Size
--
-- Returns :
--
--    The term
--
-- Exceptions :
--
--    Constraint_Error - Index in not in range 1..Disjunction_Size
--    Use_Error        - The filter type is not disjunction
--
   function Get_Disjunction_Term
            (  Filter : LDAP_Filter;
               Index  : Positive
            )  return LDAP_Filter_Ptr;
--
-- Get_Negation_Term -- The term
--
--    Filter - The filter
--
-- Returns :
--
--    The term
--
-- Exceptions :
--
--    Use_Error - The filter type is not negation
--
   function Get_Negation_Term
            (  Filter : LDAP_Filter
            )  return LDAP_Filter_Ptr;
--
-- Set -- The presence match
--
--    Filter  - The filter
--    Present - The attribute description to match to present
--
-- This procedure sets the filter to match presence of an attribute
--
   procedure Set
             (  Filter  : in out LDAP_Filter;
                Present : String
             );
--
-- Set -- The comparison
--
--    Filter      - The filter
--    Description - The attribute description to match
--    Comparison  - The comparison operation <=, =, >=
--    Value       - The attribute value to compare with
--
-- This procedure sets the filter to compare an attribute value with the
-- given value using an ordering operation.
--
   procedure Set
             (  Filter      : in out LDAP_Filter;
                Description : String;
                Comparison  : Comparison_Type;
                Value       : String
             );
--
-- Set -- The substring match
--
--    Filter      - The filter
--    Description - The attribute description to match
--    Component   - The substring anchor (beginning, none, end)
--    Value       - The substring to find in the attribute value
--
-- This procedure sets the filter to search an substring in an attribute
-- value. The search can be anchored to a string end or free. Further
-- substrings are added using Append.
--
   procedure Set
             (  Filter      : in out LDAP_Filter;
                Description : String;
                Component   : Substring_Component_Type;
                Value       : String
             );
--
-- Set -- The extensible match
--
--    Filter      - The filter
--    Value       - The assertion value for the search
--    Rule        - The rule type
--    Description - The matching rule attribute description
--    Attributes  - Match attribute values
--
-- This  procedure  sets  the  filter  to  use  extensible  match.  Rule
-- specifies  the  name or OID of the matching rule that should be used.
-- If  empty then Description must be specified, and the search will use
-- the equality matching rule for that attribute. Description if present
-- specifies  the attribute description for the attribute.  If then Rule
-- must  be  specified,  and  the  search  will be evaluated against any
-- attribute in the entry for which that matching rule may be used.  The
-- parameter  Attributes  indicates  whether to perform matching against
-- attribute-value pairs included in the entry's DN. If this element  is
-- present  with a value of true, then the filter will match an entry in
-- which  any of the attribute values matches the assertion, or in which
-- any of the attribute values that make up  any  component  of  the  DN
-- matches the assertion. Otherwise, the filter will only  be  evaluated
-- against  the entry attributes but not those attribute values included
-- in  its  DN  (except  for the attribute values in the RDN, which will
-- also be  in  the  entry  attributes).  The  parameter  Value  is  the
-- assertion value for the search.
--
-- Exceptions :
--
--    Use_Error - Both Rule and Description are empty
--
   procedure Set
             (  Filter      : in out LDAP_Filter;
                Value       : String;
                Rule        : String;
                Description : String  := "";
                Attributes  : Boolean := False
             );
--
-- Set_Conjunction -- The conjunction of filters
--
--    Filter - The filter
--
-- This procedure sets the filter to a conjunction of filters. All terms
-- must match. Further terms are added using Append_Term. The first term
-- is added and returned.  If called  consecutively each call adds a new
-- term. The procedure variant does not add any terms.
--
-- Returns :
--
--    The added conjunction term
--
   function Set_Conjunction
            (  Filter : access LDAP_Filter
            )  return LDAP_Filter_Ptr;
   procedure Set_Conjunction (Filter : in out LDAP_Filter);
--
-- Set_Disjunction -- The disjunction of filters
--
--    Filter - The filter
--
-- This procedure sets  the filter to  a disjunction of filters.  Any of
-- the terms must match. Further terms are added using Append_Term.  The
-- term is added and returned. If called  consecutively each call adds a
-- new term. The procedure variant does not add any terms.
--
-- Returns :
--
--    The added disjunction term
--
   function Set_Disjunction
            (  Filter : access LDAP_Filter
            )  return LDAP_Filter_Ptr;
   procedure Set_Disjunction (Filter : in out LDAP_Filter);
--
-- Set_Negation -- The filter negation
--
--    Filter - The filter
--
-- This procedure  sets  the filter to  the negation  of  a filter.  The
-- filter must not match.
--
-- Returns :
--
--    The first disjunction term
--
   function Set_Negation
            (  Filter : access LDAP_Filter
            )  return LDAP_Filter_Ptr;
------------------------------------------------------------------------
-- Filter construction operations
--
   type Search_Filter    (<>) is private;
   type Substring_Filter (<>) is private;
--
-- Comparisons =, <=, >=, Like
--
--    "givenName" = "John"
--
   function "="   (Description, Value : String) return Search_Filter;
   function "<="  (Description, Value : String) return Search_Filter;
   function ">="  (Description, Value : String) return Search_Filter;
   function Like  (Description, Value : String) return Search_Filter;
--
-- Attribute presense check
--
   function Present (Description : String) return Search_Filter;
--
-- Substring match:
--
--    "givenName" = Prefix ("Jo") / Suffix ("n")
--
   function Prefix    (Value : String) return Substring_Filter;
   function Substring (Value : String) return Substring_Filter;
   function Suffix    (Value : String) return Substring_Filter;

   function "="
            (  Description : String;
               Substrings  : Substring_Filter
            )  return Search_Filter;
   function "/" (Left, Right : Substring_Filter)
      return Substring_Filter;

   function Extended
            (  Value       : String;
               Rule        : String;
               Description : String  := "";
               Attributes  : Boolean := False
            )  return Search_Filter;
--
-- Logical lattice
--
   function Failure return Search_Filter;
   function Success return Search_Filter;
   function "and" (Left, Right : Search_Filter) return Search_Filter;
   function "or"  (Left, Right : Search_Filter) return Search_Filter;
   function "not" (Left : Search_Filter) return Search_Filter;
------------------------------------------------------------------------
   type LDAP_Intermediate_Response is
      new Implicit_Tagged_Sequence_Data_Item with
   record
      Name  : LDAP_OID;
      Value : Octet_String;
   end record;
   procedure Initialized
             (  Response : in out LDAP_Intermediate_Response
             );

   type LDAP_Search_Request is
      new Implicit_Sequence_Data_Item with
   record
      Base_Object   : LDAP_DN;
      Scope         : LDAP_Scopes.Enumeration_Data_Item;
      Deref_Aliases : LDAP_Derefs.Enumeration_Data_Item;
      Size_Limit    : ASN1.Integers_32.Integer_Data_Item;
      Time_Limit    : ASN1.Integers_32.Integer_Data_Item;
      Types_Only    : ASN1.Booleans.Boolean_Data_Item;
      Filter        : aliased LDAP_Filter;
      Attributes    : LDAP_String_Sequence;
   end record;

   type LDAP_Search_Result_Entry is
      new Implicit_Sequence_Data_Item with
   record
      Object_Name : LDAP_DN;
      Attributes  : LDAP_Attribute_List;
   end record;

   subtype LDAP_Abandon_Request is
           ASN1.Integers_32.Implicit_Integer_Data_Item;

   Bind_Request_Choice            : constant := 1;
   Bind_Response_Choice           : constant := 2;
   Unbind_Request_Choice          : constant := 3;
   Search_Request_Choice          : constant := 4;
   Search_Result_Entry_Choice     : constant := 5;
   Search_Result_Done_Choice      : constant := 6;
   Search_Result_Reference_Choice : constant := 7;
   Modify_Request_Choice          : constant := 8;
   Modify_Response_Choice         : constant := 9;
   Add_Request_Choice             : constant := 10;
   Add_Response_Choice            : constant := 11;
   Delete_Request_Choice          : constant := 12;
   Delete_Response_Choice         : constant := 13;
   Modify_DN_Request_Choice       : constant := 14;
   Modify_DN_Response_Choice      : constant := 15;
   Compare_Request_Choice         : constant := 16;
   Compare_Response_Choice        : constant := 17;
   Abandon_Request_Choice         : constant := 18;
   Extended_Request_Choice        : constant := 19;
   Extended_Response_Choice       : constant := 20;
   Intermediate_Response_Choice   : constant := 21;

   type LDAP_Operation is new ASN1.Choices.Choice_Data_Item with record
      Bind_Request            : LDAP_Bind_Request;
      Bind_Response           : LDAP_Bind_Response;
      Unbind_Request          : ASN1.Nulls.Implicit_Null_Data_Item;
      Search_Request          : LDAP_Search_Request;
      Search_Result_Entry     : LDAP_Search_Result_Entry;
      Search_Result_Done      : LDAP_Result;
      Search_Result_Reference : LDAP_Implicit_String_Sequence;
      Modify_Request          : LDAP_Modify_Request;
      Modify_Response         : LDAP_Result;
      Add_Request             : LDAP_Add_Request;
      Add_Response            : LDAP_Result;
      Delete_Request          : LDAP_Implicit_DN;
      Delete_Response         : LDAP_Result;
      Modify_DN_Request       : LDAP_Modify_DN_Request;
      Modify_DN_Response      : LDAP_Result;
      Compare_Request         : LDAP_Compare_Request;
      Compare_Response        : LDAP_Result;
      Abandon_Request         : LDAP_Abandon_Request;
      Extended_Request        : LDAP_Extended_Request;
      Extended_Response       : LDAP_Extended_Response;
      Intermediate_Response   : LDAP_Intermediate_Response;
   end record;
   procedure Initialized (Operation : in out LDAP_Operation);

   type LDAP_Control is
      new Implicit_Tagged_Sequence_Data_Item with
   record
      Control_Type  : LDAP_OID;
      Criticality   : ASN1.Booleans.Boolean_Data_Item;
      Control_Value : Octet_String;
   end record;
   procedure Initialized (Control : in out LDAP_Control);
------------------------------------------------------------------------
--
-- LDAP_Message -- Message to send or receive
--
   type LDAP_Message is new Tagged_Sequence_Data_Item with record
      Message_ID  : ASN1.Integers_32.Implicit_Integer_Data_Item;
      Protocol_Op : LDAP_Operation;
      Control     : LDAP_Control;
   end record;
   procedure Initialized (Message : in out LDAP_Message);
--
-- Get_Search_Request_Filter -- Get the filter used in the search
--                              request
--
--    Message - The LDAP message
--
-- Returns :
--
--    The filter
--
-- Exceptions :
--
--    Use_Error - Search is not current request set
--
   function Get_Search_Request_Filter
            (  Message : LDAP_Message
            )  return LDAP_Filter_Ptr;
--
-- Set_Abandon_Request -- Set add request into the message
--
--    Message - The LDAP message
--    ID      - The ID of the request to abandon
--
   procedure Set_Abandon_Request
             (  Message : in out LDAP_Message;
                ID      : Integer_32
             );
--
-- Set_Add_Attribute -- Append attribute to the add request
--
--    Message     - The LDAP message
--    Description - The attribute description
--    Value       - The (first) attribute value to add
--
-- This   procedure    can   be    called   after   Set_Add_Request   or
-- Set_Search_Result_Entry to add another attribute to the request.
--
-- Exceptions :
--
--    Use_Error - Add request or  search result entry  is not set in the
--                message
--
   procedure Set_Add_Attribute
             (  Message     : in out LDAP_Message;
                Descrpition : String;
                Value       : String := ""
             );
--
-- Set_Add_Request -- Set add request into the message
--
--    Message     - The LDAP message
--    Name        - The entry name to add
--    Description - The attribute description
--    Value       - The (first) attribute value to add
--
-- This procedure adds  the first attribute value if Value is not empty.
-- Further  values  can   be  added  using   Set_Attribute_Value.   More
-- attributes can be added using Set_Add_Attribute.
--
   procedure Set_Add_Request
             (  Message     : in out LDAP_Message;
                Name        : Distinguished_Name;
                Descrpition : String;
                Value       : String := ""
             );
--
-- Set_Add_Request -- Set add request into the message
--
--    Message    - The LDAP message
--    Name       - The entry name to add
--    Attributes - The list of attribute description with values list
--
   procedure Set_Add_Request
             (  Message    : in out LDAP_Message;
                Name       : Distinguished_Name;
                Attributes : Attributes_List
             );
--
-- Set_Add_Response -- Set add response into the message
--
--    Message     - The LDAP message
--    Code        - The result code
--    Matched     - The matched distinguished name
--    Diagnostics - The diagnostic message
--    Referral    - The referral values list
--
   procedure Set_Add_Response
             (  Message     : in out LDAP_Message;
                Code        : Result_Code;
                Matched     : Distinguished_Name := Null_Name;
                Diagnostics : String             := "";
                Referral    : Values_List        := Empty
             );
--
-- Set_Attribute_Value -- Append an attribute value
--
--    Message - The LDAP message
--    Value   - An attribute value to add
--
-- This procedure can be called after  to add another attribute value to
-- the last attribute of the add request or search result entry.
--
-- Exceptions :
--
--    Use_Error - Add request or  search result entry  is not set in the
--                message
--
   procedure Set_Attribute_Value
             (  Message : in out LDAP_Message;
                Value   : String
             );
--
-- Set_Bind_Request -- Set bind request into the message
--
--    Message  - The LDAP message
--    Name     - The user name
--    Password - The password
--
   procedure Set_Bind_Request
             (  Message  : in out LDAP_Message;
                Name     : Distinguished_Name;
                Password : String
             );
--
-- Set_Bind_Request -- Set bind request into the message
--
--    Message     - The LDAP message
--    Name        - The name to bind to
--    Mechanism   - The SASL mechanism to use
--    Credentials - The credentials if needed
--
   procedure Set_Bind_Request
             (  Message     : in out LDAP_Message;
                Name        : Distinguished_Name;
                Mechanism   : String;
                Credentials : String
             );
--
-- Set_Bind_Response -- Set bind response into the message
--
--    Message     - The LDAP message
--    Code        - The result code
--    Matched     - The matched distinguished name
--    Diagnostics - The diagnostic message
--    Credentials - The SASL credentials
--    Referral    - The referral values list
--
   procedure Set_Bind_Response
             (  Message     : in out LDAP_Message;
                Code        : Result_Code;
                Matched     : Distinguished_Name := Null_Name;
                Diagnostics : String             := "";
                Credentials : String             := "";
                Referral    : Values_List        := Empty
             );
--
-- Set_Compare_Request -- Set compare request into the message
--
--    Message     - The LDAP message
--    Name        - The entry name
--    Description - The attribute description
--    Value       - The attribute value
--
   procedure Set_Compare_Request
             (  Message     : in out LDAP_Message;
                Name        : Distinguished_Name;
                Description : String;
                Value       : String
             );
--
-- Set_Compare_Response -- Set compare response into the message
--
--    Message     - The LDAP message
--    Code        - The result code
--    Matched     - The matched distinguished name
--    Diagnostics - The diagnostic message
--    Referral    - The referral values list
--
   procedure Set_Compare_Response
             (  Message     : in out LDAP_Message;
                Code        : Result_Code;
                Matched     : Distinguished_Name := Null_Name;
                Diagnostics : String             := "";
                Referral    : Values_List        := Empty
             );
--
-- Set_Delete_Request -- Set delete request into the message
--
--    Message - The LDAP message
--    Name    - The entry name
--
   procedure Set_Delete_Request
             (  Message : in out LDAP_Message;
                Name    : Distinguished_Name
             );
--
-- Set_Delete_Response -- Set delete response into the message
--
--    Message     - The LDAP message
--    Code        - The result code
--    Matched     - The matched distinguished name
--    Diagnostics - The diagnostic message
--    Referral    - The referral values list
--
   procedure Set_Delete_Response
             (  Message     : in out LDAP_Message;
                Code        : Result_Code;
                Matched     : Distinguished_Name := Null_Name;
                Diagnostics : String             := "";
                Referral    : Values_List        := Empty
             );
--
-- Set_Extended_Request -- Set extended request into the message
--
--    Message - The LDAP message
--    Name    - The name
--    Value   - The value
--
   procedure Set_Extended_Request
             (  Message : in out LDAP_Message;
                Name    : Object_Identifier;
                Value   : String := ""
             );
--
-- Set_Extended_Response -- Set extended response into the message
--
--    Message     - The LDAP message
--    Code        - The result code
--    Matched     - The matched distinguished name
--    Diagnostics - The diagnostic message
--    Name        - The name
--    Value       - The value
--    Referral    - The referral values list
--
   procedure Set_Extended_Response
             (  Message     : in out LDAP_Message;
                Code        : Result_Code;
                Matched     : Distinguished_Name := Null_Name;
                Diagnostics : String             := "";
                Name        : Object_Identifier  := (1..0 => 0);
                Value       : String             := "";
                Referral    : Values_List        := Empty
             );
--
-- Set_Intermediate_Reponse -- Set intermediate response into the
--                             message
--
--    Message - The LDAP message
--    Name    - The name
--    Value   - The value
--
   procedure Set_Intermediate_Response
             (  Message : in out LDAP_Message;
                Name    : Object_Identifier := (1..0 => 0);
                Value   : String            := ""
             );
--
-- Set_Modify_Request -- Set modify request into the message
--
--    Message     - The LDAP message
--    Name        - The object to modify
--    Operation   - The operation to perform
--    Description - The attribute description
--    Value       - The (first) attribute value to add
--
   procedure Set_Modify_Request
             (  Message     : in out LDAP_Message;
                Name        : Distinguished_Name;
                Operation   : Operation_Type;
                Descrpition : String;
                Value       : String := ""
             );
--
-- Set_Modify_Request -- Set modify request into the message
--
--    Message - The LDAP message
--    Name    - The object to modify
--    Update  - The updates list
--
   procedure Set_Modify_Request
             (  Message : in out LDAP_Message;
                Name    : Distinguished_Name;
                Update  : Updates_List
             );
--
-- Set_Modify_Request_Add_Operation -- Append an operation to the modify
--                                     request
--    Message     - The LDAP message
--    Operation   - The operation to perform
--    Description - The attribute description
--    Value       - The (first) attribute value to add
--
-- This procedure can be called after  Set_Modify_Request to add another
-- operation to the request.
--
-- Exceptions :
--
--    Use_Error - Modify is not current request set in the message
--
   procedure Set_Modify_Request_Add_Operation
             (  Message     : in out LDAP_Message;
                Operation   : Operation_Type;
                Descrpition : String;
                Value       : String := ""
             );
--
-- Set_Modify_Request_Add_Operation -- Append an operation value
--
--    Message - The LDAP message
--    Value   - An attribute value to add
--
-- This  procedure   can   be   called   after   Set_Modify_Request   or
-- Set_Modify_Request_Add_Operation to  add another  attribute  value to
-- the last operation of the modify request.
--
-- Exceptions :
--
--    Use_Error - Modify is not current request set in the message
--
   procedure Set_Modify_Request_Add_Value
             (  Message : in out LDAP_Message;
                Value   : String
             );
--
-- Set_Modify_Response -- Set modify response into the message
--
--    Message     - The LDAP message
--    Code        - The result code
--    Matched     - The matched distinguished name
--    Diagnostics - The diagnostic message
--    Referral    - The referral values list
--
   procedure Set_Modify_Response
             (  Message     : in out LDAP_Message;
                Code        : Result_Code;
                Matched     : Distinguished_Name := Null_Name;
                Diagnostics : String             := "";
                Referral    : Values_List        := Empty
             );
--
-- Set_Modify_DN_Request -- Set modify DN request into the message
--
--    Message      - The LDAP message
--    Name         - The entry to modify
--    New_RDN      - The new relative distinguished name
--    Delete_RDN   - Delete any attribute values in the original
--    New_Superior - The new parent for the entry
--
   procedure Set_Modify_DN_Request
             (  Message      : in out LDAP_Message;
                Name         : Distinguished_Name;
                New_RDN      : Distinguished_Name;
                Delete_RDN   : Boolean;
                New_Superior : Distinguished_Name
             );
   procedure Set_Modify_DN_Request
             (  Message      : in out LDAP_Message;
                Name         : Distinguished_Name;
                New_RDN      : Distinguished_Name;
                Delete_RDN   : Boolean
             );
--
-- Set_Modify_DN_Response -- Set modify DN response into the message
--
--    Message     - The LDAP message
--    Code        - The result code
--    Matched     - The matched distinguished name
--    Diagnostics - The diagnostic message
--    Referral    - The referral values list
--
   procedure Set_Modify_DN_Response
             (  Message     : in out LDAP_Message;
                Code        : Result_Code;
                Matched     : Distinguished_Name := Null_Name;
                Diagnostics : String             := "";
                Referral    : Values_List        := Empty
             );
--
-- Set_Response_Referral -- Set search result reference response
--
--    Message - The LDAP message
--    URL     - The URL to add
--
-- This procedure adds a referral URL to the response.
--
-- Exceptions :
--
--    Use_Error - A response is not set in the message
--
   procedure Set_Response_Referral
             (  Message : in out LDAP_Message;
                URL     : String
             );
--
-- Set_Response_Referral -- Set search result reference response
--
--    Message - The LDAP message
--    URLs    - The list of URLs to add
--
-- This procedure adds a referral URL to the response.
--
-- Exceptions :
--
--    Use_Error - A response is not set in the message
--
   procedure Set_Response_Referral
             (  Message : in out LDAP_Message;
                URLs    : Values_List
             );
--
-- Set_Search_Result_Done -- Set search result done into the message
--
--    Message     - The LDAP message
--    Code        - The result code
--    Matched     - The matched distinguished name
--    Diagnostics - The diagnostic message
--    Referral    - The referral values list
--
   procedure Set_Search_Result_Done
             (  Message     : in out LDAP_Message;
                Code        : Result_Code;
                Matched     : Distinguished_Name := Null_Name;
                Diagnostics : String             := "";
                Referral    : Values_List        := Empty
             );
--
-- Set_Search_Result_Entry -- Set search result entry response
--
--    Message     - The LDAP message
--    Name        - The entry to add
--    Description - The attribute description
--    Value       - The (first) attribute value to add
--
-- This procedure adds  the first attribute value if Value is not empty.
-- Further  values  can   be  added  using   Set_Attribute_Value.   More
-- attributes can be added using Set_Add_Attribute.
--
   procedure Set_Search_Result_Entry
             (  Message     : in out LDAP_Message;
                Name        : Distinguished_Name;
                Descrpition : String;
                Value       : String := ""
             );
--
-- Set_Search_Result_Entry -- Set search result into the message
--
--    Message    - The LDAP message
--    Name       - The entry to add
--    Attributes - The list of attribute description with values list
--
   procedure Set_Search_Result_Entry
             (  Message    : in out LDAP_Message;
                Name       : Distinguished_Name;
                Attributes : Attributes_List
             );
--
-- Set_Search_Result_Reference -- Set search result reference response
--
--    Message - The LDAP message
--    URI     - The (first) URI to add
--
-- This procedure adds the first URI to the result.  Further URIs can be
-- added using Set_Search_Result_Reference_URI.
--
   procedure Set_Search_Result_Reference
             (  Message : in out LDAP_Message;
                URI     : String
             );
--
-- Set_Search_Result_Reference -- Set search result reference response
--
--    Message - The LDAP message
--    URIs    - The URI's list
--
   procedure Set_Search_Result_Reference
             (  Message : in out LDAP_Message;
                URIs    : Values_List
             );
--
-- Set_Search_Result_Reference_URI -- Add URI to the search result
--
--    Message - The LDAP message
--    URI     - The URI to add
--
-- This procedure adds an URI to the result.
--
-- Exceptions :
--
--    Use_Error - Search result reference is not set in the message
--
   procedure Set_Search_Result_Reference_URI
             (  Message : in out LDAP_Message;
                URI     : String
             );
--
-- Set_Search_Request -- Set search request into the message
--
--    Message       - The LDAP message
--    Name          - To start the search at
--    Filter        - The filter to set
--    Scope         - Of the search relatively to Base_Object
--    Aliasing_Mode - The method to handle aliases
--    Size_Limit    - The maximum number of result entries
--    Time_Limit    - For processing the request
--    Types_Only    - Only include attribute descriptions
--
   procedure Set_Search_Request
             (  Message       : in out LDAP_Message;
                Name          : Distinguished_Name;
                Filter        : Search_Filter;
                Scope         : Scope_Type       := Whole_Subtree_Scope;
                Aliasing_Mode : Dereference_Type := Deref_Always;
                Size_Limit    : Integer_32       := 0;
                Time_Limit    : Duration         := 0.0;
                Types_Only    : Boolean          := False
             );
--
-- Set_Search_Request -- Set search request into the message
--
--    Message       - The LDAP message
--    Name          - To start the search at
--    Present       - Attribute to be present in order to match
--    Scope         - Of the search relatively to Base_Object
--    Aliasing_Mode - The method to handle aliases
--    Size_Limit    - The maximum number of result entries
--    Time_Limit    - For processing the request
--    Types_Only    - Only include attribute descriptions
--
   procedure Set_Search_Request
             (  Message       : in out LDAP_Message;
                Name          : Distinguished_Name;
                Present       : String;
                Scope         : Scope_Type       := Whole_Subtree_Scope;
                Aliasing_Mode : Dereference_Type := Deref_Always;
                Size_Limit    : Integer_32       := 0;
                Time_Limit    : Duration         := 0.0;
                Types_Only    : Boolean          := False
             );
--
-- Set_Search_Request -- Set search request into the message
--
--    Message       - The LDAP message
--    Name          - To start the search at
--    Description   - The descrption of an attribute to compare
--    Comparison    - <=, =, >=
--    Value         - The attribute value
--    Scope         - Of the search relatively to Base_Object
--    Aliasing_Mode - The method to handle aliases
--    Size_Limit    - The maximum number of result entries
--    Time_Limit    - For processing the request
--    Types_Only    - Only include attribute descriptions
--
   procedure Set_Search_Request
             (  Message       : in out LDAP_Message;
                Name          : Distinguished_Name;
                Description   : String;
                Comparison    : Comparison_Type;
                Value         : String;
                Scope         : Scope_Type       := Whole_Subtree_Scope;
                Aliasing_Mode : Dereference_Type := Deref_Always;
                Size_Limit    : Integer_32       := 0;
                Time_Limit    : Duration         := 0.0;
                Types_Only    : Boolean          := False
             );
--
-- Set_Search_Request -- Set search request into the message
--
--    Message       - The LDAP message
--    Name          - To start the search at
--    Description   - The descrption of an attribute to compare
--    Component     - The substring component to compare
--    Value         - The substring value
--    Scope         - Of the search relatively to Base_Object
--    Aliasing_Mode - The method to handle aliases
--    Size_Limit    - The maximum number of result entries
--    Time_Limit    - For processing the request
--    Types_Only    - Only include attribute descriptions
--
   procedure Set_Search_Request
             (  Message       : in out LDAP_Message;
                Name          : Distinguished_Name;
                Description   : String;
                Component     : Substring_Component_Type;
                Value         : String;
                Scope         : Scope_Type       := Whole_Subtree_Scope;
                Aliasing_Mode : Dereference_Type := Deref_Always;
                Size_Limit    : Integer_32       := 0;
                Time_Limit    : Duration         := 0.0;
                Types_Only    : Boolean          := False
             );
--
-- Set_Search_Request -- Set search request into the message
--
--    Message       - The LDAP message
--    Name          - To start the search at
--    Value         - The assertion value for the search
--    Rule          - The rule type
--    Description   - The matching rule attribute description
--    Attributes    - Match attribute values
--    Scope         - Of the search relatively to Base_Object
--    Aliasing_Mode - The method to handle aliases
--    Size_Limit    - The maximum number of result entries
--    Time_Limit    - For processing the request
--    Types_Only    - Only include attribute descriptions
--
   procedure Set_Search_Request
             (  Message       : in out LDAP_Message;
                Name          : Distinguished_Name;
                Value         : String;
                Rule          : String;
                Description   : String           := "";
                Attributes    : Boolean          := False;
                Scope         : Scope_Type       := Whole_Subtree_Scope;
                Aliasing_Mode : Dereference_Type := Deref_Always;
                Size_Limit    : Integer_32       := 0;
                Time_Limit    : Duration         := 0.0;
                Types_Only    : Boolean          := False
             );
--
-- Set_Search_Request_Attribute -- Add attribute to the set search
--                                 request
--    Message   - The LDAP message
--    Attribute - To add
--
-- The procedure does not create terms.
--
-- Returns :
--
--    The filter's conjunctive term added by the call
--
-- Exceptions :
--
--    Use_Error - Search is not the current request
--
   procedure Set_Search_Request_Attribute
             (  Message   : in out LDAP_Message;
                Attribute : String
             );
--
-- Set_Search_Request_Conjunction -- Set search request into a
--                                   conjunction of filters
--
--    Message       - The LDAP message
--    Name          - To start the search at
--    Scope         - Of the search relatively to Base_Object
--    Aliasing_Mode - The method to handle aliases
--    Size_Limit    - The maximum number of result entries
--    Time_Limit    - For processing the request
--    Types_Only    - Only include attribute descriptions
--
-- The procedure  does  not create terms.  An empty  conjunction  always
-- matches (RFC 4526).
--
-- Returns :
--
--    The filter's conjunctive term added by the call
--
   function Set_Search_Request_Conjunction
            (  Message       : LDAP_Message;
               Name          : Distinguished_Name;
               Scope         : Scope_Type       := Whole_Subtree_Scope;
               Aliasing_Mode : Dereference_Type := Deref_Always;
               Size_Limit    : Integer_32       := 0;
               Time_Limit    : Duration         := 0.0;
               Types_Only    : Boolean          := False
            )  return LDAP_Filter_Ptr;
   procedure Set_Search_Request_Conjunction
             (  Message       : in out LDAP_Message;
                Name          : Distinguished_Name;
                Scope         : Scope_Type       := Whole_Subtree_Scope;
                Aliasing_Mode : Dereference_Type := Deref_Always;
                Size_Limit    : Integer_32       := 0;
                Time_Limit    : Duration         := 0.0;
                Types_Only    : Boolean          := False
             );
--
-- Set_Search_Request_Disjunction -- Set search request into a
--                                   disjunction of filters
--
--    Message       - The LDAP message
--    Name          - To start the search at
--    Term          - The filter's disjunctive term added by the call
--    Scope         - Of the search relatively to Base_Object
--    Aliasing_Mode - The method to handle aliases
--    Size_Limit    - The maximum number of result entries
--    Time_Limit    - For processing the request
--    Types_Only    - Only include attribute descriptions
--
-- The procedure  does  not  create terms.  An empty  disjunction  never
-- matches (RFC 4526).
--
-- Returns :
--
--    The filter's conjunctive term added by the call
--
   function Set_Search_Request_Disjunction
            (  Message       : LDAP_Message;
               Name          : Distinguished_Name;
               Scope         : Scope_Type       := Whole_Subtree_Scope;
               Aliasing_Mode : Dereference_Type := Deref_Always;
               Size_Limit    : Integer_32       := 0;
               Time_Limit    : Duration         := 0.0;
               Types_Only    : Boolean          := False
            )  return LDAP_Filter_Ptr;
   procedure Set_Search_Request_Disjunction
             (  Message       : in out LDAP_Message;
                Name          : Distinguished_Name;
                Scope         : Scope_Type       := Whole_Subtree_Scope;
                Aliasing_Mode : Dereference_Type := Deref_Always;
                Size_Limit    : Integer_32       := 0;
                Time_Limit    : Duration         := 0.0;
                Types_Only    : Boolean          := False
             );
--
-- Set_Search_Request_Negation -- Set search request into a filter
--                                negation
--
--    Message       - The LDAP message
--    Name          - To start the search at
--    Term          - The filter
--    Scope         - Of the search relatively to Base_Object
--    Aliasing_Mode - The method to handle aliases
--    Size_Limit    - The maximum number of result entries
--    Time_Limit    - For processing the request
--    Types_Only    - Only include attribute descriptions
--
-- Returns :
--
--    The filter
--
   function Set_Search_Request_Negation
            (  Message       : LDAP_Message;
               Name          : Distinguished_Name;
               Scope         : Scope_Type       := Whole_Subtree_Scope;
               Aliasing_Mode : Dereference_Type := Deref_Always;
               Size_Limit    : Integer_32       := 0;
               Time_Limit    : Duration         := 0.0;
               Types_Only    : Boolean          := False
            )  return LDAP_Filter_Ptr;
--
-- Set_Search_Request_Substring -- Set a substring  into search  request
--                                 into the message
--
--    Message   - The LDAP message
--    Component - The substring component to compare
--    Value     - The substring value
--
-- Exceptions :
--
--    Use_Error - Search is not current request set or the filter is not
--                substring
--
   procedure Set_Search_Request_Substring
             (  Message   : in out LDAP_Message;
                Component : Substring_Component_Type;
                Value     : String
             );
--
-- Set_Unbind_Request -- Set unbind request into the message
--
--    Message - The LDAP message
--
   procedure Set_Unbind_Request (Message : in out LDAP_Message);
------------------------------------------------------------------------
--
-- LDAP_Output -- LDAP outgoing messages
--
--    Message_Length - The maximum LDAP message length
--    Size           - The  buffer  length   to  allocate  dynamic  LDAP
--                     objects and string bodies
--
   type LDAP_Output
        (  Message_Length : Stream_Element_Count;
           Size           : Positive
        )  is new Ada.Finalization.Limited_Controlled with
   record
      Message : LDAP_Message;
      Request : Stream_Element_Array (1..Message_Length);
      Buffer  : aliased External_String_Buffer (Size);
   end record;
   procedure Initialize (Item : in out LDAP_Output);
   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : LDAP_Output
             );
   for LDAP_Output'Write use Write;
------------------------------------------------------------------------
--
-- LDAP_Peer -- LDAP server or client
--
--    Listener           - The connection object
--    Message_Length     - The maximum output LDAP message length
--    Incoming_Data_Size - The arena pool size  to allocate incoming
--                         dynamic LDAP objects and string bodies
--    Outgoing_Data_Size - The arena pool size for outgoing LDAP objects
--    Input_Size         - The input buffer size
--    Output_Size        - The output buffer size
--
-- Message_Length must be larger than the encoded length of any outgoing
-- LDAP message.  Outgoing_Data_Size  determines  the size  of the arena
-- pool where are variable-length  values allocated.  This includes LDAP
-- objects  defined recursively or as variable-sized sets and sequences.
-- The arena must accommodate  data for any outgoing LDAP  message built
-- there before getting encoded. Incoming_Data_Size  determines the size
-- of the arena pool for the incoming LDAP message.
--
   type LDAP_Peer
        (  Listener           : access Connections_Server'Class;
           Message_Length     : Buffer_Length;
           Incoming_Data_Size : Positive;
           Outgoing_Data_Size : Positive;
           Input_Size         : Buffer_Length;
           Output_Size        : Buffer_Length
        )  is new State_Machine
                  (  Input_Size  => Input_Size,
                     Output_Size => Output_Size
                  )  with
   record
         -- Incoming message
      Buffer  : External_String_Buffer (Incoming_Data_Size);
      Message : LDAP_Message;
         -- Outgoing message
      Output : LDAP_Output (Message_Length, Outgoing_Data_Size);
   end record;
--
-- Send_Request -- Send the request set in the output buffer
--
--    Peer - The LDAP peer
--    ID   - The request ID
--
-- Exceptions :
--
--    Data_Error   - No place in the outgoing buffer
--    Socket_Error - I/O error
--
   procedure Send_Request
             (  Peer : in out LDAP_Peer;
                ID   : Integer_32
             );
--
-- Reply_Response -- Send the reply set in the output buffer
--
--    Peer - The LDAP peer
--
-- Exceptions :
--
--    Data_Error   - No place in the outgoing buffer
--    Socket_Error - I/O error
--
   procedure Reply_Response (Peer : in out LDAP_Peer);
private
   type Abstract_Node is abstract new Object.Entity with null record;
   function Get_Size (Node : Abstract_Node) return Natural;
   type Abstract_Node_Ptr is access Abstract_Node'Class;

   package Node_Handles is
      new Object.Handle (Abstract_Node, Abstract_Node_Ptr);
   type Node_Array is array (Positive range <>) of Node_Handles.Handle;

   type Search_Filter is record -- Hiding taggedness
      Reference : Node_Handles.Handle;
   end record;
   type Substring_Filter is new Search_Filter;

   type Abstract_Parent_Node (Size : Natural) is abstract
      new Abstract_Node with
   record
      Children : Node_Array (1..Size);
   end record;
   function Get_Size (Node : Abstract_Parent_Node) return Natural;
   type And_Node is new Abstract_Parent_Node with null record;
   type Or_Node  is new Abstract_Parent_Node with null record;
   type Not_Node is new Abstract_Node with record
      Child : Node_Handles.Handle;
   end record;
   function Get_Size (Node : Not_Node) return Natural;

   type Abstract_Pair_Node
        (  Description_Length : Natural;
           Value_Length       : Natural
        )  is abstract new Abstract_Node with
   record
      Description : String (1..Description_Length);
      Value       : String (1..Value_Length);
   end record;

   type Comparison_Node
        (  Mode               : Comparison_Type;
           Description_Length : Natural;
           Value_Length       : Natural
        )  is new Abstract_Pair_Node
                  (  Description_Length => Description_Length,
                     Value_Length       => Value_Length
                  )  with null record;

   type Present_Node (Length : Natural) is
      new Abstract_Node with
   record
      Description : String (1..Length);
   end record;

   type Substring_Node
        (  Component : Substring_Component_Type;
           Length    : Natural
        )  is new Abstract_Node with
   record
      Substring : String (1..Length);
   end record;

   type Substring_Sequence_Node is
      new Abstract_Parent_Node with null record;

   type Substring_List_Node
        (  Description_Length : Natural;
           List_Length        : Natural
        )  is new Abstract_Parent_Node (List_Length) with
   record
      Description : String (1..Description_Length);
   end record;

   type Rule_Node
        (  Rule_Length        : Natural;
           Description_Length : Natural;
           Value_Length       : Natural
        )  is new Abstract_Pair_Node
                  (  Description_Length => Description_Length,
                     Value_Length       => Value_Length
                  )  with
   record
      Rule       : String (1..Rule_Length);
      Attributes : Boolean := False;
   end record;
   ---------------------------------------------------------------------
   type Attribute_Value (Length : Natural) is
      new Object.Entity with
   record
      Value : String (1..Length);
   end record;
   type Attribute_Value_Ptr is access Attribute_Value'Class;

   package Value_Handles is
      new Object.Handle (Attribute_Value, Attribute_Value_Ptr);
   function Create (Value : String) return Value_Handles.Handle;
   type Values_Array is
      array (Positive range <>) of Value_Handles.Handle;
   function Get (List : LDAP_String_Set'Class) return Values_Array;

   type Values_List (Size : Natural) is record
      List : Values_Array (1..Size);
   end record;

   type Attribute_Data
        (  Length : Natural;
           Size   : Natural
        )  is new Object.Entity with
   record
      Description : String (1..Length);
      List        : Values_Array (1..Size);
   end record;
   type Attribute_Data_Ptr is access Attribute_Data'Class;

   package Attribute_Data_Handles is
      new Object.Handle (Attribute_Data, Attribute_Data_Ptr);

   type Attribute_Definition is record
      Reference : Attribute_Data_Handles.Handle;
   end record;

   type Attribute_Data_Array is
      array (Positive range <>) of Attribute_Data_Handles.Handle;

   type Attributes_List (Size : Natural) is record
      List : Attribute_Data_Array (1..Size);
   end record;
   ---------------------------------------------------------------------
   type Update_Item is record
       Operation : Operation_Type;
       Attribute : Attribute_Definition;
   end record;
   type Update_Item_Array is array (Positive range <>) of Update_Item;
   type Updates_List (Size : Natural) is record
      List : Update_Item_Array (1..Size);
   end record;

end GNAT.Sockets.Connection_State_Machine.LDAP;
