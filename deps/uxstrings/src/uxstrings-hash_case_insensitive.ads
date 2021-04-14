with Ada.Containers;

function UXStrings.Hash_Case_Insensitive (Key : UXString) return Ada.Containers.Hash_Type;
-- Returns an implementation-defined value which is a function of the value of Key, converted to lower case.
-- If A and B are strings such that Strings.Equal_Case_Insensitive (A, B) (see A.4.10) is True,
-- then Hash_Case_Insensitive(A) equals Hash_Case_Insensitive(B).
