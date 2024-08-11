with Ada.Containers;

function UXStrings.Hash (Key : UXString) return Ada.Containers.Hash_Type;
-- Return an implementation-defined value which is a function of the value of Key.
-- If A and B are strings such that A equals B then Hash(A) equals Hash(B).
