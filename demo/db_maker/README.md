# DB_Maker
A generic for creating simple DBs (one table in an RDBMS) with PragmARC.Persistent_Skip_List_Unbounded and a Gnoga UI.

Searching does not use the O(log N) search of the underlying skip list, but instead does a linear search for fields that have the corresponding search text as a substring (case insensitive). This is much slower, but is still quite fast. If you have less than 100,000 records it should be fast enough. "Search" starts from the 1st record; "Search More" from the record after the last search. "Clear" clears the edit fields to make entering a new search easier.

Because searching is case insensitive and matches substrings, searching for "son" would match "Orson Welles", "Fowler De Johnsone", and "Son House".

Only about 7 fields will fit vertically in the typical browser window. This could perhaps be increased by making the font smaller.

Movies is a small demo program that could be used to catalogue a collection of films.
