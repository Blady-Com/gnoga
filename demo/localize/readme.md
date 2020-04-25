Simple localization tool to help you to edit localization files.
On the same screen you see the master language text and the locale language text for each ressource key.
It is thus easy to translate the text.

Supported formats:
- UTF-16 strings files are supported for macOS app localization.
- Latin-1 properties files are supported for Zanyblue app localization.

General notes:
- the localization data must be valid, the program doesn't make consistency check
- localization comment is put just the line before the related key
- the keys in written files are in alphabetic order with = separator and LF line terminator

Specific notes for strings files:
- escaped characters are not processed and written as it is, so escaped double quotes is mandatory in typed text

Specific notes for properties files with Zanyblue:
- only characters valid for Ada identifiers are valid for keys
- simple quote must be written twice

Todo:
- add support of escaped characters in strings files
- fix: content is cleared when inserting an existing key
- fix: content is cleared when renaming to an existing key
- add more tooltips

Pascal Pignard, April 2020.
