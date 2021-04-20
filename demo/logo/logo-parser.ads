-------------------------------------------------------------------------------
-- NAME (specification)         : logo-parser.ads
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : Logo primitive parser unit.
-- NOTES                        : Ada 2012, GNOGA 1.4 beta
--
-- COPYRIGHT                    : (c) Pascal Pignard 2018
-- LICENCE                      : CeCILL V2 (http://www.cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Parsers.String_Source;
with Parsers.Generic_Source.Get_Blank;
with Parsers.Generic_Source.Get_Token;
with Tables;
with Tables.UTF8_Names;
with ZanyBlue.Text.Locales;

package Logo.Parser is

   type Primitive is
     (nop, help, language, absolute, additem, and_k, animation, appendlineflow, arc, arccosine, arcsine, arctan, back,
      beforep, black, blue, brown, butfirst, butlast, changedirectory, character_k, chattcp, circle, clearscreen,
      cleartext, close, closeflow, cosine, count, countdown, cyan, darkblue, darkgreen, darkred, date, define,
      deletesequence, difference, directory, distance, divide, dot, drawingquality, emptyp, endcountdown, endflow,
      equalp, erase, eraseall, executetcp, false_k, files, fill, fillzone, findcolor, first, fontname, fontsize, for_k,
      forward, fput, globalmake, gray, green, grid, guiaction, guibutton, guidraw, guimenu, guiposition, guiremove,
      heading, hideturtle, home, if_k, indexsequence, instrument, integer_k, item, key, kill, killturtle, label,
      labellength, last, left, lightgray, list, listentcp, listflow, listp, load, loadimage, local, localmake, log10,
      lput, magenta, member, memberp, message, minus, modulo, mouse, mousepos, mouseposition, msg, not_k, numberp,
      openflow, or_k, orange, output, pasttime, pencolor, pendown, penerase, penpaint, penreverse, penshape, penup,
      penwidth, pi, pick, pink, play, position, power, primitive_k, print, printoutall, procedure_k, product, purple,
      quotient, random, read, readcharacter, readcharflow, readlineflow, readmouse, red, refresh, remove, repcount,
      repeat, replace, resetall, reverse_k, right, round, run, save, saved, screencolor, screensize, sendtcp, sentence,
      separation, sequence, setdirectory, setdrawingquality, setfontname, setfontsize, setheading, setindexsequence,
      setinstrument, setpencolor, setpenshape, setpenwidth, setposition, setscreencolor, setscreensize, setsep,
      setseparation, setshape, setstyle, settextcolor, settextname, settextsize, setturtle, setturtlesmax, setx, setxy,
      sety, shape, showturtle, sin, sine, squareroot, stop, stopall, stopanimation, stopgrid, stoptrace, style, sum,
      tangent, textcolor, textname, textsize, thing, time, towards, trace, true_k, turtle, turtles, turtlesmax, unicode,
      variables, visible, wait, wash, while_k, white, window, word, wordp, wrap, write, writelineflow, yellow, zonesize,
      zoom);

   --  Token tables, UTF-8, case-insensitive
   procedure Check_Spelling (Name : Standard.String);
   function Check_Matched
     (Source  : Standard.String;
      Pointer : Integer)
      return Boolean;
   package Primitive_Tables_Raw is new Tables (Primitive);
   package Primitive_Tables is new Primitive_Tables_Raw.UTF8_Names;

   --  Get token from the source
   procedure Get_Token is new Parsers.String_Source.Code.Get_Token (Primitive_Tables_Raw);

   procedure Get_Blank is new Parsers.String_Source.Code.Get_Blank;

   procedure Fill
     (Primitives : out Primitive_Tables.Dictionary;
      Locale     :     ZanyBlue.Text.Locales.Locale_Type);

end Logo.Parser;
