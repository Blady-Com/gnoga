--------------------------------------------------------------
--  Connect Four (TM) GNAPPLET
--
--  By:  Barry Fagin and Martin Carlisle
--  US Air Force Academy, Department of Computer Science
--  mailto:carlislem@acm.org
--
--  20150511 Adapted from JVM-GNAT to GNOGA by Pascal Pignard
--  http://blady.pagesperso-orange.fr
--
--  This is free software; you can redistribute it and/or
--  modify without restriction.  We do ask that you please keep
--  the original author information, and clearly indicate if the
--  software has been modified.
--
--  This software is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--------------------------------------------------------------

--  with Java.Awt.Event.Mouselistener;
--  with Java.Awt.Event.MouseEvent;
--  with Java.Awt.Image;
--  with Java.Awt.Image.ImageObserver;
--  with Java.Awt.Graphics;
--  with Java.Applet.Applet;
--  with Java.Lang.String;
--  with Java.IO.Serializable;
--  with Java.Awt.MenuContainer;
--  with Javax.Accessibility.Accessible;
with Gnoga.Gui.View;
with Gnoga.Gui.Base; use Gnoga.Gui.Base;
with Gnoga.Gui.Element.Canvas.Context_2D;
with Gnoga.Gui.Window;
with ZanyBlue.Text.Locales;

package ConnectFour is

   Num_Rows    : constant Integer := 6;
   Num_Columns : constant Integer := 7;
   --  Constants for the board size

   type Player_Kind is (None, Computer, User);
   --  None means that neither the computer, nor the user have selected that
   --  circle, Computer indicates that the circle has been selected by the
   --  computer and User means that the circle has been selected by the user.

   --  data type for the Connect Four board
   type Board_Array_Type is array (1 .. Num_Rows, 1 .. Num_Columns) of Player_Kind;

   --  Adding these discriminants to a type is the magic way of
   --  telling JVM-GNAT that you are implementing these interfaces.
   type Typ
--       (I_Serializable  : java.io.Serializable.Ref;
--        I_MenuContainer : java.awt.MenuContainer.Ref;
--        I_ImageObserver : java.awt.image.ImageObserver.Ref;
--        I_MouseListener : Java.Awt.Event.Mouselistener.Ref;
--        I_Accessible    : Javax.Accessibility.Accessible.Ref)
      is new Gnoga.Gui.View.View_Type with record
      User_Turn   : Boolean;
      The_Canvas  : Gnoga.Gui.Element.Canvas.Canvas_Type;
      Main_Window : Gnoga.Gui.Window.Window_Access;
      --  state
      Board : Board_Array_Type;  -- the current board
      --  Is the game over, and if so who won?
      Computer_Won : Boolean := False;
      User_Won     : Boolean := False;
      Tie          : Boolean := False;
      --  if user clicks in full column, computer should not take a turn
      --  Also computer should not take turn if user wins or tie.
      Ignore_Turn : Boolean := False;
      Locale      : ZanyBlue.Text.Locales.Locale_Type;
   end record;
   type Ref is access all Typ'Class;
--     pragma Convention (Java, Typ);

   --  The following are the specifications for the overridden
   --  methods from the Applet and MouseListener interfaces

   function GetAppletInfo
     (This : access Typ)
      return String;

   procedure Init (This : access Typ);

   procedure Paint
     (This : access Typ;
      G1   : access Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type);

--     procedure Update
--       (This : access Typ;
--        G    : access Java.Awt.Graphics.Typ'Class);

   procedure mouseReleased
     (This : in out Base_Type'Class;
      E    : in     Mouse_Event_Record);
--   pragma Convention (Java, MouseReleased);

--   procedure mouseClicked (This : access Typ;
--                           P1 : access java.awt.event.MouseEvent.Typ'Class);
--   pragma Convention (Java, MouseClicked);
--
--   procedure mouseEntered (This : access Typ;
--                           P1 : access java.awt.event.MouseEvent.Typ'Class);
--   pragma Convention (Java, MouseEntered);
--
--   procedure mouseExited (This : access Typ;
--                          P1 : access java.awt.event.MouseEvent.Typ'Class);
--   pragma Convention (Java, MouseExited);

   procedure mousePressed
     (This : in out Base_Type'Class;
      E    : in     Mouse_Event_Record);
--     pragma Convention (Java, MousePressed);

end ConnectFour;
