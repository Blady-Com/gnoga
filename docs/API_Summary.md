#Gnoga API Reference Summary

#General Overview

### Application, Types, Gui, Server, Client

The Gnoga framework's root package is Gnoga. There are five child packages making up the five areas of Gnoga development.

   * Gnoga.Application and its children are related initializing and
      managing the life cycle of Gnoga applications.
   * Gnoga.Types contains Gnoga specific types used through out the framework
   * Gnoga.Gui contains the user interface portions of Gnoga. It is further
     divided in to the following child packages:
     - Gnoga.Gui.Base - Common base functionality and events to all UI objects
     - Gnoga.Gui.Document - Binding to root element of DOM in a window
     - Gnoga.Gui.Element - General binding to all UI objects
     - Gnoga.Gui.Element.Common - Common UI elements
     - Gnoga.Gui.Element.Form - Form related UI elements
     - Gnoga.Gui.Ekement.Canvas - Binding to a drawing canvas
     - Gnoga.Gui.Element.Multimedia - Multimedia bindings
     - Gnoga.Gui.Element.SVG - SVG canvas binding
     - Gnoga.Gui.Location - Browser window location control
     - Gnoga.Gui.Navigator - Browser application control
     - Gnoga.Gui.Screen - Desktop screen properties
     - Gnoga.Gui.View - Layout control of UI elements
     - Gnoga.Gui.Window - Control of connection to UI
   * Gnoga.Server - Server side bindings and features
     - Gnoga.Server - Application settings and directories
     - Gnoga.Server.Connection - Low level control of connection to UI
     - Gnoga.Server.Database - Database bindings (MySQL and SQLite3)
     - Gnoga.Server.Migration - Database schema migration interface
     - Gnoga.Server.Model - Active Record implementation for Database access
     - Gnoga.Server.Template_Parser - Template parsing (Python or simple text)
   * Gnoga.Client - Non GUI client side bindings
     - Gnoga.Client.Storage - Local storage on client side
     - Gnoga.Client.Bind_Page - Dynamically create Gnoga objects for an HTML
       page

## Package Hiearchy

Gnoga
|
|__ 