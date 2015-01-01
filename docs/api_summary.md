#Gnoga API Reference Summary

#General Overview

## Application, Types, Gui, Server, Client

The Gnoga framework's root package is Gnoga. There are five child packages
making up the five areas of Gnoga development.

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

## Hiearchy for GUI Types

```
Gnoga.GUI.Base.Base_Type
|
|__ Gnoga.Gui.Document.Document_Type
|
|__ Gnoga.Gui.Element.Element_Type
   |
   |__ Gnoga.Gui.Element.Canvas.Canvas_Type
   |                     Context_Type
   |                     |
   |                     |__ Canvas.Context_2d.Context_2d_Type
   |
   |__ Gnoga.Gui.Element.Common.A_Type
   |                            Button_Type
   |                            Div_Type
   |                            P_Type
   |                            IMG_Type
   |                            HR_Type
   |                            BR_Type
   |                            Meter_Type
   |                            Progress_Bar_Type
   |                            Span_Type
   |
   |__ Gnoga.Gui.Element.Form.Form_Type
   |                     |    Form_Element_Type
   |                     |    Data_List_Type
   |                     |    Text_Area_Type
   |                     |    Hidden_Type
   |                     |    Input_Button_Type
   |                     |    Submit_Button_Type
   |                     |    Reset_Button_Type
   |                     |    Check_Box_Type
   |                     |    Radio_Button_Type
   |                     |    Check_Box_Type
   |                     |    Input_Image_Type
   |                     |    Text_Type
   |                     |    Email_Type
   |                     |    Password_Type
   |                     |    URL_Type
   |                     |    Search_Type
   |                     |    Color_Picker_Type
   |                     |    Date_Type
   |                     |    Time_Type
   |                     |    Month_Type
   |                     |    Week_Type
   |                     |    Date_Time_Type
   |                     |    Date_Time_Local_Type
   |                     |    Number_Type
   |                     |    Range_Type
   |                     |    Label_Type
   |                     |    Selection_Type
   |                     |    Option_Type
   |                     |    Option_Group
   |                     |__ Form.Fieldset.Fieldset_Type
   |
   |__ Gnoga.Gui.Element.IFrame.IFrame_Type
   |
   |__ Gnoga.Gui.Element.List.Ordered_List_Type
   |                          Unordered_List_Type
   |                          List_Item_Type
   |                          Definition_List_Type
   |                          Term_Type
   |                          Description_Type
   |
   |__ Gnoga.Gui.Element.Multimedia.Multimedia_Type
   |                                Audio_Type
   |                                Video_Type
   |
   |__ Gnoga.Gui.Element.Phrase.Phase_Type (Abbr, Code, Strong, Em, Dfn, Samp,
   |                                        Kbd, Var, Marked, Del, Ins, S, Q,
   |                                        Big, Small, Time, Tt, Cite, I, B,
   |                                        U, Sub, Sup)
   |
   |__ Gnoga.Gui.Element.Section.Section_Type (Address, Article, Aside, Header,
   |                                           Main, Nav, P, Pre, Section,
   |                                           BlockQuote, H1, H2, H3, H4, H5,
   |                                           H6, HGroup)
   |
   |__ Gnoga.Gui.Element.Style.Style_Block.Style_Type
   |
   |__ Gnoga.Gui.Element.SVG.SVG_Type
   |
   |__ Gnoga.Gui.Element.Table.Table_Type
   |                           Table_Row_Type
   |                           Table_Column_Type
   |                           Table_Heading_Type
   |                           Table_Header_Type
   |                           Table_Body_Type
   |                           Table_Footer_Type
   |                           Table_Group_Type
   |                           Table_Column_Type
   |
   |__ Gnoga.Gui.Location.Location_Type
   |
   |__ Gnoga.Gui.Module - Place holder for 3rd party extensions
   |
   |__ Gnoga.Gui.Navigator
   |
   |__ Gnoga.Gui.Plugin - Place hordler for 3rd part JS bindings
   |           included - Ace_Editor
   |                      Boot_Strap
   |                      |
   |                      |__ Container_Type
   |                          Fluid_Container_Type
   |                          Row_Type
   |                          Jumbotron_Type
   |                          Table_Type
   |                          Form_Group_Type
   |                          Check_Box_Type
   |                          Radio_Button_Type
   |                      jQuery - Additional to Gnoga's use
   |                      jQueryUI - Interactions, Effects, Utilities
   |                      |
   |                      |__ jQueryUI.Widget.Accordian_Type
   |                                          jQueryUI Button
   |                                          Dialog_Type
   |                                          Progress_Bar_Type
   |                                          jQueryUI Menu
   |                                          jQueryUI Select Menu
   |                                          Tabs_Type
   |                                          jQueryUI Tool Tips
   |                      MacGap - Native Mac OS X functionality
   |__ Gnoga.Gui.Screen
   |
   |__ Gnoga.Gui.View.View_Base_Type
   |             |    View_Type
   |             |
   |             |__ View.Card.Card_View_Type
   |             |             Tab_Type
   |             |             Tab_Item_Type
   |             |
   |             |__ View.Console.Console_View_Type
   |             |
   |             |__ View.Docker.Docker_View_Type
   |
   |__ Gnoga.Gui.Window.Window_Type
```

## Property and Method Overview

### Base_Type
```
   * Properties
      - Height
      - Width

   * Frawmework Properties:
      - Buffer_Connection
      - Connection_Data (ro)
      - Connection_ID
      - DOM_Selector (ro)
      - Dynamic
      - ID
      - ID_Type (ro)
      - Parent
      - Unique_ID (ro)
      - Valid (ro)

   * Generic Client Side Access to Properties
      - Property

   * Methods
      - Focus
      - Blur

   * Framework Methods
      - Flush_Buffer

   * Generic Client Side Execution of Methods
      - Execute
```

### Element_Type

(All of Base_Type Plus)

```
   * Properties - General
      - Access_Key
      - Advisory_Title
      - Class_Name
      - Draggable
      - Editable
      - Inner_HTML
      - Outer_HTML (ro)

   * Properties - Text Content
      - Language_Code
      - Tab_Index
      - Spell_Check
      - Text
      - Text_Direction

   * Properties - Visibility and Layout
      - Hidden
      - Visible
      - Display
      - Clear_Side (wo)
      - Layout_Float (wo)
      - Overflow
      - Overflow_X (wo)
      - Overflow_Y (wo)
      - Resizable
      - Position
      - Verticle_Align (wo)
      - Box_Sizing
      - Z_Index (wo)
      - Margin (wo)
      - Padding (wo)

   * Properties - Position
      - Height (from Base_Type)
      - Width (from Base_Type)
      - Position_Top (ro)
      - Position_Left (ro)
      - Offset_From_Top (ro)
      - Offset_From_Left (ro)
      - Left
      - Right
      - Top
      - Bottom
      - Box_Height
      - Box_Width
      - Minimum_Height
      - Minimum_Width
      - Maximum_Height
      - Maximum_Width
      - Inner_Height
      - Inner_Width
      - Outer_Height (ro)
      - Outer_Width (ro)
      - Outer_Height_To_Margin (ro)
      - Outer_Width_To_Margin (ro)
      - Client_Width (ro)
      - Client_Height (ro)
      - Client_Left (ro)
      - Client_Top (ro)
      - Offset_Width (ro)
      - Offset_Height (ro)
      - Offset_Left (ro)
      - Offset_Top (ro)
      - Scroll_Width (ro)
      - Scroll_Height (ro)
      - Scroll_Left
      - Scroll_Top

   * Properties - Style - Color   
      - Color
      - Opacity
      - Background_Attachment
      - Background_Color
      - Background_Image
      - Background_Position
      - Background_Origin
      - Background_Repeat
      - Background_Clip
      - Background_Size
      - Border (wo)
      - Border_Radius (wo)
      - Shadow (wo) / Shadow_None
      - Outline (wo)
      - Cursor

   * Properties - Style - Text
      - Font (wo)
      - Text_Alignment (wo)

   * Framework Properties:
      - Auto_Place
      - First_Child
      - Next_Sibling
      - HTML_Tag (ro)

   * Generic Client Side Access to Properties
      - Style
      - Attribute

   * Methods
      - Click
      - Add_Class
      - Remove_Class
      - Toggle_Class
      - Place_Inside_Top_Of
      - Place_Inside_Bottom_Of
      - Place_Before
      - Place_After
      - Remove
```

## Events

```
   * Object Events
      - On_Resize
      - On_Scroll

   * Form Events
      - On_Focus
      - On_Blur
      - On_Change
      - On_Focus_In
      - On_Focus_Out
      - On_Input
      - On_Reset
      - On_Search
      - On_Select
      - On_Submit

   * Mouse Events
      - On_Click
      - On_Mouse_Click
      - On_Mouse_Right_Click
      - On_Context_Menu
      - On_Double_Click
      - On_Mouse_Double_Click
      - On_Mouse_Enter
      - On_Mouse_Leave
      - On_Mouse_Over
      - On_Mouse_Out
      - On_Mouse_Down
      - On_Mouse_Up
      - On_Mouse_Move

   * Drag and Drop Events
      - On_Drag_Start
      - On_Drag
      - On_Drag_End
      - On_Drag_Enter
      - On_Drag_Leave
      - On_Drop

   * Keyboard Events
      - On_Character
      - On_Wide_Character
      - On_Key_Down
      - On_Key_Up
      - On_Key_Press

    * Clipboard Events
      - On_Copy
      - On_Cut
      - On_Paste

    * Generic Events
      - On_Create
      - On_Destroy
      - On_Child_Added
      - On_Child_Removed
      - On_Message
```