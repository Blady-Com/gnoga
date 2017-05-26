#TIPS

Many tips will be found also in source code specification for types and subprograms.

1. Want to take an HTML snapshot of your page:

    Gnoga.Server.Template_Parser.Write_String_To_File
    ("site.html", Main_Window.Document.Document_Element.Outer_HTML);

1. Use View.Hidden to remove from browser, then View.Remove to remove from DOM. When View finalizes on the Ada side it will also tell the browser to reclaim the elements memory as well.

1. How do I add scrollbars to a view?
View.Overflow or View.Overflow\_X or View.Overflow\_Y with Visible, Hidden, Scroll, Auto.

1. When using the Grid view you always get best results when placing Views in to the grids instead of using the individual cell directly. When encapsulating what you want in to another container (especially if its size is known) will get a more predictable layout of the underlying table.

1.  If I create an object dynamically and add it to the DOM via create on a parent.  If I later free that object manually and replace it with a newly created version (still set to dynamic), does that cause any memory leaks?
If you free the memory Ada's runtime will finalize the object which will remove any references to it in the Gnoga cache on the browser side. If you leaving "dangling pointer" on the Ada side you will of course have a leak. (Unless of course they were marked dynamic before creation and so the parent Gnoga view has a reference and will deallocate it when it finalizes)

1. Does the DOM remember every object I add to a parent or will freeing it remove it completely from the DOM?
On the browser side elements created by the Gnoga framework have a reference in a global cache called gnoga[]. When the Gnoga object on the Ada side is finalized, the reference in the cache is removed. As long as that element is not in the DOM the browser will release the object's memory as part of its garbage collection on the browser side.

1. Would something like this cause any problems (memory leak or otherwise)?
If you are using dynamic you shouldn't be deallocating manually. If you do then when the garbage collection starts with App.Console is finalized it will try to deallocate already deallocated blocks of memory.
Unless you have extreme memory constraints, just overwrite the pointer since the parent view will take care of things or do not use dynamic and deallocate on connection close.

1. Use Gnoga.Server.Connection.Execute\_Script for raw JS execution (there is both a procedure and function version). You can get the Connection ID from any object on a connection (View.Connection\_ID, etc).

1. To define at connection time the window Title and the closed connection message use Main_Window.Document.Title and Gnoga.Server.Connection.HTML\_On\_Close.


1. In general if you find yourself needing to use Unrestricted_Access you are likely going to have issues. For the "tutorials" it was used since it was wanted to keep everything to a single procedure, etc. but perhaps was not the smartest thing to have laying around. Try and keep handlers at package level when possible is a good principle to live by.

1. How do I get a hold on a child element, on the gnoga side, knowing its ID?

    My_Button.Attach_Using_Parent (View, ID => "my_button");

1. Try setting any callbacks to null before deleting visual elements, that would remove any race conditions.

1. There is no way to prevent the user to close the browser window.

1. An On\_Submit\_Handler is set in the table widget. It gets called for the submit buttons. How to access anything in the handler that lets me know what row the submit button was on and how to access the view data structure for the row?
You will need to add an On\_Focus handler to the forms, you can write a single handler and just attach to all the widgets. Have it store the ID or a pointer to the last focused widget. The browser doesn't store or report the focus so no other way to get it.

1. Just freeing an object on the Gnoga side will not remove it from the DOM, that is intended. You need to call Remove first.

1. To avoid two visual elements to disappear and then reappear, somewhat slowly and flickering. Create visual element that is hidden. Fill it in. Hide first and show second. Can either delete the old one or recycle it.

1. Colors in Gnoga.Types.Colors can be displayed or chosen from [www.w3schools.com/cssref/css_colors.asp](https://www.w3schools.com/cssref/css_colors.asp) or [www.w3schools.com/colors/colors_picker.asp](https://www.w3schools.com/colors/colors_picker.asp).

1. Want to see what is going on in the browser console ?
Turn debug on in your boot page <script>var gnoga_debug = true;</script> or use debug.html.

1. Trying to use Connection\_Data in a handler bounded to On\_Destroy\_Handler did not work when closing the browser window because at that point Object.Connection\_Data returns null.
Use instead On\_Before\_Unload\_Handler. Note: it is not effective when only closing the connection.

1. How about reloading the whole page when On\_Resize is called?
Call Main\_Window.Location.Reload.

1. If the user clicks the refresh button on the browser, it breaks the connection and creates a new one. This is extremely annoying, since the program starts over from scratch.
Refresh implies a lost connection and new session on web browsers, that is expected functionality. Local storage could be used on the browser side (Gnoga.Client.Storage) to store session information or any other data on the client side and use that data after a refresh or even weeks later to restore them to some state in your software.

1. HTML\_On\_Close text is displayed only when connection is broken and not when connection is closed. In order to display some text before closing connection, remove current view, create a new one with your text and then close connection, for instance:

    App.My_View.Remove;
    View.Create (App.My_Window.all);
    View.Put_Line ("Application exited.");
    App.My_Window.Close_Connection;

1. You want to browse through Gnoga API, generate them with gnatdoc:

    $ make rm-docs
    $ open docs/html/gnoga_rm/index.html

1. Command "make" alone or "make help" prints main make targets and make environnement variables.

1. How to create a button of a specified size?

    Button.Create (Parent => Parent, Content => Content);

    creates a button of the default size for Content, with a thin border with rounded corners. Adding

    Button.Box_Width  (Value => Button_Size);
    Button.Box_Height (Value => Button_Size);

    has no effect. But also adding

    Button.Border;

    results in a button of the desired size, with a fairly thick, black border with square corners. Adding (Width => "thin") to the call to Border may be desirable.

1. next tip...
