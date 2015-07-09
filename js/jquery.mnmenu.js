/**
 * MNMenu
 * Drop down menu
 *
 * Copyright (c) 2013 Marc Nuri
 * Version: 0.0.19
 * Modified: 2014-12-22
 * Licensed under the MIT license:
 * http://www.opensource.org/licenses/mit-license.php
 *
 * http://www.marcnuri.com
 *
 * Added by David Botton
 *   - Instant close of submenu on click - July 9, 2015
 */

/**
 * Main plugin function
 * 
 * @param {jQuery} $
 * @returns {undefined}
 */
(function ($) {
    /**
     * Plugin initialization function
     * 
     * @param {object} op
     * @returns {unresolved}
     */
    $.fn.mnmenu = function (op) {
        var $this = $(this);
        ////////////////////////////////////////////////////////////////////////
        //To specify custom level settings without affecting defaults
        var tempLevelSettings = {};
        if (typeof op !== 'undefined' && typeof op.levelSettings !== 'undefined') {
            tempLevelSettings = op.levelSettings;
            delete op.levelSettings;
        }
        ////////////////////////////////////////////////////////////////////////
        var settings = $.extend({}, $.fn.mnmenu.defaults, op);
        $this.data('windowHeight', null);
        $this.data('windowWidth', null);
        ////////////////////////////////////////////////////////////////////////
        //Clone custom level settings so that defaults remain and apply custom;
        settings.levelSettings = $.extend({}, settings.levelSettings, tempLevelSettings);
        ////////////////////////////////////////////////////////////////////////
        ///////////////////////////////////////////////////////////////////////
        // Window resize event for responsive features
        $(window).resize(function () {
            $.fn.mnmenu.windowResize($this, settings);
        });
        ////////////////////////////////////////////////////////////////////////
        this.each(function () {
            var $parentMenu = $(this);
            if ($parentMenu.prop("tagName").toUpperCase() !== "UL") {
                $.error("This function can only be called in <ul> elements.");
            }
            $parentMenu.addClass(settings.menuClassName);
            //Recursion through elements to set default class names and parameters
            $.fn.mnmenu.levelRecursion(settings, $parentMenu, 0);
            //Hide every other submenu (It should be prehidden by css)
            $parentMenu.find("ul").each(function () {
                $(this).css("display", "none");
            });
        });

        //Add event listeners to every LI (When adding and removing html content, events are deleted
        $this.find("li").each(function () {
            var $this = $(this);
            $.fn.mnmenu.addEventListeners($this, settings);
        });
        $.fn.mnmenu.resetView($this, settings);
        return $this;
    };

    /**
     * 
     * @param {type} $menu
     * @param {type} settings
     * @returns {undefined}
     */
    $.fn.mnmenu.windowResize = function ($menu, settings) {
        //Mobile browser triggers window resize event when scrolling (This prevents it)
        //Reset view if Horizontal size changes (Responsive button)
        if (typeof $menu.data('windowWidth') !== 'undefined'
                && $menu.data('windowWidth') !== null
                && $menu.data('windowWidth') === $(window).width()) {
            return;
        }
        $.fn.mnmenu.resetView($menu, settings);
    };

    /**
     * Function called when mouse hovers a menu entry (&lt;li&gt;)
     * @param {jQuery} $menu
     * @param {type} settings
     * @returns {undefined}
     */
    $.fn.mnmenu.mouseEnter = function ($menu, settings) {
        var windowWidth = $(window).width();
        clearTimeout($menu.data('timer'));
        //Add hover class
        $.fn.mnmenu.elementsToHover($menu, settings).each(function () {
            $(this).addClass(settings.hoverClassName);
        });
        $menu.children("ul").each(function () {
            var $this = $(this);
            var $parent = $this.parent("li");
            var $parentContainer = $parent.closest("ul");
            //Stop previous (hiding) animation and display the object
            //Calculations had already been made
            if ($this.is(":animated")) {
                $this.stop(true, true).show();
            }
            //Was hidden
            else {
                //Set Z-Index
                var zindex = 1;
                var current = $this;
                while (current.get(0) !== $(document).get(0)) {
                    var temp = parseInt(current.css("z-index"));
                    if (!isNaN(temp) && temp > zindex) {
                        zindex = temp;
                    }
                    current = current.parent();
                }
                $this.css("z-index", zindex + 1);
                //Calculate+set container position
                // - Find level
                var currentLevel = 0;
                var classList = $this[0].className.split(/\s+/);
                for (var i = 0; i < classList.length; i++) {
                    if (classList[i].indexOf([settings.levelClassPrefix, '-'].join('')) >= 0) {
                        currentLevel = parseInt(classList[i].substring(settings.levelClassPrefix.length + 1));
                    }
                }
                var customLevelSettings = settings.levelSettings[currentLevel];
                if (typeof customLevelSettings === "undefined") {
                    customLevelSettings = settings.levelSettings[0];
                }
                //Horizontal position
                var left = "auto", right = "auto", top = "auto", bottom = "auto";
                //RtL
                if (customLevelSettings.parentAttachmentPosition.toUpperCase().indexOf("W") >= 0
                        && customLevelSettings.attachmentPosition.toUpperCase().indexOf("E") >= 0) {
                    right = $parent.outerWidth() + "px";
                    //Always show on screen (Reversi)
                    if ($parent.offset().left - $this.outerWidth() < 0) {
                        left = $parent.outerWidth() + "px";
                        right = "auto";
                    }
                } else if (customLevelSettings.parentAttachmentPosition.toUpperCase().indexOf("E") >= 0
                        && customLevelSettings.attachmentPosition.toUpperCase().indexOf("E") >= 0) {
                    right = "0px";
                }
                //LtR
                else if (customLevelSettings.parentAttachmentPosition.toUpperCase().indexOf("E") >= 0
                        && customLevelSettings.attachmentPosition.toUpperCase().indexOf("W") >= 0) {
                    left = $parent.outerWidth() + "px";
                    //Always show on screen 
                    //Display Menu in visible area. (Several options)
                    if (($parentContainer.offset().left + $parentContainer.outerWidth() + $this.outerWidth())
                            > windowWidth) {
                        // It doesn't fit to the left of the menu
                        if (($parentContainer.outerWidth() + $this.outerWidth()) > windowWidth) {
                            left = (windowWidth - $this.outerWidth())+"px";
                        }
                        // It all fits to the left of the menu (Reversi)
                        else {
                            left = "auto";
                            right = $parent.outerWidth() + "px";
                        }
                    }
                } else if (customLevelSettings.parentAttachmentPosition.toUpperCase().indexOf("W") >= 0
                        && customLevelSettings.attachmentPosition.toUpperCase().indexOf("W") >= 0) {
                    left = "0px";
                }
                //Vertical Position
                if (customLevelSettings.parentAttachmentPosition.toUpperCase().indexOf("N") >= 0
                        && customLevelSettings.attachmentPosition.toUpperCase().indexOf("S") >= 0) {
                    bottom = $parent.outerHeight() + "px";

                } else if (customLevelSettings.parentAttachmentPosition.toUpperCase().indexOf("S") >= 0
                        && customLevelSettings.attachmentPosition.toUpperCase().indexOf("S") >= 0) {
                    bottom = "0px";
                }
                else if (customLevelSettings.parentAttachmentPosition.toUpperCase().indexOf("S") >= 0
                        && customLevelSettings.attachmentPosition.toUpperCase().indexOf("N") >= 0) {
                    top = $parent.outerHeight() + "px";
                } else if (customLevelSettings.parentAttachmentPosition.toUpperCase().indexOf("N") >= 0
                        && customLevelSettings.attachmentPosition.toUpperCase().indexOf("N") >= 0) {
                    top = "0px";
                }
                $this.css("left", left);
                $this.css("right", right);
                $this.css("top", top);
                $this.css("bottom", bottom);
                $this.slideDown(settings.duration);
            }
        });
    };

    /**
     * Function called when mouse leaves a menu entry (&lt;li&gt;)
     * @param {jQuery} $menu
     * @param {type} settings
     * @returns {undefined}
     */
    $.fn.mnmenu.mouseLeave = function ($menu, settings) {
        clearTimeout($menu.data('timer'));
        //Remove hover class
        $.fn.mnmenu.elementsToHover($menu, settings).each(function () {
            $(this).removeClass(settings.hoverClassName);
        });
        $menu.children("ul").each(function () {
            var $toHide = $(this);
            $menu.data('timer', setTimeout(
                    function () {
                        $toHide.hide(settings.duration);
                    }, settings.delay));
        });
    };


    /**
     * Function called when mouse clicks a menu entry (&lt;li&gt;)
     * 
     * @param {jQuery} $menu
     * @param {type} settings
     * @returns {undefined}
     */
    $.fn.mnmenu.mouseClick = function ($menu, settings) {
        clearTimeout($menu.data('timer'));
        //Remove hover class
        $.fn.mnmenu.elementsToHover($menu, settings).each(function () {
            $(this).removeClass(settings.hoverClassName);
        });
        $menu.children("ul").each(function () {
            var $toHide = $(this);
            $menu.data('timer', setTimeout(
                    function () {
                        $toHide.hide(0);
                    }, 0));
        });
    };

    $.fn.mnmenu.resetView = function ($menu, settings) {
        //Find the responsiveMenu button
        var responsiveSelector = ['li.' + settings.responsiveMenuButtonClass].join('');
        var $responsiveMenu = $menu.find(responsiveSelector).addBack(responsiveSelector);
        if ($responsiveMenu.length !== 0) {
            //Move children to top and remove button
            var $children = $responsiveMenu.children('ul').children();
            $menu.append($children);
            $responsiveMenu.remove();
            $.fn.mnmenu.levelRecursion(settings, $menu, 0);
        }
        //Calculate expanded width
        var menuWidth = 0;
        $menu.find(['li.', settings.levelClassPrefix, '-0'].join('')).each(function () {
            menuWidth += $(this).outerWidth();
        });
        if ($(window).width() < (menuWidth + settings.responsiveMenuWindowWidthFudge)
                && settings.responsiveMenuEnabled === true) {
            //Add responsive button and move children
            var $children = $menu.children();
            var $responsiveMenu = $(["<li class='", settings.responsiveMenuButtonClass,
                " first'>", settings.responsiveMenuButtonLabel,
                "<ul></ul></li>"].join(''));
            $menu.append($responsiveMenu);
            $.fn.mnmenu.addEventListeners($responsiveMenu, settings);
            $responsiveMenu.children('ul').append($children);
            $.fn.mnmenu.levelRecursion(settings, $menu, 0);
        }
        //Set variables for future checks
        $menu.data('windowHeight', $(window).height());
        $menu.data('windowWidth', $(window).width());
    };

    /**
     * Recursive function to traverse the component hierarchy setting attributes 
     * and adding the rest of components such as arrows.
     * 
     * @param {type} settings
     * @param {jQuery} $component
     * @param {int} level
     * @returns {undefined}
     */
    $.fn.mnmenu.levelRecursion = function (settings, $component, level) {
        if ($component.prop("tagName").toUpperCase() === "LI") {
            var middle = true;
            //Add arrows to parent <li>. This can only happen from level 1
            if ($component.parent().children().first().get(0) === $component.get(0)
                    && level > 0) {
                //Add Arrow to parent (just once).
                $component.parent().closest("li").append(
                        $(["<span ", "class='", settings.arrowClassName, "'></span>"].join("")
                                ).append(settings.arrowCharacter));
                //Add FirstClassName to first <li>
                $component.addClass(settings.firstClassName);
                middle = false;
            }
            //component can be first and last (no else if)
            if ($component.parent().children().last().get(0) === $component.get(0)) {
                $component.addClass(settings.lastClassName);
                middle = false;
            }
            if (middle) {
                $component.addClass(settings.middleClassName);
            }
            level++;
        }
        //The component may not have 'li' direct descendants a span or something else may be in the way
        $component.children().each(function () {
            var $currentLevel = $(this);
            //Remove old Level class attribute
            $currentLevel.removeClass([settings.levelClassPrefix, "-", level].join(''));
            $currentLevel.removeClass([settings.levelClassPrefix, "-", (level - 1)].join(''));
            $currentLevel.removeClass([settings.levelClassPrefix, "-", (level + 1)].join(''));
            //Add current level class attribute
            $currentLevel.addClass([settings.levelClassPrefix, "-", level].join(''));
            $.fn.mnmenu.levelRecursion(settings, $currentLevel, level);
        });
    };

    /**
     * Add event listeners to menu li
     * @param {type} $li
     * @param {type} settings
     * @returns {undefined}
     */
    $.fn.mnmenu.addEventListeners = function ($li, settings) {
        if ($.fn.hoverIntent) {
            var $this = $li;
            $this.hoverIntent(
                    function () {
                        $.fn.mnmenu.mouseEnter($(this), settings);
                    },
                    function () {
                        $.fn.mnmenu.mouseLeave($(this), settings);
                    });
            // Revert contribution from ackoder.
            /*
             $this.click(function(e) {
             $.fn.mnmenu.mouseClick($(this), settings);
             e.stopImmediatePropagation();
             });
             */
        } else {
            var $this = $li;
            $this.mouseenter(function () {
                $.fn.mnmenu.mouseEnter($(this), settings);
            });
            $this.mouseleave(function () {
                $.fn.mnmenu.mouseLeave($(this), settings);
            });
            $this.click(function(e) {
                $.fn.mnmenu.mouseClick($(this), settings);
                //e.stopImmediatePropagation();
            });
        }
    };

    /**
     * Returns an array of elements to which to add/remove the "hover" 
     * class when hovered
     * @param {jQuery} $menu
     * @param {type} settings
     * @returns {jQuery}
     */
    $.fn.mnmenu.elementsToHover = function ($menu, settings) {
        //All children which aren't containers (li, span, links...)
        //This makes it easier for styling.
        return $([$menu, $menu.children(":not(ul)")]);
    };

    /**
     * Default plugin options
     */
    $.fn.mnmenu.defaults = {
        //Class for top-level menuName
        menuClassName: "mnmenu",
        //Class for hovered elements
        hoverClassName: "hover",
        //List elements levels
        levelClassPrefix: "level",
        //Class for arrow <span>
        arrowClassName: "arrow",
        arrowCharacter: " &#187;",
        //List elements position in level
        firstClassName: "first",
        middleClassName: "middle",
        lastClassName: "last",
        delay: 150,
        duration: 250,
        defaultParentAttachmentPosition: "NE",
        defaultAttachmentPosition: "NW",
        //Responsive
        responsiveMenuEnabled: true,
        responsiveMenuWindowWidthFudge: 10,
        responsiveMenuButtonClass: "mnresponsive-button",
        responsiveMenuButtonLabel: "Menu"
    };
    $.fn.mnmenu.defaults.levelSettings = {};
    //Define defaultTopLevelSettings
    $.fn.mnmenu.defaults.levelSettings[0] = new MNLevelSettings();
    //Second level settings for default behavior (typical menu)
    $.fn.mnmenu.defaults.levelSettings[1] = new MNLevelSettings();
    $.fn.mnmenu.defaults.levelSettings[1].parentAttachmentPosition = "SW";
    $.fn.mnmenu.defaults.levelSettings[1].attachmentPosition = "NW";
})(jQuery);

function MNLevelSettings() {
    this.parentAttachmentPosition = $.fn.mnmenu.defaults.defaultParentAttachmentPosition;
    this.attachmentPosition = $.fn.mnmenu.defaults.defaultAttachmentPosition;
    this.arrowCharacter = $.fn.mnmenu.defaults.arrowCharacter;
}
