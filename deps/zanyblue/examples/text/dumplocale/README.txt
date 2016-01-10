This example is a simple text based application that prints the built-in
localized strings for a command line defined locale.

Following the standard ZB convention, example applications are prefixed
with "x".

This example has no third party dependencies.

Typical build and run sequence

-----------------------------------------------------------------------------
$ make
../../../bin/zbmcompile -i -v DL_Messages dl
This is ZBMCompile, Version 0.2.0 ALPHA (r1759:1776M) at 12:02 PM on 12/22/10
Copyright (c) 2009-2010, Michael Rohan.  All rights reserved
Loaded 16 messages for the facility "dl" (1 locales)
Loaded 1 facilities, 16 keys, 1 locales and 16 messages
Wrote the spec "DL_Messages" to the file "dl_messages.ads"
Wrote the body "DL_Messages" to the file "dl_messages.adb"
ZBMCompile completed at 12:02 PM on 12/22/10, elapsed time 0:00:00.012
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -aP../../../lib -aP../../../src -P x_dumplocale.gpr
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 x_dumplocale.adb
gcc -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 dl_messages.adb
gprbind x_dumplocale.bexch
gnatbind x_dumplocale.ali
gcc -c b__x_dumplocale.adb
gcc x_dumplocale.o -o x_dumplocale

-----------------------------------------------------------------------------
$ make run
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -aP../../../lib -aP../../../src -P x_dumplocale.gpr
gprbuild: "x_dumplocale" up to date
../../../bin/x_dumplocale ""

Dump of the locale ""

    Language          
    Script            
    Territory         
    Name              
    Traits Name       
    Traits Tag                  
    Locale Level      0
    Is Root Locale    TRUE
    Lower Digits      0123456789abcdef
    Upper Digits      0123456789ABCDEF
    Hash Value        0

  Date/Time Formats
  -----------------
                      Date                  Time                  Date & Time
    FULL              EEEE, MMMM d, y       h:mm:ss a zzzz        {1} {0}
    LONG              MMMM d, y             h:mm:ss a z           {1} {0}
    MEDIUM            MMM d, y              h:mm:ss a             {1} {0}
    SHORT             M/d/yy                h:mm a                {1} {0}

  Day Periods
  -----------
    AM                AM
    WEE_HOURS         
    EARLY_MORNING     
    MORNING           
    LATE_MORNING      
    NOON              noon
    MIDDAY            
    AFTERNOON         
    EVENING           
    LATE_EVENING      
    NIGHT             
    PM                PM

  Eras
  ----
    BCE               BCE
    CE                CE

  Day Names
  -------------
                        Short               Full
    SUN                 Sun                 Sunday          
    MON                 Mon                 Monday          
    TUE                 Tue                 Tuesday         
    WED                 Wed                 Wednesday       
    THU                 Thu                 Thursday        
    FRI                 Fri                 Friday          
    SAT                 Sat                 Saturday        

  Month Names
  -----------
                        Short               Full
    JAN                 Jan                 January         
    FEB                 Feb                 February        
    MAR                 Mar                 March           
    APR                 Apr                 April           
    MAY                 May                 May             
    JUN                 Jun                 June            
    JUL                 Jul                 July            
    AUG                 Aug                 August          
    SEP                 Sep                 September       
    OCT                 Oct                 October         
    NOV                 Nov                 November        
    DEC                 Dec                 December        

  Numeric Formats
  ---------------
    DECIMAL           #,##0.###
    SCIENTIFIC        #E0
    PERCENT           #,##0%
    CURRENCY          ¤#,##0.00;(¤#,##0.00)

  Numeric Items
  -------------
    DECIMAL           .
    GROUP             ,
    LIST              ;
    ZERO              0
    PLUS              +
    MINUS             -
    EXPONENT          E
    PERCENT           %
    PERMILLE          ‰
    INFINITY          ∞
    NAN               NaN
    DIGIT_PATTERN     #
    LOCALIZED_DIGITS  0123456789

$ make run_ru
gprbuild -p -aP../../../lib -XOS=unix -XTYPE=static -XBUILD=Debug -aP../../../lib -aP../../../src -P x_dumplocale.gpr
gprbuild: "x_dumplocale" up to date
../../../bin/x_dumplocale ru

Dump of the locale "ru"

    Language          ru
    Script            
    Territory         
    Name              ru
    Traits Name       ru
    Traits Tag        RU        
    Locale Level      1
    Is Root Locale    FALSE
    Lower Digits      0123456789abcdef
    Upper Digits      0123456789ABCDEF
    Hash Value        7478403

  Date/Time Formats
  -----------------
                      Date                  Time                  Date & Time
    FULL              EEEE, d MMMM y 'г'.   H:mm:ss zzzz          {1} {0}
    LONG              d MMMM y 'г'.         H:mm:ss z             {1} {0}
    MEDIUM            dd.MM.yyyy            H:mm:ss               {1} {0}
    SHORT             dd.MM.yy              H:mm                  {1} {0}

  Day Periods
  -----------
    AM                AM
    WEE_HOURS         
    EARLY_MORNING     
    MORNING           
    LATE_MORNING      
    NOON              noon
    MIDDAY            
    AFTERNOON         
    EVENING           
    LATE_EVENING      
    NIGHT             
    PM                PM

  Eras
  ----
    BCE               до н.э.
    CE                н.э.

  Day Names
  -------------
                        Short               Full
    SUN                 вс                  воскресенье     
    MON                 пн                  понедельник     
    TUE                 вт                  вторник         
    WED                 ср                  среда           
    THU                 чт                  четверг         
    FRI                 пт                  пятница         
    SAT                 сб                  суббота         

  Month Names
  -----------
                        Short               Full
    JAN                 янв.                января          
    FEB                 февр.               февраля         
    MAR                 марта               марта           
    APR                 апр.                апреля          
    MAY                 мая                 мая             
    JUN                 июня                июня            
    JUL                 июля                июля            
    AUG                 авг.                августа         
    SEP                 сент.               сентября        
    OCT                 окт.                октября         
    NOV                 нояб.               ноября          
    DEC                 дек.                декабря         

  Numeric Formats
  ---------------
    DECIMAL           #,##0.###
    SCIENTIFIC        #E0
    PERCENT           #,##0 %
    CURRENCY          #,##0.00 ¤

  Numeric Items
  -------------
    DECIMAL           ,
    GROUP              
    LIST              ;
    ZERO              0
    PLUS              +
    MINUS             -
    EXPONENT          E
    PERCENT           %
    PERMILLE          ‰
    INFINITY          ∞
    NAN               NaN
    DIGIT_PATTERN     #
    LOCALIZED_DIGITS  0123456789
