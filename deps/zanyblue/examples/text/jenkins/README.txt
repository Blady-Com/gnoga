This example is a simple text based application that uses the messages from
Jenkins continuous integration system's (http://www.jenkins-ci.org).  The
set of message files used here were taken from the location

    jenkins/maven-plugin/src/main/resources/hudson/maven

A random selection with a reasonable language scope.  The example application
simply displays the messages.

Since this example is based on an existing Java .properties file set, the
messages are already externalized and are first compiled to the Ada package
Jenkins.Messages using zbmcompile.

Following the standard ZB convention, example applications are prefixed
with "x".

This example has no third party dependencies.

WARNING: Windows command windows do not support Unicode chararters.  This
example will display "garbage" if run on Windows for non-English locales.

Typical build and run sequence

-----------------------------------------------------------------------------
$ make
../../../bin/zbmcompile -v Oracle.Messages -d mesg ojdbc
This is ZBMCompile, Version 0.1.0 ALPHA (r1462) at 8:54 AM on 8/24/10
Copyright (c) 2009-2010, Michael Rohan.  All rights reserved
Loaded 9693 messages for the facility "ojdbc" (29 locales)
Loaded 1 facilities, 338 keys, 29 locales and 9693 messages
Created the spec "Oracle.Messages" in the file "oracle-messages.ads"
Created the body "Oracle.Messages" in the file "oracle-messages.adb"
ZBMCompile completed at 8:54 AM on 8/24/10, elapsed time 0:00:16.463
gprbuild -p -aP../../../lib -P xojdbc.gpr
creating auto.cgpr
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 xojdbc.adb
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 oracle.ads
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 oracle-messages.adb
gprbind xojdbc.bexch
gnatbind xojdbc.ali
gcc-4.4 -c b__xojdbc.adb
gcc-4.4 xojdbc.o -o xojdbc

-----------------------------------------------------------------------------
$ make run
gprbuild -p -aP../../../lib -P xojdbc.gpr
creating auto.cgpr
gprbuild: "xojdbc" up to date
../../../bin/xojdbc
The zero arg constructor for the Oracle JDBC Diagnosibility MBean
Controls the diagnosibilty features of the Oracle JDBC drivers.
All Oracle JDBC logging code is controlled by this boolean attribute. If false, no log messages will be produced. If true, log messages will be controlled by java.util.logging Levels and Filters.
The Oracle JDBC drivers cannot be started and stopped. Always returns false.
The Oracle JDBC drivers do not provide statistics via the Diagnosibility MBean.
Internal Error
Io exception
Invalid column index
Invalid column type
Unsupported column type
Invalid column name
Invalid dynamic column
Closed Connection
Closed Statement
Closed Resultset
Exhausted Resultset
Parameter Type Conflict
ResultSet.next was not called
Statement was cancelled
Statement timed out
Cursor already initialized
Invalid cursor
Can only describe a query
Invalid row prefetch
Missing defines
Missing defines at index
Unsupported feature
No data read
Error in defines.isNull ()
Numeric Overflow
Stream has already been closed
Can not do new defines until the current ResultSet is closed
setReadOnly: Read-only connections not supported
READ_COMMITTED and SERIALIZABLE are the only valid transaction levels
setAutoClose: Only support auto close mode on
cannot set row prefetch to zero
Malformed SQL92 string at position
Non supported SQL92 token at position
Character Set Not Supported !!
exception in OracleNumber
Fail to convert between UTF8 and UCS2
Byte array not long enough
Char array not long enough
Sub Protocol must be specified in connection URL
Missing IN or OUT parameter at index:
Invalid Batch Value
Invalid stream maximum size
Internal error: Data array not allocated
Internal error: Attempt to access bind values beyond the batch value
Internal error: Invalid index for data access
Error in Type Descriptor parse
Undefined type
Inconsistent java and sql object types
no such element in vector
This API cannot be be used for non-UDT types
This ref is not valid
The size is not valid
The LOB locator is not valid
Invalid character encountered in
Non supported character set (add orai18n.jar in your classpath)
Closed LOB
Internal error: Invalid NLS Conversion ratio
Fail to convert to internal representation
Fail to construct descriptor
Missing descriptor
Ref cursor is invalid
Not in a transaction
Invalid Sytnax or Database name is null
Conversion class is null
Access layer specific implementation needed
Invalid Oracle URL specified
Invalid argument(s) in call
Use explicit XA call
Data size bigger than max size for this type
Exceeded maximum VARRAY limit 
Inserted value too large for column
invalid name pattern
Invalid operation for forward only resultset 
Invalid operation for read only resultset
Fail to set REF value 
Cannot do the operation as connections are already opened
User credentials doesnt match the existing ones
invalid batch command
error occurred during batching
No current row
Not on the insert row
Called on the insert row
Value conflicts occurs
Undefined column value on the insert row
Ignored performance hint: setFetchDirection()
Unsupported syntax for requested resultset type and concurrency level 
internal error
operation not allowed
Unable to create resultset at the requested type and/or concurrency level
JDBC statements cannot be created or executed at end of call processing
OCI operation returned OCI_SUCCESS_WITH_INFO
Object type version mismatched
Statement cache size has not been set
Statement Caching cannot be enabled for this logical connection.
Invalid PL/SQL Index Table element type
Invalid empty lob operation
Invalid PL/SQL Index Table array length
Invalid database Java Object
Invalid properties in OCI Connection Pool Object
Bfile is read only
invalid connection type to return via getConnection. Use getJavaSqlConnection instead
SQL statement to execute cannot be empty or null
connection session time zone was not set
invalid JDBC-OCI driver connection pool configuration specified
invalid proxy type specified 
No max length specified in defineColumnType
standard Java character encoding not found
execution completed with warning
Invalid connection cache TTL timeout specified
Invalid thread interval specified
Thread interval value is more than the cache timeout value
could not use local transaction commit in a global transaction
could not use local transaction rollback in a global transaction
could not turn on auto-commit in an active global transaction
could not set savepoint in an active global transaction
could not obtain ID for a named Savepoint
could not obtain name for an un-named Savepoint
could not set a Savepoint with auto-commit on
could not rollback to a Savepoint with auto-commit on
could not rollback to a local txn Savepoint in a global transaction
Invalid statement cache size specified
Invalid connection cache Inactivity timeout specified
Improper statement type returned by explicit cache
Fixed Wait timeout elapsed
Invalid Fixed Wait timeout specified
SQL string is not Query
SQL string is not a DML Statement
Invalid conversion requested
UNUSED
Length of named parameter in SQL exceeded 32 characters
Parameter name used in setXXXStream appears more than once in SQL
Malformed DATALINK URL, try getString() instead
Connection Caching Not Enabled or Not a Valid Cache Enabled DataSource
Invalid Connection Cache Name. Must be a valid String and Unique
Invalid Connection Cache Properties
Connection Cache with this Cache Name already exists
Connection Cache with this Cache Name does not exist
Connection Cache with this Cache Name is Disabled
Invalid or Stale Connection found in the Connection Cache
statement handle not executed
Invalid ONS Event received
Invalid ONS Event Version received
Attempt to set a parameter name that does not occur in the SQL
Method only implemented in thin
This is already a proxy session
Wrong arguments for proxy session
Clob is too large to be stored in a Java String
This method is only implemented in logical connections
This method is only implemented in physical connections
Cannot map Oracle character to Unicode
Cannot map Unicode to Oracle character
Invalid array size for End-to-End metrics values
setString can only process strings of less than 32766 chararacters
duration is invalid for this function
metric value for end-to-end tracing is too long
execution context id sequence number out of range
Invalid transaction mode used
Unsupported holdability value
Can not use getXAConnection() when connection caching is enabled
Can not call getXAResource() from physical connection with caching on
PRIVATE_JDBC package not present in server for this connection
Cannot perform fetch on a PLSQL statement
PKI classes not found. To use connect / functionality, oraclepki.jar must be in the classpath
encountered a problem with the Secret Store. Check the wallet location for the presence of an open wallet (cwallet.sso) and ensure that this wallet contains the correct credentials using the mkstore utility
Cannot bind stream to a ScrollableResultSet or UpdatableResultSet
The Namespace cannot be empty
The attribute length cannot exceed 30 chars
That value of the attribute cannot exceed 400 chars
Not all return parameters registered
The only supported namespace is CLIENTCONTEXT
Error during remote ONS configuration
Locale not recognized
Object does not wrap anything with requested interface
ANYTYPE pickler failed
Magic number mismatch in KOTAD
Format error in KOTAD
Character converter general error
Character converter overrun error
Character converter impossible error -- contact Oracle Support
Incorrect form of use to create NCLOB
The default value of the connection property is missing
The access mode of the connection property is missing
The type of the instance variable used to store the connection property isnt supported
Got an IllegalAccessException during reflection on the connection properties
The instance variable to store the connection property is missing
Connection property: format error
Invalid commit options
Operation on freed LOB
Invalid AQ message format
Mark and reset are not supported by this class
Mark invalid or not set
The limit on the readahead is too big
The number of parameter names does not match the number of registered praremeters
The database session time zone is not set
The database session time zone is not supported
Unable to properly convert XA open string from Java to C
Unable to properly convert XA close string from Java to C
Unable to properly convert RM name from Java to C
Could not casting pointer type to jlong
Input array too short to hold OCI handles
Failed to obtain OCISvcCtx handle from C-XA using xaoSvcCtx
Failed to obtain OCIEnv handle from C-XA using xaoEnv
The tnsEntry property was not set in DataSource
C-XA returned XAER_RMERR during xa_open
C-XA returned XAER_INVAL during xa_open
C-XA returned XAER_PROTO during xa_open
C-XA returned XAER_RMERR during xa_close
C-XA returned XAER_INVAL during xa_close
C-XA returned XAER_PROTO during xa_close
couldnt retrieve localhost IP address. Got an UnknownHostException.
couldnt retrieve localhost IP address. Got a SecurityException.
error while parsing the TCP port specified in the options.
error while parsing the TIMEOUT value specified in the options.
error while parsing the CHANGELAG value specified in the options.
an attempt was made to delete a registration that is using a different database instance than the one currently connected to.
the listener cannot be null.
an attempt was made to attach a listener to a registration that was created outside of the JDBC driver.
listener is already registered.
couldnt remove the listener because it is not registered.
TCP PORT already is use.
Closed registration.
Invalid or undefined payload type.
Invalid or unsupported name for clientInfo.
Out Of Memory, Not able to allocate requested memory size
Fast Connection Failover once enabled cannot be disabled
This instance property isnt available.
Unable to connect through the DataSource
One or more of the authenticating RowSet properties not set
RowSet connection not open
This JdbcRowSet implementation does not allow deleted rows to be visible
SyncProvider instance not constructed
ResultSet not open
Fetch direction cannot be applied when RowSet type is TYPE_SCROLL_SENSITIVE
FETCH_REVERSE cannot be applied when RowSet type is TYPE_FORWARD_ONLY
Illegal fetch direction
The RowSet is not write enabled
Invalid parameter index
Error when converting column to stream type
Could not convert the column into a stream type
Invalid row position, try calling next/previous first
Invalid operation for RowSet type TYPE_FORWARD_ONLY
None of the rows are changed
Map operation failed in toCollection()
The row is not inserted
The row is not deleted
The row is not updated
Not all columns of the row are set
Error converting Reader to String
Could not read from the stream
Invalid parameter type
Invalid number of key columns
Invalid page size
Trying to mark an inserted row as original
Invalid operation on this row before insertRow is called
The underlying ResultSet does not support this operation
This operation can not be called without previous paging operations
Invalid number of row parameter specified
The start position should not be negative
Null ResultSet supplied to populate
Too few rows to start populating at this position
No match column indexes were set
No match column names were set
Invalid match column index
Invalid match column name
The match column index could not be set
The match column name could not be set
The column index being unset has not been set
The column name being unset has not been set
Could not obtain connection
Could not parse the SQL String to get the table name.
Incorrect RowSet scroll type
The object does not satisfy filtering criterion
SerialBlob constructor
SerialClob constructor
Error, could not reproduce the copy of the object
Error while creating an object copy
Invalid empty RowSet parameter
The parameter is not a RowSet instance
Join type is not supported
Number of elements in rowsets is not equal to match columns
Third-party RowSet Join not yet supported
Invalid reader
Invalid writer
Bad value; non-nullable property
Bad value; non-nullable metadata
Invalid WebRowSet argument
Protocol violation
Only one RPA message is expected
Only one RXH message is expected
Received more RXDs than expected
UAC length is not zero
Exceeding maximum buffer length 
invalid Type Representation(setRep)
invalid Type Representation(getRep)
invalid buffer length
No more data to read from socket
Data Type representations mismatch
Bigger type length than Maximum
Exceding key size
Insufficient Buffer size to store Columns Names
This type hasnt been handled
FATAL 
NLS Problem, failed to decode column names
Internal structures field length error
Invalid number of columns returned
Oracle Version not defined 
Types or Connection not defined 
Invalid class in factory 
Using a PLSQL block without an IOV defined 
Attempting different marshaling operation 
Returning a stream in PLSQL block 
Both IN and OUT binds are NULL 
Using Uninitialized OAC 
Logon must be called after connect
Must be at least connected to server
Must be logged on to server
SQL Statement to parse is null
invalid options in all7
invalid arguments in call
not in streaming mode
invalid number of in_out_binds in IOV
invalid number of outbinds
Error in PLSQL block IN/OUT argument(s)
Internal - Unexpected value
Invalid SQL type
DBItem/DBType is null 
Oracle Version not supported. Minimum supported version is 7.2.3. 
Refcursor value is invalid
TTC Protocol version received from server not supported
LOB already opened in the same transaction
LOB already closed in the same transaction
OALL8 is in an inconsistent state
transaction is currently in use

$ make run_el
gprbuild -p -aP../../../lib -P xojdbc.gpr
creating auto.cgpr
gprbuild: "xojdbc" up to date
ZB_LANG=el ../../../bin/xojdbc
Η διαδικασία δόμησης με μηδέν ορίσματα για το MBean διάγνωσης του JDBC της Oracle
Ελέγχει τις λειτουργίες διάγνωσης των προγραμμάτων οδήγησης JDBC της Oracle.
Όλος ο κωδικός καταγραφής JDBC της Oracle ελέγχεται από τη συγκεκριμένη παράμετρο boolean. Εάν είναι ψευδής, δεν θα δημιουργηθούν μηνύματα καταγραφής. Εάν είναι αληθής, τα μηνύματα καταγραφής θα ελέγχονται από τα επίπεδα και φίλτρα java.util.logging.
Δεν είναι δυνατή η εκκίνηση και ο τερματισμός των προγραμμάτων οδήγησης JDBC της Oracle. Πάντα επιστρέφεται ψευδές αποτέλεσμα.
Τα προγράμματα οδήγησης JDBC της Oracle δεν παρέχουν στατιστικά στοιχεία μέσω του MBean διάγνωσης.
Εσωτερικό σφάλμα
Εξαίρεση ΕΕ
Μη αποδεκτό ευρετήριο στήλης
Μη αποδεκτός τύπος στήλης
Μη υποστηριζόμενος τύπος στήλης
Μη αποδεκτό όνομα στήλης
Mη αποδεκτή δυναμική στήλη
Η σύνδεση έκλεισε
Η πρόταση έκλεισε
Το σύνολο αποτελεσμάτων έκλεισε
Το σύνολο αποτελεσμάτων εξαντλήθηκε
Διένεξη τύπων παραμέτρων
Δεν έγινε κλήση στο ResultSet.next
Η πρόταση ακυρώθηκε
Υπέρβαση του χρονικού ορίου της πρότασης
O Cursor έχει αρχικοποιηθεί ήδη
Mη αποδεκτός cursor
Είναι δυνατή μόνο η περιγραφή ενός ερωτήματος
Μη αποδεκτή προ-προσκόμιση γραμμής
Ορισμοί που λείπουν
Λείπουν ορισμοί από το ευρετήριο
Μη υποστηριζόμενη δυνατότητα
Δεν έγινε ανάγνωση δεδομένων
Σφάλμα στο defines.isNull ()
Αριθμητική υπερχείλιση
Το stream έχει κλείσει ήδη
Δεν είναι δυνατή η εκτέλεση νέων ορισμών μέχρι να κλείσει το τρέχον σύνολο αποτελεσμάτων
Ορισμός μόνο ανάγνωσης: Δεν υποστηρίζονται συνδέσεις μόνο ανάγνωσης
Τα μόνα αποδεκτά επίπεδα συναλλαγής είναι READ_COMMITTED και SERIALIZABLE
Ορισμός αυτόματου κλεισίματος: Υποστηρίζεται μόνο η ενεργοποίηση τρόπου αυτόματου κλεισίματος
δεν είναι δυνατό να οριστεί για την προ-προσκόμιση γραμμής η τιμή μηδέν
Μη αποδεκτό αλφαριθμητικό SQL92 στη θέση
Μη υποστηριζόμενο σύμβολο SQL92 στη θέση
Δεν υποστηρίζεται το σετ χαρακτήρων !!
εξαίρεση στον αριθμό Oracle
Αποτυχία μετατροπής του UTF8 σε UCS2 ή του UCS2 σε UTF8
Η μήτρα των Bytes δεν έχει αρκετό μέγεθος
Η μήτρα τύπου Char δεν έχει αρκετό μέγεθος
Πρέπει να προσδιοριστεί το δευτερεύον πρωτόκολλο στη διεύθυνση τοποθεσίας σύνδεσης
Λείπουν οι παράμετροι IN και OUT από το ευρετήριο:
Μη αποδεκτή τιμή μαζικής επεξεργασίας
Μη αποδεκτό μέγιστο μέγεθος stream
Εσωτερικό σφάλμα: Η μήτρα δεδομένων δεν έχει δεσμευτεί
Εσωτερικό σφάλμα: Προσπάθεια πρόσβασης σε δεσμευμένες τιμές εκτός της τιμής μαζικής επεξεργασίας
Εσωτερικό σφάλμα: Μη αποδεκτό ευρετήριο για προσπέλαση δεδομένων
Σφάλμα στην ανάλυση του παράγοντα περιγραφής τύπου
Μη ορισμένος τύπος
Μη συνεπείς τύποι αντικειμένων java και sql
δεν υπάρχει τέτοιο στοιχείο στο διάνυσμα
Αυτό το API δεν μπορεί να χρησιμοποιηθεί για τύπους που δεν είναι UDT
Αυτή η αναφορά είναι μη αποδεκτή
Το μέγεθος δεν είναι αποδεκτό
Ο παράγοντας εντοπισμού LOB δεν είναι αποδεκτός
Βρέθηκε μη αποδεκτός χαρακτήρας στο
Το σετ χαρακτήρων δεν υποστηρίζεται (προσθέστε το orai18n.jar στη διαδρομή της κλάσης)
Το LOB έκλεισε
Εσωτερικό σφάλμα: Μη αποδεκτός συντελεστής μετατροπής NLS
Αποτυχία μετατροπής στην εσωτερική αναπαράσταση
Αποτυχία κατασκευής του παράγοντα περιγραφής
Λείπει ο παράγοντας περιγραφής
Ο cursor αναφοράς είναι μη αποδεκτός
Δεν ανήκει σε συναλλαγή
Είτε η σύνταξη είναι μη αποδεκτή είτε το όνομα της βάσης δεδομένων είναι null
Η κλάση μετατροπής είναι null
Χρειάζεται ειδική υλοποίηση για το επίπεδο πρόσβασης
Προσδιορίστηκε μη αποδεκτή διεύθυνση τοποθεσίας Oracle
Υπάρχουν ένα ή περισσότερα μη αποδεκτά ορίσματα στην κλήση
Χρήση ρητής κλήσης XA
Το μέγεθος δεδομένων είναι μεγαλύτερο από το μέγιστο μέγεθος για αυτόν το τύπο
Ξεπεράστηκε το μέγιστο όριο VARRAY 
Έγινε εισαγωγή τιμής που είναι πολύ μεγάλη για τη στήλη
μη αποδεκτό πρότυπο ονόματος
Μη αποδεκτή λειτουργία για σύνολο αποτελεσμάτων που είναι για προώθηση μόνο 
Μη αποδεκτή λειτουργία για σύνολο αποτελεσμάτων που είναι για ανάγνωση μόνο
Αποτυχία ορισμού της τιμής REF 
Δεν είναι δυνατή η εκτέλεση της λειτουργίας επειδή υπάρχουν συνδέσεις που είναι ήδη ανοικτές
Τα διαπιστευτήρια χρήστη δεν συμφωνούν με τα υπάρχοντα
μη αποδεκτή εντολή μαζικής επεξεργασίας
προέκυψε σφάλμα κατά τη διάρκεια της μαζικής επεξεργασίας
Δεν υπάρχει τρέχουσα γραμμή
Δεν βρίσκεται στην εισαγόμενη γραμμή
Έγινε κλήση για τη γραμμή εισαγωγής
Προκύπτουν διενέξεις τιμών
Μη ορισμένη τιμή στήλης στη γραμμή εισαγωγής
Η υπόδειξη απόδοσης αγνοήθηκε: setFetchDirection()
Μη υποστηριζόμενη σύνταξη για τον τύπο του συνόλου αποτελεσμάτων και το επίπεδο ταυτόχρονης εκτέλεσης που ζητήθηκε 
εσωτερικό σφάλμα
η λειτουργία δεν επιτρέπεται
Η δημιουργία του συνόλου αποτελεσμάτων στο ζητούμενο επίπεδο τύπου και/ή ταυτόχρονης εκτέλεσης δεν είναι δυνατή
Δεν είναι δυνατή η δημιουργία ή η εκτέλεση προτάσεων JDBC στο τέλος της επεξεργασίας κλήσεων
Η λειτουργία OCI επέστρεψε OCI_SUCCESS_WITH_INFO
Η έκδοση του τύπου αντικειμένου δεν συμφωνεί
Δεν έχει οριστεί κρυφή μνήμη για την πρόταση
Η εγγραφή προτάσεων στην κρυφή μνήμη δεν μπορεί να ενεργοποιηθεί για αυτή τη λογική σύνδεση.
Mη αποδεκτός τύπος στοιχείου για πίνακα ευρετηρίου PL/SQL
Mη αποδεκτή λειτουργία κενού lob
Mη αποδεκτό μήκος μήτρας για πίνακα ευρετηρίου PL/SQL
Μη αποδεκτό αντικείμενο Java της ΒΔ
Μη αποδεκτές ιδιότητες στο αντικείμενο περιοχής συγκέντρωσης συνδέσεων OCI
Το Bfile είναι μόνο για ανάγνωση
μη αποδεκτός τύπος σύνδεσης για επιστροφή μέσω getConnection. Αντί αυτής, χρησιμοποιήστε την getJavaSqlConnection
Η πρόταση SQL προς εκτέλεση δεν μπορεί να είναι κενή ή null
δεν ορίστηκε ζώνη ώρας για την περίοδο λειτουργίας της σύνδεσης
έχει προσδιοριστεί μη αποδεκτή διαμόρφωση της περιοχής συγκέντρωσης συνδέσεων του οδηγού JDBC-OCI
προσδιορίστηκε μη αποδεκτός τύπος ενδιάμεσου server 
Δεν καθορίστηκε μέγιστο μήκος στο defineColumnType
δεν βρέθηκε τυποποιημένη κωδικοποίηση χαρακτήρων Java
η εκτέλεση ολοκληρώθηκε με προειδοποίηση
Προσδιορίστηκε μη αποδεκτός χρόνος αναμονής TTL
Προσδιορίστηκε μη αποδεκτό διάστημα νήματος (thread)
Το χρονικό διάστημα νήματος (thread) είναι μεγαλύτερο από τον χρόνο αναμονής
δεν ήταν δυνατή η χρήση οριστικοποίησης τοπικής συναλλαγής σε μία καθολική συναλλαγή
δεν ήταν δυνατή η αναίρεση τοπικής συναλλαγής σε μία καθολική συναλλαγή
δεν ήταν δυνατή η ενεργοποίηση της αυτόματης οριστικοποίησης σε μια ενεργό καθολική συναλλαγή (global transaction)
δεν ήταν δυνατός ο ορισμός savepoint σε μια ενεργό καθολική συναλλαγή (global transaction)
δεν ήταν δυνατή η λήψη αναγνωριστικού για ένα ονομασμένο savepoint
δεν ήταν δυνατή η λήψη ονόματος για ένα savepoint χωρίς όνομα
δεν ήταν δυνατός ο ορισμός savepoint με ενεργοποιημένη την αυτόματη οριστικοποίηση
δεν ήταν δυνατή η αναίρεση συναλλαγής σε savepoint με ενεργοποιημένη την αυτόματη οριστικοποίηση
δεν ήταν δυνατή η αναίρεση συναλλαγής σε τοπικό Savepoint καθολικής συναλλαγής
Έχει προσδιοριστεί μη αποδεκτό μέγεθος κρυφής μνήμης για την πρόταση
Προσδιορίστηκε μη αποδεκτός χρόνος αναμονής κρυφής μνήμης σύνδεσης σε περίπτωση αδράνειας
Ο τύπος εντολής που έχει επιστραφεί από την ρητή κρυφή μνήμη είναι ακατάλληλος
Ο σταθερός χρόνος αναμονής έχει λήξει
Ο σταθερός χρόνος αναμονής που καθορίστηκε δεν είναι αποδεκτός
Η συμβολοσειρά SQL δεν αποτελεί ερώτημα
Η συμβολοσειρά SQL δεν αποτελεί πρόταση DML
Η μετατροπή που ζητήθηκε δεν είναι αποδεκτή
UNUSED
Το μήκος της παραμέτρου στον κώδικα SQL υπερβαίνει τους 32 χαρακτήρες
Το όνομα της παραμέτρου που χρησιμοποιείται στο setXXXStream εμφανίζεται πάνω από μία φορά στον κώδικα SQL
Διεύθυνση τοποθεσίας DATALINK με λανθασμένη μορφή, χρησιμοποιήστε τη συνάρτηση getString()
Δεν έχει ενεργοποιηθεί η εγγραφή στην κρυφή μνήμη για τη σύνδεση ή η προέλευση δεδομένων δεν είναι αποδεκτή για να ενεργοποιηθεί η εγγραφή στην κρυφή μνήμη
Μη αποδεκτό όνομα κρυφής μνήμης για τη σύνδεση. Πρέπει να αποτελεί αποδεκτή συμβολοσειρά και να είναι μοναδικό
Μη αποδεκτές ιδιότητες της κρυφής μνήμης για τη σύνδεση
Υπάρχει ήδη κρυφή μνήμη για τη σύνδεση με αυτό το όνομα
Δεν υπάρχει κρυφή μνήμη για τη σύνδεση με αυτό το όνομα
Η κρυφή μνήμη για τη σύνδεση με αυτό το όνομα είναι απενεργοποιημένη
Βρέθηκαν μη αποδεκτά ή παλαιά στοιχεία σύνδεσης στην κρυφή μνήμη για τη σύνδεση
ο παράγοντας χειρισμού για την πρόταση δεν έχει εκτελεστεί
Λήψη μη αποδεκτού συμβάντος ONS
Λήψη μη αποδεκτής έκδοσης συμβάντος ONS
Απόπειρα ορισμού ονόματος παραμέτρου που δεν εμφανίζεται στον κώδικα SQL
Η μέθοδος εφαρμόζεται μόνο στην περίπτωση περιορισμένων δυνατοτήτων σύνδεσης
Αυτή είναι περίοδος λειτουργίας του ενδιάμεσου server
Λανθασμένα ορίσματα για την περίοδο λειτουργίας του ενδιάμεσου server
Το αντικείμενο clob είναι πολύ μεγάλο για να αποθηκευτεί στη συμβολοσειρά Java
Η συγκεκριμένη μέθοδος εφαρμόζεται μόνο στην περίπτωση λογικής σύνδεσης
Η συγκεκριμένη μέθοδος εφαρμόζεται μόνο στην περίπτωση φυσικής σύνδεσης
Δεν είναι δυνατή η αντιστοίχιση του χαρακτήρα Oracle σε χαρακτήρα Unicode
Δεν είναι δυνατή η αντιστοίχιση του χαρακτήρα Unicode σε χαρακτήρα Oracle
Μη αποδεκτό μέγεθος πίνακα για End-to-End τιμές μέτρησης
η μέθοδος setString μπορεί να επεξεργαστεί μόνο συμβολοσειρές με μήκος μικρότερο από 32766 χαρακτήρες
η διάρκεια δεν είναι αποδεκτή για αυτή τη συνάρτηση
το μήκος της τιμής μέτρησης για την end-to-end ανίχνευση είναι υπερβολικά μεγάλο
ο αριθμός ακολουθίας για το αναγνωριστικό του context εκτέλεσης βρίσκεται εκτός του εύρους τιμών
Χρήση μη αποδεκτής κατάστασης συναλλαγής.
Μη υποστηριζόμενη τιμή "holdability"
Δεν είναι δυνατή η χρήση getXAConnection() όταν είναι ενεργοποιημένη η λειτουργία καταχώρισης στη μνήμη cache για τη σύνδεση
Δεν είναι δυνατή η κλήση του getXAResource() από φυσική σύνδεση με ενεργοποιημένη την εγγραφή στη μνήμη cache
Το πακέτο PRIVATE_JDBC δεν υπάρχει στον server για αυτή τη σύνδεση
Δεν είναι δυνατή η εκτέλεση ανάκτησης σε μια δήλωση PLSQL
Δεν βρέθηκαν κλάσεις PKI. Για να χρησιμοποιήσετε τη λειτουργία σύνδεσης, το oraclepki.jar πρέπει να βρίσκεται στη διαδρομή κλάσης
Παρουσιάστηκε πρόβλημα με το Secret Store. Ελέγξτε αν στην τοποθεσία του ψηφιακού πορτοφολιού υπάρχει ανοικτό ψηφιακό πορτοφόλι (cwallet.sso) και βεβαιωθείτε ότι αυτό το ψηφιακό πορτοφόλι περιέχει τις σωστές πιστοποιήσεις χρησιμοποιώντας το βοηθητικό πρόγραμμα mkstore
Δεν είναι δυνατή η δέσμευση ροής σε ένα ScrollableResultSet ή UpdatableResultSet
Το διάστημα ονόματος δεν μπορεί να είναι κενό
Το μήκος της παραμέτρου δεν μπορεί να υπερβαίνει τους 30 χαρακτήρες
Η τιμή της παραμέτρου δεν μπορεί να υπερβαίνει τους 400 χαρακτήρες
Δεν έχουν καταχωρηθεί όλες οι παράμετροι επιστροφής
Ο μόνος χώρος ονομάτων που υποστηρίζεται είναι ο CLIENTCONTEXT
Παρουσιάστηκε σφάλμα κατά την απομακρυσμένη διαμόρφωση του ONS.
Οι τοπικές ρυθμίσεις δεν αναγνωρίζονται
Το αντικείμενο δεν συμπεριλαμβάνει τίποτα με το ενδιάμεσο περιβάλλον που ζητήθηκε
Το pickler ANYTYPE απέτυχε
Ασυμφωνία μαγικού αριθμού στο KOTAD
Σφάλμα μορφής στο KOTAD
Γενικό σφάλμα μετατροπέα χαρακτήρων
Σφάλμα υπέρβασης μετατροπέα χαρακτήρων
Αδύνατο σφάλμα μετατροπέα χαρακτήρων -- επικοινωνήστε με την τεχνική υποστήριξη της Oracle
Εσφαλμένη μορφή χρήσης για τη δημιουργία NCLOB
Η προεπιλεγμένη τιμή της ιδιότητας σύνδεσης λείπει
Ο τρόπος πρόσβασης της ιδιότητας σύνδεσης λείπει
Ο τύπος της μεταβλητής στιγμιοτύπου (instance) που χρησιμοποιείται για την αποθήκευση της ιδιότητας σύνδεσης δεν υποστηρίζεται
Ελήφθη IllegalAccessException κατά την ανταλλαγή πληροφοριών στις ιδιότητες σύνδεσης
Η μεταβλητή στιγμιοτύπου (instance) για την αποθήκευση της ιδιότητας σύνδεσης λείπει
Ιδιότητα σύνδεσης: σφάλμα μορφής
Μη αποδεκτές επιλογές οριστικοποίησης (commit)
Εκτέλεση λειτουργίας σε αποδεσμευμένο LOB
Μη αποδεκτή μορφή μηνύματος AQ
Η ένδειξη και η επαναφορά δεν υποστηρίζονται σε αυτή την κλάση
Η ένδειξη δεν είναι αποδεκτή ή δεν έχει οριστεί
Το όριο προ-ανάγνωσης είναι πολύ μεγάλο
Ο αριθμός των ονομάτων των παραμέτρων δεν συμφωνεί με τον αριθμό των καταχωρημένων παραμέτρων
Η ζώνη ώρας για την περίοδο σύνδεσης της βάσης δεδομένων δεν έχει οριστεί
Η ζώνη ώρας για την περίοδο λειτουργίας της βάσης δεδομένων δεν υποστηρίζεται
Δεν είναι δυνατή η κατάλληλη μετατροπή της συμβολοσειράς ανοίγματος XA από Java σε C
Δεν είναι δυνατή η κατάλληλη μετατροπή της συμβολοσειράς κλεισίματος XA από Java σε C
Δεν είναι δυνατή η κατάλληλη μετατροπή του ονόματος RM από Java σε C
Δεν είναι δυνατή η μετατροπή (casting) του τύπου δείκτη σε jlong
Η μήτρα εισόδου είναι πολύ μικρή για να περιλάβει τους δείκτες χειρισμού OCI
Αποτυχία στη λήψη του δείκτη χειρισμού OCISvcCtx από C-XA με χρήση xaoSvcCtx
Αποτυχία στη λήψη του δείκτη χειρισμού OCIEnv από C-XA με χρήση xaoEnv
Η ιδιότητα tnsEntry δεν ορίστηκε στο DataSource
C-XA επέστρεψε XAER_RMERR κατά την xa_open
C-XA επέστρεψε XAER_INVAL κατά την xa_open
C-XA επέστρεψε XAER_PROTO κατά την xa_open
C-XA επέστρεψε XAER_RMERR κατά την xa_close
C-XA επέστρεψε XAER_INVAL κατά την xa_close
C-XA επέστρεψε XAER_PROTO κατά την xa_close
Δεν ήταν δυνατή η ανάκτηση της διεύθυνσης IP του τοπικού κεντρικού υπολογιστή. Ελήφθη UnknownHostException.
Δεν ήταν δυνατή η ανάκτηση της διεύθυνσης IP του τοπικού κεντρικού υπολογιστή. Ελήφθη SecurityException.
Σφάλμα κατά την ανάλυση της θύρας TCP που έχει καθοριστεί στις επιλογές.
Σφάλμα κατά την ανάλυση της τιμής TIMEOUT που έχει καθοριστεί στις επιλογές.
Σφάλμα κατά την ανάλυση της τιμής CHANGELAG που έχει καθοριστεί στις επιλογές.
Έγινε προσπάθεια διαγραφής μιας καταχώρισης που χρησιμοποιεί διαφορετικό στιγμιότυπο (instance) βάσης δεδομένων από αυτό με το οποίο υπάρχει σύνδεση αυτή τη στιγμή.
Η διεργασία ακρόασης δεν μπορεί να είναι null.
Έγινε προσπάθεια επισύναψης διεργασίας ακρόασης σε μια καταχώριση που δημιουργήθηκε εκτός του προγράμματος οδήγησης JDBC.
Η διεργασία ακρόασης έχει ήδη καταχωρηθεί.
Δεν ήταν δυνατή η κατάργηση της διεργασίας ακρόασης επειδή δεν είναι καταχωρημένη.
Το TCP PORT χρησιμοποιείται ήδη.
Κλειστή καταχώριση.
Μη αποδεκτός ή μη ορισμένος τύπος payload.
Μη αποδεκτό ή μη υποστηριζόμενο όνομα για clientInfo.
Η μνήμη εξαντλήθηκε. Δεν είναι δυνατή η εκχώρηση του απαιτούμενου μεγέθους μνήμης
Μετά την ενεργοποίηση της λειτουργίας μεταφοράς σε περίπτωση σφάλματος (σε λειτουργία γρήγορης σύνδεσης), δεν είναι δυνατή η απενεργοποίησή της
Η ιδιότητα στιγμιότυπου δεν είναι διαθέσιμη.
Δεν είναι δυνατή η σύνδεση μέσω DataSource
Μία ή περισσότερες από τις ιδιότητες RowSet ελέγχου ταυτότητας δεν έχουν οριστεί
Η σύνδεση RowSet δεν είναι ανοιχτή
Η συγκεκριμένη υλοποίηση JdbcRowSet δεν επιτρέπει να είναι ορατές οι γραμμές που διαγράφηκαν
Το στιγμιότυπο (instance) SyncProvider δεν έχει δημιουργηθεί
Το ResultSet δεν είναι ανοιχτό
Η κατεύθυνση ανάκτησης δεν μπορεί να εφαρμοστεί όταν ο τύπος RowSet είναι TYPE_SCROLL_SENSITIVE
Το FETCH_REVERSE δεν μπορεί να εφαρμοστεί όταν ο τύπος RowSet είναι TYPE_FORWARD_ONLY
Μη αποδεκτή κατεύθυνση ανάκτησης
Δεν έχει ενεργοποιηθεί η εγγραφή για το RowSet
Μη αποδεκτός δείκτης παραμέτρου
Σφάλμα κατά τη μετατροπή της στήλης σε τύπο ροής
Δεν ήταν δυνατή η μετατροπή της στήλης σε τύπο ροής
Μη αποδεκτή θέση γραμμής, προσπαθήστε να κάνετε πρώτα κλήση της επόμενης/προηγούμενης
Μη αποδεκτή λειτουργία για τον τύπο RowSet TYPE_FORWARD_ONLY
Δεν τροποποιήθηκε καμία γραμμή
Η λειτουργία αντιστοίχισης απέτυχε στο toCollection()
Δεν ήταν δυνατή η εισαγωγή της γραμμής
Η γραμμή δεν έχει διαγραφεί
Δεν ήταν δυνατή η ενημέρωση της γραμμής
Δεν έχουν οριστεί όλες οι στήλες της γραμμής
Σφάλμα κατά την μετατροπή του reader σε συμβολοσειρά
Δεν ήταν δυνατή η ανάγνωση από τη ροή δεδομένων
Μη αποδεκτός τύπος παραμέτρου
Μη αποδεκτός αριθμός στηλών κλειδιών
Μη αποδεκτό μέγεθος σελίδας
Προσπάθεια σήμανσης μιας εισαχθείσας γραμμής ως πρωτότυπης
Μη αποδεκτή λειτουργία στη συγκεκριμένη γραμμή πριν προηγηθεί κλήση του insertRow
Το υποκείμενο ResultSet δεν υποστηρίζει τη συγκεκριμένη λειτουργία
Η συγκεκριμένη λειτουργία δεν μπορεί να κληθεί χωρίς να προηγηθούν λειτουργίες σελιδοποίησης
Προσδιορίστηκε μη αποδεκτός αριθμός παραμέτρου γραμμής
Η θέση έναρξης δεν πρέπει να είναι αρνητική
Δόθηκε null ResultSet για συμπλήρωση
Υπάρχουν πολύ λίγες γραμμές για να ξεκινήσει η συμπλήρωση σε αυτή τη θέση
Δεν ορίστηκαν δείκτες στηλών αντιστοίχισης
Δεν ορίστηκαν ονόματα στηλών αντιστοίχισης
Μη αποδεκτός δείκτης στήλης αντιστοίχισης
Μη αποδεκτό όνομα στήλης αντιστοίχισης
Δεν ήταν δυνατό να οριστεί ο δείκτης της στήλης αντιστοίχισης
Δεν ήταν δυνατό να οριστεί το όνομα της στήλης αντιστοίχισης
Ο δείκτης στήλης, τον ορισμό του οποίου θέλετε να αναιρέσετε, δεν έχει οριστεί
Το όνομα στήλης, τον ορισμό του οποίου θέλετε να αναιρέσετε, δεν έχει οριστεί
Δεν ήταν δυνατή η πραγματοποίηση σύνδεσης
Δεν ήταν δυνατή η ανάλυση της συμβολοσειράς SQL για να γίνει λήψη του ονόματος πίνακα.
Εσφαλμένος τύπος κύλισης RowSet
Το αντικείμενο δεν πληροί το κριτήριο φίλτρου
Διαδικασία δόμησης SerialBlob
Διαδικασία δόμησης SerialClob
Σφάλμα, δεν ήταν δυνατή η αναπαραγωγή αντιγράφου του αντικειμένου
Σφάλμα κατά τη δημιουργία αντιγράφου του αντικειμένου
Μη αποδεκτή παράμετρος κενού RowSet
Η παράμετρος δεν είναι παρουσία (instance) RowSet
Ο τύπος συνένωσης δεν υποστηρίζεται
Ο αριθμός των στοιχείων στα σύνολα γραμμών δεν είναι ίσος με τον αριθμό των στηλών αντιστοίχισης
Η συνένωση RowSet τρίτου κατασκευαστή δεν υποστηρίζεται ακόμη
Μη αποδεκτή λειτουργία ανάγνωσης
Μη αποδεκτή λειτουργία σύνταξης
Λανθασμένη τιμή, ιδιότητα που δεν μπορεί να έχει τιμή null
Λανθασμένη τιμή, μεταδεδομένα που δεν μπορούν να έχουν τιμή null
Μη αποδεκτό όρισμα WebRowSet
Παραβίαση πρωτοκόλλου
Αναμένεται μόνο ένα μήνυμα RPA
Αναμένεται μόνο ένα μήνυμα RXH
Έγινε λήψη περισσότερων RXD από τα αναμενόμενα
Το μήκος UAC δεν είναι μηδενικό
Ξεπεράστηκε το μέγιστο μέγεθος της ενδιάμεσης μνήμης 
μη αποδεκτή αναπαράσταση τύπου(setRep)
μη αποδεκτή αναπαράσταση τύπου(getRep)
μη αποδεκτό μέγεθος ενδιάμεσης μνήμης
Δεν υπάρχουν άλλα δεδομένα για ανάγνωση από την υποδοχή
Ασυμφωνία αναπαραστάσεων τύπων δεδομένων
Το μήκος του τύπου είναι μεγαλύτερο από το μέγιστο επιτρεπτό
Υπέρβαση του μήκους του κλειδιού
Το μέγεθος της ενδιάμεσης μνήμης είναι ανεπαρκές για την αποθήκευση των ονομάτων στηλών
Δεν έχει γίνει χειρισμός αυτού του τύπου
FATAL 
Πρόβλημα NLS, αποτυχία αποκωδικοποίησης ονομάτων στηλών
Σφάλμα μήκους πεδίου εσωτερικής δομής
Επιστροφή μη αποδεκτού αριθμού στηλών
Δεν έχει οριστεί η έκδοση της Oracle 
Δεν έχουν οριστεί οι τύποι ή η σύνδεση 
Μη αποδεκτή κλάση σε εργοστάσιο 
Χρήση μπλοκ PLSQL χωρίς ορισμό IOV 
Απόπειρα εκτέλεσης διαφορετικής λειτουργίας διευθέτησης (marshaling) 
Επιστροφή ενός stream σε μπλοκ εντολών PLSQL 
Τόσο οι δεσμευμένες μεταβλητές IN όσο και οι OUT είναι NULL 
Χρήση μη αρχικοποιημένου OAC 
Η διεργασία Logon πρέπει να κληθεί μετά την σύνδεση
Πρέπει τουλάχιστον να υπάρχει σύνδεση με τον server
Πρέπει να είστε συνδεδεμένοι με τον server
Η πρόταση SQL που θα αναλυθεί είναι null
μη αποδεκτές επιλογές και στις 7 εντολές
μη αποδεκτά ορίσματα στην κλήση
Ο τρόπος λειτουργίας δεν είναι streaming
μη αποδεκτός αριθμός in_out_binds στο IOV
μη αποδεκτός αριθμός outbinds
Σφάλμα στα ορίσματα IN/OUT του μπλοκ εντολών PLSQL
Εσωτερικό σφάλμα - Μη αναμενόμενη τιμή
Μη αποδεκτός τύπος SQL
Οι παράμετροι DBItem/DBType είναι null 
Δεν υποστηρίζεται αυτή η έκδοση της Oracle. Η παλαιότερη έκδοση που υποστηρίζεται είναι η 7.2.3. 
Η τιμή του cursor αναφοράς είναι μη αποδεκτή
Η έκδοση του πρωτοκόλλου TTC που λήφθηκε από τον server δεν υποστηρίζεται
Το LOB είναι ήδη ανοικτό στην ίδια συναλλαγή.
Το LOB είναι ήδη κλειστό στην ίδια συναλλαγή.
Δεν υπάρχει συνέπεια ως προς την κατάσταση του OALL8.
η συναλλαγή χρησιμοποιείται στην παρούσα κατάσταση

