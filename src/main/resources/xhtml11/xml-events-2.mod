<!-- ...................................................................... -->
<!-- XML Events Module .................................................... -->
<!-- file: xml-events-2.mod

     This is XML Events - the Events Module for XML.
     a redefinition of access to the DOM events model.

     Copyright 2000-2007 W3C (MIT, ERCIM, Keio), All Rights Reserved.

     This DTD module is identified by the PUBLIC and SYSTEM identifiers:

       PUBLIC "-//W3C//ENTITIES XML Events 2.0//EN"
       SYSTEM "http://www.w3.org/MarkUp/DTD/xml-events-2.mod"

     Revisions:
     (none)
     ....................................................................... -->


<!-- XML Events defines the listener element and its attributes -->

<!ENTITY % xml-events.listener.content "EMPTY" >

<!ELEMENT %xml-events.listener.qname; %xml-events.listener.content;>
<!ATTLIST %xml-events.listener.qname;
    xml:id           ID           #IMPLIED
    event            NMTOKEN      #REQUIRED
    observer         IDREF        #IMPLIED
    targetid         IDREF        #IMPLIED
    handler          %anyURI.datatype;        #IMPLIED
    phase            (capture|default) #IMPLIED
    propagate        (stop|continue) #IMPLIED
    defaultAction    (cancel|perform)  #IMPLIED
>

<!-- end of xml-events-2.mod -->
