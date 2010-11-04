#!/usr/bin/env python
# -*- coding: utf-8 -*-
 
"""Instruct an AVM FRITZ!Box via UPnP_ to reconnect.

This is usually realized with tools like Netcat_ or cURL_.  However, when
developing in Python_ anyway, it is more convenient to integrate a native
implementation.  This one requires Python_ 2.5 or higher.

UPnP_ (Universal Plug and Play) control messages are based on SOAP_, which is
itself based on XML_, and transmitted over HTTP_.

Make sure UPnP_ is enabled on the FRITZ!Box.

A reconnect only takes a few second while restarting the box takes about up to
a minute; not counting the time needed to navigate through the web interface.

.. _Netcat: http://netcat.sourceforge.net/
.. _cURL:   http://curl.haxx.se/
.. _Python: http://www.python.org/
.. _UPnP:   http://www.upnp.org/
.. _SOAP:   http://www.w3.org/TR/soap/
.. _XML:    http://www.w3.org/XML/
.. _HTTP:   http://tools.ietf.org/html/rfc2616

:Copyright: 2008 Jochen Kupperschmidt
:Date: 04-Apr-2008
:License: MIT
"""

from __future__ import with_statement
from contextlib import closing
import socket


def reconnect(host='192.168.178.1', port=49000, debug=False):
    # Prepare HTTP data to send.
    http_body = '\r\n'.join((
        '<?xml version="1.0" encoding="utf-8"?>',
        '<s:Envelope s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/" xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">',
        '  <s:Body>',
        '    <u:ForceTermination xmlns:u="urn:schemas-upnp-org:service:WANIPConnection:1"/>',
        '  </s:Body>',
        '</s:Envelope>'))
    http_data = '\r\n'.join((
        'POST /upnp/control/WANIPConn1 HTTP/1.1',
        'Host: %s:%d' % (host, port),
        'SoapAction: urn:schemas-upnp-org:service:WANIPConnection:1#ForceTermination',
        'Content-Type: text/xml; charset="utf-8"',
        'Content-Length: %d' % len(http_body),
        '',
        http_body))

    # Connect to the box and submit SOAP data via HTTP.
    with closing(socket.socket(socket.AF_INET, socket.SOCK_STREAM)) as s:
        s.connect((host, port))
        s.send(http_data)
        if debug:
            data = s.recv(1024)
            print 'Received:', data

if __name__ == '__main__':
    reconnect()
