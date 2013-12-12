# -*- coding: utf-8 -*-

import urusai_plugin
import re
import time
import urllib2
import lxml.html


# format bytes to human-readable format
def sizeof_fmt(num):
    for x in ['bytes','KB','MB','GB']:
        if num < 1024.0:
            return "%3.1f %s" % (num, x)
        num /= 1024.0
    return "%3.1f %s" % (num, 'TB')


class pluginHttpTitle(urusai_plugin.MucMessage):
    """
    Posts page title or meta information for links in chat.
    """
    triggers = { "https?\:\/\/" : "Link" }

    @staticmethod
    def triggerLink(fromName, fromJID, message):
        title_fmt = "Title: {0} ({1:.2f}s)"
        obj_fmt = "Type: {0}; Size: {1} ({2:.2f}s)"
        link = re.search("(https?\:\/\/.+)(\s|$)", message).group(1)
        start_time = time.time()
        response = urllib2.urlopen(link)
        if any("Content-Type: text/html" in s for s in response.info().headers):
            doc = lxml.html.document_fromstring(response.read())
            title = doc.find(".//title").text.encode('utf-8')
            request_time = time.time() - start_time
            return title_fmt.format(title, request_time)
        else:
            h_type = "unknown"
            h_size = 0
            for elem in response.info().headers:
                if "Content-Type" in elem:
                    h_type = elem.strip().split()[1]
                if "Content-Length" in elem:
                    h_size = elem.strip().split()[1]
            request_time = time.time() - start_time
            return obj_fmt.format(h_type, sizeof_fmt(float(h_size)), request_time)
