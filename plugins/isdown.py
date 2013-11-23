# -*- coding: utf-8 -*-

import urusai_plugin
import re
import urllib2

class pluginIsdown(urusai_plugin.MucMessage):
    """
    Checking, is site down or not (MUC message plugin).
    Usage: "down google.com" or "isdown wikipedia.org"
    """
    triggers = { "^(isdown|down)\s": "Isdown" }

    @staticmethod
    def triggerIsdown(fromName, fj, message):
        request_fmt = "http://downforeveryoneorjustme.com/{0}"
        site = re.match(r"^(isdown|down)\s(.+)$", message).group(2)
        html = urllib2.urlopen(request_fmt.format(site)).read()
        if re.search(r"not just", html, re.M):
            return "It's not just you! http://{0} looks down from here.".format(site)
        else:
            return "It's just you. http://{0} is up.".format(site)
