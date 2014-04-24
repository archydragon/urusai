# -*- coding: utf-8 -*-

import urusai_plugin
import json
import ast
import re
import urllib2


class pluginWeather(urusai_plugin.Private):
    """
    Get current weather in required location (private message plugin).
    Usage: "weather Dublin" or "w Dublin"
    """
    triggers = { "^w\s": "Weather",
                 "^weather\s": "Weather" }

    @staticmethod
    def triggerWeather(fromName, fj, message):
        api_fmt = "http://api.openweathermap.org/data/2.5/find?q={0}&units=metric&mode=json"
        response_fmt = "Weather in {0}, {1}: {2}, temperature {3}°C, pressure {4} GPa, wind {5} m/s"
        match = re.match(r"^w(eather)?\s+([a-zA-Z\-\.]+)", message)
        if match:
            location = match.group(2)
            request = api_fmt.format(location)
            data = json.load(urllib2.urlopen(request))
            try:
                w = data['list'][0]
            except (KeyError, IndexError):
                return "Location not found"
            (name, country, sky, temp, pressure, wind) = \
                (w["name"], w["sys"]["country"], w["weather"][0]["description"], \
                 w["main"]["temp"], w["main"]["pressure"], w["wind"]["speed"])
            return response_fmt.format(name, country, sky, temp, pressure, wind)
        else:
            return "Invalid request, should be like \"weather Dublin\"."

