# -*- coding: utf-8 -*-

import urusai_plugin
import json
import re
import urllib2


class pluginExchange(urusai_plugin.MucMessage):
    """
    Exchange conversion (MUC plugin).
    Usage: "ex 100 USD in EUR"
    """
    triggers = { "^ex\s": "Exchange" }

    @staticmethod
    def triggerExchange(fromName, fj, message):
        api_fmt = "http://rate-exchange.appspot.com/currency?from={0}&to={1}"
        response_fmt = "{0} {1} = {2} {3}"
        match = re.match(r"^ex\s([0-9\,\.]*).*([a-zA-Z]{3})\s+(to|in)\s+([a-zA-Z]{3})", message)
        if match:
            (amount, currencyFrom, currencyTo) = match.group(1, 2, 4)
            amount = amount.replace(",", ".")
            return str(amount)
            request = api_fmt.format(currencyFrom, currencyTo)
            data = json.load(urllib2.urlopen(request))
            amountConverted = data['rate'] * float(amount)
            return response_fmt.format(amount, currencyFrom, amountConverted, currencyTo)
        else:
            return "Invalid request, should be like \"100 USD in EUR\"."


class pluginExchange(urusai_plugin.Private):
    """
    Exchange conversion (private message plugin).
    Usage: "ex 100 USD in EUR"
    """
    triggers = { "^ex\s": "Exchange" }

    @staticmethod
    def triggerExchange(fromName, fj, message):
        api_fmt = "http://rate-exchange.appspot.com/currency?from={0}&to={1}"
        response_fmt = "{0} {1} = {2} {3}"
        match = re.match(r"^ex\s([0-9\,\.]*).*([a-zA-Z]{3})\s+(to|in)\s+([a-zA-Z]{3})", message)
        if match:
            (amount, currencyFrom, currencyTo) = match.group(1, 2, 4)
            amount = amount.replace(",", ".")
            request = api_fmt.format(currencyFrom, currencyTo)
            data = json.load(urllib2.urlopen(request))
            amountConverted = data['rate'] * float(amount)
            return response_fmt.format(amount, currencyFrom, amountConverted, currencyTo)
        else:
            return "Invalid request, should be like \"100 USD in EUR\"."

