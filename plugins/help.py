# -*- coding: utf-8 -*-

import urusai_plugin
import re


class pluginHelp(urusai_plugin.MucMessage):
    """
    Information about available modules in this MUC and their usability.
    The data is being given from classes' docstrings.
    Warning: don't move this plugin under any subdirectories to keep it working fine.
    Usage:
        "help" for available modules listing
        "help <MODULE>" for the details about specific module
    """
    triggers = {
        "^help$": "List",
        "^help\s[0-9a-zA-Z_\.]+": "Info"
    }

    @staticmethod
    def triggerList(fromName, fromJid, message):
        reply_fmt = "Available plugins: {0}\nTo get detailed information about plugin's possibility use \"help <PLUGIN>\"."
        [muc, author] = fromName.split("/", 1)
        plugins = urusai_plugin.getAvailablePlugins(muc)
        return reply_fmt.format(" ".join(plugins))

    @staticmethod
    def triggerInfo(fromName, fromJid, message):
        plugin = re.match(r"^help\s(.+)$", message).group(1)
        [muc, author] = fromName.split("/", 1)
        doc = urusai_plugin.getPluginDocs(muc, plugin)
        if doc:
            return doc
        else:
            return "There is no any helpful information about plugin " + plugin + "."


class pluginHelpPrivate(urusai_plugin.Private):
    """
    Information about available modules for private messages.
    The data is being given from classes' docstrings.
    Warning: don't move this plugin under any subdirectories to keep it working fine.
    Usage:
        "help" for available modules listing
        "help <MODULE>" for the details about specific module
    """
    triggers = {
        "^help$": "List",
        "^help\s[0-9a-zA-Z_\.]+": "Info"
    }

    @staticmethod
    def triggerList(fromName, fromJid, message):
        reply_fmt = "Available plugins: {0}\nTo get detailed information about plugin's possibility use \"help <PLUGIN>\"."
        plugins = urusai_plugin.getAvailablePrivPlugins()
        return reply_fmt.format(" ".join(plugins))

    @staticmethod
    def triggerInfo(fromName, fromJid, message):
        plugin = re.match(r"^help\s(.+)$", message).group(1)
        doc = urusai_plugin.getPluginDocs(False, plugin)
        if doc:
            return doc
        else:
            return "There is no any helpful information about plugin " + plugin + "."
