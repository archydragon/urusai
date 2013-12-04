# -*- coding: utf-8 -*-

import urusai_plugin
import subprocess
import shlex

# List of allowed base commands (e.g. 'uname -a' cannot be specifically banned in this version)
ALLOWED = [
    "uname",
    "uptime",
    "date",
    "dig",
    "whois"
]

class pluginSh(urusai_plugin.MucMessage):
    """
    Shell command execution.
    Usage: 'sh <COMMAND>' (e.g. 'sh uname -a')
    """
    triggers = { "^sh\s": "Shell" }

    @staticmethod
    def triggerShell(fromName, fromJid, message):
        command = shlex.split(message)
        command.pop(0)
        if not command[0] in ALLOWED:
            return "Not avaliable."
        return subprocess.check_output(command)
