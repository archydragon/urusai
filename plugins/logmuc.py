# -*- coding: utf-8 -*-

import urusai_plugin
import os
import re
from datetime import datetime

FILE_FMT = "log/chatlog/{0}/{0}_%Y-%m-%d.log"  # {0} will be replaced with MUC name
STRING_FMT = "[%H:%M:%S] {0}"                  # {0} will be replaced with log element

def checkFile(f):
    try:
        with open(f):
            pass
    except IOError:
        d = os.path.dirname(f)
        if not os.path.exists(d):
            os.makedirs(d)
        open(f, 'w').close()
    return open(f, "a+")

def putElement(filename, element):
    f = checkFile(filename)
    f.write(element + "\n")
    f.close()

class pluginLogMuc(urusai_plugin.MucMessage):
    """
    MUC logging plugin.
    """
    triggers = { "": "Log" }

    @staticmethod
    def triggerLog(fromName, fromJid, message):
        msg_fmt = "<{0}> {1}"
        [muc, author] = fromName.split("/", 1)
        now = datetime.now()
        f = now.strftime(FILE_FMT.format(muc))
        e = now.strftime(STRING_FMT.format(msg_fmt.format(author, message)))
        putElement(f, e)
        return ''

class pluginLogMucPresences(urusai_plugin.MucPresence):
    triggers = { "": "Log" }

    @staticmethod
    def triggerLog(fromName, fromJid, params):
        (presence, fromJid, affiliation, role, newNick) = params
        msg_join_fmt = "*** {0} has joined the room as {1} and {2}"
        msg_left_fmt = "*** {0} has left the room"
        msg_nick_fmt = "*** {0} is now known as {1}"
        [muc, author] = fromName.split("/", 1)
        lastRenamed = urusai_plugin.dbGet(muc + "_lastrenamed")
        if presence == "available" and lastRenamed != author:
            presence = msg_join_fmt.format(author, affiliation, role)
        else:
            if newNick:
                presence = msg_nick_fmt.format(author, newNick)
                urusai_plugin.dbSet(muc + "_lastrenamed", newNick)
            else:
                if lastRenamed != author:
                    presence = msg_left_fmt.format(author)
                else:
                    urusai_plugin.dbSet(muc + "_lastrenamed", '')
                    return ''
        now = datetime.now()
        f = now.strftime(FILE_FMT.format(muc))
        e = now.strftime(STRING_FMT.format(presence))
        putElement(f, e)
        return ''
