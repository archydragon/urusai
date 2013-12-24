# -*- coding: utf-8 -*-

import urusai_plugin
import marshal


BADWORDS = [
    " fuck",
    " cunt",
    " faggot",
    " bitch",
    " bullshit"
]

UPDATES = {
    "-": "*",
    "*": "**",
    "**": "+",
    "+": "++",
    "++": False
}


def loadStat(muc):
    stat = urusai_plugin.dbGet(muc + "_badwords")
    if not stat:
        return {}
    else:
        return marshal.loads(stat)

def saveStat(muc, stat):
    urusai_plugin.dbSet(muc + "_badwords", marshal.dumps(stat))


class pluginCensorship(urusai_plugin.MucMessage):
    """
    No prohibited words in my conference!
    Warns those who uses bad language. After the third warning kicks from MUC. After the third kick bans from MUC.
    Available commands:
        "badwords" — get global warnings and kicks statistics
        "badwords my" — get your own warnings and kicks statistics
    """
    triggers = {
        "": "Censor",
        "^badwords$": "Stat",
        "^badwords my$": "MyStat"
    }

    @staticmethod
    def triggerCensor(fromName, fromjid, message):
        [muc, author] = fromName.split("/", 1)
        rawjid = fromjid.split("/", 1)[0]
        for word in BADWORDS:
            if message.find(word) != -1:
                stat = loadStat(muc)
                userstat = stat.get(rawjid)
                if not userstat:
                    userstat = "-"
                    retval = "{0}, you've used bad word! Please be aware further.".format(author)
                elif userstat == "*":
                    retval = "{0}, you're still abusing the chat! One more time, and you'll be kicked without warnings.".format(author)
                elif userstat == "**":
                    retval = ("kick", author)
                elif userstat == "+":
                    retval = ("kick", author)
                else:
                    retval = ("ban", rawjid)
                stat[rawjid] = UPDATES[userstat]
                saveStat(muc, stat)
                return retval
        return ""

    @staticmethod
    def triggerStat(fromName, fromjid, message):
        [muc, author] = fromName.split("/", 1)
        stat = loadStat(muc)
        out = ["Bad words usage statistics ('*' means warning, '+' means kick, bans aren't shown):"]
        for jid in stat.keys():
            if stat[jid]:
                out.append("{0}:\t {1}".format(jid, stat[jid]))
        if len(out) is 1:
            return "Nobody abuses at this chat, how sweet!"
        else:
            return "\n".join(out)

    @staticmethod
    def triggerMyStat(fromName, fromjid, message):
        [muc, author] = fromName.split("/", 1)
        rawjid = fromjid.split("/", 1)[0]
        mystat = loadStat(muc).get(rawjid)
        if mystat:
            warns = mystat.count("*")
            if warns != 0:
                return "You have been warned {0} times.".format(warns)
            else:
                return "You have been kicked {0} times.".format(mystat.count("+"))
        else:
            return "You haven't ever been warned or kicked there due to abusing, I like you."
