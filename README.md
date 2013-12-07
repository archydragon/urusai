# Urusai 0.0.4-dev [![Build Status](https://travis-ci.org/Mendor/urusai.png)](https://travis-ci.org/Mendor/urusai)

Jabber (XMPP) bot with extendability using Python plugins.


Software requirements
---------------------

  * **Erlang R16B** or newer
  * **Python 2.7.x**
  * **autotools** and **rebar** for building


External dependencies
---------------------

  * [exmpp](https://github.com/processone/exmpp)
  * [erlport](https://github.com/hdima/erlport)
  * [pooler](https://github.com/seth/pooler)
  * [lager](https://github.com/basho/lager)
  * [cowboy](https://github.com/extend/cowboy)
  * [jsonx](https://github.com/iskra/jsonx)
  * [eredis](https://github.com/wooga/eredis)
  * [emysql](https://github.com/Eonblast/Emysql)


Installation
------------

```
git clone https://github.com/Mendor/urusai.git
cd urusai
make deps
make
```


Configuration
-------------

Configuration example can be found in ``default.config.sample``. You may edit this file and rename it to ``default.config`` later or create new configuration file based on this one. [Look inside it](https://github.com/Mendor/urusai/blob/master/default.config.sample) for configuration details.


Running
-------

    ./start.sh default

Where ``default`` is the base name for your configuration (``default.config`` in this case).


Control
-------

Send to the bot subscription from JID set as owner in configuration file. Then use the following commands for bot management (you may use either short or full names of them):

  * ``h[elp]`` — show information about available commands
  * ``p[ing]`` — pong!
  * ``v[ersion]`` — bot version information
  * ``u[ptime]`` — bot and system uptime
  * ``s[tatus] <YOUR_STATUS_MESSAGE>`` — update status message
  * ``o[wner] l[ist]`` — list of bot's owners
  * ``o[wner] a[dd] <JID>`` — add <JID> to owners list
  * ``o[wner] d[el] <JID>`` — remove <JID> from owners list (warning: all the owners have the same rights, but the main owner cannot be deleted from the list)
  * ``m[uc] j[oin] <MUC_ADDRESS> [<NICK>]`` — join MUC, custom nick may be set on this stage
  * ``m[uc] pj[oin] <MUC_ADDRESS> <PASSWORD>`` — join password protected MUC
  * ``m[uc] l[eave] <MUC_ADDRESS>`` — leave MUC
  * ``m[uc] n[ick] <MUC_ADDRESS>`` — change bot's shown nick for this MUC
  * ``m[uc] k[ick] <MUC_ADDRESS> <NICK>`` — kick user from MUC
  * ``m[uc] b[an] <MUC_ADDRESS> <JID>`` — ban JID from MUC
  * ``pl[ugins] l[ist]`` — list of loaded plugins' triggers information
  * ``pl[ugins] r[eload]`` — reload plugins
  * ``g[et] <KEY>`` — get the value of <KEY> field from the database
  * ``e[xec] <COMMAND>`` — execute private message plugin command
  * ``restart`` — restart bot (command has no shortcut!)
  * ``die`` — kill bot (command has no shortcut!)

After the bot joined MUC, the owners of MUC can manage its behaviour sending private messages inside the room:

  * ``h[elp]`` — show information about available commands
  * ``p[ing]`` — guess, what?
  * ``w`` — the name of room you are in
  * ``pl[ugins] l[ist]`` — get list of loaded plugins and their state
  * ``pl[ugins] t[oggle] <PLUGIN>`` — toggle plugin enabled or disabled
  * ``ht[tp] s[tate]`` — get the state of is message sending to this MUC via HTTP API allowed
  * ``ht[tp] t[oggle]`` — enable or disable possibility of sending messages to this MUC via HTTP API
  * ``l[eave]`` — force the bot to go out

**Warning:** the plugins are unloaded automatically after plugin module file deletion and reloading plugins by the bot owner or full bot restart, but if you delete a file, reload plugins, put the file back and then reload plugins again, it will become active for all the MUCs it has been active for before deletion.


Plugin API
----------

All the plugins are stored in modules with ``.py`` or ``.pyc`` extension under ``plugins`` directory. To make its management more easy, you may create subdirectories there and move plugin modules to them (multiple directory nesting level is allowed). There should be empty ``__init__.py`` file under an every directory containing plugins you need to use.

### Plugin module code structure:

The only strongly required import for plugin modules is ``urusai_plugin``.

```python
# -*- coding: utf-8 -*-

import urusai_plugin
from datetime import datetime  # module specific import
```

Plugin class name should start from 'plugin' and be inherited from one of the following classes:

  * ``urusai_plugin.Private`` — to handle the messages sent to the bot directly
  * ``urusai_plugin.MucMessage`` — to handle the messages sent to groupchats
  * ``urusai_plugin.MucPresence`` — to handle presence changes in groupchats

```python
class pluginTime(urusai_plugin.Private):
```

Plugin class' docstrings are used for generation of ``help`` plugin output.

``triggers`` dict is strongly required class property. Every key in dict is regular expression (without 'r' prefix!) used for plugin action triggering, and every value is the name of method running when key regexp triggered. The only exceptions are presence-typed plugins which currently don't use regular expressions and should have the only key-value pair with any key.

```python
    triggers = { "^time": "Time" }
```

Trigger method name should start from 'trigger' and being preceded by ``@staticmethod`` decorator.

Trigger method should has three parameters:

  * 1st — JID or conference JID/nickname of message sender
  * 2nd — real JID of sender (used only for MUC plugins) (if real JIDs are not accessible for bot's role, there will be empty string)
  * 3rd — origin message body for ``Private`` and ``MucMessage`` types and a tuple of (``presenceType``, ``fromJid``, ``affiliation``, ``role``, ``newNick``) for ``MucPresence`` one

``presenceType`` could has the following values:

  * ``join`` — user joined MUC
  * ``leave`` — user left MUC
  * ``kick`` — user has been kicked from MUC
  * ``ban`` — user has been banned from MUC
    
Method should return string with the message which will be send back to the user or MUC. If the string is ``''``, nothing will be sent.

```python
    @staticmethod
    def triggerTime(fromName, fromJid, message):
        return str(datetime.now())
```

All other classes and methods inside modules are not parsed and may be used for coding convenience.

**Plugin method execution time is limited to 60 seconds.**

### Extra methods available in plugins

Plugin management:
  * ``urusai_plugin.getAvailablePrivPlugins()`` — get list of loaded plugins available in private messages as space separated string
  * ``urusai_plugin.getAvailablePlugins(muc)`` — get list of loaded plugins available in specific MUC as space separated string

Bot's KV-database access:

  * ``urusai_plugin.dbGet(key)`` — get the value by key from the database
  * ``urusai_plugin.dbSet(key, value)`` — put KV-pair to the database

----------

For real plugin examples look [``plugins`` directory](https://github.com/Mendor/urusai/tree/master/plugins) of this repo.


HTTP API
--------

Allows sending messages via bot from outer world. The default API URL is [http://localhost:8011/api](http://localhost:8011/api), port and location could be changed via configuration file.

To send message via bot you should send the following JSON in POST request body:

```javascript
{"type":   "message",
 "target": "victim@jabber.org",
 "body":   "Hello from HTTP API"}
```

If you're sending valid JSON, you will receive:

```javascript
{"result":"ok","message":"sent"}
```

Otherwise the result will be ``"error"`` with the details in ``"message"`` field.

**Allowed types:**

  * ``message`` for direct message sending
  * ``plugin`` to pass message body via plugin mechanism

**Allowed targets:**

  * ``jid@server.org`` — send message directly to this JID (if you need this feature, you should enable appropriate option in configuration file)
  * ``room@conference.server.org`` — send message to MUC (if you're sending ``plugin`` typed message to MUC, MUC plugins will be applied); you should be joined MUC to send messages there (if you need this feature, MUC's owner should enable it)
  * ``room@conference.server.org/Interlocutor`` — send private message to user Interlocutor from MUC room@


TODO
----

  * more features for plugin returning values


License
-------

[WTFPL](http://www.wtfpl.net/)
