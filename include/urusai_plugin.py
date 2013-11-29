import pyclbr
import pkgutil
import re
from importlib import import_module
from erlport.erlterms import List, Atom
from erlport.erlang import call

class UrusaiPlugin:
    """
    Main plugin class.
    """
    @classmethod
    def clsTriggers(self):
        try:
            triggers = self.triggers
        except AttributeError:
            return Atom('notriggers')
        if type(triggers) is not dict:
            return Atom('notriggers')
        out = []
        for key in triggers:
            out.append((key, triggers[key]))
        return List(out)

class Private(UrusaiPlugin):
    """
    Parent class for private messages plugins.
    """
    pass

class MucMessage(UrusaiPlugin):
    """
    Parent class for MUC messages plugins.
    """
    pass

def getPluginsE(emodule):
    """
    Get plugins from module (module name presented as Erlang string).
    """
    getPlugins(emodule.to_string())

def flatten(lst, acc):
    """
    Recursive flatten lists. Thx to @dimasmz
    """
    for elem in lst:
        if isinstance(elem, list):
            flatten(elem, acc)
        else:
            acc.append(elem)

def findPlugins(root, prefix):
    """
    Recursively find all modules under the directory.
    """
    mods = [mod for mod in pkgutil.iter_modules([root], prefix)]
    reply = []
    for (_, name, ispkg) in mods:
        if ispkg:
            reply.append(findPlugins(root + "/" + name, name + "."))
        else:
            reply.append(name)
    return reply

def getPlugins(module):
    """
    Get plugins from module.
    """
    plugins = pyclbr.readmodule(module)
    out = []
    for className in plugins:
        pluginName = re.sub(r"^plugin(.*)", r"\1", className)
        if (pluginName != className):
            pluginType = Atom(plugins[className].super[0].name.lower())
            try:
                i = import_module(module)
                triggers = getattr(i, className).clsTriggers()
                out.append((pluginName, pluginType, triggers))
            except:
                return (Atom("bad_triggers"), className)
    return List(out)

def getPluginsFromAll():
    """
    Find all plugin modules and look them for plugins.
    """
    root = "plugins"
    available_plugins = []
    flatten(findPlugins(root, ''), available_plugins)
    out = map(lambda x: (x, getPlugins(x)), available_plugins)
    return List(out)

def getAvailablePlugins(muc):
    """
    "Callback" to Erlang to get available plugins for MUC.
    """
    return call(Atom("urusai_erlapi"), Atom("available_plugins"), [muc])

def getPluginDocs(muc, module):
    if not module in getAvailablePlugins(muc):
        return "No such module."
    plugins = pyclbr.readmodule(module)
    out = []
    for className in plugins:
        pluginName = re.sub(r"^plugin(.*)", r"\1", className)
        if (pluginName != className):
            pluginType = Atom(plugins[className].super[0].name.lower())
            try:
                i = import_module(module)
                doc = getattr(i, className).__doc__
                out.append(doc)
            except:
                return "No such module."
    return "\n".join(out)
