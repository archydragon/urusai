import pyclbr
import pkgutil
import re
from erlport.erlterms import List, Atom

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
                i = __import__(module)
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
    available_plugins = [name for _, name, _ in pkgutil.iter_modules([root])]
    out = map(lambda x: (x, getPlugins(x)), available_plugins)
    return List(out)
