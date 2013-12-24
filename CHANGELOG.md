## 0.1.1-dev

_Stable 0.1.0 has just released. Stay tuned~_

## 0.1.0

  * IQ handlers for replying client version and time
  * Fixed bot behaviour on connection lost
  * MUC kick and ban features
  * ``privmessage``, ``kick`` and ``ban`` alternative returns for MUC plugins
  * ``uptime`` command
  * ``version`` command
  * ``die`` and ``restart`` commands
  * MySQL (and other compatible engines like MariaDB or Percona) is supported as database backend
  * ``censorship`` plugin
  * ``sh`` plugin

## 0.0.3

  * A piece of control possibilities for MUCs' owners
  * Load/unload plugins mechanism for MUCs
  * Plugins can be grouped under subdirectories
  * ``mucpresence`` plugins type implementation
  * Plugins now are able to keep some data in bot's KV-database
  * HTTP API can be disabled for private messages and for specific MUCs
  * ``logmuc`` plugin
  * ``help`` plugin for common users
  * ``help`` command for owners
  * ``get`` command to access DB records
  * Fixed spontaneous crashes on bot's start
  * Command shortcuts

## 0.0.2

  * SSL
  * Join password protected MUC
  * HTTP API
  * Redis is supported as database backend
  * Pass real JIDs from conferences to plugins
  * ``http`` plugin
  * ``quote`` plugin
  * Fixed errors and crashes appearing during plugins usage
  * Plugin method execution is now limited to 60 seconds
  * Optimize DETS backend responsibility

## 0.0.1

  * Initial release
  * Digest MD5 authentication support
  * Database backend API
  * Support DETS as database backend
  * Ownership mechanism
  * Autosave and autojoin conferences when started
  * Python plugin API
  * ``isdown`` plugin
  * ``currency`` plugin
