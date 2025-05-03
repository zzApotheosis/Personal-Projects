-- Module definition
local pkgmanager = {}
local m = {}

-- Setup function
function m.setup()
  require('init.lazyvim').init()
end

-- Init function
function pkgmanager.init()
  m.setup()
end

-- Export module
return pkgmanager
