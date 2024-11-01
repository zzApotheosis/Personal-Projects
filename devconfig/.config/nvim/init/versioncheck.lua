-- Module definition
local versioncheck = {}
local m = {}

-- Expected Neovim version
local min_expected = {}
min_expected['major'] = 0
min_expected['minor'] = 8
min_expected['patch'] = 0

local actual = vim.version()

function m.to_string(arg)
  return tostring(arg['major']) .. '.' .. tostring(arg['minor']) .. '.' .. tostring(arg['patch'])
end

-- Custom function to check running version
function versioncheck.versioncheck()
  if actual['major'] < min_expected['major'] then
    return -1
  elseif actual['major'] > min_expected['major'] then
    return 1
  end

  if actual['minor'] < min_expected['minor'] then
    return -1
  elseif actual['minor'] > min_expected['minor'] then
    return 1
  end

  if actual['patch'] < min_expected['patch'] then
    return -1
  elseif actual['patch'] > min_expected['patch'] then
    return 1
  end

  return 0
end

function versioncheck.expected()
  return m.to_string(min_expected)
end

function versioncheck.actual()
  return m.to_string(actual)
end

-- Init function
function versioncheck.init()
  -- Nothing needed
end

-- Export module
return versioncheck
