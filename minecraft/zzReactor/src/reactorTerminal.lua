--[[
zzReactor - reactorTerminal
Copyright 2017 Steven Jennings (zzApotheosis)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
]]--

r = peripheral.wrap("bottom")

while true do
  term.write("> ")
  
  input = read()
  
  info = r.getReactorInfo()
  status = info["status"]
  a = info["fieldStrength"]
  b = info["maxFieldStrength"]
  
  if (input == "charge") then
    if (status == "offline" or status == "stopping") then
      r.chargeReactor()
      print("Charging!")
    else
      print("Unable to charge.")
    end
  elseif (input == "start") then
    if (status == "charged") then
      r.activateReactor()
      print("Activating zzApotheosis's Draconic Reactor.")
    else
      print("Unable to activate.")
    end
  elseif (input == "shutdown") then
    if (status == "online" or status == "charged" or status == "charging") then
      r.stopReactor()
      print("Shutting down.")
    else
      print("Unable to shutdown.")
    end
  elseif (input == "status") then
    print(status)
  elseif (input == "help") then
    print("Valid commands: charge, start, shutdown, status, help")
  else
    print("Invalid input. Try 'help'.")
  end
end
