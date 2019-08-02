--[[
zzReactor - reactorInfo
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

-- Change these values for your setup. You should only need to change the monitor ID.
r = peripheral.wrap("bottom")
m = peripheral.wrap("monitor_0")

term.clear()
m.clear()

term.write("Initializing...")

-- Initialize some variables
previousTemp = 0
currentTemp = 0
previousField = 0
currentField = 0
previousSat = 0
currentSat = 0
emergency = false

-- Program Introduction
sleep(1)
term.setCursorPos(1,1)
m.setCursorPos(1,1)
m.setTextScale(2)
m.setTextColor(colors.red)
m.write("zzApotheosis")
sleep(1)
m.setCursorPos(1,2)
m.write("Draconic Reactor")
sleep(1)

while true do
  index = 1
  
  -- Print terminal information
  term.clear()
  term.setCursorPos(1,1)
  info = r.getReactorInfo()
  print(textutils.serialize(info))
  
  -- Get monitor ready for information
  m.clear()
  m.setTextScale(1)
  
  -- Assign variables
  status = info["status"]
  gen = info["generationRate"]
  temp = info["temperature"]
  fieldStrength = info["fieldStrength"]
  fieldPower = info["fieldDrainRate"]
  maxFieldStrength = info["maxFieldStrength"]
  sat = info["energySaturation"]
  maxSat = info["maxEnergySaturation"]
  fuel = info["fuelConversion"]
  fuelRate = info["fuelConversionRate"]
  maxFuel = info["maxFuelConversion"]
  
  -- Write Reactor Status
  m.setCursorPos(1,index)
  m.setTextColor(colors.white)
  m.write("Draconic Reactor Status: ")
  if (status == "online") then
    m.setTextColor(colors.lime)
    m.write("Online")
  elseif (status == "stopping") then
    if (emergency) then
      m.setTextColor(colors.red)
      m.write("Emergency Shutdown")
    else
      m.setTextColor(colors.lightBlue)
      m.write("Shutting down")
    end
  elseif (status == "charging") then
    m.setTextColor(colors.yellow)
    m.write("Charging")
  elseif (status == "charged") then
    m.setTextColor(colors.lime)
    m.write("Ready")
  elseif (status == "offline") then
    m.setTextColor(colors.red)
    m.write("Offline")
  else
    m.setTextColor(colors.white)
    m.write("???")
  end
  
  -- Write Power
  index = index + 2
  m.setCursorPos(1,index)
  m.setTextColor(colors.white)
  m.write("Total Power: ")
  m.setTextColor(colors.red)
  m.write(gen.." RF/t")
  
  -- Write Temperature
  index = index + 2
  m.setCursorPos(1,index)
  m.setTextColor(colors.white)
  m.write("Temperature: ")
  if (temp < 7000) then
    m.setTextColor(colors.lightBlue)
    m.write(temp)
  elseif (temp >= 7500) then
    m.setTextColor(colors.red)
    m.write(temp)
  else
    m.setTextColor(colors.yellow)
    m.write(temp)
  end
  -- Display Temperature Status
  if (temp >= 7500 and (status == "online" or status == "stopping")) then
    m.setTextColor(colors.red)
    m.setCursorPos(45,index)
    m.write("DANGER")
  elseif (temp < 7000 and (status == "online" or status == "stopping")) then
    m.setTextColor(colors.lime)
    m.setCursorPos(49,index)
    m.write("OK")
  elseif ((temp >= 7000 and temp < 7500) and (status == "online" or status == "stopping")) then
    m.setTextColor(colors.yellow)
    m.setCursorPos(44,index)
    m.write("WARNING")
  end
  
  -- Write Containment Field Strength
  index = index + 2
  m.setCursorPos(1,index)
  m.setTextColor(colors.white)
  m.write("Field Strength: ")
  if ((fieldStrength/maxFieldStrength) <= 0.15) then
    m.setTextColor(colors.red)
    m.write((fieldStrength/maxFieldStrength*100).."%")
  elseif ((fieldStrength/maxFieldStrength) >= 0.20) then
    m.setTextColor(colors.lime)
    m.write((fieldStrength/maxFieldStrength*100).."%")
  elseif ((fieldStrength/maxFieldStrength) > 0.15 and (fieldStrength/maxFieldStrength) < 0.20) then
    m.setTextColor(colors.yellow)
    m.write((fieldStrength/maxFieldStrength*100).."%")
  else
    m.setTextColor(colors.lightBlue)
    m.write("0%")
  end
  -- Display Containment Field Strength Status
  if ((fieldStrength/maxFieldStrength) <= 0.15 and (status == "online" or status == "stopping")) then
    m.setCursorPos(45,index)
    m.setTextColor(colors.red)
    m.write("DANGER")
  elseif ((fieldStrength/maxFieldStrength) >= 0.20 and (status == "online" or status == "stopping")) then
    m.setCursorPos(49,index)
    m.setTextColor(colors.lime)
    m.write("OK")
  elseif ((fieldStrength/maxFieldStrength) > 0.15 and (fieldStrength/maxFieldStrength) < 0.20 and (status == "online" or status == "stopping")) then
    m.setCursorPos(44,index)
    m.setTextColor(colors.yellow)
    m.write("WARNING")
  end
  
  -- Write Containment Field Power Demand
  index = index + 1
  m.setCursorPos(1,index)
  m.setTextColor(colors.white)
  m.write("Field Power Demand: ")
  m.setTextColor(colors.red)
  m.write(fieldPower.." RF/t")
  
  -- Write Energy Saturation
  index = index + 2
  m.setCursorPos(1,index)
  m.setTextColor(colors.white)
  m.write("Energy Saturation: ")
  if ((sat/maxSat) <= 0.15) then
    m.setTextColor(colors.red)
    m.write(((sat/maxSat)*100).."%")
  elseif ((sat/maxSat) >= 0.20) then
    m.setTextColor(colors.lime)
    m.write(((sat/maxSat)*100).."%")
  elseif ((sat/maxSat) > 0.15 and (sat/maxSat) < 0.20) then
    m.setTextColor(colors.yellow)
    m.write(((sat/maxSat)*100).."%")
  else
    m.setTextColor(colors.lightBlue)
    m.write("0%")
  end
  -- Display Energy Saturation Status
  if ((sat/maxSat) <= 0.15 and (status == "online" or status == "stopping")) then
    m.setCursorPos(45,index)
    m.setTextColor(colors.red)
    m.write("DANGER")
  elseif ((sat/maxSat) >= 0.20 and (status == "online" or status == "stopping")) then
    m.setCursorPos(49,index)
    m.setTextColor(colors.lime)
    m.write("OK")
  elseif ((sat/maxSat) > 0.15 and (sat/maxSat) < 0.20 and (status == "online" or status == "stopping")) then
    m.setCursorPos(44,index)
    m.setTextColor(colors.yellow)
    m.write("WARNING")
  end
  
  -- Write Fuel Conversion
  index = index + 2
  m.setCursorPos(1,index)
  m.setTextColor(colors.white)
  m.write("Fuel Conversion: ")
  if ((fuel/maxFuel) <= 0.70) then
    m.setTextColor(colors.lime)
    m.write(((fuel/maxFuel)*100).."%")
    index = index + 1
    m.setCursorPos(18,index)
    m.write(fuelRate.." nb/t")
  elseif ((fuel/maxFuel) >= 0.80) then
    m.setTextColor(colors.red)
    m.write(((fuel/maxFuel)*100).."%")
    index = index + 1
    m.setCursorPos(18,index)
    m.write(fuelRate.." nb/t")
  elseif ((fuel/maxFuel) > 0.70 and (fuel/maxFuel) < 0.80) then
    m.setTextColor(colors.yellow)
    m.write(((fuel/maxFuel)*100).."%")
    index = index + 1
    m.setCursorPos(18,index)
    m.write(fuelRate.." nb/t")
  else
    m.setTextColor(colors.lightBlue)
    m.write("0%")
    index = index + 1
    m.setCursorPos(18,index)
    m.write(fuelRate.." nb/t")
  end
  -- Display Fuel Conversion Status
  if ((fuel/maxFuel) < 0.70 and (status == "online" or status == "stopping")) then
    index = index - 1
    m.setCursorPos(49,index)
    m.setTextColor(colors.lime)
    m.write("OK")
  elseif ((fuel/maxFuel) >= 0.80 and (status == "online" or status == "stopping")) then
    index = index - 1
    m.setCursorPos(45,index)
    m.setTextColor(colors.red)
    m.write("DANGER")
  elseif ((fuel/maxFuel) >= 0.70 and (fuel/maxFuel) < 0.80 and (status == "online" or status == "stopping")) then
    index = index - 1
    m.setCursorPos(44,index)
    m.setTextColor(colors.yellow)
    m.write("WARNING")
  end
  
  -- Write any problems
  if (status == "online" or status == "stopping") then
    index = index + 2
    m.setCursorPos(1,index)
    m.setTextColor(colors.white)
    m.write("Problems:")
    if (temp < 7000 and (fieldStrength/maxFieldStrength) >= 0.20 and (sat/maxSat) >= 0.20 and (fuel/maxFuel) < 0.70) then
      problems = false
      index = index + 1
      m.setCursorPos(1,index)
      m.setTextColor(colors.lightBlue)
      m.write("No problems!")
    else
      problems = true -- Houston... we have a problem...
      
      -- Display Temperature Problems
      if (temp >= 7000 and temp < 7500) then
        index = index + 1
        m.setCursorPos(1,index)
        m.setTextColor(colors.yellow)
        m.write("*Reactor is too hot.")
      elseif (temp >= 7500) then
        index = index + 1
        m.setCursorPos(1,index)
        m.setTextColor(colors.red)
        m.write("*Reactor is too hot.")
      end
      
      -- Display Containment Field Strength Problems
      if ((fieldStrength/maxFieldStrength) > 0.15 and (fieldStrength/maxFieldStrength) < 0.20) then
        index = index + 1
        m.setCursorPos(1,index)
        m.setTextColor(colors.yellow)
        m.write("*Containment field is not strong enough.")
      elseif ((fieldStrength/maxFieldStrength) <= 0.15) then
        index = index + 1
        m.setCursorPos(1,index)
        m.setTextColor(colors.red)
        m.write("*Containment field is not strong enough.")
      end
      
      -- Display Energy Saturation Problems
      if (sat/maxSat > 0.15 and sat/maxSat < 0.20) then
        index = index + 1
        m.setCursorPos(1,index)
        m.setTextColor(colors.yellow)
        m.write("*Energy Saturation is too low.")
      elseif (sat/maxSat < 0.15) then
        index = index + 1
        m.setCursorPos(1,index)
        m.setTextColor(colors.red)
        m.write("*Energy Saturation is too low.")
      end
      
      -- Display Fuel Conversion Problems
      if (fuel/maxFuel >= 0.70 and fuel/maxFuel < 0.80) then
        index = index + 1
        m.setCursorPos(1,index)
        m.setTextColor(colors.yellow)
        m.write("*Fuel is getting too old.")
      elseif (fuel/maxFuel >= 0.80) then
        index = index + 1
        m.setCursorPos(1,index)
        m.setTextColor(colors.red)
        m.write("*Fuel is getting too old.")
      end
    end
  elseif (status == "charging") then
    index = index + 2
    m.setCursorPos(1,index)
    m.setTextColor(colors.white)
    m.write("Problems:")
    
    previousTemp = currentTemp
    currentTemp = temp
    previousField = currentField
    currentField = fieldStrength
    previousSat = currentSat
    currentSat = sat
    
    if (previousTemp == currentTemp and previousField == currentField and previousSat == currentSat) then
      problems = true
      index = index + 1
      m.setCursorPos(1,index)
      m.setTextColor(colors.yellow)
      m.write("*Reactor is not charging.")
    else
      problems = false
      index = index + 1
      m.setCursorPos(1,index)
      m.setTextColor(colors.lightBlue)
      m.write("No problems!")
    end
  elseif (status == "charged") then
    index = index + 2
    m.setCursorPos(1,index)
    m.setTextColor(colors.white)
    m.write("Problems:")
    
    problems = false
    index = index + 1
    m.setCursorPos(1,index)
    m.setTextColor(colors.lightBlue)
    m.write("No problems!")
  end
  
  -- Write Solutions to Existing Problems
  if (problems) then
    index = index + 2
    m.setCursorPos(1,index)
    m.setTextColor(colors.white)
    m.write("Solutions:")
    m.setTextColor(colors.lightBlue)
    if (status == "online" or status == "stopping") then
      if (temp > 7000 or sat/maxSat < 0.20) then
        index = index + 1
        m.setCursorPos(1,index)
        m.write("*Reduce power demand.")
      end
      if (fieldStrength/maxFieldStrength < 0.20) then
        index = index + 1
        m.setCursorPos(1,index)
        m.write("*Supply more power to the Energy Injector.")
      end
      if (fuel/maxFuel >= 0.70) then
        index = index + 1
        m.setCursorPos(1,index)
        m.write("*Change fuel.")
      end
    elseif (status == "charging") then
      if (previousTemp == currentTemp and previousField == currentField and previousSat == currentSat) then
        index = index + 1
        m.setCursorPos(1,index)
        m.write("Supply power to the Energy Injector.")  
      end
    end
  end
  
  -- Handle Energy Injector for shutdown and charging
  local d = (fieldStrength/maxFieldStrength)
  local e = (sat/maxSat)
  if ((status == "stopping" and d <= 0.25) or status == "charging") then
    redstone.setAnalogOutput("left", 15)
  else
    redstone.setAnalogOutput("left", 0)
  end
  
  -- Monitor for Emergency Shutdown Situation
  if (status == "online" and (d <= 0.1 or e <= 0.1)) then
    emergency = true
    r.stopReactor()
    m.clear()
    m.setCursorPos(1,1)
    m.setTextColor(colors.red)
    m.setTextScale(2)
    m.write("Commencing Emergency")
    m.setCursorPos(1,2)
    m.write("Shutdown")
    sleep(1.5)
    m.setTextScale(1)
  elseif (emergency and status ~= "stopping") then
    emergency = false
  end
  
  sleep(0.2)
end

m.clear()
m.setCursorPos(1,3)
m.setTextColor(colors.red)
m.setTextScale(1)
m.write("Please reboot computer")
