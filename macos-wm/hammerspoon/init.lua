-- this file is reloaded automatically because of the ReloadConfiguration spoon
hs.loadSpoon("ReloadConfiguration")
spoon.ReloadConfiguration:start()
-- manual reload can be achieved by opening hammerspoon and clicking on the "reload configuration" button

--hs.hotkey.bind({"cmd", "alt", "ctrl"}, "B", function()
--  hs.notify.new({title="Hammerspoon", informativeText="Hello World"}):send()
--end)

usbWatcher = nil

-- this method switch the keyboard layout when my bepo keyboard is plugged in/out
function usbDeviceCallback(data)
  local bepo_layout = "French NF (bépo)"
  local us_layout = "U.S. International – PC"
  if (data["vendorID"] == 7764 and data["productID"] == 8240) then
    if (data["eventType"] == "added") then
        print("[init] bepo keyboard added")
        hs.keycodes.setLayout(bepo_layout)
    elseif (data["eventType"] == "removed") then
        print("[init] bepo keyboard removed")
        hs.keycodes.setLayout(us_layout)
    end
    local newlay = hs.keycodes.currentLayout()
    hs.notify.new({title="Switched to", informativeText=newlay}):send()
  else
    print ("[init] device:" .. data["vendorID"] .. " (" .. data["vendorName"] ..")" .. data["productID"] .. " (" .. data["productName"] .. "): " .. data["eventType"])
  end

end

usbWatcher = hs.usb.watcher.new(usbDeviceCallback)
usbWatcher:start()

hs.loadSpoon("Emojis")
spoon.Emojis:bindHotkeys({
  toggle = {{"cmd"}, "r"}
})

function configureAllCameraPropertyWatchers(arg1, arg2)
  local allCameras = hs.camera.allCameras()
  for k, camera in pairs(allCameras) do
    print("[init] camera detected: " .. camera:name())
    if camera:isPropertyWatcherRunning() then
      camera:stopPropertyWatcher()
    end

    camera:setPropertyWatcherCallback(function(camera, property, scope, element)
      logCamera(camera)
    end)
    camera:startPropertyWatcher()
  end
end

-- hs.camera.setWatcherCallback(configureAllCameraPropertyWatchers)
-- hs.camera.startWatcher()
-- the two line above should help to detect when a new webcame is plugged in/out but it’s simpler to to just call it when hammerspoon starts
configureAllCameraPropertyWatchers(1, 2)

function logCamera(camera)
  print("[init] camera detected: " .. camera:name())
  local anyCameraInUse = false
  local allCameras = hs.camera.allCameras()
  for k, camera in pairs(allCameras) do
    if camera:isInUse() then
      anyCameraInUse = true
    end
  end

  filename=os.date(os.getenv("HOME") .. '/.hammerspoon/camera-%Y-%m.log')
  file = io.open(filename, "a")
  if anyCameraInUse then
    file:write(string.format('%s %s ON\n', os.date('%Y-%m-%dT%H:%M:%S'), camera:name()))
  else
    file:write(string.format('%s %s OFF\n', os.date('%Y-%m-%dT%H:%M:%S'), camera:name()))
  end
  file:close()
end


aerospace_switcher = require("aerospace_switch")
switcher = aerospace_switcher.new({title_width = 1000, item_height = 32,})
local function mapCmdTab(event)
   local flags = event:getFlags()
   local chars = event:getCharacters()
   if chars == "\t" and flags:containExactly{'cmd'} then
      switcher:next()
      return true
   elseif chars == string.char(25) and flags:containExactly{'cmd','shift'} then
      switcher:previous()
      return true
   end
end
tapCmdTab = hs.eventtap.new({hs.eventtap.event.types.keyDown}, mapCmdTab)
tapCmdTab:start()
