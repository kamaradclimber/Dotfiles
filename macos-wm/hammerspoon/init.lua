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
        print("added")
        hs.keycodes.setLayout(bepo_layout)
    elseif (data["eventType"] == "removed") then
        print("removed")
        hs.keycodes.setLayout(us_layout)
    end
    local newlay = hs.keycodes.currentLayout()
    hs.notify.new({title="Switched to", informativeText=newlay}):send()
  else
    print (data["vendorID"] .. " (" .. data["vendorName"] ..")" .. data["productID"] .. " (" .. data["productName"] .. ")")
  end

end

usbWatcher = hs.usb.watcher.new(usbDeviceCallback)
usbWatcher:start()

hs.loadSpoon("Emojis")
spoon.Emojis:bindHotkeys({
  toggle = {{"cmd"}, "r"}
})
