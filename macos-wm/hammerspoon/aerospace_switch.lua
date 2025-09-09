--[[ This is largely copypasta from Hammerspoon's hs.window.switcher
     ( https://github.com/Hammerspoon/hammerspoon/blob/master/extensions/window/window_switcher.lua )
     The bits where it actually integrates with Aerospace are the
     refresh_current_workspace_windows function and the workspace_window_filter.
  ]]--

local M = {}

local current_workspace_windows = {}
local focused_monitor_name = "NONE"

local interesting_window_events = {
   hs.window.filter.windowCreated,
   hs.window.filter.windowDestroyed,
   hs.window.filter.windowFocused,
   hs.window.filter.windowMoved,
   hs.window.filter.windowHidden,
   hs.window.filter.windowUnhidden,
   hs.window.filter.windowMinimized,
   hs.window.filter.windowUnminimized,
}

local hsdrawing = require('hs.drawing')
local hseventtap = require('hs.eventtap')
local hsgeom = require('hs.geometry')
local hsimage = require('hs.image')
local hsscreen = require('hs.screen')
local hstimer = require('hs.timer')

local min = math.min
local max = math.max
local checkMods = hseventtap.checkKeyboardModifiers
hs.window.animationDuration = 0
local UNAVAILABLE=hsimage.imageFromName('NSStopProgressTemplate')

local icons=setmetatable({},{__mode='kv'})
local function getIcon(bundle)
   if not bundle then return UNAVAILABLE
   elseif not icons[bundle] then icons[bundle]=hsimage.imageFromAppBundle(bundle) or UNAVAILABLE end
   return icons[bundle]
end

local function rgba(r, g, b, a) return {red = r, green = g, blue = b, alpha = a} end
local function rgb(r, g, b, _) return rgba(r, g, b, 1) end

local function gc(self)
   self.screen_watcher:stop()
end

local function set_frames(nwindows, drawings, ui)
   local screen = hsscreen.find(focused_monitor_name)
   if screen == nil then screen = hsscreen.mainScreen() end
   drawings.screen_frame = screen:frame()
   local screen_frame = drawings.screen_frame
   local padding = ui.item_height * 0.1
   local size = min(
      ui.item_height,
      (screen_frame.h - padding*(nwindows+1))/nwindows
   )
   drawings.size = size
   -- TODO: pin to some corner (top-left) + padding, or center?
   -- TODO: expand in x to longest title?
   drawings.background = hsdrawing.rectangle(hsgeom(0,0,1,1))
   local bg_frame = hsgeom(0,0,
			   size+padding+ui.title_width+padding,
			   (size+padding)*nwindows+padding)
      :setcenter(screen_frame.center)
   drawings.background
      :setFillColor(ui.background_color)
      :setStroke(false)
      :setFrame(bg_frame)
      :setRoundedRectRadii(padding, padding)

   drawings.highlight_rect = hsdrawing.rectangle(hsgeom(0,0,1,1))
      :setFillColor(ui.highlight_color)
      :setStroke(false)
      :setRoundedRectRadii(padding, padding)

   local title_height = hsdrawing.getTextDrawingSize(
      'O', ui.title_text_style).h

   for i = 1,nwindows do
      local dr = drawings[i]
      local icon_frame = hsgeom(bg_frame.x, bg_frame.y, size, size)
	 :move(padding, padding + (size+padding)*(i-1))
      dr.icon:setFrame(icon_frame)
      dr.title_frame = hsgeom.copy(icon_frame)
	 :move(padding*2 + size, max(0,size - title_height)/2)
	 :setw(ui.title_width)
      local sel_frame = hsgeom.copy(icon_frame)
	 :setw(size + ui.title_width)
      dr.sel_frame = sel_frame
   end
end

local function show_selected(selected, drawings)
   local dr = drawings[selected]
   drawings.highlight_rect:setFrame(dr.sel_frame)
end

local function draw(windows, drawings)
   drawings.background:show()
   drawings.highlight_rect:show()
   for i = 1,#windows do
      local win = windows[i]
      local dr = drawings[i]
      dr.icon:setImage(getIcon(win:application():bundleID())):show()
      local title = win:title() or ' '
      dr.title_text:setFrame(dr.title_frame)
      dr.title_text:setText(title):show()
   end
end

local MODS_INTERVAL=0.05 -- recheck for (lack of) mod keys after this interval
local function mods_pressed()
  local mods = checkMods(true)._raw
  return mods>0 and mods ~= 65536 -- caps lock
end

local function exit(self)
   local selected = self.selected
   local windows = self.windows
   local drawings = self.drawings
   self.windows = nil
   self.selected = nil
   self.modsTimer = nil
   drawings.background:hide()
   drawings.highlight_rect:hide()
   for i = 1,#windows do
      local dr = drawings[i]
      dr.icon:hide()
      dr.title_text:hide()
   end
   windows[selected]:focus()
end

local function show(self, direction)
   local windows = self.windows
   local drawings = self.drawings

   if not windows then
      windows = self.window_filter:getWindows(hs.window.filter.sortByFocusedLast)
      self.windows = windows
   end
   local nwindows = #windows or 0
   if nwindows == 0 then return end

   self.ui.title_text_style = {
      font = self.ui.font_name,
      size = self.ui.font_size,
      color = self.ui.text_color,
      lineBreak = 'truncateTail',
   }

   local selected = self.selected
   if not selected then
      local temp_frame = hsgeom(0,0,1,1)
      for n = 1,nwindows do
	 local dr = {}
	 dr.icon = hsdrawing.image(temp_frame, UNAVAILABLE)
	 dr.title_rect = hsdrawing.rectangle(temp_frame)
	    :setFillColor(self.ui.title_background_color)
	    :setStroke(false)
	 dr.title_text = hsdrawing.text(temp_frame, ' ')
	    :setTextStyle(self.ui.title_text_style)
	 drawings[n] = dr
      end
      set_frames(nwindows, drawings, self.ui)
      draw(windows, drawings)
      selected = 1
      self.modsTimer = hstimer.waitWhile(
	 mods_pressed,
	 function() exit(self) end,
	 MODS_INTERVAL
      )
   end

   selected = selected + direction
   if selected <= 0 then
      selected = nwindows
   elseif selected > nwindows then
      selected = 1
   end
   self.selected = selected
   show_selected(selected, drawings)
end

function M:next() return show(self, 1) end
function M:previous() return show(self, -1) end

local ui_defaults = {
   text_color = rgb(1,1,1),
   font_name = "Monaco",
   font_size = 22,

   background_color = rgba(0.2, 0.2, 0.08, 0.8),
   highlight_color = rgba(0.6, 0.6, 0.3, 0.5),

   title_background_color = rgb(0,0,0),

   item_height = 64,
   title_width = 600,
   -- TODO: thumbnails
}

local function workspace_window_filter(hwin)
   if not hwin or not hwin:id() then
      return false
   end
   local hwin_id = hwin:id()
   for _, wid in pairs(current_workspace_windows) do
      if wid == hwin_id then return true end
   end
   return false
end

local function refresh_current_workspace_windows()
   hs.task.new(
      "/opt/homebrew/bin/aerospace",
      function(exitCode, stdOut, _)
         if exitCode ~= 0 or not stdOut or stdOut == "" then
            return
         end
         current_workspace_windows = {}
         for line in stdOut:gmatch("[^\r\n]+") do
            if line and line ~= "" then
               table.insert(current_workspace_windows, tonumber(line))
            end
         end
      end,
      {"list-windows", "--workspace", "focused", "--format", "%{window-id}"}
   ):start()
   hs.task.new(
      "/opt/homebrew/bin/aerospace",
      function(exitCode, stdOut, _)
         if exitCode ~= 0 or not stdOut or stdOut == "" then
            return
         end
         for line in stdOut:gmatch("[^\r\n]+") do
            if line and line ~= "" then
               focused_monitor_name = line
               print("focused monitor name is " .. focused_monitor_name)
            end
         end
      end,
      {"list-monitors", "--format", "%{monitor-name}", "--focused"}
   ):start()
end

function M.new(ui)
   if not ui then ui = {} end
   local self = setmetatable(
      {drawings={}},
      {__index=M,__gc=gc}
   )
   self.window_activity_filter = hs.window.filter.new():subscribe(interesting_window_events, refresh_current_workspace_windows)
   self.window_filter = hs.window.filter.new(workspace_window_filter):setSortOrder(hs.window.filter.sortByFocusedLast)
   self.drawings.screen_frame = nil
   self.screen_watcher = hs.screen.watcher.newWithActiveScreen(
      function() self.drawings.screen_frame = nil end
   ):start()
   self.ui = setmetatable(ui, {__index=ui_defaults})
   refresh_current_workspace_windows()
   return self
end

return M
