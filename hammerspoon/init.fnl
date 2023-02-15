;; Inspired by https://github.com/kbd/setup/blob/master/HOME/.hammerspoon/init.fnl

;; helpers

(fn fuzzy [choices func]
  (doto (hs.chooser.new func)
    (: :searchSubText true)
    (: :fgColor {:hex "#111"})
    (: :subTextColor {:hex "#555"})
    (: :width 15)
    (: :show)
    (: :choices choices)))

;; Previous window
(fn get-previous-window []
  "Return the window object for most-recently used window."
  (var found-current false)
  (var prev nil)
  (each [_ win (pairs (hs.window.orderedWindows)) &until prev]
    (if (= (win:subrole) "AXUnknown") nil
        (not found-current) (set found-current true)
        (set prev win)))
  prev)

(fn focus-previous-window []
  (: (get-previous-window) :focus))

(hs.hotkey.bind ["cmd" "alt"] "x" focus-previous-window)

;; grid

(hs.grid.setGrid "3x2")
(set hs.grid.HINTS [["f7" "f8" "f9" "f10"]
                    ["7" "8" "9" "0"]
                    ["g" "c" "r" "l"]
                    ["h" "t" "n" "s"]
                    ["m" "w" "v" "z"]])
(hs.hotkey.bind hyper "G" hs.grid.show)

;; moving windows - helpers

(fn move-window-to [unit-rect]
  (let [w (hs.window.focusedWindow)
        scr (w:screen)
        f (scr:frame)]
    (w:setFrame (unit-rect:fromUnitRect f))))

(hs.hotkey.bind hyper "Left" (fn [] (move-window-to (hs.geometry.rect 0 0 0.5 1))))
(hs.hotkey.bind hyper "Right" (fn [] (move-window-to (hs.geometry.rect 0.5 0 0.5 1))))
(hs.hotkey.bind hyper "Up" (fn [] (move-window-to (hs.geometry.rect 0 0 1 1))))

;; closing notifications

(fn close-notifications []
  (hs.osascript.applescript "
tell application \"System Events\"
	set notificationWindows to windows of process \"Notification Center\"
	set theGroups to UI elements of first item of UI elements of first item of UI elements of first item of notificationWindows
	set theGroup to first item of theGroups
	repeat with theAction in actions of theGroup
		if (description of theAction) = \"Close\" or (description of theAction) = \"Clear All\" then
			perform theAction
			return
		end if
	end repeat
end tell"))

(hs.hotkey.bind hyper "C" close-notifications)

;; audio switching

(fn select-audio [audio]
  (when audio
    (let [device (hs.audiodevice.findDeviceByUID audio.uid)]
      (hs.alert.show (.. "Setting " audio.subText " device: " (device:name)))
      (if (device:isOutputDevice)
        (device:setDefaultOutputDevice)
        (device:setDefaultInputDevice)))))

(fn show-audio-fuzzy []
  (let [devices (hs.audiodevice.allDevices)
        input-uid (: (hs.audiodevice.defaultInputDevice) :uid)
        output-uid (: (hs.audiodevice.defaultOutputDevice) :uid)
        choices #(icollect [_ device (ipairs devices)]
          (let [uid (device:uid)
                (active subText) (if (device:isOutputDevice)
                                  (values (= uid output-uid) "output")
                                  (values (= uid input-uid) "input"))
                text (device:name)
                subText (.. subText (if active " (active)" ""))
                valid (not active)]
            {: text : uid : subText : valid}))]
    (fuzzy choices select-audio)))

(hs.hotkey.bind hyper "A" show-audio-fuzzy)

;; fuzzy-find windows

(fn show-window-fuzzy []
  (let [imgs-cache {}
        focused (: (hs.window.focusedWindow) :id)
        choices #(icollect [_ window (ipairs (hs.window.orderedWindows))]
                   (let [win-app (window:application)]
                     (when (= (. imgs-cache win-app) nil)
                       (tset imgs-cache win-app (hs.image.imageFromAppBundle (win-app:bundleID))))
                     {:text (window:title)
                      :subText (.. (win-app:title) (if (= (window:id) focused) " (active)" ""))
                      :valid (not= focused (window:id))
                      :image (. imgs-cache win-app)
                      : window}))]
    (fuzzy choices (fn [win] (when win (win.window:focus))))))

(hs.hotkey.bind hyper "Space" show-window-fuzzy)

;; fast switching

(fn switch-to [bundle-id]
  (let [app (. (hs.application.applicationsForBundleID bundle-id) 1)]
    (when app (app:activate))))

(hs.hotkey.bind hyper "e" (fn [] (switch-to "org.gnu.Emacs")))
(hs.hotkey.bind hyper "f" (fn [] (switch-to "org.mozilla.firefox")))
(hs.hotkey.bind hyper "t" (fn [] (switch-to "com.tapbots.IvoryMac")))
