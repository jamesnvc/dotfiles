;; Previous window
(fn get-previous-window []
  "Return the window object for most-recently used window."
  (var found-current false)
  (var prev nil)
  (each [_ win (pairs (hs.window.orderedWindows)) &until prev]
    (if (= (win:subrole) "AXUnknown")
        nil
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

(hs.hotkey.bind ["cmd" "ctrl"] "Left" (fn [] (move-window-to (hs.geometry.rect 0 0 0.5 1))))
(hs.hotkey.bind ["cmd" "ctrl"] "Right" (fn [] (move-window-to (hs.geometry.rect 0.5 0 0.5 1))))
(hs.hotkey.bind ["cmd" "shift"] "Space" (fn [] (move-window-to (hs.geometry.rect 0 0 1 1))))

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
