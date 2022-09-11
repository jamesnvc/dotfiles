(fn move [axis incr]
  (let [win (hs.window.focusedWindow)
        f (win:frame)]
    (tset f axis (+ (. f axis) incr))
    (win:setFrame f)))


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
