import System.IO
import System.Exit
import Control.Monad
import XMonad
import XMonad.Actions.Commands
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.UpdatePointer
import XMonad.Actions.CopyWindow
import XMonad.Actions.GridSelect
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.WindowArranger
import XMonad.Layout.ShowWName
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.WindowNavigation
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutHints
import XMonad.Layout.PerWorkspace
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Layout.IM
import Data.Ratio ((%))
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.AppendFile
import XMonad.Util.NamedScratchpad
import qualified XMonad.Actions.Submap as SM
import XMonad.Hooks.DynamicLog
import XMonad.Prompt.KillProcesses


workspaceNames = map return ['a'..'z']

specialWorkspaces = ["NSP"]

bigXPConfig = defaultXPConfig
        { font = "xft:terminal-20"
        , fgColor = "#DDDDDD"
        , bgColor = "#333333"
        , fgHLight = "#000000"
        , bgHLight = "#FFFFFF"
        , promptBorderWidth = 0
        , borderColor =  "#3f3c6d"
        , height = 40
        }

smallXPConfig = bigXPConfig
 {
  font = "xft:terminal-8"
  , height = 20
  }

scratchpads = [
     NS "htop" "urxvtc -e htop" (title =? "htop")
         (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)),
     NS "social" "gwibber" (className =? "gwibber")
         (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)),
     NS "music" "urxvtc -title music -e ncmpc" (title =? "music")
         (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)),
     NS "notes" "gvim --role notes -c 'set autoread' -c'set wrap' -c 'au FocusLost * :wa' -c 'colorscheme slate' -c 'Note'" (role =? "notes")
         (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)),
     NS "information" "conky" (className =? "Conky")
         (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)),
     NS "volume" "urxvtc -e alsamixer" (title =? "alsamixer")
         (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
 ] where role = stringProperty "WM_WINDOW_ROLE"

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask .|. controlMask, xK_l     ), spawn "xtrlock")
    -- , ((modMask .|. controlMask, xK_s     ), spawn "pm-suspend")
    , ((modMask .|. shiftMask, xK_c     ), kill1)
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask, xK_f ), sendMessage $ Toggle "Full")
    , ((modMask,               xK_n     ), refresh)
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask .|. shiftMask,               xK_Tab   ), windows W.swapDown)
    , ((modMask,               xK_Return), windows W.swapMaster)
    , ((modMask,               xK_bracketleft     ), sendMessage Shrink)
    , ((modMask,               xK_bracketright     ), sendMessage Expand)
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q     ), restart "xmonad" True)
    , ((modMask, xK_g), goToSelected defaultGSConfig)
    , ((modMask .|. shiftMask, xK_g), bringSelected defaultGSConfig)
    , ((modMask              , xK_BackSpace), focusUrgent)
    , ((modMask .|. controlMask, xK_y), defaultCommands >>= runCommand)
    , ((modMask, xK_x), runOrRaisePrompt bigXPConfig)
    , ((modMask, xK_x), killProcesses bigXPConfig)
    , ((modMask,                 xK_Right), sendMessage $ Go R)
    , ((modMask,                 xK_Left), sendMessage $ Go L)
    , ((modMask,                 xK_Up), sendMessage $ Go U)
    , ((modMask,                 xK_Down), sendMessage $ Go D)
    , ((modMask,                 xK_l), sendMessage $ Go R)
    , ((modMask,                 xK_h), sendMessage $ Go L)
    , ((modMask,                 xK_k), sendMessage $ Go U)
    , ((modMask,                 xK_j), sendMessage $ Go D)
    , ((modMask .|. shiftMask, xK_Right), sendMessage $ Swap R)
    , ((modMask .|. shiftMask, xK_Left), sendMessage $ Swap L)
    , ((modMask .|. shiftMask, xK_Up), sendMessage $ Swap U)
    , ((modMask .|. shiftMask, xK_Down), sendMessage $ Swap D)
    , ((modMask .|. shiftMask, xK_l), sendMessage $ Swap R)
    , ((modMask .|. shiftMask, xK_h), sendMessage $ Swap L)
    , ((modMask .|. shiftMask, xK_k), sendMessage $ Swap U)
    , ((modMask .|. shiftMask, xK_j), sendMessage $ Swap D)

    , ((modMask, xK_c), SM.submap . M.fromList $
            [((modMask, xK_d), changeDir bigXPConfig)])
    , ((modMask, xK_i), SM.submap . M.fromList $
            [((modMask, xK_n), namedScratchpadAction scratchpads "notes"),
            ((modMask, xK_i), namedScratchpadAction scratchpads "information"),
            ((modMask, xK_p), namedScratchpadAction scratchpads "htop"),
            ((modMask, xK_m), namedScratchpadAction scratchpads "music"),
            ((modMask, xK_s), namedScratchpadAction scratchpads "social"),
            ((modMask, xK_v), namedScratchpadAction scratchpads "volume")])
    , ((modMask, xK_m), SM.submap . M.fromList $
            [ ((modMask, xK_p), spawn "mpc toggle")
            , ((modMask, xK_comma), spawn "mpc prev")
            , ((modMask, xK_i), spawn "notify-send \"`mpc current`\"")
            , ((modMask, xK_period), spawn "mpc next")
            ])
    , ((modMask, xK_o), SM.submap . M.fromList $
            [((modMask, xK_e), spawn "emacs")
            , ((modMask, xK_v), spawn "vlc")
            , ((modMask, xK_t), spawn $ XMonad.terminal conf)
            , ((modMask, xK_o), spawn "synapse")
            , ((modMask, xK_f), spawn "urxvtc -e ranger")
            ])
    , ((modMask, xK_b), SM.submap . M.fromList $
            [ ((modMask, xK_f), spawn "google-chrome --new-window facebook.com")
            , ((modMask, xK_t), spawn "google-chrome --new-window twitter.com")
            , ((modMask, xK_g), spawn "google-chrome --new-window google.co.in")
            , ((modMask, xK_u), spawn "google-chrome --new-window youtube.com")
            , ((modMask, xK_m), spawn "google-chrome --new-window mail.google.com")
            , ((modMask, xK_o), spawn "firefox")
            , ((modMask, xK_space), spawn "google-chrome")
            , ((modMask, xK_BackSpace), spawn "google-chrome --incognito")
            ])
    , ((modMask, xK_w), SM.submap . M.fromList $
            [ ((modMask, xK_period), killAllOtherCopies)
            , ((modMask, xK_8), windows copyToAll)
            ])
    , ((modMask, xK_a), SM.submap . M.fromList $
            [ ((modMask, xK_n), appendFilePrompt bigXPConfig "~/Dropbox/notes/Everything")
            ])
-- Workspace handling idea from here http://www.reddit.com/r/xmonad/comments/fgyzc/xmonadactionsdynamicworkspaces_where_have_you/c1g0re2 
-- and http://www.reddit.com/r/xmonad/comments/fgyzc/xmonadactionsdynamicworkspaces_where_have_you/c1qprcu
    , ((modMask, xK_d), SM.submap . M.fromList $
            zip (zip (repeat (modMask)) [xK_a..xK_z]) (map (windows . W.greedyView) workspaceNames)
            ++
            zip (zip (repeat (modMask .|. shiftMask)) [xK_a..xK_z]) (map (windows . (liftM2 (.) W.view W.shift)) workspaceNames)
            ++
            zip (zip (repeat (modMask .|. controlMask)) [xK_a..xK_z]) (map (windows . (liftM2 (.) W.view copy)) workspaceNames)
            ++
            [
            ((modMask, xK_space), renameWorkspace bigXPConfig)
            , ((modMask, xK_BackSpace), removeWorkspace)
            , ((modMask, xK_Left), moveTo Prev HiddenNonEmptyWS)
            , ((modMask, xK_Right), moveTo Next HiddenNonEmptyWS)
            , ((modMask, xK_Return), toggleWS' specialWorkspaces)
            , ((modMask .|. shiftMask, xK_Return), toggleWSWhileShiftingCurrentWindow)
            ]
      )
    , ((modMask, xK_s), SM.submap . M.fromList $
            [
            ((modMask, xK_Return), swapNextScreen),
            ((modMask, xK_Left), viewScreen 0),
            ((modMask, xK_Right), viewScreen 1),
            ((modMask, xK_Up), viewScreen 0),
            ((modMask, xK_Down), viewScreen 1),
            ((modMask .|. shiftMask, xK_Left), do
             sendToScreen 0
             viewScreen 0),
            ((modMask .|. shiftMask, xK_Right), do
             sendToScreen 1
             viewScreen 1),
            ((modMask .|. shiftMask, xK_Up), do
             sendToScreen 0
             viewScreen 0),
            ((modMask .|. shiftMask, xK_Down), do
             sendToScreen 1
             sendToScreen 1),
            ((modMask, xK_h), viewScreen 0),
            ((modMask, xK_l), viewScreen 1),
            ((modMask, xK_k), viewScreen 0),
            ((modMask, xK_j), viewScreen 1),
            ((modMask .|. shiftMask, xK_h), do
             sendToScreen 0
             viewScreen 0),
            ((modMask .|. shiftMask, xK_l), do
             sendToScreen 1
             viewScreen 1),
            ((modMask .|. shiftMask, xK_k), do
             sendToScreen 0
             viewScreen 0),
            ((modMask .|. shiftMask, xK_j), do
             sendToScreen 1
             sendToScreen 1)
            ]
            )
            ]

data MinimalPrompt = MinimalPrompt String

instance XPrompt MinimalPrompt where
  showXPrompt (MinimalPrompt s) = ""

minimalPrompt c = mkXPromptWithReturn (MinimalPrompt undefined) c (const (return [])) return

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

defaultLayout = layoutHintsToCenter (tiled)
            ||| layoutHintsToCenter (Mirror tiled)
  where
     tiled  = Tall 1 (3 / 100) (1 / 2)

myLayout = showWName' (defaultSWNConfig {swn_fade = 0.1, swn_font = "xft: Ubuntu-30", swn_color = "#a8f7a3", swn_bgcolor = "#3f3c6d"}) $ toggleLayouts Full $ workspaceDir "" $ windowNavigation $ avoidStruts
        $ onWorkspace "i" (withIM (1%7) (Role "buddy_list") defaultLayout)
        $ defaultLayout

myManageHook = composeAll .concat $ [[namedScratchpadManageHook scratchpads, manageDocks], [className =? "Do" --> doIgnore ]]
main = xmonad $ ewmh defaultConfig {
        focusFollowsMouse  = True,
        terminal  = "urxvtc",
        modMask            = mod4Mask,
        workspaces         = workspaceNames,
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
        focusedBorderColor = "#00FF00",
        normalBorderColor = "#000000"
        , borderWidth        = 3
        , manageHook         = manageHook defaultConfig <+> myManageHook
        , logHook            = do
                       dynamicLog
                       updatePointer (Relative 0.5 0.5)
        , startupHook        = do
                  setWMName "LG3D"
                  startupHook defaultConfig
                  spawn "killall xflux; ~/xflux  -l 12.9833 -g 77.5833"
                  spawn "synclient PalmDetect=1"
                  spawn "synclient TapButton1=1"
                  spawn "synclient TapButton2=3"
                  spawn "synclient TapButton3=2"
                  spawn "pidgin"
                  spawn "killall parcellite; parcellite"
                  spawn "xsetroot -solid darkred"
                  spawn "xmodmap ~/.xmodmaprc"
                  spawn "urxvtd"
                  spawn "dropbox start"
        , layoutHook         = windowArrange $ smartBorders $ myLayout
  }


toggleWSWhileShiftingCurrentWindow = do
   hs <- gets $ (flip skipTags) specialWorkspaces . W.hidden . windowset
   unless (null hs) (windows . (liftM2 (.) W.view W.shift) . W.tag $ head hs)
