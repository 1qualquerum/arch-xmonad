------------------------------------------------------------------------
-- IMPORTS
------------------------------------------------------------------------

  -- Base
import XMonad
import System.Directory
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.WithAll (sinkAll, killAll)

    -- Data
import Data.Monoid

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed

    -- Modificadores de layout
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

   -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

------------------------------------------------------------------------
-- VARIÁVEIS
------------------------------------------------------------------------

myModMask :: KeyMask
myModMask = mod4Mask            -- Definindo a tecla mod como a tecla Super(windows)

myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

myTerminal :: String
myTerminal = "alacritty"        -- Define o terminal padrão

myBrowser :: String
myBrowser = "firefox "          -- Define o navegador padrão

myBorderWidth :: Dimension
myBorderWidth = 2               -- Define a largura da borda das janelas

myNormColor :: String
myNormColor   = "#221d36"       -- Define a Cor Normal

myFocusColor :: String
myFocusColor  = "#6959a8"       -- Define a Cor em foco

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False     -- Se o foco segue o mouse

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

------------------------------------------------------------------------
-- AUTOSTART
------------------------------------------------------------------------

myStartupHook :: X ()
myStartupHook = do
          spawnOnce "lxsession &"
          spawnOnce "nitrogen --restore &"
          spawnOnce "picom &"
          spawnOnce "nm-applet &"
          spawnOnce "volumeicon &"
          spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x282c34  --height 22 &"
          setWMName "LG3D"

------------------------------------------------------------------------
-- LOGHOOK
------------------------------------------------------------------------
-- Sets opacity for inactive (unfocused) windows. I prefer to not use
-- this feature so I've set opacity to 1.0. If you want opacity, set
-- this to a value of less than 1 (such as 0.9 for 90% opacity).

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 1.0
------------------------------------------------------------------------
-- LAYOUTS
------------------------------------------------------------------------

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Definindo alguns layouts. para mudar o tamnho do espaçamanto mude o valor de
-- "mySpacing" indivudialmente por layout.

tall     = renamed [Replace "tall"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (Simplest)
           $ limitWindows 12
           $ mySpacing 4
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme  
           $ subLayout [] (Simplest)
           $ limitWindows 20 Full
floats   = renamed [Replace "floats"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (Simplest)
           $ limitWindows 20 simplestFloat
grid     = renamed [Replace "grid"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (Simplest)
           $ limitWindows 12
           $ mySpacing 4
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
tabs     = renamed [Replace "tabs"]
           -- Não posso adicionar espaçamento nesse layout, pois irá
           -- adiconnar espaços entre as janelas e as tabs, ficando feio.
           $ tabbed shrinkText myTabTheme

-- Definindo as cores para o layout e sublayout tipo tab.
myTabTheme = def { fontName            = myFont
                 , activeColor         = "#46d9ff"
                 , inactiveColor       = "#313846"
                 , activeBorderColor   = "#46d9ff"
                 , inactiveBorderColor = "#282c34"
                 , activeTextColor     = "#282c34"
                 , inactiveTextColor   = "#d0d0d0"
                 }

-- Tematizando o showName que mostra o nome do workspace ao mudar de workspace.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Ubuntu:bold:size=60"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#1c1f24"
    , swn_color             = "#ffffff"
    }

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $
               mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               -- I've commented out the layouts I don't use.
               myDefaultLayout =     tall
                                 ||| noBorders monocle
                                 ||| floats
                                 ||| grid
                                 ||| noBorders tabs

------------------------------------------------------------------------
-- WORKSPACES
------------------------------------------------------------------------

myWorkspaces :: [String]
myWorkspaces = [" web ", " chat ", " dev ", " call ", " sys ", " doc ", " vid ", " mus ", " rdn "]

------------------------------------------------------------------------
-- MANAGEHOOK
------------------------------------------------------------------------

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     -- Usando "doShift ( myWorkspaces !! 7)"" manda o programa para o workspace 8!!
     -- É feito desta maneira, pois de outra forma seria necessário escrever o nome completo dos workspaces
     -- e os nomes seriam muito se estivermos usando workspaces clicáveis.
     [ title =? "Mozilla Firefox"     --> doShift ( myWorkspaces !! 0 )
     , className =? "mpv"     --> doShift ( myWorkspaces !! 6 )
     , title =? "WhatsApp"    --> doShift ( myWorkspaces !! 1 )
     , className =? "discord"     --> doShift ( myWorkspaces !! 3 )
     , className =? "Gimp"    --> doShift ( myWorkspaces !! 8 )
     , className =? "Gimp"    --> doFloat
     , title =? "Oracle VM VirtualBox Manager"     --> doFloat
     , className =? "VirtualBox Manager" --> doShift  ( myWorkspaces !! 4 )
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     ]

------------------------------------------------------------------------
-- KEYBINDINGS
------------------------------------------------------------------------

myKeys :: String -> [([Char], X ())]
myKeys home =
    -- Xmonad
        [ ("M-C-r", spawn "xmonad --recompile") -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")   -- Restarts xmonad
        , ("M-S-q", io exitSuccess)             -- Quits xmonad

    -- Iniciar um prompt de sua escolha
        , ("M-<Space>", spawn "dmenu_run") -- Dmenu
        -- , ("M-S-<Return>", spawn "rofi -show drun -config ~/.config/rofi/themes/grimaldi-dmenu.rasi -display-drun \"Run: \" -drun-display-format \"{name}\"") -- Rofi

    -- Programas úteis para se ter atalhos para iniciar
        , ("M-M1-t", spawn myTerminal)
        , ("M-b", spawn myBrowser)
        , ("M-M1-h", spawn (myTerminal ++ " -e htop"))
        , ("M-S-s", spawn "spectacle -r")     -- tirar print de uma região da tela

    -- Fechar (matar) janela
        , ("M-c", kill1)     -- Mata a janela atualmente em foco
        , ("M-S-c", killAll)   -- Mata todas as janelas do workspace atual

    -- Floating windows
        , ("M-f", sendMessage (T.Toggle "floats")) -- Alterna para o modo float
        , ("M-t", withFocused $ windows . W.sink)  -- Manda janela flutuando de volta para o modo tiling
        , ("M-S-t", sinkAll)                       -- Manda todas as janelas flutuantes para o modo tiling

    -- Increase/decrease spacing (gaps)
        , ("M-d", decWindowSpacing 4)           -- Diminui o espaçamento da janela
        , ("M-a", incWindowSpacing 4)           -- Aumenta o espaçamento da janela
        , ("M-C-d", decScreenSpacing 4)         -- Diminui o espaçamento da tela
        , ("M-C-a", incScreenSpacing 4)         -- Aumenta o espaçamento da tela

    -- Windows navigation
        , ("M-m", windows W.focusMaster)  -- Move o foco para a janela master
        , ("M-j", windows W.focusDown)    -- Move o foco para a proxima janela
        , ("M-k", windows W.focusUp)      -- Move o foco para a janela anterior
        , ("M-C-m", windows W.swapMaster) -- Troca a janela em foco com a master
        , ("M-C-j", windows W.swapDown)   -- Troca a janela em foco com a próxima
        , ("M-C-k", windows W.swapUp)     -- Troca a janela em foco com a anterior
        , ("M-<Backspace>", promote)      -- Move a janela em fofo para a master, mantendo a ordem dos outros.
        , ("M-S-<Tab>", rotSlavesDown)    -- Gira todas as janelas exceto a master e mantém o foco no lugar.
        , ("M-C-<Tab>", rotAllDown)       -- Gira todas as janelas no plano atual

    -- Layouts
        , ("M-<Tab>", sendMessage NextLayout)           -- Troca para o próximo layout
        , ("M-C-M1-<Up>", sendMessage Arrange)
        , ("M-C-M1-<Down>", sendMessage DeArrange)
        , ("M1-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Alterna para sem bordas + fullscreen
        , ("M-S-<Space>", sendMessage ToggleStruts)     -- habilita/desabilita as estruturas (janela cobre o xmobar)
        , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)  -- alterna o modo sem bordas

    -- Increase/decrease windows in the master pane or the stack
        , ("M-S-<Up>", sendMessage (IncMasterN 1))      -- Aumenta o número de janelas no painel master
        , ("M-S-<Down>", sendMessage (IncMasterN (-1))) -- Diminui o número de janelas no painel master
        , ("M-C-<Up>", increaseLimit)                   -- Aumenta o número de janelas
        , ("M-C-<Down>", decreaseLimit)                 -- Diminui o número de janelas

    -- Window resizing
        , ("M-h", sendMessage Shrink)                   -- Diminui horizontalmente a largura das janelas
        , ("M-l", sendMessage Expand)                   -- Aumenta horizontalmente a largura das janelas
        , ("M-M1-k", sendMessage MirrorShrink)          -- Diminui verticalmente a altura das janelas
        , ("M-M1-j", sendMessage MirrorExpand)          -- Aumenta verticalmente a altura das janelas

    -- Sublayouts
    -- Atalhos usados para mandar ou tirar janelas do sublayout de abas.
        , ("M-S-h", sendMessage $ pullGroup L)          -- esquerda
        , ("M-S-l", sendMessage $ pullGroup R)          -- direita
        , ("M-S-k", sendMessage $ pullGroup U)          -- cima
        , ("M-S-j", sendMessage $ pullGroup D)          -- baixo
        , ("M-S-m", withFocused (sendMessage . MergeAll))
        , ("M-S-u", withFocused (sendMessage . UnMerge))
        , ("M-S-/", withFocused (sendMessage . UnMergeAll))
        , ("M-S-.", onGroup W.focusUp')    -- Mudar o foco para a próxima aba
        , ("M-S-,", onGroup W.focusDown')  -- Mudar o foco para a próxima aba
        ]

------------------------------------------------------------------------
-- MAIN
------------------------------------------------------------------------

main :: IO ()
main = do
    home <- getHomeDirectory
    -- Launching three instances of xmobar on their monitors.
    xmproc <- spawnPipe "xmobar"
    -- the xmonad, ya know...what the WM is named after!
    xmonad $ ewmh def
        { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
        -- Run xmonad commands from command line with "xmonadctl command". Commands include:
        -- shrink, expand, next-layout, default-layout, restart-wm, xterm, kill, refresh, run,
        -- focus-up, focus-down, swap-up, swap-down, swap-master, sink, quit-wm. You can run
        -- "xmonadctl 0" to generate full list of commands written to ~/.xsession-errors.
        -- To compile xmonadctl: ghc -dynamic xmonadctl.hs
        , handleEventHook    = docksEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , focusFollowsMouse  = myFocusFollowsMouse
        , startupHook        = myStartupHook
        , layoutHook         = showWName' myShowWNameTheme $ myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP
                        { ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]" -- Current workspace in xmobar
                        , ppVisible = xmobarColor "#98be65" ""                -- Visible but not current workspace
                        , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#c792ea" ""        -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "#b3afc2" "" . shorten 60     -- Title of active window in xmobar
                        , ppSep =  "<fc=#666666> <fn=1>|</fn> </fc>"          -- Separators in xmobar
                        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                        , ppExtras  = [windowCount]                           -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
        } `additionalKeysP` myKeys home
