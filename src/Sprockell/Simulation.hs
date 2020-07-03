module Sprockell.Simulation where

import Control.DeepSeq
import Sprockell.BasicFunctions
import Sprockell.HardwareTypes
import Sprockell.Sprockell
import Sprockell.System
import Sprockell.Debugger

import System.IO         (BufferMode(..),Handle,stdin,stdout,stderr,hGetBuffering,hSetBuffering,hPutStrLn,IOMode(..),hClose)
import Control.Exception (bracket)

import Network.Socket

-- ====================================================================================================
-- Sprockell Test
-- ====================================================================================================

sprockellSim :: InstructionMem
                -> SprockellState
                -> [Reply]
                -> [(Instruction, SprockellState, Request)]

sprockellSim instrs s []     = []
sprockellSim instrs s (i:is) | instr /= EndProg    = (instr,s',o) : sprockellSim instrs s' is
                             | otherwise           = []
                where
                  (s',o) = sprockell instrs s i
                  instr  = instrs ! pc s

initSprockellState :: Value -> SprockellState
initSprockellState sprID = SprState
        { pc       = 0
        , sp       = localMemSize
        , regbank  = replicate regbankSize 0 <~ (regSprID,sprID)
        , localMem = fromList $ replicate localMemSize 0
        }

sprTest :: Value -> InstructionMem -> [Reply] -> IO ()
sprTest sprID instrs input = putStr
                           $ unlines
                           $ map show
                           $ sprockellSim instrs (initSprockellState sprID) input

-- ====================================================================================================
-- System Test
-- ====================================================================================================

systemSim :: DebuggerPair st -> [InstructionMem] -> SystemState -> Clock -> IO ()
systemSim (dbg,dbgSt) instrss s []     = return ()
systemSim (dbg,dbgSt) instrss s (t:ts) | sysHalted = return ()
                                       | otherwise = do
                                           let curInstrs = zipWith (!) instrss (map pc $ sprStates s)
                                           s' <- deepseq s $ system instrss s t
                                           (dbgSt',s'') <- dbg dbgSt (curInstrs,s')
                                           systemSim (dbg,dbgSt') instrss s'' ts
    where
        instrs    = zipWith (!) instrss (map pc $ sprStates s)
        sysHalted = (and $ map (==EndProg) $ zipWith (!) instrss $ map pc $ sprStates s)
                  && (and $ map and $ map (map (==NoRequest)) $ requestChnls s)
                  && (and $ map (\(_,r) -> r == NoRequest) $ requestFifo s)


initSystemState nrOfSprockells = SystemState
        { sprStates     = map initSprockellState [0 .. nrOfSprockells-1]
        , requestChnls  = replicate nrOfSprockells $ replicate channelDelay NoRequest
        , replyChnls    = replicate nrOfSprockells $ replicate channelDelay Nothing
        , requestFifo   = []
        , sharedMem     = fromList $ replicate shMemSize 0
        }

run :: [[Instruction]] -> IO ()                                  -- list of programs per Sprockell
run = runWithDebugger noDebugger

runWithDebugger :: Debugger st -> [[Instruction]] -> IO ()       -- debugger + list of programs per Sprockell
runWithDebugger dbg instrss = do
    bracket setupBuffering
            restoreBuffering
            (\_ -> systemSim dbgPair instrss' (initSystemState nrOfSprockells) clock)
    return ()
    where nrOfSprockells = length instrss
          instrss' = map fromList instrss  -- conversion to Memory
          dbgPair = dbg (stdin,stdout)



runWithDebuggerOverNetwork :: Debugger st -> [[Instruction]] -> IO ()       -- debugger + list of programs per Sprockell
runWithDebuggerOverNetwork dbg instrss = do
    bracket setupBuffering
            restoreBuffering
            (\_ -> bracket  listenForFirstConnect
                            (\(h,_) -> hPutStrLn stderr "Closing connection" >> hClose h)
                            (\(h,addr) -> do
                                hPutStrLn stderr $ "Accepted connection from " ++ show addr ++", starting Sprockell program...\n"
                                let dbgPair = dbg (h,h)
                                systemSim dbgPair instrss' (initSystemState nrOfSprockells) clock
                            )
            )
    return ()
    where nrOfSprockells = length instrss
          instrss' = map fromList instrss  -- conversion to Memory


listenForFirstConnect :: IO (Handle, SockAddr)
listenForFirstConnect = withSocketsDo $ do
    addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}))
                    (Just "127.0.0.1")
                    (Just "1235") -- pick any free port
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bind sock (addrAddress serveraddr)

    listen sock 1
    port <- socketPort sock
    hPutStrLn stderr $ "Now listening on port " ++ show port
    hPutStrLn stderr "To start the Sprockells, connect to the debugger using the command:\n"
    hPutStrLn stderr $ "\ttelnet 127.0.0.1 " ++ show port ++"\n\n"
    (clSock,clAddr) <- accept sock
    clHandle <- socketToHandle clSock ReadWriteMode
    close sock -- close listening socket, not accepting any more incoming connections
    return (clHandle,clAddr)

setupBuffering :: IO (BufferMode,BufferMode)
setupBuffering = do
    oldin  <- hGetBuffering stdin
    oldout <- hGetBuffering stdout
    hSetBuffering stdin  NoBuffering  -- needed to make charIO work nicely
    hSetBuffering stdout NoBuffering
    return (oldin,oldout)

restoreBuffering :: (BufferMode,BufferMode) -> IO ()
restoreBuffering (modeIn,modeOut) = do
    hSetBuffering stdin  modeIn
    hSetBuffering stdout modeOut
