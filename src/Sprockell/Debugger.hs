module Sprockell.Debugger where
import Sprockell.HardwareTypes
import Control.Monad (when, void)
import System.IO (Handle,hPutStrLn,hGetLine)
import Text.Printf

-- | A Debugger will get this as input each clock cycle:
--   * a list of current 'Instruction's, one for each Sprockell core
--   * the 'SystemState' after executing those instructions
type DbgInput = ([Instruction], SystemState)


-- | A DebuggerF represents a debugger for the Sprockell system
--
-- It's a function from a abstract debugger state and a 'DebuggerInput'
-- to an IO action returning an potentially updated debugger state and SystemState.
type DebuggerF st = st -> DbgInput -> IO (st, SystemState)


-- |  A Debugger is a thing that you can pass to runWithDebugger of runWithDebuggerOverNetwork
--
-- Internally its a function that gets two Handles:
--    * the first  Handle is where the Debugger reads its input from
--    * the second Handle is where the Debugger writes it output to
-- , and produces a DebuggerPair connected to those handles.
type Debugger st = (Handle,Handle) -> DebuggerPair st

-- |  A DebuggerPair is a combination of a 'DebuggerF' and its initial state.
type DebuggerPair st = (DebuggerF st, st)

-- | No debugging at all, just run the system.
noDebugger :: Debugger ()
noDebugger _ = (noDebuggerF,())
    where
        noDebuggerF st (_,sys) = return (st,sys)

-- | Given a show function for the SystemState,
--   this creates a 'Debugger' that prints the 'SystemState'
--   at each time step using the supplied show function.
debuggerSimplePrint :: (DbgInput -> String) -> Debugger ()
debuggerSimplePrint showF = debuggerPrintCondWaitCond showF always never

-- | Like 'debuggerSimplePrint',
--   but pauses execution after each time step
--   and waits for you to hit enter to continue.
--   It let you step through the execution slowly.
debuggerSimplePrintAndWait :: (DbgInput -> String) -> Debugger ()
debuggerSimplePrintAndWait showF = debuggerPrintCondWaitCond showF always always


debuggerPrintCondWaitCond :: (DbgInput -> String) -- ^ show function used to show (parts of) the state
                          -> (DbgInput -> Bool)   -- ^ when this condition is True the state is printed
                          -> (DbgInput -> Bool)   -- ^ when this condition is True execution pauses
                          -> Debugger ()
debuggerPrintCondWaitCond showF showCond waitCond (inp,out) = (dbg, ())
    where
        dbg dbgSt now@(instrs,sysSt) = do
            when (showCond now) (hPutStrLn out $ showF now)
            when (waitCond now) (void $ hGetLine inp) -- wait for enter key
            return (dbgSt,sysSt)


-- ======================================================================
-- examples of show functions that you could with the above debuggers
-- ======================================================================
myShow :: DbgInput -> String
myShow (instrs,s) = printf "instrs: %s\nsprStates:\n%s\nrequests: %s\nreplies: %s\nrequestFifo: %s\nsharedMem: %s\n"
                    (show instrs)
                    (unlines $ map show $ sprStates s)
                    (show $ requestChnls s)
                    (show $ replyChnls s)
                    (show $ requestFifo s)
                    (show $ sharedMem s)

myShow' (instrs,s) = show instrs ++ "\n"
                     ++ (unlines $ map show $ sprStates s)


-- ======================================================================
-- examples of conditions that can be used with debuggerPrintCondWaitCond
-- ======================================================================
always,never :: DbgInput -> Bool
always = const True
never  = const False

-- | Checks whether any core in executing a Jump instruction
whenJumping :: DbgInput -> Bool
whenJumping (instrs,st) = any isJump instrs
    where
        isJump (Jump _) = True
        isJump _        = False

-- | Checks whether any of the program counters are in a given range
pcInRange :: CodeAddr -> CodeAddr -> (DbgInput -> Bool)
pcInRange min max (_,sysSt) = any inRange $ map pc $ sprStates sysSt
    where
        inRange x = min <= x && x <= max


-- ======================================================================
-- examples of conditions that can be used with debuggerPrintCondWaitCond
-- ======================================================================
class ToJson a where
  toJson :: a -> String

instance (ToJson a, ToJson b) => ToJson (a, b) where
  toJson (ii, state) = "{ \"instructions\": " ++ toJson ii ++ ", \"state\": " ++ toJson state ++ " }"

instance (ToJson a) => ToJson ([a]) where
  toJson xs = "[" ++ list xs ++ "]"
    where list [] = ""
          list (y:[]) = toJson y
          list (y:ys) = toJson y ++ ", " ++ list ys

instance ToJson Instruction where
  toJson (Compute op a b res) = "{ \"name\": \"Compute\", \"operator\": \"" ++ show op ++ "\", \"a\": " ++ show a ++ ", \"b\": " ++ show b ++ ", \"res\": " ++ show res ++ " }"
  
  toJson (Jump target) = "{ \"name\": \"Jump\", \"target\": " ++ toJson target ++ " }"
  toJson (Branch cond target) = "{ \"name\": \"Branch\", \"condition\": " ++ show  cond ++ ", \"target\": " ++ toJson target ++ " }"

  toJson (Load addr target) = "{ \"name\": \"Load\", \"source\": " ++ toJson addr ++ ", \"target\": " ++ show target ++ " }"
  toJson (Store source addr) = "{ \"name\": \"Store\", \"source\": " ++ show source ++ ", \"target\": " ++ toJson addr ++ " }"

  toJson (Push reg) = "{ \"name\": \"Push\", \"source\": " ++ show reg ++ " }"
  toJson (Pop reg) = "{ \"name\": \"Pop\", \"target\": " ++ show reg ++ " }"

  toJson (ReadInstr addr) = "{ \"name\": \"ReadInstr\", \"source\": " ++ toJson addr ++ " }"
  toJson (Receive reg) = "{ \"name\": \"Receive\", \"target\": " ++ show reg ++ " }"
  toJson (WriteInstr source addr) = "{ \"name\": \"WriteInstr\", \"source\": " ++ show source ++ ", \"target\": " ++ toJson addr ++ " }"
  toJson (TestAndSet target) = "{ \"name\": \"TestAndSet\", \"target\": " ++ toJson target ++ " }"


  toJson (Debug text) = "{ \"name\": \"Debug " ++ text ++ "\" }"

  toJson x = "{ \"name\": \"" ++ show x ++ "\" }"

instance ToJson AddrImmDI where
  toJson (ImmValue value) = "{ \"type\": \"ImmValue\", \"value\":" ++ show value ++ " }"
  toJson (DirAddr address) = "{ \"type\": \"DirAddr\", \"address\":" ++ show address ++ " }"
  toJson (IndAddr reg) = "{ \"type\": \"IndAddr\", \"reg\":" ++ show reg ++ " }"

instance ToJson Target where
  toJson (Abs addr) = "{ \"type\": \"Abs\", \"address\": " ++ show addr ++ " }"
  toJson (Rel addr) = "{ \"type\": \"Rel\", \"address\": " ++ show addr ++ " }"
  toJson (Ind reg) = "{ \"type\": \"Ind\", \"reg\": " ++ show reg ++ " }"

instance ToJson SystemState where
  toJson (SystemState spr _ _ _ mem) = "{ \"sprockels\": " ++ toJson spr ++ ", \"sharedMem\": " ++ show mem ++ " }"

instance ToJson SprockellState where
  toJson (SprState pc sp regs mem) = "{ \"pc\": " ++ show pc ++ ", \"sp\": " ++ show sp ++ ", \"regs\": " ++ show regs ++ ", \"localMem\": " ++ show mem ++ " }"

