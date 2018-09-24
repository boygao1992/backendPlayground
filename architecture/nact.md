[ncthbrt/nact](https://github.com/ncthbrt/nact)

```haskell
type Unknown = Unit

type Actor state msg =
  { parent :: Actor
  , children :: Array Actor
  , name :: String -- identity
  , path :: String
  , system :: Unknown
  , reference :: ActorReference
  , log :: Logger -- dependency, should move to type class constraint
  , f :: Unknown -- stateful message handler, :: state -> msg -> Actor
--, stopped :: Boolean
--, busy :: Boolean
  , status :: ActorStatus
  , mailbox :: Queue msg
  , immediate :: Unknown -- forall err. Eff (immediate :: IMMEDIATE | err)
  , onCrash :: Unknown -- event handler
  , initialState :: state
  , initialStateFunc :: Unknown -- :: context -> state
  }
  
data ActorStatus
  = Busy
  | Stopped
  
type ActorReference =
  { path :: Path
  , name :: String
  , parent :: Actor
  , system :: { name :: String } -- ?
  }

data SupervisionActions
  = Stop
  | StopAll
  | Escalate
  | Resume
  | Reset
  | ResetAll
```
