# Request (class)
`okhttp/src/main/java/okhttp3/Request`

```elm
type alias Request =
  { url : HttpUrl
  , method : String
  , headers : Headers
  , body : Maybe RequestBody
  , tags : Dict (Class a) Object
  }
  
type alias Headers =
  List (String, String) -- (name, value)
  
getUrl : Request -> HttpUrl
getUrl { url } = url

getMethod : Request -> String
getMethod { method } = method

getHeaders : Request -> Headers
getHeaders { headers } = headers

getFirstHeaderByName : String -> Request -> Maybe String
getFirstHeaderByName name { headers } =
  let
    sameName : ( String, String ) -> Maybe String
    sameName n value =
      if String.toLower(n) == String.toLower(name)
      then Maybe.Just value
      else Maybe.Nothing
      
    firstMatch : Headers -> Maybe String
    firstMatch = 
        Maybe.fromFirst
      <<
        List.foldr First.mappend First.mempty -- not sure how to implement this as `mconcat` without type class, possibly seperate implementation for each Monoid
      <<
        List.map First.fromMaybe(sameName)
  in 
    firstMatch headers

getHeadersByName : String -> Request -> Maybe String
getHeadersByName name { headers } =
  let
    isSameName : (String, _ ) -> Bool
    isSameName (n, _) = String.toLower(n) == String.toLower(name)
    
    allMatches : Headers -> List String
    allMatch =
        List.map Tuple.second
      <<
        List.filter isSameName
  in
    allMatch headers

getBody : Request -> Maybe RequestBody
getBody { body } = body

```

# Response (class)
`okhttp/src/main/java/okhttp3/Response`

```elm

type alias Response =
  { request : Request
  , protocol : Protocol
  , code : Int
  , message : String
  , handshake : Maybe Handshake
  , headers : Headers
  , body : Maybe ResponseBody
  , networkResponse : Maybe Response -- recursive definition
  , cacheResponse : Maybe Response -- recursive definition
  , priorResponse : Maybe Response -- recursive definition
  , sentRequestAtMillis : Long
  , receivedResponseAtMillis : Long
  , cacheControl : CacheControl
  }

```

# Interceptor (interface)
`okhttp/src/main/java/okhttp3/Interceptor`

[Avoiding IO > 6.Custom monad type class](https://wiki.haskell.org/Avoiding_IO#Custom_monad_type_class)

```purescript
data Interceptor
  = BridgeInterceptor { cookieJar : CookieJar }
  | CacheInterceptor { cache : InternalCache }
  | CallServerInterceptor { forWebSocket : Bool }
  | ConnectInterceptor { client : OkHttpClient }

data Chain = 
  RealInterceptorChain
    { interceptors : Interceptors
    , index : Int
    , request : Request
    , streamAllocation : StreamAllocation
    , httpCodec : HttpCodec
    , connection : RealConnection
    , call : Call
    , eventListener : EventListener
    , connectTimeout : Int
    , readTimeout : Int
    , writeTimeout : Int
    , calls : Int
    }
    

intercept :: Interceptor -> Chain -> Eff (http :: HTTP, exception :: EXCEPTION | eff) Response
intercept interceptor chain =
  case interceptor of
    BridgeInterceptor { cookieJar } -> 
      let
        { request } = chain
      in 
        bridgeIntercept cookieJar request
      
    CacheInterceptor { cache } ->
      let
        { request } = chain
      in
        cacheIntercept cache request

    CallServerInterpretor { forWebSocket } ->
      callServerInterpret forWebSocket chain
        
    ConnectInterceptor { client } ->
      let
        { connectTimeout, readTimeout, writeTimeout } = chain
      connectIntercept client connectTimeout readTimeout writeTimeout


proceed :: Request -> StreamAllocation -> HttpCodec -> RealConnection -> Eff (http :: HTTP, exception :: EXCEPTION | eff) Response

```


# OkHttpClient (class)

> OkHttpClients should be shared.
> each client holds its own connection pool and thread pools.
> Reusing connections and threads reduces latency and saves memory.


# Global Comments

Certainly, it's not following "single source of truth" principle.
Copying is everywhere.

Builder (`Request.Builder`, `Respond.Builder`) can be implemented as an Applicative validator (validation logic for each field) wrapping a higher-order constructor function.
This way, validation logic can be modulized and reused.
Error handling would be way cleaner without throwing exceptions everywhere.

Missing Union type in Java forces programmer to manually trim down a power set to one of its subset by conditionals without any guarantee from the compiler.
