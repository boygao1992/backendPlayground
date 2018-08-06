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

# Interceptor (interface)
`okhttp/src/main/java/okhttp3/Interceptor`

```purescript
class Chain a where
  proceed :: Request -> Eff (http :: HTTP, exception :: EXCEPTION | eff) Response

class Chain a <= Interceptor a where
  intercept :: a -> Eff (http :: HTTP, exception :: EXCEPTION | eff) Response
```

direct(rough) translation from `interface` to `type class`, anti-pattern

[Avoiding IO > 6.Custom monad type class](https://wiki.haskell.org/Avoiding_IO#Custom_monad_type_class)
