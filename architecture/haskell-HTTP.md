[Network.HTTP](http://hackage.haskell.org/package/HTTP)

# Modules

Basic division
- HTTP
 - Headers
  - Base
  - HandleStream
- TCP
- Stream
- URI

# Model

## URI

### Data Model

`network-uir/Network/URI.hs`
``` haskell
data URI = URI
    { uriScheme     :: String           -- ^ @foo:@
    , uriAuthority  :: Maybe URIAuth    -- ^ @\/\/anonymous\@www.haskell.org:42@
    , uriPath       :: String           -- ^ @\/ghc@
    , uriQuery      :: String           -- ^ @?query@
    , uriFragment   :: String           -- ^ @#frag@
    } 
```

## Headers

### Data Model

`/Network/HTTP/Headers.hs`
``` haskell
-- | The @Header@ data type pairs header names & values.
data Header = Header HeaderName String

data HeaderName 
    -- Generic Headers
 = HdrCacheControl
 | HdrConnection
 | HdrDate
 | HdrPragma
 | HdrTransferEncoding
 | HdrUpgrade
 | HdrVia
    -- Request Headers
 | HdrAccept
 | HdrAcceptCharset
 | HdrAcceptEncoding
 | HdrAcceptLanguage
 | HdrAuthorization
 | HdrCookie
 | HdrExpect
 | HdrFrom
 | HdrHost
 | HdrIfModifiedSince
 | HdrIfMatch
 | HdrIfNoneMatch
 | HdrIfRange
 | HdrIfUnmodifiedSince
 | HdrMaxForwards
 | HdrProxyAuthorization
 | HdrRange
 | HdrReferer
 | HdrUserAgent
    -- Response Headers
 | HdrAge
 | HdrLocation
 | HdrProxyAuthenticate
 | HdrPublic
 | HdrRetryAfter
 | HdrServer
 | HdrSetCookie
 | HdrTE
 | HdrTrailer
 | HdrVary
 | HdrWarning
 | HdrWWWAuthenticate
    -- Entity Headers
 | HdrAllow
 | HdrContentBase
 | HdrContentEncoding
 | HdrContentLanguage
 | HdrContentLength
 | HdrContentLocation
 | HdrContentMD5
 | HdrContentRange
 | HdrContentType
 | HdrETag
 | HdrExpires
 | HdrLastModified
    -- MIME entity headers (for sub-parts)
 | HdrContentTransferEncoding
    -- Allows for unrecognised or experimental headers.
 | HdrCustom String
```

## Request

### Data Model
`/Network/HTTP/Base.hs`
```haskell
data Request a =
  Request 
    { rqURI       :: URI
    , rqMethod    :: RequestMethod
    , rqHeaders   :: [Header]
    , rqBody      :: a
    }

data RequestMethod 
  = HEAD 
  | PUT 
  | GET 
  | POST 
  | DELETE 
  | OPTIONS 
  | TRACE 
  | CONNECT 
  | Custom String

-- | @RequestData@ contains the head of a HTTP request; method,
-- its URL along with the auxillary/supporting header data.
type RequestData   = (RequestMethod,URI,[Header])
```

### Building

## Respond

### Data Model

`/Network/HTTP/Base.hs`
``` haskell
data Response a =
  Response 
    { rspCode     :: ResponseCode
    , rspReason   :: String
    , rspHeaders  :: [Header]
    , rspBody     :: a
    }

-- | For easy pattern matching, HTTP response codes @xyz@ are
-- represented as @(x,y,z)@.
type ResponseCode  = (Int,Int,Int)

-- | @ResponseData@ contains the head of a response payload;
-- HTTP response code, accompanying text description + header
-- fields.
type ResponseData  = (ResponseCode,String,[Header])
```

### Building

## Constants

`/Network/HTTP/Base.hs`
``` haskell
-- Protocol version
httpVersion = "HTTP/1.1"
```
