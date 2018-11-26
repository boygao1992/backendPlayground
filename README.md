# GraphQL

## References

### 1.[Tutorial: Designing a GraphQL API](https://gist.github.com/swalkinshaw/3a33e2d292b60e68fcebe12b62bbb3e2)

### 2.[GraphQL Tour: Interfaces and Unions](https://medium.com/the-graphqlhub/graphql-tour-interfaces-and-unions-7dd5be35de0d)

doesn't even explain the differences

### 3.[Interfaces and Unions in GraphQL](https://docs.aws.amazon.com/appsync/latest/devguide/interfaces-and-unions.html)

> Unions are identical to interfaces, except that they don't define a common set of fields. 
> Unions are generally preferred over interfaces when the possible types do not share a logical hierarchy. 
> to query any field on a union, you must use inline fragments

`GraphQLInterfaceType` is isomorphic to a Product type of
- a common set of fields (also grouped in a Product type)
- a Union type for distinct fields (enforced, that's why a `resolveType` function is required when defining an Interface)

#### Interface solution, equivalent encoding in Elm by extensible Record

```elm
type alias Event a =
    { a
        | id : ID
        , name : String
        , startsAt : Maybe String
        , endsAt : Maybe String
        , venue : Maybe Venue
        , minAgeRestriction : Maybe Int
    }
    
type alias Concert =
    Event 
        { performingBand : Maybe String }

type alias Festival =
    Event 
        { performers : Maybe (List String) }

type alias Conference =
    Event 
        { speakers : Maybe (List String)
        , workshops : Maybe (List String) }
        
```

#### Product and Union solution, equivalent encoding in Elm by extensible Record and Union type

```elm
type alias Event a =
    { a
        | id : ID
        , name : String
        , startsAt : Maybe String
        , endsAt : Maybe String
        , venue : Maybe Venue
        , minAgeRestriction : Maybe Int
    }
    
type alias Concert = 
    { performingBand : Maybe String }

type alias Festival =
    { performers : Maybe (List String) }

type alias Conference =
    { speakers : Maybe (List String)
    , workshops : Maybe (List String) }

type Situation =
      Concert
    | Festival
    | Conference

type alias SituationalEvent =
    Event Situation
```

#### extensible Record (like inheritance in OOP) is not recommended for everchanging model, use named field instead (composition over inheritance)

```elm
type alias Event =
    { eventSpec : EventSpec
    , situation : Situation }

type alias EventSpec =
    { id : ID
    , name : String
    , startsAt : Maybe String
    , endsAt : Maybe String
    , venue : Maybe Venue
    , minAgeRestriction : Maybe Int
    }
    
type Situation =
      Concert ConcertSpec
    | Festival FestivalSpec
    | Conference ConferenceSpec

type alias ConcertSpec = 
    { performingBand : Maybe String }

type alias FestivalSpec =
    { performers : Maybe (List String) }

type alias ConferenceSpec =
    { speakers : Maybe (List String)
    , workshops : Maybe (List String) }
    
```


### 4.[jamesmacaulay/elm-graphql - A GraphQL library for Elm, written entirely in Elm](https://github.com/jamesmacaulay/elm-graphql)

### 5.[chrisbolin/understanding-relay-mutations](https://github.com/chrisbolin/understanding-relay-mutations)

> Remember, the `clientMutationId` input is **required** by the mutation; 
> don't worry, we can spoof it when we're interacting with the mutation outside of Relay.

[Remove the `clientMutationId` requirement by creating a new root id for each executed mutation #2349](https://github.com/facebook/relay/pull/2349)

### 6.[graphql/graphql-relay-js](https://github.com/graphql/graphql-relay-js)

> Mutations

> ``` javascript
> var shipMutation = mutationWithClientMutationId({
>   name: 'IntroduceShip',
>   inputFields: {
>     shipName: {
>       type: new GraphQLNonNull(GraphQLString)
>     },
>     factionId: {
>       type: new GraphQLNonNull(GraphQLID)
>     }
>   },
>   outputFields: {
>     ship: {
>       type: shipType,
>       resolve: (payload) => data['Ship'][payload.shipId]
>     },
>     faction: {
>       type: factionType,
>       resolve: (payload) => data['Faction'][payload.factionId]
>     }
>   },
>   mutateAndGetPayload: ({shipName, factionId}) => {
>     var newShip = {
>       id: getNewShipId(),
>       name: shipName
>     };
>     data.Ship[newShip.id] = newShip;
>     data.Faction[factionId].ships.push(newShip.id);
>     return {
>       shipId: newShip.id,
>       factionId: factionId,
>     };
>   }
> });
>
> var mutationType = new GraphQLObjectType({
>   name: 'Mutation',
>   fields: () => ({
>     introduceShip: shipMutation
>   })
> });
> ```

Assume `data` is an Object in global scope which represents a external database and thus any operation (CRUD) on it is an IO.

`mutateAndGetPayload` sends an mutation IO to the server and receives a payload Object from the server.
But after receiving the payload from the "server", namely `{ shipId, factionId }`, `outputFields` which postprocesses the payload accesses the server (`data`) again.
Very likely an anti-pattern.

### 7.[Todo example for koa-graphql and relay](https://github.com/chentsulin/koa-graphql-relay-example)

## Tools

### 1.[Apollo](https://www.apollographql.com/)

> Apollo Client
> The Query component uses the React render prop API (with a function as a child) to bind a query to our component and render it based on the results of our query.
> cache for repeated query

> Apollo Server

### 2.[reindexio/reindex-api](https://github.com/reindexio/reindex-api)
> reindex-api is a multi-tenant, hosted GraphQL database solution.
> reindex-api converts a JSON based schema into a GraphQL API in addition to creating a database storage (MongoDB or RethinkDB) underneath. 



## Language binding
### 1.[elm-graphql](https://github.com/jamesmacaulay/elm-graphql)

# Node

## ORM

### [loopback/Model](https://loopback.io/doc/en/lb4/Model.html)

### [Sequelize](https://github.com/sequelize/sequelize)

[flow-typed/sequelize_v4.x.x](https://github.com/flow-typed/flow-typed/tree/master/definitions/npm/sequelize_v4.x.x)

[graphql-sequelize](https://github.com/mickhansen/graphql-sequelize)

### [Objection.js](https://github.com/Vincit/objection.js/)

[objection-graphql](https://github.com/Vincit/objection-graphql)

### [Postgraphile](https://github.com/graphile/postgraphile)

## Web Framework

### [PaperPlane](https://github.com/articulate/paperplane)
> Lighter-than-air node.js server framework 
> - pure, functional, Promise-based route handlers
> - composeable json body parsing

con: currently no GraphQL middleware

### [Loopback](https://github.com/strongloop/loopback)


# Purescript

## Database

### 1.[purescript-redis-client](https://pursuit.purescript.org/packages/purescript-redis-client/0.5.0)

# Java

## Spring

## Database ORM

### Object-Relational Mapping (ORM)

#### The Paradigm Mismatch
##### 1. abstraction granularity

missing higher-level object abstraction in relational calculus

> Classes in the Java domain model come in a range of different levels of granularity:
> from coarse-grained entity classes like `User`, 
> to finer-grained class like `Address`,
> down to simple `SwissZipCode` extending `AbstractNumbericZipCode`.

```elm
type alias User =
  { username : String
  , address : Address
  }
  
type alias Address =
  { street : String
  , zipcode : Zipcode
  , city : String
  }
  
type Zipcode
  = NumericZipcode Int
  | LiteralZipcode String
  
type alias SwissZipcode = NumericZipcode
```

> In the contrast, just two levels of type granularity are visible in the SQL database:
> - relation type created by you, like `Users` and `BillingDetails`
> - built-in data types, such as `VARCHAR`, `BIGINT`, `TIMESTAMP`

> ```sql
> create table USERS 
>   ( Username VARCHAR(15) NOT NULL PRIMARY KEY
>   , Address ADDRESS NOT NULL
>   );
> ```
> A new `Address` type (class) in Java and a new `ADDRESS` SQL data type should guarantee interoperability.

```sql
create type ADDRESS as table
    ( Street VARCHAR(255) NOT NULL
    , Zipcode VARCHAR(5) NOT NULL
    , City VARCHAR(255) NOT NULL
    );
```

> User-defined data types (UDT) support is one of a number of so-called *object-relational extensions* to traditional SQL.
> Unfortunately, UDT support is a somewhat obscure feature of mast SQL DBMSs and certainly isn't portable between different products.
> Furthermore, the SQL standard supports UDT, but poorly.

> The pragmatic solution for this problem has several columns of built-in vendor-defined SQL types:
> ```sql
> create table USERS 
>   ( Username VARCHAR(15) NOT NULL PRIMARY KEY
>   , Address_Street VARCHAR(255) NOT NULL
>   , Address_Zipcode VARCHAR(5) NOT NULL
>   , Address_City VARCHAR(255) NOT NULL
>   );
> ```

##### 2. subtyping

common fields in the superclass

> Each of these subclasses defines slightly different data (and completely differnt functionality that acts on that data).

regardless of attached methods, this is a typical use case of Union type

> This is a polymorphic association.
> Similarly, you want to be able to write polymorphic queries, and have the query return instances of its subclasses.
> SQL databases lack an obvious way ( or at least a standardized way) to represent a polymorphic association.

##### 3. identity
> Java defines two different notions of sameness:
> - Instance identity (equality by reference)
> - Instance equality (equality by value)

> the identity of a database row is expressed as a comparison of primary key values.

> It's common for several non-identical instances in Java to simultaneously represent the same row of the database.
> for example, in concurrently running application threads.

unified by system-generated global identity

##### 4. associations
> The challenge is to map a completely open data model, which is independent of the application that works with the data, to an application-dependent navigational model.
> a constrained view of the associations needed by this particular application.


> Object-oriented languages represent associations using object references
> - directional
> - can have many-to-many multiplicity

> in the relational world, a foreign key-constrained column represents an association, with copies of key values.
> - The constraint is a rule that guarantees integrity of the association.
> - many-to-one association

use an extra link table between two entities to represent many-to-many association

##### 5. data navigation

> walking the object network
> lazy loading

> minimize the number of queries to the database

### Hibernate

### MyBatis

## HTTP

### [Retrofit](https://github.com/square/retrofit)

### [OkHttp](https://github.com/square/okhttp)


## Database Middleware

### [Sharding Sphere](https://github.com/sharding-sphere/sharding-sphere)

## Web Framework

### [Play](https://www.playframework.com/)

### [Spring MVC / WebFlux](https://spring.io/)
> SpringMVC: synchronous, one-request-per-thread
> WebFlux: concurrent; ReactorStream(RxJava), Netty

# Kotlin

## HTTP

### [Fuel](https://github.com/kittinunf/Fuel)

# Haskell

[Simon Meier / The Service Pattern](https://www.schoolofhaskell.com/user/meiersi/the-service-pattern)

## Database

### [Beam](https://tathougies.github.io/beam/)
> No Template Haskell

### [Persistent](http://hackage.haskell.org/package/persistent)
> Template Haskell

[Persistent :: Yesod Web Framework Book- Version 1.6](https://www.yesodweb.com/book/persistent)

### [esqueleto](http://hackage.haskell.org/package/esqueleto)
> Template Haskell

### [Groundhog](http://hackage.haskell.org/package/groundhog)

[Working with databases using Groundhog - School of Haskell](https://www.schoolofhaskell.com/user/lykahb/groundhog)

## Network

### [Network.URI](http://hackage.haskell.org/package/network-uri)

### [Network.HTTP](http://hackage.haskell.org/package/HTTP)

## JSON

### [aeson](http://hackage.haskell.org/package/aeson)

[aeson-lens: Lens of Aeson](http://hackage.haskell.org/package/aeson-lens)

[overloaded string literal - school of Haskell](https://www.schoolofhaskell.com/user/kseo/overloaded-string-literals)
```haskell
class IsString a where
    fromString :: String -> a

-- String, ByteString and Text are examples of IsString instances

{-# LANGUAGE OverloadedStrings #-}
a :: String
a = "Hello World"

b :: ByteString
b = "Hello World"

c :: Text
c = "Hello World"
```

> derive instances of `ToJSON` and `FromJSON` from derived instances of `Generic` type class
```haskell
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

data Person = Person
  { name :: String
  , age :: Int
  , occupation :: Occupation
  } deriving (Show, Generic, ToJSON, FromJSON)

data Occupation = Occupation
  { title :: String
  , tenure :: Int
  , salary :: Int
  } deriving (Show, Generic, ToJSON, FromJSON)
```

> generate instances of `ToJSON` and `FromJSON` by Template Haskell
```haskell
{-# LANGUAGE TemplateHaskell #-}

import Data.Aeson.TH (deriveJSON, defaultOptions)
-- The two apostrophes before a type name is template haskell syntax
deriveJSON defaultOptions ''Occupation
deriveJSON defaultOptions ''Person

deriveJSON
  (defaultOptions { fieldLabelModifier = ("occupation_" ++) })
  ''Occupation
deriveJSON
  (defaultOptions { fieldLabelModifier = ("person_" ++)})
  ''Person
```

> manually define instances of `ToJSON` and `FromJSON` for each data-type
```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (ToJSON(..), Value(..), object, (.=), (.:), FromJSON(..), withObject)

instance ToJSON Occupation where
  toJSON :: Occupation -> Value
  toJSON occupation = object
    [ “title” .= toJSON (title occupation)
    , “tenure” .= toJSON (tenure occupation)
    , “salary” .= toJSON (salary occupation)
    ]

instance ToJSON Person where
  toJSON person = object
    [ “name” .= toJSON (name person)
    , “age” .= toJSON (age person)
    , “occupation” .= toJSON (occupation person)
    ]

instance FromJSON Occupation where
  parseJSON = withObject “Occupation” $ \o -> do
    title_ <- o .: “title”
    tenure_ <- o .: “tenure”
    salary_ <- o .: “salary”
    return $ Occupation title_ tenure_ salary_

instance FromJSON Person where
  parseJSON = withObject “Person” $ \o -> do
    name_ <- o .: “name”
    age_ <- o .: “age”
    occupation_ <- o .: “occupation”
    return $ Person name_ age_ occupation_

```

#### library spec
```haskell
-- | A JSON \"object\" (key\/value map).
type Object = HashMap Text Value

-- | A JSON \"array\" (sequence).
type Array = Vector Value

-- | A JSON value represented as a Haskell value.
data Value = Object !Object
           | Array !Array
           | String !Text
           | Number !Number
           | Bool !Bool
           | Null
             deriving (Eq, Show, Typeable)

class ToJSON a where
  toJSON :: a -> Value
  
class FromJSON a where
  parseJSON :: Value -> Parser a

encode :: ToJSON a => a -> ByteString
decode :: FromJSON a => ByteString -> Maybe a
eitherDecode :: FromJSON a => ByteString -> Either String a


-- | The result of running a 'Parser'.
data Result a = Error String
              | Success a
                deriving (Eq, Show, Typeable)  
-- | Run a 'Parser'.
parse :: (a -> Parser b) -> a -> Result b
parse m v = runParser (m v) Error Success

-- A newtype wrapper for UTCTime that uses the same non-standard serialization format as Microsoft .NET
-- The number represents milliseconds since the Unix epoch.
newtype DotNetTime = DotNetTime {
      fromDotNetTime :: UTCTime
    } deriving (Eq, Ord, Read, Show, Typeable, FormatTime)
```

### [purescript-bridge: Generate PureScript data types from Haskell data types](http://hackage.haskell.org/package/purescript-bridge)

[Connecting a Haskell Backend to a PureScript Frontend](https://www.stackbuilders.com/tutorials/functional-full-stack/purescript-bridge/)

[javcasas/purescript-bridge-tutorial](https://github.com/javcasas/purescript-bridge-tutorial)

## Web Server

### [wai: Web Application Interface.](http://hackage.haskell.org/package/wai)
> Provides a common protocol for communication between web applications and web servers.

[wai-extra: Provides some basic WAI handlers and middleware.](http://hackage.haskell.org/package/wai-extra)

### [servant – A Type-Level Web DSL](https://haskell-servant.readthedocs.io/en/stable/)
> taking as input a description of the web API as a Haskell type.
encode the structure of a Restful API into Type
> Servant is then able to
> - check that your server-side request handlers indeed implement your web API faithfully,
> - automatically derive Haskell functions that can hit a web application that implements this API,
> - generate a Swagger description or code for client functions in some other languages directly.


### [warp: A fast, light-weight web server for WAI applications.](http://hackage.haskell.org/package/warp)

### [Yesod](https://www.yesodweb.com/)
Declaration of routes through DSL and Template Haskell

### [Happstack](http://hackage.haskell.org/package/happstack-server)

### [Snap](http://snapframework.com/)

# Actor-based Concurrent System

## Akka (Scala/Java)

## Scala Actor

## Erlang Actor


# Scala

## HTTP

### [spray](http://spray.io/)

## Web Framework

### [Play](https://www.playframework.com/)

# Ruby

## ORM

### [Rails/ActiveRecord](https://github.com/rails/rails/tree/master/activerecord)

### [Sequel](https://github.com/jeremyevans/sequel)

# Elixir

## ORM

### [Phoenix/Ecto](https://github.com/elixir-ecto/ecto)

## Web Framework

### [Phoenix](https://github.com/phoenixframework/phoenix)
