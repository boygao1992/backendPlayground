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

## [PaperPlane](https://github.com/articulate/paperplane)
> Lighter-than-air node.js server framework 
> - pure, functional, Promise-based route handlers
> - composeable json body parsing

> con: currently no GraphQL middleware

# Purescript

## Database

### 1.[purescript-redis-client](https://pursuit.purescript.org/packages/purescript-redis-client/0.5.0)

# Java

## Spring

## Database ORM

### Hibernate

### MyBatis
