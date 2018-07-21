# GraphQL

### 1.[Tutorial: Designing a GraphQL API](https://gist.github.com/swalkinshaw/3a33e2d292b60e68fcebe12b62bbb3e2)

### 2.[GraphQL Tour: Interfaces and Unions](https://medium.com/the-graphqlhub/graphql-tour-interfaces-and-unions-7dd5be35de0d)

doesn't even explain the differences

### 3.[Interfaces and Unions in GraphQL](https://docs.aws.amazon.com/appsync/latest/devguide/interfaces-and-unions.html)

> GraphQL's type system also features Unions. Unions are identical to interfaces, except that they don't define a common set of fields. 
> to query any field on a union, you must use inline fragments

Then interface is isomorphic to a Product type of
- a common set of fields (Product type)
- a Union type for distinct fields

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
