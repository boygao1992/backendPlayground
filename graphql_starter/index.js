'use strict'

/*
GraphQL comes with a set of default scalar types out of the box:

- Int: A signed 32‐bit integer.
- Float: A signed double-precision floating-point value.
- String: A UTF‐8 character sequence.
- Boolean: true or false.
- ID: The ID scalar type represents a unique identifier, often used to refetch an object or as the key for a cache. The ID type is serialized in the same way as a String; however, defining it as an ID signifies that it is not intended to be human‐readable.
*/

const { inspect } = require( 'util' )

const {
  GraphQLSchema,
  GraphQLObjectType,
  GraphQLID,
  GraphQLString,
  GraphQLInt,
  GraphQLBoolean,
} = require( 'graphql' )

const G = {
  schema: x => new GraphQLSchema( x ),
  object: x => new GraphQLObjectType( x ),
  string: GraphQLString,
  int: GraphQLInt,
  bool: GraphQLBoolean,
  id: GraphQLID,
}

const express = require( 'express' )
const graphqlHTTP = require( 'express-graphql' )

const PORT = process.env.PORT || 3000;
const server = express()

/* --- Data --- */

const videoA = {
  id: '1',
  title: 'bar',
  duration: 1,
  watched: true,
}

const videoB = {
  id: '2',
  title: 'foo',
  duration: 180,
  watched: false,
}

const videos = [ videoA, videoB ]

/* --- GraphQL Schema --- */

/*
const schema = buildSchema( `
  type Video
    { id : ID
    , title : String
    , duration: Int
    , watched : Boolean }

  type Query
    { video : Video
    , videos : [Video] }

  type Schema
    { query : Query }
` )

const resolvers = {
  video: _ => ( {
    id: '1',
    title: 'bar',
    duration: 1,
    watched: true,
  } ),
  videos: _ => videos
}
const query = `
query myFirstQuery {
  videos {
    id
    title
    duration
    watched
  }
}
`
*/

const videoType = G.object( {
  name: 'Video',
  description: 'A video',
  fields: {
    id: {
      type: G.id,
      description: 'The id of the video.',
    },
    title: {
      type: G.string,
      description: 'The title of the video.',
    },
    duration: {
      type: G.int,
      description: 'The duration of the video.',
    },
    watched: {
      type: G.bool,
      description: 'Whether or not the user has watched the video.',
    },
  }
} )

const queryType = G.object( {
  name: 'QueryType',
  description: 'The root query type',
  fields: {
    video: {
      type: videoType,
      resolve: _ => new Promise( resolve => ( resolve( videoA ) ) )
    }
  }
} )

const schema = G.schema( {
  query: queryType,
} )

server.use( '/graphql', graphqlHTTP( {
  schema,
  graphiql: true,
} ) )

server.listen( PORT, _ => {
  console.log( `Listening on http://localhost:${PORT}` )
} )

// graphql( schema, query, resolvers )
//   .then( x => console.log( inspect( x, { depth: 4, colors: true } ) ) )
//   .catch( console.log )
