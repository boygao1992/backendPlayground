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
  // Schema
  GraphQLSchema,
  // Definitions
  GraphQLObjectType,
  GraphQLList,
  GraphQLInputObjectType,
  GraphQLNonNull,
  GraphQLInterfaceType,
  // Scalars
  GraphQLID,
  GraphQLString,
  GraphQLInt,
  GraphQLBoolean,
} = require( 'graphql' )

const G = {
  schema: x => new GraphQLSchema( x ),
  object: x => new GraphQLObjectType( x ),
  list: x => new GraphQLList( x ),
  input: x => new GraphQLInputObjectType( x ),
  required: x => new GraphQLNonNull( x ),
  interface: x => new GraphQLInterfaceType( x ),
  string: GraphQLString,
  int: GraphQLInt,
  bool: GraphQLBoolean,
  id: GraphQLID,
}

const express = require( 'express' )
const graphqlHTTP = require( 'express-graphql' )

const {
  getVideoById,
  getAllVideos,
  createVideo,
} = require( './data/index' )

const PORT = process.env.PORT || 3000;
const server = express()


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

const VideoType = G.object( {
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
    released: {
      type: G.bool,
      description: 'Whether or not the video is released.',
    },
  }
} )

const NodeInterface = G.interface( {
  name: 'Node',
    fields: {
      id: {
        type: G.required( G.id ),
      }
    },
    resolveType: object => {
      if ( object.title ) {
        return VideoType
      }
      return null
    }
} )

const QueryType = G.object( {
  name: 'QueryType',
  description: 'The root query type',
  fields: {
    video: {
      type: VideoType,
      args: {
        id: {
          type: G.required( G.id ),
          description: 'The id of the video.',
        }
      },
      resolve: ( _, args ) => getVideoById( args )
    },

    videos: {
      type: G.list( VideoType ),
      resolve: getAllVideos
    },
  }
} )

const VideoInputType = G.input( {
  name: 'VideoInput',
  fields: {
    title: {
      type: G.required( G.string ),
      description: 'The title of the video.',
    },
    duration: {
      type: G.required( G.int ),
      description: 'The duration of the video (in seconds).',
    },
    released: {
      type: G.required( G.bool ),
      description: 'Whether or not the video is released.',
    }
  }
} )

const MutationType = G.object( {
  name: 'Mutation',
  description: 'The root mutation type.',
  fields: {
    createVideo: {
      type: VideoType,
      args: {
        video: {
          type: G.required( VideoInputType ),
        },
      },
      resolve: ( _, { video } ) => createVideo( video )
    },
  }
} )

const Schema = G.schema( {
  query: QueryType,
  mutation: MutationType,
} )

server.use( '/graphql', graphqlHTTP( {
  schema: Schema,
  graphiql: true,
} ) )

server.listen( PORT, _ => {
  console.log( `Listening on http://localhost:${PORT}/graphql` )
} )

// graphql( schema, query, resolvers )
//   .then( x => console.log( inspect( x, { depth: 4, colors: true } ) ) )
//   .catch( console.log )
