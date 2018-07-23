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

const {
  nodeDefinitions,
  fromGlobalId,
  globalIdField,
  connectionDefinitions,
  connectionFromPromisedArray,
  connectionArgs,
  mutationWithClientMutationId,
} = require( 'graphql-relay' )

const express = require( 'express' )
const graphqlHTTP = require( 'express-graphql' )

const {
  getVideoById,
  getAllVideos,
  createVideo,
  getObjectById,
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

// const NodeInterface = G.interface( {
//   name: 'Node',
//     fields: {
//       id: {
//         type: G.required( G.id ),
//       }
//     },
//     resolveType: object => {
//       // an Interface with one subtype, pretty much useless abstraction
//       if ( object.title ) {
//         return VideoType
//       }
//       return null
//     }
// } )

const {
  nodeInterface: NodeInterface,
  nodeField: NodeField
} = nodeDefinitions(
  globalId => {
    const { type, id } = fromGlobalId( globalId )
    return getObjectById( type, id )
  },
  object => {
    if ( object.title ) {
      return VideoType
    }
    return null
  },
);

const VideoType = G.object( {
  name: 'Video',
  description: 'A video',
  fields: {
    id: globalIdField( 'Video' ),
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
  },
  interfaces: [ NodeInterface ],
} )

const { connectionType: VideoConnection } = connectionDefinitions( {
  nodeType: VideoType,
  connectionFields: _ => ( {
    totalCount: {
      type: G.int,
      description: `A count of the total number of objects in this connection.`,
      resolve: connections =>
        connections.edges.length
    }
  } )
} )

const QueryType = G.object( {
  name: 'QueryType',
  description: 'The root query type',
  fields: {
    node: NodeField,
    video: {
      type: VideoType,
      args: {
        id: globalIdField( 'Video' ),
        // id: {
        //   type: G.required( G.id ),
        //   description: 'The id of the video.',
        // }
      },
      resolve: ( _, args ) => getVideoById( args )
    },

    videos: {
      type: VideoConnection,
      args: connectionArgs,
      resolve: ( _, args ) => connectionFromPromisedArray(
        getAllVideos(),
        args
      )
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

/*
query {
  videos {
    totalCount
    edges {
      node {
        id
        title
        duration
        released
      }
    }
  }
}
*/

const videoMutation = mutationWithClientMutationId( {
  name: 'AddVideo',
  inputFields: {
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
  },
  outputFields: {
    video: {
      type: VideoType
    }
  },
  mutateAndGetPayload: args => new Promise( ( res, rej ) => {
    Promise.resolve( createVideo( args ) )
      .then( video => res( { video } ) )
      .catch( rej )
  } ),
} )

/*
mutation AddVideoQuery($input: AddVideoInput!) {
  createVideo(input: $input) {
    video {
      id
      title
    }
  }
}

// query variables
{
  "input": {
    "title": "New Video",
    "duration": 300,
    "released": false,
    "clientMutationId": "dsklfjdfk"
  }
}
 */

const MutationType = G.object( {
  name: 'Mutation',
  description: 'The root mutation type.',
  fields: {
    createVideo: videoMutation,
    // createVideo: {
    //   type: VideoType,
    //   args: {
    //     video: {
    //       type: G.required( VideoInputType ),
    //     },
    //   },
    //   resolve: ( _, { video } ) => createVideo( video )
    // },
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
