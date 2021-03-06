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
  nonNull: x => new GraphQLNonNull( x ),
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

// const AuthorInputType = G.input({
//   name: "AuthorInputType",
//   fields: {
//     id: {
//       type: G.id
//     },
//     name: {
//       type: G.string
//     }
//   }
// })

// const PostDraftContentTodoInputType = G.input({
//   name: "PostDraftContentTodoInputType",
//   fields: {
//     id: {
//       type: G.id
//     },
//     todo: {
//       type: G.string
//     }
//   }
// })

// const PostDraftContentInputType = G.input ({
//   name: "PostDraftContentInputType",
//   fields: _ => ({ // NOTE allow recursive input type
//     date: {
//       type: G.string
//     },
//     lines: {
//       type: G.int
//     },
//     todolist: {
//       type: G.nonNull(G.list(G.nonNull(PostDraftContentTodoInputType)))
//     },
//     // NOTE recursive input type
//     c: {
//       type: PostDraftContentInputType
//     }
//   })
// })

// const PostDraftInputType = G.input ({
//   name: "PostDraftInputType",
//   fields: {
//     author: {
//       type: AuthorInputType
//     },
//     content: {
//       type: PostDraftContentInputType
//     }
//   }
// })

const PostDraftInputType = G.input ({
  name: "PostDraft",
  fields: {
    author: {
      type: G.input({
        name: "PostDraft_author",
        fields: {
          id: {
            type: G.id
          },
          name: {
            type: G.string
          }
        }
      })
    },
    content: {
      type: G.input ({
        name: "PostDraft_content",
        fields: {
          date: {
            type: G.string
          },
          lines: {
            type: G.int
          },
          todolist: {
            type: G.nonNull(G.list(G.nonNull(
              G.input({
                name: "PostDraft_content_todolist",
                fields: {
                  id: {
                    type: G.id
                  },
                  todo: {
                    type: G.string
                  }
                }
              })
            )))
          },
        }
      })
    }
  }
})

/* mutation
mutation A {
  createPost(
    postDraft: {
      author: {
        id: "001"
        name: "wenbo"
      }
      content: {
        date: "2019-01-17"
        lines: 1
        todolist: [{ id: "000", todo: "first todo"}]
      }
    }
  ) {
    id
  }
}

mutation B {
  createPost(postDraft: {
    author: {
      id: "001"
      name: "wenbo"
    }
    content: {
      date: "2019-01-17"
      lines: 1
      todolist: [{ id: "000", todo: "first todo"}]
      c: {
        todolist: []
        c: {
          todolist: []
          c: {
            todolist: []
          }
        }
      }
    }
  }) {
    id
  }
}
*/

const UserType = G.object( {
  name: "User",
  description: ".",
  fields: _ => ({
    id: {
      type: G.nonNull(G.id),
      description: ".",
      resolve: source => new Promise(res => res(source.id_))
    },
    post: {
      type: PostType, // NOTE reference to PostType
      description: ".",
      resolve: source => new Promise(res => res({ id_: "post_" + source.id_ }))
    }
  })
})

const PostType = G.object( {
  name: "Post",
  description: ".",
  fields: _ => ({
    id: {
      type: G.nonNull(G.id),
      description: ".",
      resolve: source => new Promise(res => res(source.id_))
    },
    author: {
      type: UserType, // NOTE reference to UserType
      description: ".",
      resolve: source => new Promise(res => res({ id_: "user_" + source.id_ }))
    }
  })
})

/* query

query {
  user(id: 1) {
    id
    post {
      id
      author {
        id
        post {
          id
          author {
            id
          }
        }
      }
    }
  }
}
*/

/* response

{
  "data": {
    "user": {
      "id": "user_1",
      "post": {
        "id": "post_user_1",
        "author": {
          "id": "user_post_user_1",
          "post": {
            "id": "post_user_post_user_1",
            "author": {
              "id": "user_post_user_post_user_1"
            }
          }
        }
      }
    }
  }
}
*/

// const userTypeConstructor = dependencies => G.object( {
//   name: "User",
//   description: ".",
//   fields: _ => ({
//     id: {
//       type: G.nonNull(G.id),
//       description: ".",
//     },
//     posts: {
//       type: G.nonNull(G.list(dependencies.PostType)),
//       description: ".",
//       resolve: source => new Promise(res => res([{ id: "post_" + source.id }]))
//     },
//     comments: {
//       type: G.nonNull(G.list(dependencies.CommentType)),
//       description: ".",
//       resolve: source => new Promise(res => res([{ id: "comment_" + source.id }]))
//     }
//   })
// })

// const postTypeConstructor = dependencies => G.object( {
//   name: "Post",
//   description: ".",
//   fields: _ => ({
//     id: {
//       type: G.nonNull(G.id),
//       description: "."
//     },
//     author: {
//       type: G.nonNull(dependencies.UserType),
//       description: ".",
//       resolve: source => new Promise(res => res({ id: "user_" + source.id }))
//     },
//     comments: {
//       type: G.nonNull(G.list(dependencies.CommentType)),
//       description: ".",
//       resolve: source => new Promise(res => res([{ id: "comment_" + source.id }]))
//     }
//   })
// })

// const commentTypeConstructor = dependencies => G.object({
//   name: "Comment",
//   description: ".",
//   fields: _ => ({
//     id: {
//       type: G.nonNull(G.id),
//       description: "."
//     },
//     post: {
//       type: G.nonNull(dependencies.PostType),
//       description: ".",
//       resolve: source => new Promise(res => res({ id: "post_" + source.id }))
//     },
//     author: {
//       type: G.nonNull(dependencies.UserType),
//       description: ".",
//       resolve: source => new Promise(res => res({ id: "user_" + source.id }))
//     }
//   })
// })

// let ST = {}

// ST.UserType = userTypeConstructor(ST)
// ST.PostType = postTypeConstructor(ST)
// ST.CommentType = commentTypeConstructor(ST)

// const { UserType, PostType, CommentType } = ST

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
    user: {
      type: UserType,
      args: {
        id: {
          type: G.nonNull(G.id)
        }
      },
      resolve: (_, { id }) => new Promise( res => res({ id_: "user_" + id }) )
      // NOTE output type doesn't have to align with field type:
      //   here, id_ is not part of UserType
    },
    post: {
      type: PostType,
      args: {
        id: {
          type: G.nonNull(G.id)
        }
      },
      resolve: (_, { id }) => new Promise( res => res({ id_: "post_" + id }))
    },
    nestedScalar: {
      type: G.list(G.list(G.list(G.int))),
      args: {
        init: {
          type: G.nonNull(G.int)
        }
      },
      resolve: (_, { init }) => new Promise( res => res([[[init]]]))
    },
    nestedPost: {
      type: G.list(G.list(G.list(PostType))),
      args: {
        id: { type: G.nonNull(G.id)}
      },
      resolve: (_, { id }) => new Promise( res => res( [[[{ id_: "post_" + id }]]]))
    }
    // comment: {
    //   type: CommentType,
    //   args: {
    //     id: {
    //       type: G.nonNull(G.id)
    //     }
    //   },
    //   resolve: (_, { id }) => new Promise( res => res({ id: "comment_" + id }))
    // }
  }
} )
/* query
query {
  user(id: "001") {
    id
    posts {
      id
      comments {
        id
        author {
          id
          comments {
            id
          }
        }
      }
    }
  }
}
*/
/* response
{
  "data": {
    "user": {
      "id": "user_001",
      "posts": [
        {
          "id": "post_user_001",
          "comments": [
            {
              "id": "comment_post_user_001",
              "author": {
                "id": "user_comment_post_user_001",
                "comments": [
                  {
                    "id": "comment_user_comment_post_user_001"
                  }
                ]
              }
            }
          ]
        }
      ]
    }
  }
}
*/
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
    createPost: {
      type: PostType,
      args: {
        postDraft: { type: PostDraftInputType}
      },
      resolve: (_, postdraft) => { console.log(postdraft); return new Promise( res => res({ id_ : "007" })) }
    }
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

