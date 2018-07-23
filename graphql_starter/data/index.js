const {
  // ADT
  List,
  Maybe,
  // List
  filter,
  head,
  // Maybe
  option,
  prop,
  // Predicate Function
  propEq,
  // Combinator
  compose,
  // Helper
  ap,
  curry,
} = require( 'crocks' )

/* --- Data --- */

const videoA = {
  id: '1',
  title: 'bar',
  duration: 1,
  released: true,
}

const videoB = {
  id: '2',
  title: 'foo',
  duration: 180,
  released: false,
}

const videos = [ videoA, videoB ]

// getById :: String -> Array a -> a
// under unique id assumption
const getById = curry( id =>
  compose(
    option( null ),
    head,
    filter( propEq( 'id', id ) )
  )
)

// getVideoById :: String -> Promise (Array Video)
const getVideoById = ( { id } ) => new Promise(
  ( res ) => res( getById( id, videos ) )
)

const getAllVideos = _ => new Promise(
  ( res ) => res( videos )
)

const createVideo = ( { title, duration, released } ) => {
  const video = {
    id: ( new Buffer( title, 'utf8' )
      .toString( 'base64' ) ),
    title,
    duration,
    released,
  }
  // direct mutation, use State Monad
  videos.push(
    video
  )
  return video
}

const VIDEO = 'Video'
const types = {
  [ VIDEO ]: getVideoById,
}

const getObjectById = ( type, id ) => compose(
  option( null ),
  ap( Maybe( { id } ) ),
  prop( type )
)(
  types // a free variable in the global scope, better use Reader to inject the environment
)

module.exports = {
  getVideoById,
  getAllVideos,
  createVideo,
  getObjectById,
}
