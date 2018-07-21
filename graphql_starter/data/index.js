const {
  // ADT
  List,
  // List
  filter,
  head,
  // Maybe
  option,
  // Predicate Function
  propEq,
  // Combinator
  compose,
  // Helper
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
  videos.push(
    video
  )
  return video
}

module.exports = {
  getVideoById,
  getAllVideos,
  createVideo,
}
