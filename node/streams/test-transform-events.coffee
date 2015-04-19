fs = require 'fs'

Transform = require('stream').Transform

TestTransformEventsStream =
class TestTransformEventsStream extends Transform
  constructor: (opts) ->
    if not @ instanceof TestTransformEventsStream
      return new TestTransformEventsStream
    else
      Transform.call @, opts

    # adding this cbEnd actually causes the 'end' event to fire twice
    # cbEnd = =>
    #   @emit 'end'
    cbError = (err) =>
      @emit 'error', err
    @on 'pipe', (src) =>
      # src.on 'end', cbEnd
      src.on 'error', cbError
    @on 'unpipe', (src) =>
      # src.removeListener 'end', cbEnd
      src.removeListener 'error', cbError

  _transform: (chunk, enc, cb) ->
    @push chunk
    cb?()

# test 'end' propagation
midPipe = fs.createReadStream('./test-transform-events.coffee')
  .pipe(new TestTransformEventsStream)
midPipe.pipe process.stdout
midPipe.on 'end', ->
  console.log "done!"

# test 'error' propagation
midPipe2 = fs.createReadStream('./nonexistent.txt')
  .pipe(new TestTransformEventsStream)
midPipe2.pipe process.stdout
midPipe2.on 'error', (err) ->
  console.error "NOFILE, BAD: #{err}"
