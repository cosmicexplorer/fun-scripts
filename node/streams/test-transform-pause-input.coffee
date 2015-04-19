fs = require 'fs'

BylineStream = require './test-transform'
Transform = require('stream').Transform

TT =
class TT extends Transform
  constructor: (opts) ->
    if not @ instanceof TT
      return new TT
    else
      Transform.call @, opts

    @src = null
    @on 'pipe', (src) =>
      @src = src
    @on 'unpipe', (src) =>
      @src = null

  _transform: (chunk, enc, cb) ->
    strAAA = chunk.toString()
    @push strAAA
    if strAAA.match /strAAA\.match/g
      newStream = new TT
      newStream.on 'data', (chunk) =>
        @push "-->" + chunk.toString()
      newStream.on 'error', (err) =>
        @emit 'error', err
      @src?.pause()
      newStream.on 'end', =>
        @src?.resume()
      fs.createReadStream('./test.coffee')
        .pipe(new BylineStream).pipe(newStream)
    cb?()

fs.createReadStream('./test-transform-pause-input.coffee')
  .pipe(new BylineStream)
  .pipe(new TT)
  .pipe(fs.createWriteStream("test_transform_out"))
