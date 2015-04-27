fs = require 'fs'
Transform = require('stream').Transform

byline = require 'byline'

class TT extends Transform
  @number: 0

  constructor: (opts) ->
    if not @ instanceof TT
      return new TT opts
    else
      Transform.call @, opts

    @stop = opts?.stop

    @on 'pipe', (src) =>
      @src = src
      src.on 'error', (err) =>
        @emit 'error', err

    ++@constructor.number

    @counter = 0

  _transform: (chunk, enc, cb) ->
    if @constructor.number > 70
      console.error "PPPPPPPPPPPP"
      throw -1
    @push chunk.toString() + "\n"
    ++@counter
    if @counter is 7 and @stop
      @src.pause()
      outStream = byline(fs.createReadStream(__filename))
        .pipe(new TT(stop: yes))
      outStream.on 'data', =>
        @push chunk
      outStream.on 'end', =>
        @src.resume()
      outStream.on 'error', (err) =>
        @emit 'error', err
    cb?()

byline(fs.createReadStream(__filename))
  .pipe(new TT(stop: yes))
  .pipe(process.stdout)
