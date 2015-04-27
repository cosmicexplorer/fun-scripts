# doesn't work!!!!
fs = require 'fs'
Transform = require('stream').Transform

BylineStream = require './test-transform.coffee'
CFormatStream = require '../../../projects/complete/c-format-stream/src/c-format-stream'
await = require 'await'

MS =
class MS extends Transform
  constructor: (opts) ->
    if not @ instanceof MS
      return new MS opts
    else
      Transform.call @, opts

    @lineBreakerStream = new BylineStream
    @outFormatterStream = new CFormatStream
      numNewlinesToPreserve: 0
      indentationString: "  "
    @lineBreakerStream.on 'data', (chunk) =>
      @outFormatterStream.write @processLine(chunk.toString())
    @lineBreakerStream.on 'end', =>
      @outFormatterStream.end()
    @outFormatterStream.on 'data', (chunk) =>
      @push chunk

    @waitThing = await

  _transform: (chunk, enc, cb) ->
    @lineBreakerStream.write chunk, enc
    cb?()

  _flush: (cb) ->
    @lineBreakerStream.end()
    cb?()

  processLine: (line) ->
    line.replace /\bASDF\b/g, "FDSA"

fs.createReadStream('./fun.c').pipe(new MS).pipe(process.stdout)
