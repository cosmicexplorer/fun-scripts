# http://stackoverflow.com/questions/29721851/how-to-pause-and-unpause-node-object-stream-while-processing-its-output

fs = require 'fs'

TestTransform = require './test-transform'

inStream = new TestTransform

fs.createReadStream("./test.coffee").pipe(inStream)

outStream = fs.createWriteStream("./test_out")

inStream.on 'data', (chunk) ->
  line = chunk.toString()
  outStream.write "-->#{line}"
  if line.match /line\.match/g
    inStream.pause()
    f = fs.createReadStream("./test.coffee")
    f.pipe(outStream)
    f.removeAllListeners 'end'
    f.on 'end', ->
      f.unpipe(outStream)
      inStream.resume()
