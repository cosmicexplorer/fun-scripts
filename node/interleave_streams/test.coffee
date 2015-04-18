# http://stackoverflow.com/questions/29721851/how-to-pause-and-unpause-node-object-stream-while-processing-its-output

fs = require 'fs'

TestTransform = require './test-transform'

inStream = new TestTransform

fs.createReadStream("./test.coffee").pipe(inStream)

inStream.on 'data', (chunk) ->
  line = chunk.toString()
  process.stdout.write "-->#{line}"
  if line.match /line\.match/g
    inStream.pause()
    f = fs.createReadStream("./test.coffee")
    f.on 'end', ->
      inStream.resume()
    f.pipe(process.stdout)
