# https://stackoverflow.com/questions/29721851/how-to-pause-and-unpause-node-object-stream-while-processing-its-output

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

# test whether you can get data from events as well as piping
t = new TestTransform
linesArr = []
pushArr = []
t.on 'line', (line) ->
  linesArr.push line
t.on 'data', (chunk) ->
  pushArr.push chunk.toString()
t.on 'end', ->
  console.log "linesArr: #{linesArr.length}"
  console.log "pushArr: #{pushArr.length}"
  bad = no
  for i in [0..(linesArr.length - 1)] by 1
    if linesArr[i] isnt pushArr[i]
      bad = yes
      console.error "linesArr[#{i}]: #{linesArr[i]}"
      console.error "pushArr[#{i}]: #{pushArr[i]}"
  if not bad
    console.log "yay!"
  else
    console.log "boo :("
fs.createReadStream('./test.coffee').pipe(t)
# you can! cool
