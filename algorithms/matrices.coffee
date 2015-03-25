# finds number of operations required to multiply matrices in a specified order
# usage:
#   coffee matrices.coffee "((5x4 4x4) 4x3)"

matStr = process.argv[2]

curSum = 0

# just take first match
innermostMats = matStr.match(/\([^\(\)]+ [^\(\)]+\)/g)?[0]

while innermostMats
  leftLeft = parseInt(innermostMats.match(/\([0-9]+x/g)[0]
    .replace(/[\(x]/, ""))
  leftMid = parseInt(innermostMats.match(/x[0-9]+ /g)[0]
    .replace(/[ x]/, ""))
  rightMid = parseInt(innermostMats.match(/ [0-9]+x/g)[0]
    .replace(/[ x]/, ""))
  rightRight = parseInt(innermostMats.match(/x[0-9]+\)/g)[0]
    .replace(/[x\)]/, ""))

  if leftMid != rightMid
    throw "your matrices are dumb"
  else
    curSum += (leftLeft * leftMid * rightRight)
    # re-escape the parens so they're parsed as actual parens
    r = new RegExp(innermostMats.replace(/\(/, "\\(").replace(/\)/, "\\)"))
    # console.log "matStr: #{matStr}, sum: #{curSum}"
    matStr = matStr.replace(r, "#{leftLeft}x#{rightRight}")

  innermostMats = matStr.match(/\([^\(\)]+ [^\(\)]+\)/g)?[0]

console.log curSum
