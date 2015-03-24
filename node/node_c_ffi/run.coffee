NodeObjectStream = require './node-object-stream'

fs = require 'fs'

f = fs.createReadStream '../file'

p = f.pipe(new NodeObjectStream)

res = 0
p.on 'object', (obj) ->
  ++res

p.on 'end', ->
  console.log res

# ffi = require 'ffi'
# ref = require 'ref'

# libm = ffi.Library 'libm',
#   'ceil': ['double', ['double']]

# console.log libm.ceil 1.5

# size_t_ptr = ref.refType 'size_t'

# libmylib = ffi.Library 'mylib',
#   'set_zero': ['void', [size_t_ptr]]
#   'get_str': ['string', ['string', size_t_ptr]]

# outNum = ref.alloc 'size_t'

# libmylib.set_zero outNum

# console.log libmylib.get_str "supercalsifrsagilisticexpialodocious", outNum

# console.log outNum.deref()
