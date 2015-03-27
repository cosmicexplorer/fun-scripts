ffi = require 'ffi'
ref = require 'ref'

libm = ffi.Library 'libm',
  'ceil': ['double', ['double']]

console.log libm.ceil 1.5

size_t_ptr = ref.refType 'size_t'

# the following lines will error out that it's unable to find our library
# run "LD_LIBRARY_PATH=. node run.js" to avoid this
# unfortunately the value of LD_LIBRARY_PATH is cached at the beginning of the
# program's run, so it cannot be set programmatically, unless the following is
# done in a child process (typically a bash script is used, although node's
# child process module can also allow for this)

libmylib = ffi.Library 'mylib',
  'set_zero': ['void', [size_t_ptr]]
  'get_str': ['string', ['string', size_t_ptr]]

outNum = ref.alloc 'size_t'

libmylib.set_zero outNum

console.log libmylib.get_str "supercalsifrsagilisticexpialodocious", outNum

console.log outNum.deref()
