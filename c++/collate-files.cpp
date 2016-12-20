/*
  questions:
    - what if input file blocks on read but isn't eof?
  per-file options (all with defaults):
    - mmap: bool (= false)
      - not available for all files
        - if option "mmap_quit" on, immediately exits if used on file for which
          this option does not make sense
    - block size: size_t (= <determined by heuristics, filetype, guesswork>)
    - default_entry: optional<string> (= None)
      - used if eof reached for this file but other files still have entries
      - be careful! may want to remove this
  options for SEPARATE tr-like tool? or maybe just this one?
    - entry_delimiter: char/string/pattern
      - tells how entries in the file are delimited
      - how to represent char or string or pattern?
        - can use special syntax
    - keep_initial_delimiter: bool (= true)
      - if file starts with delimiter, do we make the first entry empty (true)
        or move to the next one?
    - keep_final_delimiter: bool (= false)
      - as above, but if file ends with a delimiter
    - after_end_strategy: ???
      - some function or string?
      - continues output after input file reaches eof
      - defaults to just ending file (which ends the output of the whole
        program)
    - blocking_read_strategy: ???
      - tells what to do if a read blocks on the input /AND NOT YET EOF!!!/
      - options:
        - timeout
        - retry?
          - start file again?
          - retry and discard everything (NO)? or just append retry?
        - run some (arbitrary?) function
          - e.g. let user send some signal to process
          - be careful about arbitrary user-provided functions!
          - what arguments provided to function?
          - how many times is the function run?
            - once, then timeout
            - multiple times with backoff up to some limit (or not!)
        - consider using nonblocking reads by default and selecting?
      - this can create pathological behavior easily! BE CAREFUL
  output options:
    - collate_type: enum(Vertical | Horizontal) (= Horizontal)
    - mmap_quit: bool (= true)
    - blocking_read_strategy_all: ???
      - see per-file blocking_read_strategy
        - same except run if all input files are blocked
 */

int main()
{

}
