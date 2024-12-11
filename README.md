# Mini LSM (hs)

Remaster of minilsm, but in haskell.

## TODO
- [ ] LSMT
    - [ ] MemTable
        - [ ] Look Up
        - [ ] Iterate
        - [ ] Write
        - [ ] To SSTable
    
    - [ ] SSTable
        - [ ] Look Up
        - [ ] Iterate
        - [ ] Serialize to disk
        - [ ] Deserialize from disk
        - [ ] Compaction
        - [ ] Other ...
    
    - [ ] WAL
        - [x] Write to WAL
        - [x] Recovery
        - [ ] Other...
- [ ] Project Related
    - [x] Test
        - [x] Tasty Discovery
        - [x] HUnit
    - [ ] Benchmark
    - [ ] Documentation
