This is a directory for local data.

It contains hardlinks (to save inodes: http://stackoverflow.com/a/33639131),
keyed by URL hashes, to our content info, keyed by ID. We use hex SHA-256 for
hashing URLs.

To generate example data, run this in your terminal:

```sh
# assuming you’re in `data/content-by-url` directory!
ln ../content-by-id/100.json \
    f47ebb016118ec8733629bf2bee23d3838dec72f03c4e72de871b5f5cab99e29.json
ln ../content-by-id/100_U.json \
    62cc9aec7e8bf5b3c7ecb452ac1e8e41848757279f0acb76475cb0dd5a2311bf.json
ln ../content-by-id/4rcn.json \
    02e584171d18e29140e98f0d3a1793317c433b4473e743112444118ad125aa18.json
ln ../content-by-id/hdfm.json \
    ca896364115afba10473c4e86b43c939a6265f0afb3c26a74d0fa861678e24df.json
```

**TEMP:** We are temporarily checking this data file into the Git repository, in
order to test lookups of existing content (that are known in the local
database). We can/should remove these files (and update our tests) once we have
new content acceptance hooked up.
