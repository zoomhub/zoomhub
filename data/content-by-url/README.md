This is a directory for local data.

It contains symlinks, keyed by URL hashes, to our content info, keyed by ID.
We use hex SHA-256 for hashing URLs.

To generate example data, run this in your terminal:

```sh
# assuming you're in this directory!
ln -s ../content-by-id/100.json \
    f47ebb016118ec8733629bf2bee23d3838dec72f03c4e72de871b5f5cab99e29.json
ln -s ../content-by-id/100_U.json \
    62cc9aec7e8bf5b3c7ecb452ac1e8e41848757279f0acb76475cb0dd5a2311bf.json
ln -s ../content-by-id/4rcn.json \
    02e584171d18e29140e98f0d3a1793317c433b4473e743112444118ad125aa18.json
ln -s ../content-by-id/hdfm.json \
    ca896364115afba10473c4e86b43c939a6265f0afb3c26a74d0fa861678e24df.json
```

TEMP: [For now][01d9bf3], we actually store these symlinks as one giant
JSON dictionary. Save this to `data.json`:

[01d9bf3]: https://github.com/zoomhub/zoomhub/commit/01d9bf3e02f7eead653872dc6bcdfe48cd9f0768

```json
{
    "02e584171d18e29140e98f0d3a1793317c433b4473e743112444118ad125aa18.json": "../content-by-id/4rcn.json",
    "62cc9aec7e8bf5b3c7ecb452ac1e8e41848757279f0acb76475cb0dd5a2311bf.json": "../content-by-id/100_U.json",
    "ca896364115afba10473c4e86b43c939a6265f0afb3c26a74d0fa861678e24df.json": "../content-by-id/hdfm.json",
    "f47ebb016118ec8733629bf2bee23d3838dec72f03c4e72de871b5f5cab99e29.json": "../content-by-id/100.json"
}
```

**TEMP:** We are temporarily checking this data file into the git repo,
in order to test lookups of existing content (that are known in the local db).
We can/should remove these files (and update our tests) once we have
new content acceptance hooked up.

**TODO:** We should also update this checked-in data file to symlinks
once we [return to symlinks](https://github.com/zoomhub/zoomhub/issues/95).
