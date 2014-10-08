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
```
