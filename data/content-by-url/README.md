This is a directory for local data.

It contains hardlinks (to save inodes: http://stackoverflow.com/a/33639131),
keyed by URL hashes, to our content info, keyed by ID. We use hex SHA-256 for
hashing URLs.

**TEMP:** We are temporarily checking this data file into the Git repository, in
order to test lookups of existing content (that are known in the local
database). We can/should remove these files (and update our tests) once we have
new content acceptance hooked up.
