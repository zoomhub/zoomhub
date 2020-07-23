# Examples

The examples in this directory illustrate usage of various APIs provided by this library. Each file is self-contained and can be run like a script directly.

To build the examples, the build flag `examples` needs to be turned on:

```sh
stack build --flag minio-hs:examples
```

Now to run and example script [BucketExists.hs](https://github.com/minio/minio-hs/blob/master/examples/BucketExists.hs):

```sh
stack exec BucketExists
```

The CI system is configured to build these examples with every change, so they should be current.
