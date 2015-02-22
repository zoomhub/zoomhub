This is a directory for local data.

It contains our content info as flat JSON files, keyed by ID.
(Note: uppercase letters get an underscore prefix, to work around
case-insensitive filesystems.)

Example data follows.

`100.json`:

```json
{
    "id": "100",
    "url": "http://e.i.uol.com.br/outros/0907/090731cielao1.jpg",
    "ready": false,
    "failed": false,
    "progress": 1,
    "mime": "image/jpeg",
    "size": 33998,
    "dzi": {
        "width": 208,
        "height": 208,
        "tileSize": 254,
        "tileOverlap": 1,
        "tileFormat": "jpg"
    }
}
```

`100_U.json`:

```json
{
    "id": "100U",
    "url": "http://www.archdrawing.ireland.anglican.org/archive/files/a0630962f3164841684eea66b6552445.JPG",
    "ready": false,
    "failed": false,
    "progress": 1,
    "mime": "image/jpeg",
    "size": 202199,
    "dzi": {
        "width": 1824,
        "height": 1368,
        "tileSize": 254,
        "tileOverlap": 1,
        "tileFormat": "jpg"
    }
}
```

`4rcn.json`:

```json
{
    "id": "4rcn",
    "url": "http://media.stenaline.com/media_SE/lalandia-map-zoomit/lalandia-map.jpg",
    "ready": true,
    "failed": false,
    "progress": 1,
    "mime": "image/jpeg",
    "size": 9115770,
    "dzi": {
        "width": 5058,
        "height": 3750,
        "tileSize": 254,
        "tileOverlap": 1,
        "tileFormat": "jpg"
    }
}
```

`hdfm.json`:

```json
{
    "id": "hdfm",
    "url": "http://wdh.blob.core.windows.net/deepzoom-sources/hampi/HampiCB/02-Lakshmi,%20The%20Temple%20Elephant/09-Lakshmi%20and%20little%20girl.jpg",
    "ready": false,
    "failed": false,
    "progress": 0,
    "mime": "image/jpeg",
    "size": 15533083,
    "dzi": {
        "width": 4032,
        "height": 6048,
        "tileSize": 254,
        "tileOverlap": 1,
        "tileFormat": "jpg"
    }
}
```
