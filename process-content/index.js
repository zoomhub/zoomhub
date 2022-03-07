const AmazonS3URI = require("amazon-s3-uri")
const AWS = require("aws-sdk")
const axios = require("axios")
const checkDiskSpace = require("check-disk-space").default
const ClientError = require("./lib/ClientError")
const FileType = require("file-type")
const fs = require("fs")
const path = require("path")
const pLimit = require("p-limit")
const readdir = require("recursive-readdir")
const replaceExt = require("replace-ext")
const rmfr = require("rmfr")
const sharp = require("sharp")
const xml2js = require("xml2js")

const ROOT_PATH = process.env.ROOT_PATH || "/tmp"
const TILE_FORMAT = {
  jpg: "image/jpeg",
  png: "image/png",
}

const TMPDIR = process.env.TMPDIR

const s3Client = new AWS.S3({ apiVersion: "2006-03-01" })
const limit = pLimit(parseInt(process.env.NUM_CONCURRENT_UPLOADS, 10) || 10)

exports.handler = async ({ contentURL }) => {
  log("start", {
    contentURL,
    config: {
      ROOT_PATH,
      TMPDIR,
    },
  })

  if (!contentURL) {
    return {
      status: 400,
      body: "Please specify a content URL to process, i.e. https://www.zoomhub.net/v1/content/:id",
    }
  }

  try {
    await processContent({ contentURL })
  } catch (error) {
    const isClientError = error instanceof ClientError
    if (isClientError) {
      return {
        status: error.status,
        body: error.message,
      }
    }

    try {
      await markAsFailure({ contentURL, error: error.message })
    } catch (failureError) {
      error = failureError
    }

    return {
      status: 500,
      body: error.message,
    }
  }

  return {
    status: 200,
    body: "success",
  }
}

// Public API
const processContent = async ({ contentURL }) => {
  const content = await fetchContent(contentURL)
  if (!content.id) {
    throw new ClientError("Invalid content")
  }

  const contentId = parseContentId(content.id)
  if (!contentId) {
    throw new ClientError(`Invalid content ID: ${contentId}`)
  }

  if (content.ready) {
    throw new ClientError("Content already processed successfully", {
      status: 200,
    })
  }

  const sourceURL = content.url
  const s3URL = parseS3URL(sourceURL)
  log("meta", { contentId, contentURL, sourceURL, s3URL })

  const body = s3URL
    ? await fetchS3URL({ ...s3URL, s3Client })
    : await fetchGenericURL(sourceURL)

  const outputPath = `${ROOT_PATH}/${contentId}`
  log("output", { outputPath })

  // Clean up output directory
  // According to SO, `/tmp` is not shared between different invocations:
  // https://stackoverflow.com/a/37990409
  const tmpFiles = await fs.promises.readdir(ROOT_PATH)
  const tmpDiskSpace = await checkDiskSpace(ROOT_PATH)
  log("/tmp info", { tmpFiles, tmpDiskSpace })
  await rmfr(`${ROOT_PATH}/*`)

  // Write source file
  await fs.promises.writeFile(outputPath, body)

  const fileType = await FileType.fromFile(outputPath)
  const isPNG = fileType && fileType.mime === "image/png"
  const tileFormat = isPNG ? { id: "png" } : { id: "jpg", quality: 90 }
  log("file meta", { fileType, isPNG, tileFormat })

  await sharp(outputPath, { limitInputPixels: false })
    .rotate() // auto-rotate based on EXIF
    .toFormat(tileFormat)
    .tile({
      depth: "onepixel",
      layout: "dz",
      overlap: 1,
      size: 254,
    })
    .toFile(`${outputPath}.dz`)

  const dziXMLString = await fs.promises.readFile(`${outputPath}.dzi`)
  const fixedDZIXMLString = await fixDZITileFormat(dziXMLString)
  await fs.promises.writeFile(`${outputPath}.dzi`, fixedDZIXMLString)

  const [fileSize, dzi] = await Promise.all([
    (await fs.promises.stat(outputPath)).size,
    await parseDZI(fixedDZIXMLString),
  ])

  await uploadDZI({
    basePath: outputPath,
    s3Client,
    tileFormat,
  })

  await markAsSuccess({
    contentURL,
    mime: fileType && fileType.mime,
    size: fileSize,
    dzi,
  })
}

// Helpers
const parseS3URL = (url) => {
  try {
    return AmazonS3URI(url)
  } catch (error) {
    // ignore errors
    return null
  }
}

const parseContentId = (value) => {
  const isValid = /[a-zA-Z0-9]+/.test(value)
  if (!isValid) {
    return null
  }

  return value
}

const log = (message, props = {}) =>
  console.log({ time: new Date().toISOString(), message, ...props })

const fetchContent = async (url) => (await axios.get(url)).data
const fetchS3URL = async ({ s3Client, bucket, key }) =>
  (await s3Client.getObject({ Bucket: bucket, Key: key }).promise()).Body
const fetchGenericURL = async (url) =>
  (await axios.get(url, { responseType: "arraybuffer" })).data

const uploadDZI = async ({ s3Client, basePath, tileFormat }) => {
  const tileFileNames = await readdir(`${basePath}_files`)
  const manifestFileName = `${basePath}.dzi`
  log("DZI", { manifestFileName })

  const tileOperations = tileFileNames.map((fileName) =>
    limit(() => {
      const baseKey = path.relative(ROOT_PATH, fileName)
      const key =
        tileFormat.id === "jpg" ? replaceExt(baseKey, ".jpg") : baseKey
      return uploadFile({
        s3Client,
        fileName,
        key,
        contentType: TILE_FORMAT[tileFormat.id],
      })
    })
  )

  const dziManifestOperation = limit(() =>
    uploadFile({
      s3Client,
      fileName: manifestFileName,
      key: path.relative(ROOT_PATH, manifestFileName),
      contentType: "application/xml",
    })
  )

  return Promise.all([...tileOperations, dziManifestOperation])
}

const uploadFile = async ({ s3Client, fileName, key, contentType }) => {
  await s3Client
    .upload({
      ACL: "public-read",
      Body: fs.createReadStream(fileName),
      Bucket: process.env.S3_CACHE_BUCKET,
      ContentType: contentType,
      Key: `content/${key}`,
    })
    .promise()
}

const markAsSuccess = async ({ contentURL, mime, size, dzi }) => {
  const type = "success"
  await axios.put(
    `${contentURL}/completion`,
    {
      type,
      mime,
      size,
      dzi,
    },
    {
      auth: {
        username: process.env.ZH_API_USERNAME,
        password: process.env.ZH_API_PASSWORD,
      },
    }
  )
  log("content completed", { type, contentURL, mime, size, dzi })
}

const markAsFailure = async ({ contentURL, error }) => {
  const type = "failure"
  await axios.put(
    `${contentURL}/completion`,
    { type, error },
    {
      auth: {
        username: process.env.ZH_API_USERNAME,
        password: process.env.ZH_API_PASSWORD,
      },
    }
  )
  log("content completed", { type, contentURL, error })
}

const parseDZI = async (xmlString) => {
  const dzi = await xml2js.parseStringPromise(xmlString)
  const image = dzi.Image["$"]
  const size = dzi.Image.Size[0]["$"]
  return {
    width: parseInt(size.Width, 10),
    height: parseInt(size.Height, 10),
    tileFormat: image.Format,
    tileOverlap: parseInt(image.Overlap, 10),
    tileSize: parseInt(image.TileSize, 10),
  }
}

// Renames `Format="jpeg"` to `Format="jpg"` for backwards compatibility
// See: https://github.com/lovell/sharp/issues/1206
const fixDZITileFormat = async (xmlString) => {
  const dzi = await xml2js.parseStringPromise(xmlString)
  const format = dzi.Image["$"].Format
  const fixedFormat = format === "jpeg" ? "jpg" : format
  dzi.Image["$"].Format = fixedFormat

  return new xml2js.Builder().buildObject(dzi)
}
