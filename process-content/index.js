const AmazonS3URI = require("amazon-s3-uri")
const AWS = require("aws-sdk")
const axios = require("axios")
const checkDiskSpace = require("check-disk-space").default
const ClientError = require("./lib/ClientError")
const FileType = require("file-type")
const fs = require("fs")
const mkdirp = require("mkdirp")
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

const TMPDIR = process.env.TMPDIR || "/tmp"
const TILE_SIZE = parseInt(process.env.TILE_SIZE, 10) || 254
const VIPS_DISC_THRESHOLD = process.env.VIPS_DISC_THRESHOLD
const NUM_CONCURRENT_UPLOADS =
  parseInt(process.env.NUM_CONCURRENT_UPLOADS, 10) || 10

const s3Client = new AWS.S3({ apiVersion: "2006-03-01" })
const limit = pLimit(NUM_CONCURRENT_UPLOADS)

exports.handler = async ({ contentURL }, context) => {
  log("start", {
    contentURL,
    context: {
      functionName: context.functionName,
      functionVersion: context.functionVersion,
      invokedFunctionArn: context.invokedFunctionArn,
      memoryLimitInMB: context.memoryLimitInMB,
    },
    config: {
      NUM_CONCURRENT_UPLOADS,
      ROOT_PATH,
      TILE_SIZE,
      TMPDIR,
      VIPS_DISC_THRESHOLD,
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
    throw new ClientError("Invalid content: Missing ID")
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

  await logTime("createTMPDIR", () => mkdirp(TMPDIR), { TMPDIR })

  // Clean up output directory
  // According to SO, `/tmp` is not shared between different invocations:
  // https://stackoverflow.com/a/37990409
  const rootFiles = await logTime("readRootFiles", () =>
    fs.promises.readdir(ROOT_PATH)
  )
  const rootDiskSpace = await logTime("checkDiskSpace", () =>
    checkDiskSpace(ROOT_PATH)
  )
  log("root info", { rootFiles, rootDiskSpace })

  const sourceURL = content.url
  const outputPath = `${ROOT_PATH}/${contentId}`

  try {
    await processContentUnsafe({ contentId, contentURL, sourceURL, outputPath })
  } finally {
    // Always delete all output to prevent stale files incurring storage costs:
    await logTime("post:cleanOutputPath", () => deleteOutput(outputPath), {
      outputPath,
    })
  }
}

const processContentUnsafe = async ({
  contentId,
  contentURL,
  sourceURL,
  outputPath,
}) => {
  await logTime("pre:cleanOutputPath", () => deleteOutput(outputPath), {
    outputPath,
  })

  const s3URL = parseS3URL(sourceURL)
  log("meta", { contentId, contentURL, sourceURL })

  const body = await logTime(
    "fetchFile",
    () =>
      s3URL ? fetchS3URL({ ...s3URL, s3Client }) : fetchGenericURL(sourceURL),
    { source: s3URL ? "s3" : "generic" }
  )

  await logTime(
    "writeSourceFile",
    () => fs.promises.writeFile(outputPath, body),
    { outputPath }
  )

  const fileType = await FileType.fromFile(outputPath)
  const isPNG = fileType && fileType.mime === "image/png"
  const tileFormat = isPNG ? { id: "png" } : { id: "jpg", quality: 90 }
  log("file meta", { fileType, isPNG, tileFormat })

  await logTime("createDZI", () =>
    sharp(outputPath, { limitInputPixels: false })
      .rotate() // auto-rotate based on EXIF
      .toFormat(tileFormat)
      .tile({
        depth: "onepixel",
        layout: "dz",
        overlap: 1,
        size: TILE_SIZE,
      })
      .toFile(`${outputPath}.dz`)
  )

  const dziXMLString = await fs.promises.readFile(`${outputPath}.dzi`)
  const fixedDZIXMLString = await fixDZITileFormat(dziXMLString)
  await fs.promises.writeFile(`${outputPath}.dzi`, fixedDZIXMLString)

  const [fileSize, dzi] = await Promise.all([
    (await fs.promises.stat(outputPath)).size,
    await parseDZI(fixedDZIXMLString),
  ])
  log("DZI meta", { fileSize, dzi })

  await logTime("uploadDZI", () =>
    uploadDZI({
      basePath: outputPath,
      s3Client,
      tileFormat,
    })
  )

  const mime = fileType && fileType.mime
  const size = fileSize
  log("pre:marking", { contentURL, mime, size, dzi })
  return markAsSuccess({ contentURL, mime, size, dzi })
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

const logTime = async (message, action, props = {}) => {
  log(`start:${message}`)
  const start = Date.now()
  const result = await action()
  log(`end:${message}`, {
    ...props,
    duration: { value: Date.now() - start, unit: "ms" },
  })
  return result
}

const fetchContent = async (url) => (await axios.get(url)).data
const fetchS3URL = async ({ s3Client, bucket, key }) =>
  (await s3Client.getObject({ Bucket: bucket, Key: key }).promise()).Body
const fetchGenericURL = async (url) =>
  (await axios.get(url, { responseType: "arraybuffer" })).data

const uploadDZI = async ({ s3Client, basePath, tileFormat }) => {
  const tileFileNames = await readdir(`${basePath}_files`)
  const numTiles = tileFileNames.length
  const manifestFileName = `${basePath}.dzi`
  log("DZI", { manifestFileName, numTiles })

  const tileOperations = tileFileNames.map((fileName, index) =>
    limit(() => {
      const baseKey = path.relative(ROOT_PATH, fileName)
      const key =
        tileFormat.id === "jpg" ? replaceExt(baseKey, ".jpg") : baseKey
      return uploadFile({
        index,
        numTiles,
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

const uploadFile = async ({
  index,
  numTiles,
  s3Client,
  fileName,
  key,
  contentType,
}) => {
  const fullKey = `content/${key}`
  await s3Client
    .upload({
      ACL: "public-read",
      Body: fs.createReadStream(fileName),
      Bucket: process.env.S3_CACHE_BUCKET,
      ContentType: contentType,
      Key: fullKey,
    })
    .promise()

  if (
    typeof numTiles === "number" &&
    typeof index === "number" &&
    (index + 1) % 100 == 0
  ) {
    log("DZI: tile uploaded", {
      key: fullKey,
      contentType,
      index: index + 1,
      numTiles,
    })
  }
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

const deleteOutput = (outputPath) =>
  Promise.all([
    rmfr(outputPath),
    rmfr(`${outputPath}_files`),
    rmfr(`${outputPath}.dzi`),
  ])
