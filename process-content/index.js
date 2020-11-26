const AmazonS3URI = require("amazon-s3-uri")
const AWS = require("aws-sdk")
const axios = require("axios")
const FileType = require("file-type")
const fs = require("fs")
const path = require("path")
const pLimit = require("p-limit")
const readdir = require("recursive-readdir")
const rmfr = require("rmfr")
const sharp = require("sharp")

const ROOT_PATH = "/tmp"
const TILE_FORMAT = {
  jpg: "image/jpeg",
  png: "image/png",
}

const s3Client = new AWS.S3({ apiVersion: "2006-03-01" })
const limit = pLimit(10)

exports.handler = async (event) => {
  if (!event.url) {
    return {
      status: 400,
      body: "Please specify a URL to process",
    }
  }

  const content = await fetchContent({ url: event.url })
  if (!content.id) {
    return {
      status: 400,
      body: "Invalid content",
    }
  }

  const contentId = parseContentId(content.id)
  if (!contentId) {
    return {
      status: 400,
      body: `Invalid content ID: ${contentId}`,
    }
  }

  const url = content.url
  const s3URL = parseS3URL(url)
  log("meta", { contentId, url, s3URL })

  const body = await (s3URL
    ? fetchS3URL({ ...s3URL, s3Client })
    : fetchGenericURL({ url }))

  const outputPath = `${ROOT_PATH}/${contentId}`
  log("output", { outputPath })

  // Clean up output directory
  await Promise.all([
    rmfr(outputPath),
    rmfr(`${outputPath}_files`),
    rmfr(`${outputPath}.dzi`),
  ])

  // Write source file
  await fs.promises.writeFile(outputPath, body)

  const fileType = await FileType.fromFile(outputPath)
  const isPNG = fileType && fileType.mime === "image/png"
  const tileFormat = isPNG ? { id: "png" } : { id: "jpg", quality: 90 }
  log("file meta", { fileType, isPNG, tileFormat })

  await sharp(outputPath)
    .toFormat(tileFormat)
    .tile({
      depth: "onepixel",
      layout: "dz",
      overlap: 1,
      size: 254,
    })
    .toFile(`${outputPath}.dz`)

  await uploadDZI({
    basePath: outputPath,
    s3Client,
    tileFormat,
  })
  log("DZI uploaded")
}

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

const fetchContent = async ({ url }) => (await axios.get(url)).data
const fetchS3URL = async ({ s3Client, bucket, key }) =>
  (await s3Client.getObject({ Bucket: bucket, Key: key }).promise()).Body
const fetchGenericURL = async ({ url }) =>
  (await axios.get(url, { responseType: "arraybuffer" })).data

const uploadDZI = async ({ s3Client, basePath, tileFormat }) => {
  const tileFileNames = await readdir(`${basePath}_files`)
  const manifestFileName = `${basePath}.dzi`
  log("DZI", { manifestFileName, tileFileNames })

  const tileOperations = tileFileNames.map((fileName) =>
    limit(() =>
      uploadFile({
        s3Client,
        fileName,
        key: path.relative(ROOT_PATH, fileName),
        contentType: TILE_FORMAT[tileFormat.id],
      })
    )
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
      Bucket: process.env["S3_CACHE_BUCKET"],
      ContentType: contentType,
      Key: `content/${key}`,
    })
    .promise()

  log("file uploaded", { fileName, key, contentType })
}
