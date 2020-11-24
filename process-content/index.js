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
  if (!event.contentId) {
    return "Please specify a contentID to process"
  }
  if (!event.url) {
    return "Please specify a URL to process"
  }

  const contentId = event.contentId
  const url = event.url
  const s3URL = parseS3URL(url)

  console.log({ message: "meta", contentId, url, s3URL })

  const body = await (s3URL
    ? fetchS3URL({ ...s3URL, s3Client })
    : fetchGenericURL({ url }))

  const outputPath = `${ROOT_PATH}/${contentId}`
  console.log({ outputPath })

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
  console.log({ message: "file meta", fileType, isPNG, tileFormat })

  await sharp(outputPath)
    .toFormat(tileFormat)
    .tile({
      depth: "onepixel",
      layout: "dz",
      overlap: 1,
      size: 254,
    })
    .toFile(`${outputPath}.dz`)

  await uploadDZI({ s3Client, basePath: outputPath, tileFormat })
  console.log({ message: "DZI uploaded" })
}

const parseS3URL = (url) => {
  try {
    return AmazonS3URI(url)
  } catch (error) {
    // ignore errors
    return null
  }
}

const fetchS3URL = async ({ s3Client, bucket, key }) =>
  (await s3Client.getObject({ Bucket: bucket, Key: key }).promise()).Body

const fetchGenericURL = async ({ url }) =>
  (await axios.get(url, { responseType: "arraybuffer" })).data

const uploadDZI = async ({ s3Client, basePath, tileFormat }) => {
  const tileFileNames = await readdir(`${basePath}_files`)
  const manifestFileName = `${basePath}.dzi`
  console.log({ manifestFileName, tileFileNames })

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

  return await Promise.all([...tileOperations, dziManifestOperation])
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

  console.log({ message: "file uploaded", fileName, key, contentType })
}
