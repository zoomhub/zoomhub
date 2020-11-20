const AmazonS3URI = require("amazon-s3-uri")
const AWS = require("aws-sdk")
const axios = require("axios")
const FileType = require("file-type")
const fs = require("fs")
const path = require("path")
const PQueue = require("p-queue").default
const readdir = require("recursive-readdir")
const rmfr = require("rmfr")
const sharp = require("sharp")

const ROOT_PATH = "/tmp"
const TILE_FORMAT = {
  jpg: "image/jpeg",
  png: "image/png",
}

const s3Client = new AWS.S3({ apiVersion: "2006-03-01" })

exports.handler = async (event, context) => {
  if (!event.contentId) {
    return "Please specify a contentID to process"
  }
  if (!event.url) {
    return "Please specify a URL to process"
  }

  const contentId = event.contentId
  const url = event.url
  const s3URL = parseS3URL(url)

  console.log("meta", { contentId, url, s3URL })

  const body = await (s3URL
    ? fetchS3URL({ ...s3URL, s3Client })
    : fetchGenericURL({ url }))

  const outputPath = `${ROOT_PATH}/${contentId}`
  console.log("outputPath:", outputPath)

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
  console.log("file meta:", { fileType, isPNG, tileFormat })

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
  const queue = new PQueue({ concurrency: 25 })

  // tiles
  const tileFileNames = await readdir(`${basePath}_files`)
  tileFileNames.forEach((fileName) =>
    queue.add(() =>
      uploadFile({
        s3Client,
        fileName,
        key: path.relative(ROOT_PATH, fileName),
        contentType: TILE_FORMAT[tileFormat.id],
      })
    )
  )

  // manifest
  const dziFileName = `${basePath}.dzi`
  queue.add(() =>
    uploadFile({
      s3Client,
      fileName: dziFileName,
      key: path.relative(ROOT_PATH, dziFileName),
      contentType: "application/xml",
    })
  )
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
  console.log("File uploaded:", { fileName, key, contentType })
}
