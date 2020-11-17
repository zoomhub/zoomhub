const AmazonS3URI = require("amazon-s3-uri")
const AWS = require("aws-sdk")
const axios = require("axios")
const FileType = require("file-type")
const fs = require("fs").promises
const sharp = require("sharp")
const uuid = require("uuid")

exports.handler = async (event, context) => {
  if (!event.url) {
    return "Please specify a URL to process"
  }

  const url = event.url
  console.log("url:", url)

  const s3URL = parseS3URL(url)
  console.log("s3URL:", s3URL)

  const body = await (s3URL ? fetchS3URL(s3URL) : fetchGenericURL({ url }))

  console.log("typeof body", typeof body)

  const outputPath = `/tmp/${uuid.v4()}`
  console.log("outputPath:", outputPath)

  await fs.writeFile(outputPath, body)

  const fileType = await FileType.fromFile(outputPath)
  console.log("fileType:", fileType)

  const isPNG = fileType && fileType.mime === "image/png"
  const tileFormat = isPNG ? { id: "png" } : { id: "jpg", quality: 90 }

  console.log("tileFormat:", tileFormat)

  await sharp(outputPath)
    .toFormat(tileFormat)
    .tile({
      depth: "onepixel",
      layout: "dz",
      overlap: 1,
      size: 254,
    })
    .toFile(`${outputPath}.dz`)
  const dziXML = await fs.readFile(`${outputPath}.dzi`, "utf8")

  // output.dzi is the Deep Zoom XML definition
  // output_files contains 512x512 tiles grouped by zoom level
  console.log(dziXML)
}

const parseS3URL = (url) => {
  try {
    return AmazonS3URI(url)
  } catch (error) {
    return null
  }
}

const fetchS3URL = async ({ bucket, key }) => {
  const s3 = new AWS.S3({ apiVersion: "2006-03-01" })
  const result = await s3.getObject({ Bucket: bucket, Key: key }).promise()
  return result.Body
}

const fetchGenericURL = async ({ url }) => {
  const response = await axios.get(url, { responseType: "arraybuffer" })
  return response.data
}
