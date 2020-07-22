"use strict";

const AWS = require("aws-sdk");
const uuid = require("uuid");

const BUCKET_NAME = process.env["BUCKET_NAME"];
const MAX_UPLOAD_SIZE = 512 * 1024 * 1024; // 512MB

const sign = async () => {
  const s3 = new AWS.S3({
    region: "us-east-2",
  });

  // NOTE: Hoping this reduces the chance of overwrites.
  let id = uuid.v4();
  const key = `uploads/${id}`;

  let params = {
    Bucket: BUCKET_NAME,
    Fields: {
      key,
      success_action_redirect: "http://localhost:3000/success",
    },
    Expires: 300,
    Conditions: [["content-length-range", 0, MAX_UPLOAD_SIZE], { key }],
  };

  let data = await createPresignedPostPromise(s3, params);

  return {
    statusCode: 200,
    body: JSON.stringify({ id, data }),
    headers: {
      "Access-Control-Allow-Origin": "*",
    },
  };
};

function createPresignedPostPromise(s3, params) {
  return new Promise((resolve, reject) => {
    s3.createPresignedPost(params, (err, data) => {
      if (err) reject(err);
      else resolve(data);
    });
  });
}

// ---

const express = require("express");
const cors = require("cors");

const app = express();
const port = 3000;

app.use(cors());
app.use(express.static("public"));
app.get("/signed-post", async (req, res) => {
  const data = await sign();
  return res.send(data.body);
});

app.get("/success", async (req, res) => {
  return res.json({ query: req.query });
});

app.listen(port, () =>
  console.log(`Example app listening at http://localhost:${port}`)
);
