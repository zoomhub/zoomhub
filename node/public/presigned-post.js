async function uploadViaPresignedPost(api, file) {
  // Get presigned POST URL and form fields
  let response = await fetch(api + "signed-post");
  let json = await response.json();

  // Build a form for the request body
  let form = new FormData();
  Object.keys(json.data.fields).forEach((key) =>
    form.append(key, json.data.fields[key])
  );
  form.append("file", file);

  // Send the POST request
  // response = await fetch(json.data.url, { method: "POST", body: form });
  const uploadResponse = await fetch(
    // HACK: For CORS to work, we need HTTP based direct bucket URL:
    "http://uploads-development.zoomhub.net.s3.amazonaws.com",
    { method: "POST", body: form }
  );
  if (!uploadResponse.ok) {
    return "Failed to upload via presigned POST";
  }

  const successMetadata = await uploadResponse.json();

  return `
    <p>File uploaded via presigned POST with key: ${json.id}</p>
    <p>JSON response: <code>${JSON.stringify(
      successMetadata,
      null,
      2
    )}</code></p>
  `;
}
