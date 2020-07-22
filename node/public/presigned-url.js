async function uploadViaPresignedUrl(api, file) {
  // Get presigned URL
  let response = await fetch(api + "signed-url");
  let json = await response.json();

  // Read the file for the request body
  let data = await readFile(file);

  // Send the PUT request
  response = await fetch(json.data.url, { method: "PUT", body: data });
  if (!response.ok) return "Failed to upload via presigned URL";

  // Done!
  return `File uploaded via presigned URL with key: ${json.id}`;
}

async function readFile(file) {
  // Wrap the read in a Promise so it can be awaited
  return new Promise((resolve) => {
    // A FileReader is used to read the file :)
    let reader = new FileReader();

    // The load event fires when the file is fully read
    reader.addEventListener("load", () => {
      resolve(reader.result);
    });

    // Start reading the file
    reader.readAsArrayBuffer(file);
  });
}
