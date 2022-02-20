export default {
  mount: {
    "frontend/public": "/",
    "frontend/src": "/scripts",
    "frontend/styles": "/styles",
    src: "/_haskell",
  },
  buildOptions: {
    out: "./frontend/build",
  },
  devOptions: {
    output: "stream", // don't clear terminal
    tailwindConfig: "./tailwind.config.js",
  },
  plugins: ["@snowpack/plugin-postcss"],
}
