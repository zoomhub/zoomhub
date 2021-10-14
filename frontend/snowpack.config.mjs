export default {
  mount: {
    public: "/",
    src: "/dist",
    styles: "/styles",
  },
  devOptions: {
    tailwindConfig: "./tailwind.config.js",
  },
  plugins: ["@snowpack/plugin-postcss"],
}
