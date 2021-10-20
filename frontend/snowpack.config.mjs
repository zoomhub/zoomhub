export default {
  mount: {
    public: "/",
    src: "/scripts",
    styles: "/styles",
  },
  devOptions: {
    tailwindConfig: "./tailwind.config.js",
  },
  plugins: ["@snowpack/plugin-postcss"],
}
