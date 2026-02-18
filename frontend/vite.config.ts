import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],
  server: {
    port: 8080,
  },
  css: {
    postcss: './postcss.config.js',
  },
  build: {
    manifest: true,
    rollupOptions: {
      input: {
        index: 'index.html',
        globalStyles: 'src/styles/global.css',
      },
    },
  },
})