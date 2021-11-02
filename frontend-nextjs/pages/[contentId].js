import { useRouter } from "next/router"
import { useEffect, useState } from "react"

const Post = () => {
  const router = useRouter()
  const { contentId } = router.query

  const [content, setContent] = useState(null)
  useEffect(async () => {
    if (typeof contentId !== "string") {
      return
    }
    await fetch(`https://api.zoomhub.net/v1/content/${contentId}`)
      .then((_) => _.json())
      .then(setContent)
  }, [contentId])

  useEffect(() => {
    if (typeof contentId !== "string" || typeof document === "undefined") {
      return
    }

    const script = document.createElement("script")

    script.src = `https://www.zoomhub.net/${contentId}.js?id=viewer&width=auto&height=100px`
    script.async = true

    document.body.appendChild(script)

    return () => document.body.removeChild(script)
  }, [contentId])

  return (
    <div>
      {typeof contentId !== "string" ? (
        <h1>Loading {contentId}…</h1>
      ) : (
        <div id="viewer" style={{ width: "100vw", height: "100vh" }}></div>
      )}
      {content && typeof content !== "undefined" ? (
        <pre
          style={{
            position: "fixed",
            bottom: 0,
            left: 0,
            right: 0,
            paddingLeft: "1rem",
            paddingRight: "1rem",
          }}
        >
          {content.embedHtml}
        </pre>
      ) : null}
    </div>
  )
}

export default Post
