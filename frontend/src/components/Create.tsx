import * as React from "react"

const UploadIcon = ({ className }) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    className={`h-5 w-5 ${className}`}
    viewBox="0 0 20 20"
    fill="currentColor"
  >
    <path
      fillRule="evenodd"
      d="M3 17a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM6.293 6.707a1 1 0 010-1.414l3-3a1 1 0 011.414 0l3 3a1 1 0 01-1.414 1.414L11 5.414V13a1 1 0 11-2 0V5.414L7.707 6.707a1 1 0 01-1.414 0z"
      clipRule="evenodd"
    />
  </svg>
)

const LinkIcon = ({ className }) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    className={`h-5 w-5 ${className}`}
    viewBox="0 0 20 20"
    fill="currentColor"
  >
    <path
      fillRule="evenodd"
      d="M12.586 4.586a2 2 0 112.828 2.828l-3 3a2 2 0 01-2.828 0 1 1 0 00-1.414 1.414 4 4 0 005.656 0l3-3a4 4 0 00-5.656-5.656l-1.5 1.5a1 1 0 101.414 1.414l1.5-1.5zm-5 5a2 2 0 012.828 0 1 1 0 101.414-1.414 4 4 0 00-5.656 0l-3 3a4 4 0 105.656 5.656l1.5-1.5a1 1 0 10-1.414-1.414l-1.5 1.5a2 2 0 11-2.828-2.828l3-3z"
      clipRule="evenodd"
    />
  </svg>
)

export const Create = () => (
  <div className="rounded-lg bg-gray-800 p-8 mt-4 shadow-md">
    <form className="grid gap-5">
      <h2 className="text-gray-200 text-center text-3xl font-semibold">
        Create your own zoomable image
      </h2>
      <div className="inline-grid w-full grid-cols-2 gap-2">
        <button
          type="button"
          className="
            font-semibold
            inline-flex
            items-center
            justify-center
            px-4
            py-3
            uppercase
            bg-gray-400
            hover:bg-gray-600
            text-sm
            rounded-lg
            shadow-lg
            cursor-pointer
            text-gray-800
            hover:text-white

            outline-none
            focus:ring-4
            focus:ring-gray-300
            focus:ring-opacity-90
          "
        >
          <LinkIcon className="mr-1" />
          Post image link
        </button>

        <label
          className="
            font-semibold
            inline-flex
            items-center
            justify-center
            px-4
            py-3
            uppercase
            bg-gray-400
            hover:bg-gray-600
            text-sm
            rounded-lg
            shadow-lg
            cursor-pointer
            text-gray-800
            hover:text-white

            outline-none
            focus:ring-4
            focus:ring-gray-300
            focus:ring-opacity-90
          "
        >
          <UploadIcon className="mr-1" />
          Upload image
          <input type="file" className="hidden" />
        </label>
      </div>

      <div className="grid gap-1">
        <label className="text-gray-50 inline-flex items-center text-sm">
          <svg
            xmlns="http://www.w3.org/2000/svg"
            className="h-5 w-5 mr-1 inline-block"
            viewBox="0 0 20 20"
            fill="currentColor"
          >
            <path d="M2.003 5.884L10 9.882l7.997-3.998A2 2 0 0016 4H4a2 2 0 00-1.997 1.884z" />
            <path d="M18 8.118l-8 4-8-4V14a2 2 0 002 2h12a2 2 0 002-2V8.118z" />
          </svg>
          Give us your email and we’ll let you know when your image is ready{" "}
          <sup>★</sup>
        </label>
        <input
          type="text"
          className="
            w-full
            px-4
            py-3
            rounded
            placeholder-gray-400
            text-center
            outline-none
            focus:ring-4
            focus:ring-orange-500
            focus:ring-opacity-90
            text-lg
            font-medium
          "
          placeholder="you@example.com"
        ></input>
      </div>

      <div>
        <button
          type="button"
          className="
            w-full
            font-semibold
            inline-flex
            items-center
            justify-center
            px-4
            py-3
            uppercase
            bg-orange-500
            hover:bg-orange-600
            rounded-lg
            shadow-lg
            cursor-pointer
            text-gray-800
            hover:text-white
            text-lg

            outline-none
            focus:ring-4
            focus:ring-orange-500
            focus:ring-opacity-90
          "
        >
          Create
          <svg
            xmlns="http://www.w3.org/2000/svg"
            className="h-5 w-5 ml-1"
            viewBox="0 0 20 20"
            fill="currentColor"
          >
            <path
              fillRule="evenodd"
              d="M10.293 15.707a1 1 0 010-1.414L14.586 10l-4.293-4.293a1 1 0 111.414-1.414l5 5a1 1 0 010 1.414l-5 5a1 1 0 01-1.414 0z"
              clipRule="evenodd"
            />
            <path
              fillRule="evenodd"
              d="M4.293 15.707a1 1 0 010-1.414L8.586 10 4.293 5.707a1 1 0 011.414-1.414l5 5a1 1 0 010 1.414l-5 5a1 1 0 01-1.414 0z"
              clipRule="evenodd"
            />
          </svg>
        </button>

        <div className="text-gray-500 text-sm mt-3">
          <sup>★</sup> We won’t spam you or share your email — promise!
        </div>
      </div>
    </form>
  </div>
)
