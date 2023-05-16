module.exports = {
    darkMode: "class",
    content: [
	"/publish/output/**/*.{html,js}"
    ],
    theme: {
	extend: {
	    colors: {
		midgray: "#2B3033",
		darkgray: "#1E1E1E"
	    }
	},
    },
    variants: {
	extend: {
	    backgroundColor: ["checked"],
	    borderColor: ["checked"]
	}
    },
    plugins: [],
}
