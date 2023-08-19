module.exports = {
    darkMode: "class",
    content: [
	"/publish/output/**/*.{html,js}"
    ],
    plugins: [
	({ addBase, theme }) => {
	    addBase({
		'.scrollbar': {
		    scrollbarColor: `${theme('colors.secondary.light')} ${theme('colors.primary.light')}`,
		    scrollbarWidth: 'thick',
		},
		'.scrollbar-dark': {
		    scrollbarColor: `${theme('colors.secondary.dark')} ${theme('colors.primary.dark')}`,
		    scrollbarWidth: 'thick',
		},
		'.scrollbar::-webkit-scrollbar': {
		    height: '6px',
		    width: '6px',
		},
		'.scrollbar-dark::-webkit-scrollbar': {
		    height: '6px',
		    width: '6px',
		},
		'.scrollbar::-webkit-scrollbar-thumb': {
		    backgroundColor: theme('colors.secondary.light'),
		},
		'.scrollbar::-webkit-scrollbar-track-piece': {
		    backgroundColor: theme('colors.primary.light'),
		},
		'.scrollbar-dark::-webkit-scrollbar-thumb': {
		    backgroundColor: theme('colors.secondary.dark'),
		},
		'.scrollbar-dark::-webkit-scrollbar-track-piece': {
		    backgroundColor: theme('colors.primary.dark'),
		},
	    });
	},
    ],
    theme: {
	extend: {
	    colors: ({ colors }) => ({
		primary: {
		    light: colors.zinc[100],
		    dark: colors.neutral[900],
		},
		secondary: {
		    light: colors.gray[900],
		    dark: colors.gray[100],
		},
		code: {
		    dark: colors.lime[400],
		    light: colors.green[600],
		},
		'code-secondary': {
		    dark: colors.neutral[800],
		    light: colors.neutral[200],
		},
		'stat-cookie': {
		    dark: colors.yellow[400],
		    light: colors.amber[500],
		},
		'link-active': colors.orange[400],
		'link-inactive': {
		    dark: colors.orange[100],
		    light: colors.orange[900],
		},
		'table': {
		    dark: colors.neutral[800],
		    light: colors.slate[200],
		},
		'table-header': {
		    dark: colors.neutral[600],
		    light: colors.slate[400],
		},
		'code-secondary-fade': {
		    light: colors.zinc[300],
		    dark: colors.neutral[700],
		},
		'code-dim': {
		    dark: colors.lime[500],
		    light: colors.green[500],
		},
	    }),
	    spacing: {
		'header': '3rem',
		'sidebar': '350px',
	    }
	}
    }
}
