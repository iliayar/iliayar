hljs.highlightAll({showLanguageLabel: true})

let toggleLightBtn = document.querySelector('#toggle-light-btn')
let hljsCssLight = document.querySelector(`link[title="hljs-light"]`)
let hljsCssDark = document.querySelector(`link[title="hljs-dark"]`)


function updateLightMode() {
    var is_dark = localStorage.theme === 'dark' || (!('theme' in localStorage) && window.matchMedia('(prefers-color-scheme: dark)').matches)

    if (is_dark) {
	toggleLightBtn.innerText = 'go light'
	document.documentElement.classList.add('dark')
	hljsCssDark.removeAttribute('disabled')
	hljsCssLight.setAttribute('disabled', 'disabled')
    } else {
	toggleLightBtn.innerText = 'go dark'
	document.documentElement.classList.remove('dark')
	hljsCssLight.removeAttribute('disabled')
	hljsCssDark.setAttribute('disabled', 'disabled')
    }

}

window.addEventListener('load', (event) => {
    updateLightMode()
})

toggleLightBtn.addEventListener('click', (event) => {
    if (localStorage.theme === 'light') {
	localStorage.theme = 'dark'
    } else {
	localStorage.theme = 'light'
    }

    updateLightMode()
})

let toggleTocBtn = document.querySelector('#toggle-toc-btn')
let toc = document.querySelector('#toc')

function toggleToc() {
    var enabled = !toc.hasAttribute('hidden')

    if (enabled) {
	toc.setAttribute('hidden', '')
    } else {
	toc.removeAttribute('hidden')
    }
}

if (toggleTocBtn) {
    toggleTocBtn.addEventListener('click', (event) => {
	toggleToc()
    })
}
