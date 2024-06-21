function updateLightMode() {
    let toggleLightBtn = document.querySelector('#toggle-light-btn')

    var is_dark = localStorage.theme === 'dark' || (!('theme' in localStorage) && window.matchMedia('(prefers-color-scheme: dark)').matches)

    if (is_dark) {
	    toggleLightBtn.innerText = 'dark'
	    document.documentElement.setAttribute('data-theme', 'dark')
    } else {
	    toggleLightBtn.innerText = 'light'
	    document.documentElement.setAttribute('data-theme', 'light')
    }

}

function setupTheme() {
    let toggleLightBtn = document.querySelector('#toggle-light-btn')
    
    toggleLightBtn.addEventListener('click', (event) => {
        if (localStorage.theme === 'light') {
    	    localStorage.theme = 'dark'
        } else {
    	    localStorage.theme = 'light'
        }
    
        updateLightMode()
    });

    updateLightMode();
}


function setupScrollSpy() {
  const TableOfContents = {
    container: document.querySelector('.toc'),
    links: null,
    headings: null,
    intersectionOptions: {
      rootMargin: '0px',
      threshold: 1
    },
    previousSection: null,
    observer: null,

    init() {
      if (!this.container) {
          return;
      }

      this.handleObserver = this.handleObserver.bind(this)

      this.setUpObserver()
      this.findLinksAndHeadings()
      this.observeSections()

    },

    handleObserver(entries, observer) {
      entries.forEach(entry => {
        let href = `#${entry.target.getAttribute('id')}`,
          link = this.links.find(l => l.getAttribute('href') === href)
        

        if (entry.isIntersecting && entry.intersectionRatio >= 1) {
          link.classList.add('is-visible')
          this.previousSection = entry.target.getAttribute('id')
        } else {
          link.classList.remove('is-visible')
        }

        this.highlightFirstActive()
      })
    },

    highlightFirstActive() {
      let firstVisibleLink = this.container.querySelector('.is-visible')

      this.links.forEach(link => {
        link.classList.remove('is-active')
      })

      if (firstVisibleLink) {
        firstVisibleLink.classList.add('is-active')
      }

      if (!firstVisibleLink && this.previousSection) {
        var entry = this.container.querySelector(
          `a[href="#${this.previousSection}"]`
        )
        entry.classList.add('is-active')
      }

      this.scrollTocToActive()
    },

    scrollTocToActive() {
        var active = this.container.querySelector('.is-active')
        if (active !== null) {
          var elems = Math.round(this.container.offsetHeight / active.offsetHeight)
          this.container.scroll({top: active.offsetTop - active.offsetHeight * (elems / 2)})
        }
    },

    observeSections() {
      this.headings.forEach(heading => {
        this.observer.observe(heading)
      })
    },

    setUpObserver() {
      this.observer = new IntersectionObserver(
        this.handleObserver,
        this.intersectionOptions
      )
    },

    findLinksAndHeadings() {
      this.links = [...this.container.querySelectorAll('a')]
      this.headings = this.links.map(link => {
        let id = link.getAttribute('href')
        return document.querySelector(id)
      })
    }
  }


  TableOfContents.init()
}

function renderMermaid() {
    blocks = document.querySelectorAll('pre.mermaid');
    for (block of blocks) {
        block.innerHTML = block.childNodes[0].innerHTML;
        block.classList.remove('block-code')
    }

    mermaid.init({})
}

window.addEventListener('load', (event) => {
    setupTheme()
    setupScrollSpy();
    renderMermaid();
})
