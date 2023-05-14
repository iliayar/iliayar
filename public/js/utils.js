function toggleLight() {
  if (localStorage.theme === 'light') {
    localStorage.theme = 'dark'
  } else {
    localStorage.theme = 'light'
  }
  location.reload();
  return false;
}

window.addEventListener('load', (event) => {
    if (localStorage.theme === 'dark' || (!('theme' in localStorage) && window.matchMedia('(prefers-color-scheme: dark)').matches)) {
	document.documentElement.classList.add('dark')
    } else {
	document.documentElement.classList.remove('dark')
    }

    hljs.highlightAll({showLanguageLabel: true})
})

// Scroll Spy
// Everything is run inside the function because offsets
// are created due to tailwind.css classes as it changes
// the height/position of the elements after the page is loaded
function scrollSpy () {
  let headings = document.querySelectorAll('[id^=\"toc-link-\"]');
  let headingsTopOffsets = {};
  let i = 0;

  let selectedClassName = \"%s\";

  Array.prototype.forEach.call(headings, function(s) {
    headingsTopOffsets[s.id] = s.offsetTop;
  });
  
  let scrollEl = document.getElementById(\"content-container\");
  let scrollPosition = scrollEl.scrollTop + 150;

  let sidebar = document.getElementById('sidebar');

  for (i in headingsTopOffsets) {
    if (headingsTopOffsets[i] <= scrollPosition) {
      document.getElementsByClassName(selectedClassName)[0].classList.removeMany(selectedClassName);
      document.querySelector(`a[id^=\"goto-${i}\"]`).classList.addMany(selectedClassName);

      // Scroll the sidebar to the current heading if not visible
      let tocItem = document.getElementById(`goto-${i}`);
      if (!isElementVisible(tocItem, sidebar)) {
          tocItem.scrollIntoView();
      }
    }
  }
};
