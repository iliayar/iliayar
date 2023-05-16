let toggleLightBtn = document.querySelector('#toggle-light-btn')

function updateLightMode() {
    var is_dark = localStorage.theme === 'dark' || (!('theme' in localStorage) && window.matchMedia('(prefers-color-scheme: dark)').matches)

    if (is_dark) {
	toggleLightBtn.innerText = 'Go Light'
	document.documentElement.classList.add('dark')
    } else {
	toggleLightBtn.innerText = 'Go Dark'
	document.documentElement.classList.remove('dark')
    }
}

window.addEventListener('load', (event) => {
    updateLightMode()
    hljs.highlightAll({showLanguageLabel: true})
})

toggleLightBtn.addEventListener('click', (event) => {
    if (localStorage.theme === 'light') {
	localStorage.theme = 'dark'
    } else {
	localStorage.theme = 'light'
    }

    updateLightMode()
})

// function getElementsByTagNames(list,obj) {
//     if (!obj) var obj = document;
//     var tagNames = list.split(',');
//     var resultArray = new Array();
//     for (var i=0;i < tagNames.length;i++) {
// 	var tags = obj.getElementsByTagName(tagNames[i]);
// 	for (var j=0;j < tags.length;j++) {
// 	    resultArray.push(tags[j]);
// 	}
//     }
//     var testNode = resultArray[0];
//     if (!testNode) return [];
//     if (testNode.sourceIndex) {
// 	resultArray.sort(function (a,b) {
// 	    return a.sourceIndex - b.sourceIndex;
// 	});
//     }
//     else if (testNode.compareDocumentPosition) {
// 	resultArray.sort(function (a,b) {
// 	    return 3 - (a.compareDocumentPosition(b) & 6);
// 	});
//     }
//     return resultArray;
// }

// function createTOC() {
//     // Add go to top button
//     let top = document.createElement('a');
//     top.innerHTML = 'Top';
//     top.href = '#top';
//     top.className += 'top %s';

//     // The tags with the headlines
//     let headlines = getElementsByTagNames('%s');

//     if (headlines.length < 2) return false;

//     // Populate the #toc div
//     let toc = document.getElementById('toc');

//     // The title
//     // let title = document.getElementById('toc-link-title');
//     // let tocTitle = document.createElement('a');
//     // tocTitle.className = 'px-2 py-1 ';

//     // Set the classes to be used by the title
//     let title_classes = ' %s %s';

//     // Header counts for numbering
//     let header_2 = 0;
//     let header_3 = 0;
//     let header_4 = 0;
//     let header_5 = 0;
//     let header_6 = 0;

//     for (var i=0;i < headlines.length;i++) {

// 	let tocHeader = document.createElement('a');

// 	// Tailwind.css classes
// 	tocHeader.className = 'px-2 py-1';
// 	tocHeader.className += ' %s';

// 	// Add the header to the table of contents
// 	toc.appendChild(tocHeader);

// 	switch (headlines[i].nodeName) {
// 	case 'H1':
// 	    tocHeader.className += title_classes;

// 	    break;
// 	case 'H2':
// 	    tocHeader.className += ' block mt-2 ml-0';
// 	    header_2 += 1;
// 	    header_3 = 0;
// 	    header_4 = 0;
// 	    header_5 = 0;
// 	    header_6 = 0;
// 	    header_7 = 0;

// 	    // Numbering
// 	    tocHeader.innerHTML += '<b>' + header_2 + '.</b> ';
	    
// 	    break;

// 	case 'H3':
// 	    tocHeader.className += ' block ml-5';
// 	    header_3 += 1;
// 	    header_4 = 0;
// 	    header_5 = 0;
// 	    header_6 = 0;
// 	    header_7 = 0;

// 	    // Numbering
// 	    tocHeader.innerHTML += '<b>' + header_2 + '.' + header_3 + '.</b> ';
// 	    break;

// 	case 'H4':
// 	    tocHeader.className += ' block ml-12';
// 	    header_4 += 1;
// 	    header_5 = 0;
// 	    header_6 = 0;
// 	    header_7 = 0;

// 	    // Numbering
// 	    tocHeader.innerHTML += '<b>' + header_2 + '.' + header_3 + '.' + header_4 + '.</b> ';
// 	    break;

// 	case 'H5':
// 	    tocHeader.className += ' block ml-20';
// 	    header_5 += 1;
// 	    header_6 = 0;
// 	    header_7 = 0;

// 	    // Numbering
// 	    tocHeader.innerHTML += '<b>' + header_2 + '.' + header_3 + '.' + header_4 + '.' + header_5 + '.</b> ';
// 	    break;

// 	case 'H6':
// 	    tocHeader.className += ' block ml-32';
// 	    header_6 += 1;
// 	    header_7 = 0;

// 	    // Numbering
// 	    tocHeader.innerHTML += '<b>' + header_2 + '.' + header_3 + '.' + header_4 + '.' + header_5 + '.' + header_6 + '.</b> ';
// 	    break;

// 	case 'H7':
// 	    tocHeader.className += ' block ml-48';
// 	    header_7 += 1;

// 	    // Numbering
// 	    tocHeader.innerHTML += '<b>' + header_2 + '.' + header_3 + '.' + header_4 + '.' + header_5 + '.' + header_6 + '.' + header_7 + '.</b> ';
// 	    break;
// 	}
	
// 	// Header title
// 	tocHeader.innerHTML += headlines[i].innerHTML;
	
// 	// Add a link to the header
// 	let headerId = headlines[i].id || 'toc-link-' + i;
// 	headlines[i].id = headerId;
// 	tocHeader.id = \"goto-\" + headerId;
// 	tocHeader.href = '#' + headerId;
//     }
// }

// createTOC();


// // Populate search bar
// let searchBar = document.getElementById('search-bar')
// let searchBarResults = document.getElementById('search-bar-results')
// for(let i = 0; i < tocTree.length; i++) {
//     let heading = tocTree[i]
//     let item = document.createElement('li')
//     let link = document.createElement('a')
//     link.href = heading.file + '#toc-link-' + heading.index
//     link.className = \"%s\"
//     link.style.display = 'none'

//     if (heading.name === heading.parent) {
// 	link.innerText = heading.name
//     } else {
// 	link.innerText = heading.name + ' > ' + heading.parent
//     }

//     item.appendChild(link)
//     searchBarResults.appendChild(item)
// }


// // Show results on search bar focus
// function showResults(){
//     searchBarResults.style.display = ''
// }


// function hideResults(){
//     searchBarResults.style.display = 'none'
//     searchBar.value = ''
// }


// // Search Bar
// function search(){
//     let searchBar = document.getElementById('search-bar')
//     let filter = searchBar.value.toUpperCase()
//     let count = 0

//     let searchResults = searchBarResults.getElementsByTagName('li')

//     for(let i = 0; i < searchResults.length; i++) {
// 	let link = searchResults[i].getElementsByTagName('a')[0]
// 	let txtValue = link.textContent || link.innerText

// 	if (txtValue.toUpperCase().indexOf(filter) > -1 && count < 10) {
// 	    link.style.display = ''
// 	    count += 1
// 	} else {
// 	    link.style.display = 'none'
// 	}
//     }
// }


// // Check if an element is visible
// function isElementVisible (el, parent) {
//     let rect = el.getBoundingClientRect();

//     return (
// 	rect.top >= parent.getBoundingClientRect().top &&
// 	    rect.left >= 0 &&
// 	    rect.bottom <= (parent.clientHeight + parent.getBoundingClientRect().top) &&
// 	    rect.right <= (parent.clientWidth)
//     );
// }


// DOMTokenList.prototype.addMany = function(classes) {
//     var array = classes.split(' ');
//     for (var i = 0, length = array.length; i < length; i++) {
// 	this.add(array[i]);
//     }
// }


// DOMTokenList.prototype.removeMany = function(classes) {
//     var array = classes.split(' ');
//     for (var i = 0, length = array.length; i < length; i++) {
// 	this.remove(array[i]);
//     }
// }


// // Scroll Spy
// // Everything is run inside the function because offsets
// // are created due to tailwind.css classes as it changes
// // the height/position of the elements after the page is loaded
// function scrollSpy () {
//     let headings = document.querySelectorAll('[id^=\"toc-link-\"]');
//     let headingsTopOffsets = {};
//     let i = 0;

//     let selectedClassName = \"%s\";

//     Array.prototype.forEach.call(headings, function(s) {
// 	headingsTopOffsets[s.id] = s.offsetTop;
//     });
    
//     let scrollEl = document.getElementById(\"content-container\");
//     let scrollPosition = scrollEl.scrollTop + 150;

//     let sidebar = document.getElementById('sidebar');

//     for (i in headingsTopOffsets) {
// 	if (headingsTopOffsets[i] <= scrollPosition) {
// 	    document.getElementsByClassName(selectedClassName)[0].classList.removeMany(selectedClassName);
// 	    document.querySelector(`a[id^=\"goto-${i}\"]`).classList.addMany(selectedClassName);

// 	    // Scroll the sidebar to the current heading if not visible
// 	    let tocItem = document.getElementById(`goto-${i}`);
// 	    if (!isElementVisible(tocItem, sidebar)) {
// 		tocItem.scrollIntoView();
// 	    }
// 	}
//     }
// };
