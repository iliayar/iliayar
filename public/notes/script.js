function paginate() {
  document.getElementById('style').setAttribute('href', 'style-paginate.css');
   document.getElementById('script').setAttribute('href', 'script-paginate.css');
}

document.addEventListener('DOMContentLoaded', () => {
    var blocks = [
	{'class': '.theorem'
	 , 'gen_prefix': counter => "<b>Теорема " + counter + ". </b>"
	 , 'gen_content': content => content
	},
	{'class': '.lemma'
	 , 'gen_prefix': counter => "<b>Лемма " + counter + ". </b>"
	 , 'gen_content': content => "<i>" + content + "</i>"
	},
	{'class': '.proof'
	 , 'gen_prefix': counter => "<i>Доказательство. </i>"
	 , 'gen_content': content => content
	},
	{'class': '.examp'
	 , 'gen_prefix': counter => "<i>Пример. </i>"
	 , 'gen_content': content => content
	},
	{'class': '.remark'
	 , 'gen_prefix': counter => "<i>Замечание. </i>"
	 , 'gen_content': content => content
	},
	{'class': '.task'
	 , 'gen_prefix': counter => "<b>Задача " + counter + ". </b>"
	 , 'gen_content': content => content
	},
	{'class': '.solution'
	 , 'gen_prefix': counter => "<i>Решение. </i>"
	 , 'gen_content': content => content
	},
    ]
    blocks.forEach((rules) => {
	var blocks_list = document.querySelectorAll(rules.class);
	var counter = 1
	blocks_list.forEach((block) => {
	    block.innerHTML = rules.gen_prefix(counter) + rules.gen_content(block.innerHTML)
	    counter++;
	})
	
    })
})
