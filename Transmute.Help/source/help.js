const headerHeight = 55;
let isHighlightDisabled = true;

function scrollPastHeader() {
	window.scrollTo(0, window.scrollY - headerHeight);
}

function disableHighlight() {
	isHighlightDisabled = true;
}

function setColorScheme(colorScheme) {
	if (document && document.body && document.body.parentElement) {
		document.body.parentElement.dataset['scheme'] = colorScheme;
	}
}

function selectDemo(lexiconFile, rulesFile) {
	invokeCSharpAction({
		type: 'loadSample',
		lexiconFile: lexiconFile,
		rulesFile: rulesFile
	});
}

window.addEventListener('hashchange', () => {
	scrollPastHeader();
	disableHighlight();
	
	invokeCSharpAction({
		type: 'highlightHeading',
		href: document.location.href
	});
});

const { protocol, host, pathname } = document.location;

const headingsObserver = new IntersectionObserver(
	(entries, observer) => {
		if (isHighlightDisabled) {
			isHighlightDisabled = false;
			return;
		}

		const sortedEntries = Array.from(entries)
			.filter(e => e.isIntersecting)
			.toSorted((a, b) => a.target.offsetTop - b.target.offsetTop);

		let delay = 0;
			
		for (const e of sortedEntries) {
			setTimeout(
				() => invokeCSharpAction({
					type: 'highlightHeading',
					href: `${protocol}/${host}${pathname}#${e.target.id}`
				}),
				delay);

			delay += 25;
		}
	},
	{
		index: -1,
		root: document,
		rootMargin: '0px',
		scrollMargin: '0px',
		threshold: 1.0
	});

window.addEventListener('load', () => {
	if (document.location.hash) {
		scrollPastHeader();
	}
	
	for (const h of document.querySelectorAll('h1, h2, h3, h4, h5, h6')) {
		headingsObserver.observe(h);
	}
});
