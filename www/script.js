// plot-editor/www/script.js

(function() {
	function updateMenuState(activeTab) {
		var items = document.querySelectorAll('.sidebar-menu li');
		items.forEach(function(li) { li.classList.remove('disabled'); });
		// Always keep Grid and Export enabled
		var gridItem   = document.querySelector('.sidebar-menu a[data-value="grid"]');
		var exportItem = document.querySelector('.sidebar-menu a[data-value="export"]');
		// Disable Text/Theme/Axes if active tab is Grid; otherwise disable Grid
		if (activeTab === 'Grid') {
			['text','theme','axes'].forEach(function(name) {
				var el = document.querySelector('.sidebar-menu a[data-value="' + name + '"]');
				if (el && el.parentElement) el.parentElement.classList.add('disabled');
			});
		} else {
			var gridEl = document.querySelector('.sidebar-menu a[data-value="grid"]');
			if (gridEl && gridEl.parentElement) gridEl.parentElement.classList.add('disabled');
		}
	}
	
	// Observe Shiny tabset changes via mutation observer (fallback to input binding would need R init)
	var obs = new MutationObserver(function() {
		var active = document.querySelector('#active_tabset .active a');
		if (active && active.getAttribute('data-value')) {
			updateMenuState(active.getAttribute('data-value'));
		}
	});
	document.addEventListener('DOMContentLoaded', function() {
		updateMenuState('Grid');
		var tabset = document.getElementById('active_tabset');
		if (tabset) {
			obs.observe(tabset, { childList: true, subtree: true, attributes: true });
		}
	});
})();